'use strict';

import { spawnSync, SpawnSyncOptions, SpawnSyncReturns } from 'child_process';
import { chmodSync, copySync, removeSync, writeFileSync } from 'fs-extra';
import * as path from 'path';
import Serverless from 'serverless';
import Service from 'serverless/classes/Service';

import * as AWSEnvironment from './AWSEnvironment';
import * as config from './config';
import * as ld from './ld';
import * as version from './version';

const PACKAGE_NAME = 'serverless-haskell';

const ADDITIONAL_EXCLUDE = [
    '**/.stack-work/**',
    'node_modules/**',
];

// Dependent libraries not to suggest adding
const IGNORE_LIBRARIES = [
    'linux-vdso.so.1',
    '/lib64/ld-linux-x86-64.so.2',
].concat(AWSEnvironment.libraries);

const BOOTSTRAP = '#!/bin/sh\nexec ${_HANDLER}';

const NO_OUTPUT_CAPTURE: SpawnSyncOptions = {stdio: ['ignore', process.stdout, process.stderr]};
const OUTPUT_CAPTURE: SpawnSyncOptions = {maxBuffer: 1024 * 1024 * 100};

type Custom = {
    stackBuildArgs: string[];
    docker: boolean;
    buildAll: boolean;
};

// FIXME: Service is missing 'package' property in @types/serverless
type ServiceEx = Service & {
    package: {
        exclude: string[];
        excludeDevDependencies?: boolean;
    };
}

type Options = {
    function?: string;
};

class ProcessError extends Error {
    result: SpawnSyncReturns<Buffer>;
    constructor(message: string, result: SpawnSyncReturns<Buffer>) {
        super(message);
        this.result = result;
        Object.setPrototypeOf(this, new.target.prototype);
    }
}

class ServerlessPlugin {
    serverless: Serverless;
    service: ServiceEx;
    options: Options;
    hooks: { [hook: string]: (options: {}) => void };
    servicePath: string;
    additionalFiles: string[];

    constructor(serverless: Serverless, options: Options) {
        this.serverless = serverless;
        this.service = serverless.service as ServiceEx;
        this.options = options;

        this.hooks = {
            'before:package:createDeploymentArtifacts': this.buildHandlers.bind(this),
            'after:package:createDeploymentArtifacts': this.cleanupHandlers.bind(this),

            // deploy function
            'before:deploy:function:packageFunction': this.buildHandlers.bind(this),
            'after:deploy:function:packageFunction': this.cleanupHandlers.bind(this),

            // invoke local
            'before:invoke:local:invoke': this.buildHandlers.bind(this),
            'after:invoke:local:invoke': this.cleanupHandlers.bind(this),

            // serverless-offline
            'before:offline:start:init': this.buildHandlers.bind(this),
            'after:offline:start:end': this.cleanupHandlers.bind(this),
        };

        this.servicePath = this.serverless.config.servicePath || '';

        // By default, Serverless examines node_modules to figure out which
        // packages there are from dependencies versus devDependencies of a
        // package. While there will always be a node_modules due to Serverless
        // and this plugin being installed, it will be excluded anyway.
        // Therefore, the filtering can be disabled to speed up the process.
        this.service.package.excludeDevDependencies = false;

        this.additionalFiles = [];
    }

    custom(): Custom {
        return Object.assign(
            {
                stackBuildArgs: [],
                docker: true,
                buildAll: true,
            },
            this.serverless.service.custom &&
                this.serverless.service.custom.haskell ||
                {}
        );
    }

    runStack(directory: string, args: string[], options: {captureOutput?: boolean} = {}): SpawnSyncReturns<Buffer> {
        options = options || {};
        const envArgs = [];
        if (this.custom().docker) {
            envArgs.push('--docker');
            envArgs.push('--docker-image', config.BUILD_DOCKER_IMAGE);
            envArgs.push('--no-nix');
        }

        if (directory) {
            envArgs.push('--stack-yaml', `${directory}stack.yaml`);
        }

        const stackArgs = [
            ...envArgs,
            ...this.custom().stackBuildArgs,
            ...args,
        ];

        const result = spawnSync(
            'stack',
            stackArgs,
            options.captureOutput ? OUTPUT_CAPTURE : NO_OUTPUT_CAPTURE
        );

        if (result.error || result.status) {
            const reasons = [];
            if (result.error) {
                reasons.push(result.error);
            }
            if (result.status) {
                reasons.push(`exit code: ${result.status}`);
            }
            const stderr = result.stderr?.toString().trim();
            if (stderr) {
                reasons.push(stderr);
            }
            const message = `Error when running Stack: ${reasons.join('; ')}\n` +
                  `Stack command: stack ${stackArgs.join(" ")}`;
            throw new ProcessError(message, result);
        }

        return result;
    }

    runStackOutput(directory: string, args: string[]): string {
        const result = this.runStack(directory, args, {captureOutput: true});
        return result.stdout.toString().trim();
    }

    dependentLibraries(directory: string, executablePath: string): ld.Paths {
        try {
            const lddOutput = this.runStackOutput(
                directory,
                [
                    'exec',
                    'ldd',
                    executablePath,
                ]
            );
            return ld.parseLdOutput(lddOutput);
        }
        catch (error) {
            if (error.result &&
                error.result.stdout &&
                error.result.stdout.includes("not a dynamic executable")) {
                // Static executables have no dependencies
                return {};
            } else if (process.platform === 'darwin' && !this.custom().docker) {
                // Even if ldd was available on macOS, the dependencies won't
                // translate
                return {};
            } else {
                throw error;
            }
        }
    }

    glibcVersion(directory: string, executablePath: string): version.Version | null {
        const objdumpOutput = this.runStackOutput(
            directory,
            [
                'exec',
                'objdump',
                '--',
                '-T',
                executablePath,
            ]
        );
        return ld.parseObjdumpOutput(objdumpOutput);
    }

    assertServerlessPackageVersionsMatch(directory: string): void {
        // Check that the Haskell package version corresponds to our own
        const stackDependencies = this.runStackOutput(
            directory,
            [
                'ls',
                'dependencies',
            ]
        ).split("\n");
        const haskellPackageVersions = stackDependencies.filter(dep => dep.startsWith(`${PACKAGE_NAME} `));
        if (haskellPackageVersions.length === 0) {
            this.serverless.cli.log(`Could not find ${PACKAGE_NAME} in stack's dependencies. Make sure ${PACKAGE_NAME} you are using LTS 12 (or newer), or add it as an extra-dep in your stack.yaml, and reference it in package.yaml or the Cabal file.`);
            throw new Error("Package not found.");
        }
        const haskellPackageVersion = haskellPackageVersions[0].split(' ')[1];

        const javascriptPackageVersion = JSON.parse(spawnSync(
            'npm',
            [
                'list',
                PACKAGE_NAME,
                '--json',
            ]
        ).stdout).dependencies[PACKAGE_NAME].version;

        if (haskellPackageVersion !== javascriptPackageVersion) {
            this.serverless.cli.log(`Package version mismatch: serverless-haskell installed from NPM: ${javascriptPackageVersion}, installed from Stack: ${haskellPackageVersion}. Versions must be in sync to work correctly. Please install matching versions of serverless-haskell from NPM and Stack by either pinning your NPM version to match stack, or adding an extra-dep in your stack.yaml to match the NPM version.`);
            throw new Error("Package version mismatch.");
        }
    }

    writeBootstrap(): void {
        const bootstrapPath = path.resolve(this.servicePath, 'bootstrap');
        writeFileSync(bootstrapPath, BOOTSTRAP);
        chmodSync(bootstrapPath, 0o755);
        this.additionalFiles.push(bootstrapPath);
    }

    // Which functions are being deployed now - all (default) or only one of
    // them ('deploy function')
    deployedFunctions(): string[] {
        if (this.options.function) {
            return [this.options.function];
        } else {
            return this.serverless.service.getAllFunctions();
        }
    }

    buildHandlers(): void {
        const service = this.service;

        if (!this.custom().docker) {
            // Warn when Docker is disabled
            this.serverless.cli.log(
                "Warning: not using Docker to build. " +
                    "The resulting binary might not match the AWS environment.");
        }

        // Exclude Haskell artifacts from uploading
        service.package.exclude = service.package.exclude || [];
        service.package.exclude = [
            ...service.package.exclude,
            ...ADDITIONAL_EXCLUDE,
        ];

        // Keep track of which extra libraries were copied
        const libraries: { [name: string]: boolean } = {};

        let haskellFunctionsFound = false;

        this.deployedFunctions().forEach(funcName => {
            const func = service.getFunction(funcName);

            // Only process Haskell functions
            const runtime = func.runtime || service.provider.runtime;
            if (runtime !== config.HASKELL_RUNTIME) {
                return;
            }
            haskellFunctionsFound = true;
            service.getFunction(funcName).runtime = config.BASE_RUNTIME;

            const handlerPattern = /(.*\/)?([^./]*)\.(.*)/;
            const matches = handlerPattern.exec(func.handler);

            if (!matches) {
                throw new Error(`handler ${func.handler} was not of the form 'packageName.executableName' or 'dir1/dir2/packageName.executableName'.`);
            }

            const [, directory, packageName, executableName] = matches;

            // Ensure package versions match
            this.assertServerlessPackageVersionsMatch(directory);

            // Ensure the executable is built
            this.serverless.cli.log(`Building handler ${funcName} with Stack...`);
            const buildCommand = this.custom().buildAll ?
                ['build'] :
                ['build', `${packageName}:exe:${executableName}`];

            this.runStack(directory, buildCommand);

            // Copy the executable to the destination directory
            const stackInstallRoot = this.runStackOutput(
                directory,
                [
                    'path',
                    '--local-install-root',
                ]
            );
            const targetDirectory = directory ? directory : "./";
            const executablePath = path.resolve(stackInstallRoot, 'bin', executableName);
            const targetPath = path.resolve(this.servicePath, targetDirectory, executableName);
            copySync(executablePath, targetPath);
            this.additionalFiles.push(targetPath);
            service.getFunction(funcName).handler = targetDirectory + executableName;

            // Check glibc version
            const glibcVersion = this.glibcVersion(directory, executablePath);
            if (glibcVersion && version.greater(glibcVersion, AWSEnvironment.glibcVersion)) {
                this.serverless.cli.log(
                    "Warning: glibc version required by the executable (" + version.format(glibcVersion) + ") is " +
                        "higher than the one in AWS environment (" + version.format(AWSEnvironment.glibcVersion) + ").");
                throw new Error("glibc version mismatch.");
            }

            // Copy libraries not present on AWS Lambda environment
            const executableLibraries = this.dependentLibraries(directory, executablePath);

            for (const name in executableLibraries) {
                if (!libraries[name] && !IGNORE_LIBRARIES.includes(name)) {
                    const libPath = executableLibraries[name];
                    const libTargetPath = path.resolve(this.servicePath, name);
                    this.runStack(
                        directory,
                        [
                            'exec',
                            'cp',
                            libPath,
                            libTargetPath,
                        ]);
                    this.additionalFiles.push(libTargetPath);
                    libraries[name] = true;
                }
            }
        });

        if (!this.options.function && !haskellFunctionsFound) {
            throw new Error(
                `Error: no Haskell functions found. ` +
                `Use 'runtime: ${config.HASKELL_RUNTIME}' in global or ` +
                `function configuration to use this plugin.`
            );
        }

        this.writeBootstrap();

        // Ensure the runtime is set to a sane value for other plugins
        if (service.provider.runtime === config.HASKELL_RUNTIME) {
            service.provider.runtime = config.BASE_RUNTIME;
        }
    }

    cleanupHandlers(): void {
        this.additionalFiles.forEach(fileName => removeSync(fileName));
    }
}

module.exports = ServerlessPlugin;
