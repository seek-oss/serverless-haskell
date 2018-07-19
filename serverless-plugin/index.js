'use strict';

const {spawnSync} = require('child_process');
const fs = require('fs-extra');
const copyFileSync = require('fs-copy-file-sync');
const path = require('path');

const ld = require('./ld');

const PACKAGE_NAME = 'serverless-haskell';

const ADDITIONAL_EXCLUDE = [
    '**/.stack-work/**',
    'node_modules/**',
];

// Dependent libraries not to suggest adding
const IGNORE_LIBRARIES = [
    'linux-vdso.so.1',
    '/lib64/ld-linux-x86-64.so.2',
] + require('./aws_libraries');

const TEMPLATE = path.resolve(__dirname, 'handler.template.js');

const SERVERLESS_DIRECTORY = '.serverless';

const NO_OUTPUT_CAPTURE = {stdio: ['ignore', process.stdout, process.stderr]};

class ServerlessPlugin {
    constructor(serverless, options) {
        this.serverless = serverless;
        this.options = options;

        this.hooks = {
            'before:package:createDeploymentArtifacts': this.buildHandlers.bind(this),
            'after:package:createDeploymentArtifacts': this.cleanupHandlers.bind(this),

            // invoke local
            'before:invoke:local:invoke': this.buildHandlersLocal.bind(this),
            'after:invoke:local:invoke': this.cleanupHandlers.bind(this),

            // serverless-offline
            'before:offline:start:init': this.buildHandlersLocal.bind(this),
            'after:offline:start:end': this.cleanupHandlers.bind(this),
        };

        this.servicePath = this.serverless.config.servicePath || '';

        this.custom = Object.assign(
            {
                stackBuildArgs: [],
                arguments: {},
                docker: false,
            },
            this.serverless.service.custom &&
                this.serverless.service.custom.haskell ||
                {}
        );

        this.docker = {
            required: this.custom.docker || process.platform !== 'linux',
            haveImage: false,
        };

        // By default, Serverless examines node_modules to figure out which
        // packages there are from dependencies versus devDependencies of a
        // package. While there will always be a node_modules due to Serverless
        // and this plugin being installed, it will be excluded anyway.
        // Therefore, the filtering can be disabled to speed up the process.
        this.serverless.service.package.excludeDevDependencies = false;

        this.additionalFiles = [];
    }

    runStack(directory, args, options) {
        options = options || {};
        const envArgs = [];
        if (this.docker.required) {
            if (!this.docker.haveImage) {
                this.serverless.cli.log("Using Stack's Docker image.");
                spawnSync('stack', ['docker', 'pull'], NO_OUTPUT_CAPTURE);
                this.docker.haveImage = true;
            }
            envArgs.push('--docker');
            envArgs.push('--no-nix');
        }

        if (directory) {
            envArgs.push('--stack-yaml', `${directory}stack.yaml`);
        }

        const stackArgs = [
            ...envArgs,
            ...this.custom.stackBuildArgs,
            ...args,
        ];

        const result = spawnSync(
            'stack',
            stackArgs,
            options.captureOutput ? {} : NO_OUTPUT_CAPTURE
        );

        if (result.error || result.status > 0) {
            const message = `Error when running Stack: ${result.stderr}\n` +
                  `Stack command: stack ${stackArgs.join(" ")}`;
            const error = new Error(message);
            error.result = result;
            throw error;
        }

        return result;
    }

    runStackOutput(directory, args, options) {
        options = options || {};
        options.captureOutput = true;
        const result = this.runStack(directory, args, options);
        return result.stdout.toString('utf8').trim();
    }

    dependentLibraries(directory, executablePath) {
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
            } else {
                throw error;
            }
        }
    }

    assertServerlessPackageVersionsMatch(directory, packageName) {
        // Check that the Haskell package version corresponds to our own
        const haskellPackageVersion = this.runStackOutput(
            directory,
            [
                'exec', '--',
                'ghc-pkg',
                'field', PACKAGE_NAME, 'version',
                '--simple-output'
            ]
        );

        const javascriptPackageVersion = JSON.parse(spawnSync(
            'npm',
            [
                'list',
                PACKAGE_NAME,
                '--json',
            ]
        ).stdout)['dependencies'][PACKAGE_NAME]['version'];

        if (haskellPackageVersion != javascriptPackageVersion) {
            this.serverless.cli.log(`Package version mismatch: NPM: ${javascriptPackageVersion}, Stack: ${haskellPackageVersion}. Versions must be in sync to work correctly.`);
            throw new Error("Package version mismatch.");
        }
    }

    buildHandlerFileName(directory, packageName) {
        const fileName = `${packageName}.js`;

        return path.resolve(this.servicePath, directory, fileName);
    }

    writeHandlers(handlerOptions) {
        const handlerTemplate = fs.readFileSync(TEMPLATE).toString('utf8');

        for (const directory in handlerOptions) {
            for (const packageName in handlerOptions[directory]) {
                let handler = handlerTemplate + handlerOptions[directory][packageName].map(
                    ([executableName, options]) => `exports['${executableName}'] = wrapper(${JSON.stringify(options)});`
                ).join("\n") + "\n";
                const handlerFileName = this.buildHandlerFileName(directory, packageName);

                fs.writeFileSync(handlerFileName, handler);
                this.additionalFiles.push(handlerFileName);
            }
        }
    }

    addToHandlerOptions(handlerOptions, funcName, directory, packageName, executableName) {
        // Remember the executable that needs to be handled by this package's shim
        handlerOptions[directory] = handlerOptions[directory] || {};
        handlerOptions[directory][packageName] = handlerOptions[directory][packageName] || [];
        handlerOptions[directory][packageName].push([executableName, {
            executable: path.join(directory, executableName),
            arguments: this.custom.arguments[funcName] || [],
        }]);
    }

    buildHandlersLocal(options) {
        options = options || {};
        this.buildHandlers(Object.assign(options, {
            localRun: true
        }));
    }

    buildHandlers(options) {
        const service = this.serverless.service;

        options = options || {};
        if (options.localRun) {
            this.docker.required = false;
        }

        // Exclude Haskell artifacts from uploading
        service.package.exclude = service.package.exclude || [];
        service.package.exclude = [
            ...service.package.exclude,
            ...ADDITIONAL_EXCLUDE,
        ];

        // Each package will have its own wrapper; remember its options to add
        // to the handler template
        const handlerOptions = {};

        // Keep track of which extra libraries were copied
        const libraries = {};

        service.getAllFunctions().forEach(funcName => {
            const func = service.getFunction(funcName);
            const handlerPattern = /(.*\/)?([^\./]*)\.(.*)/;
            const matches = handlerPattern.exec(func.handler);

            if (!matches) {
                throw new Exception(`handler ${func.handler} was not of the form 'packageName.executableName' or 'dir1/dir2/packageName.executableName'.`);
            }

            const [_, directory, packageName, executableName] = matches;

            //Ensure the executable is built
            this.serverless.cli.log("Building handler with Stack...");
            const res = this.runStack(
                directory,
                ['build', `${packageName}:exe:${executableName}`]
            );

            this.assertServerlessPackageVersionsMatch(directory, packageName);

            // Copy the executable to the destination directory
            const stackInstallRoot = this.runStackOutput(
                directory,
                [
                    'path',
                    '--local-install-root',
                ]
            );
            const targetDirectory = directory ? directory : ".";
            const executablePath = path.resolve(stackInstallRoot, 'bin', executableName);
            const targetPath = path.resolve(this.servicePath, targetDirectory, executableName);
            copyFileSync(executablePath, targetPath);
            this.additionalFiles.push(targetPath);
            this.addToHandlerOptions(handlerOptions, funcName, targetDirectory, packageName, executableName);

            if (!options.localRun) {
                // Copy libraries not present on AWS Lambda environment
                const executableLibraries = this.dependentLibraries(directory, executablePath);

                for (const name in executableLibraries) {
                    if (!libraries[name] && !IGNORE_LIBRARIES.includes(name)) {
                        const libPath = executableLibraries[name];
                        const targetPath = path.resolve(this.servicePath, name);
                        this.runStack(
                            directory,
                            [
                                'exec',
                                'cp',
                                libPath,
                                targetPath,
                            ]);
                        this.additionalFiles.push(targetPath);
                        libraries[name] = true;
                    }
                }
            }
        });

        this.writeHandlers(handlerOptions);
    }

    cleanupHandlers(options) {
        this.additionalFiles.forEach(fileName => fs.removeSync(fileName));
    }
}

module.exports = ServerlessPlugin;
