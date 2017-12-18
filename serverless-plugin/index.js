'use strict';

const {spawnSync} = require('child_process');
const fs = require('fs-extra');
const path = require('path');

const ADDITIONAL_EXCLUDE = [
    '.stack-work/**',
    'node_modules/**',
];

const TEMPLATE = path.resolve(__dirname, 'handler.template.js');

const SERVERLESS_DIRECTORY = '.serverless';

const NO_OUTPUT_CAPTURE = {stdio: ['ignore', process.stdout, process.stderr]};

class ServerlessPlugin {
    constructor(serverless, options) {
        this.serverless = serverless;
        this.options = options;

        this.hooks = {
            'before:package:createDeploymentArtifacts': this.beforeCreateDeploymentArtifacts.bind(this),
            'after:package:createDeploymentArtifacts': this.afterCreateDeploymentArtifacts.bind(this),
        };

        this.servicePath = this.serverless.config.servicePath || '';

        this.custom = Object.assign(
            {
                stackBuildArgs: [],
            },
            this.serverless.service.custom &&
                this.serverless.service.custom.haskell ||
                {}
        );

        this.stackArgs = [];
        if (process.platform !== 'linux') {
            // Use Stack's Docker build
            this.serverless.cli.log("Using Stack's Docker image.");
            this.runStack(['docker', 'pull']);
            this.stackArgs.push('--docker');
        }

        this.additionalFiles = [];
    }

    runStack(args, captureOutput) {
        return spawnSync(
            'stack',
            [
                ...args,
                ...this.stackArgs,
                ...this.custom.stackBuildArgs,
            ],
            captureOutput ? {} : NO_OUTPUT_CAPTURE
        );
    }

    beforeCreateDeploymentArtifacts() {
        const service = this.serverless.service;

        // Exclude Haskell artifacts from uploading
        service.package.exclude = service.package.exclude || [];
        service.package.exclude = [
            ...service.package.exclude,
            ...ADDITIONAL_EXCLUDE,
        ];

        let handledFunctions = {};

        service.getAllFunctions()
            .map(funcName => service.getFunction(funcName))
            .forEach((func) => {
                // Extract the executable name, assuming the second component is
                // 'main'
                const [ packageName, executableName ] = func.handler.split('.');

                //Ensure the executable is built
                this.serverless.cli.log("Building handler with Stack...");
                const res = this.runStack([
                    'build',
                    `${packageName}:exe:${executableName}`,
                ]);
                if (res.error || res.status > 0) {
                    this.serverless.cli.log("Stack build encountered an error.");
                    throw new Error(res.error);
                }

                // Copy the executable to the destination directory
                const stackInstallRoot = this.runStack(
                    [
                        'path',
                        '--local-install-root',
                    ],
                    true
                ).stdout.toString('utf8').trim();
                const haskellBinary = path.resolve(stackInstallRoot, 'bin', executableName);
                const haskellBinaryPath = path.resolve(this.servicePath, executableName);
                fs.copyFileSync(haskellBinary, haskellBinaryPath);
                this.additionalFiles.push(haskellBinaryPath);

                // Remember the executable that needs to be handled by this package's shim
                handledFunctions[packageName] = handledFunctions[packageName] || [];
                handledFunctions[packageName].push(executableName);
            });

        // Create a shim to start the executable and copy it to all the
        // destination directories
        const handlerTemplate = fs.readFileSync(TEMPLATE).toString('utf8');

        Object.keys(handledFunctions).forEach(packageName => {
            let handler = handlerTemplate + handledFunctions[packageName].map(
                executableName => `exports['${executableName}'] = wrapper('${executableName}');`
            ).join("\n") + "\n";

            const handlerFileName = path.resolve(this.servicePath, `${packageName}.js`);
            fs.writeFileSync(handlerFileName, handler);
            this.additionalFiles.push(handlerFileName);
        });
    }

    afterCreateDeploymentArtifacts() {
        this.additionalFiles.forEach(fileName => fs.removeSync(fileName));
    }
}

module.exports = ServerlessPlugin;
