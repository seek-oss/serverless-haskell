'use strict';

const {spawnSync} = require('child_process');
const fs = require('fs-extra');
const path = require('path');

const ADDITIONAL_EXCLUDE = [
    '.stack-work/**',
    'node_modules/**',
];

// Dependent libraries not to suggest adding
const IGNORE_LIBRARIES = [
    'linux-vdso.so.1',
    '/lib64/ld-linux-x86-64.so.2',
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
                extraLibraries: [],
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

    addFile(fileName, filePath) {
        const targetPath = path.resolve(this.servicePath, fileName);
        fs.copyFileSync(filePath, targetPath);
        this.additionalFiles.push(targetPath);
    }

    beforeCreateDeploymentArtifacts() {
        const service = this.serverless.service;

        // Exclude Haskell artifacts from uploading
        service.package.exclude = service.package.exclude || [];
        service.package.exclude = [
            ...service.package.exclude,
            ...ADDITIONAL_EXCLUDE,
        ];

        const handledFunctions = {};

        // Keep track of which extra libraries were copied
        const libraries = {};
        this.custom.extraLibraries.forEach(lib => { libraries[lib] = false; });
        const foundLibraries = {};

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
                const executablePath = path.resolve(stackInstallRoot, 'bin', executableName);
                this.addFile(executableName, executablePath);

                // Remember the executable that needs to be handled by this package's shim
                handledFunctions[packageName] = handledFunctions[packageName] || [];
                handledFunctions[packageName].push(executableName);

                // Copy specified extra libraries, if needed
                if (this.custom.extraLibraries.length > 0) {
                    const lddOutput = this.runStack(
                        [
                            'exec',
                            'ldd',
                            executablePath,
                        ],
                        true
                    ).stdout.toString('utf8');
                    const lddList = lddOutput.trim().split('\n');
                    lddList.forEach(s => {
                        const [name, _, path] = s.trim().split(' ');
                        if (libraries[name] === false) {
                            this.addFile(name, path);
                            libraries[name] = true;
                        }
                        foundLibraries[name] = true;
                    });
                }
            });

        // Error if a requested extra library could not be found
        Object.keys(libraries).forEach(name => {
            if (!libraries[name]) {
                const msg = `Extra library not found: ${name}.`;
                this.serverless.cli.log(msg);
                // Show the libraries found, in case the name was misspelled
                const foundLibrariesStr = Object.keys(foundLibraries)
                      .filter(l => !IGNORE_LIBRARIES.includes(l))
                      .sort()
                      .join(", ");
                this.serverless.cli.log(
                    `Dependent libraries found: ${foundLibrariesStr}`);
                throw new Error(msg);
            }
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
