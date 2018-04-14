'use strict';

const {spawnSync} = require('child_process');
const fs = require('fs-extra');
const copyFileSync = require('fs-copy-file-sync');
const path = require('path');

const PACKAGE_NAME = 'serverless-haskell';

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

    runStack(args, captureOutput) {
        const dockerArgs = [];
        if (this.docker.required) {
            if (!this.docker.haveImage) {
                this.serverless.cli.log("Using Stack's Docker image.");
                spawnSync('stack', ['docker', 'pull'], NO_OUTPUT_CAPTURE);
                this.docker.haveImage = true;
            }
            dockerArgs.push('--docker');
            dockerArgs.push('--no-nix');
        }
        return spawnSync(
            'stack',
            [
                ...args,
                ...dockerArgs,
                ...this.custom.stackBuildArgs,
            ],
            captureOutput ? {} : NO_OUTPUT_CAPTURE
        );
    }

    addFile(fileName, filePath) {
        const targetPath = path.resolve(this.servicePath, fileName);
        copyFileSync(filePath, targetPath);
        this.additionalFiles.push(targetPath);
    }

    beforeCreateDeploymentArtifacts() {
        const service = this.serverless.service;

        // Check that the Haskell package version corresponds to our own
        const haskellPackageVersions = this.runStack(
            [
                'list-dependencies',
                '--depth', '1',
            ],
            true
        ).stdout.toString('utf8').trim().split('\n')
              .reduce((packageDict, str) => {
                  let [packageName, version] = str.split(' ');
                  packageDict[packageName] = version;
                  return packageDict;
              }, {});
        const haskellPackageVersion =
              haskellPackageVersions[PACKAGE_NAME];

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
        this.custom.extraLibraries.forEach(lib => { libraries[lib] = false; });
        const foundLibraries = {};

        service.getAllFunctions().forEach(funcName => {
            const func = service.getFunction(funcName);

            // Extract the package and executable name
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
            handlerOptions[packageName] = handlerOptions[packageName] || [];
            handlerOptions[packageName].push([executableName, {
                executable: executableName,
                arguments: this.custom.arguments[funcName] || [],
            }]);

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
                    const [name, _, libPath] = s.trim().split(' ');
                    if (libraries[name] === false) {
                        const targetPath = path.resolve(this.servicePath, name);
                        this.runStack([
                            'exec',
                            'cp',
                            libPath,
                            targetPath,
                        ]);
                        this.additionalFiles.push(targetPath);
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

        Object.keys(handlerOptions).forEach(packageName => {
            let handler = handlerTemplate + handlerOptions[packageName].map(
                ([executableName, options]) => `exports['${executableName}'] = wrapper(${JSON.stringify(options)});`
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
