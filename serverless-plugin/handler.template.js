'use strict';

const { spawn, spawnSync } = require('child_process');
const net = require('net');

// Ensure the child processes can be started from and find libraries in the
// right directory
process.env['PATH'] = process.env['PATH'] + ':' +
    process.env['LAMBDA_TASK_ROOT'];

function wrapper(options) {
    const executable = options['executable'];
    const execArguments = options['arguments'];

    // Keep track of the port the background process is listening on
    let portPromise = null;

    // Start the backend process and track its running state
    function startBackend() {
        if (portPromise !== null) {
            // Process is already started, return the existing promise
            return portPromise;
        }

        portPromise = new Promise(function (resolve, reject) {
            const main = spawn('./' + executable, execArguments, {
                stdio: ['ignore', process.stdout, process.stdout, 'pipe'],
            });
            const communication = main.stdio[3];

            // Get the port number to connect to from the backend
            communication.on('data', portBuf => {
                let portStr = portBuf.toString();
                resolve(parseInt(portStr, 10));
            });

            main.on('exit', function (code) {
                if (portPromise) {
                    portPromise = null;

                    console.error("Child process exited with code " + code);
                }
            });

            main.on('error', function (err) {
                if (portPromise) {
                    portPromise = null;

                    console.error("Child process error: " + err);
                }
            });
        });

        return portPromise;
    }

    return function (event, context, callback) {
        // The process and its events survive the invocation. Make AWS return a
        // value immediately rather than time out waiting for all events to be
        // processed.
        context.callbackWaitsForEmptyEventLoop = false;

        // Ensure the backend is started
        startBackend().then(port => {
            // Keep track of the data sent via socket
            let output = '';

            // Open a new connection to the persistent process
            const client = net.createConnection({ port }, function () {
                // Accumulate the process output
                client.on('data', chunk => output += chunk);

                // Send the result back when it's ready
                client.on('end', function () {
                    try {
                        const result = JSON.parse(output);
                        callback(null, result);
                    } catch (err) {
                        console.error('child process output bad JSON: ' + output);
                        callback('child process output bad JSON: ' + output);
                    }
                });

                // Send the input to be processed
                client.end(JSON.stringify(event) + '\n', 'utf8');
            });
        });
    };
}

// exports such as below will be added here by the plugin
// exports['EXECUTABLENAME'] = wrapper({
//   executable: 'EXECUTABLENAME',
//   arguments: ['--arg1', '--arg2'],
// });
