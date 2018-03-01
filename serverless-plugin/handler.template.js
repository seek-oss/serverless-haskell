'use strict';

const { spawn, spawnSync } = require('child_process');
const net = require('net');

// The port used to communicate with the Haskell process
const PORT = 4275;

// Ensure the child processes can be started from and find libraries in the
// right directory
process.env['PATH'] = process.env['PATH'] + ':' +
    process.env['LAMBDA_TASK_ROOT'];

function wrapper(options) {
    const executable = options['executable'];
    const execArguments = options['arguments'];

    // Keep track of whether the backend process is started
    let running = false;

    // Start the backend process and update its running state as needed
    function startBackend() {
        return new Promise(function (resolve, reject) {
            if (running) {
                // FIXME: Wait for actual startup?
                resolve();
            } else {
                const main = spawn('./' + executable, execArguments, {
                    stdio: ['ignore', 'pipe', process.stdout],
                });
                // FIXME: Track running and available separately
                running = true;

                // Assume the backend is listening as soon as output is received
                main.stdout.on('data', outputBuf => {
                    let output = outputBuf.toString();
                    resolve();
                });

                main.on('exit', function (code) {
                    if (running) {
                        running = false;

                        console.error("Child process exited with code " + code);
                    }
                });

                main.on('error', function (err) {
                    if (running) {
                        running = false;

                        console.error("Child process error: " + err);
                    }
                });
            }
        });
    }

    return function (event, context, callback) {
        // The process and its events survive the invocation. Make AWS return a
        // value immediately rather than time out waiting for all events to be
        // processed.
        context.callbackWaitsForEmptyEventLoop = false;

        // Ensure the backend is started
        startBackend().then(() => {
            // Keep track of the data sent via socket
            let output = '';

            // Open a new connection to the persistent process
            const client = net.createConnection({ port: PORT }, function () {
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
