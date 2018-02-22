'use strict';

const { spawn } = require('child_process');
const net = require('net');

// The port used to communicate with the Haskell process
const PORT = 4275;

function wrapper(options) {
    const executable = options['executable'];
    const execArguments = options['arguments'];

    // Start a persistent process to handle the events
    process.env['PATH'] = process.env['PATH'] + ':' +
        process.env['LAMBDA_TASK_ROOT'];
    const main = spawn('./' + executable, execArguments, {
        stdio: ['ignore', process.stdout, process.stderr],
    });

    // TODO handle the exit of the persistent process

    return function (event, context, callback) {
        // Keep track of the process output
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
    };
}

// exports such as below will be added here by the plugin
// exports['EXECUTABLENAME'] = wrapper({
//   executable: 'EXECUTABLENAME',
//   arguments: ['--arg1', '--arg2'],
// });
