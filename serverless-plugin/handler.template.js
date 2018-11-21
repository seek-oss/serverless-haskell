'use strict';

const { spawn } = require('child_process');
const net = require('net');

function wrapper(options) {
    const executable = options['executable'];
    const execArguments = options['arguments'];
    return async function (event, context) {
        process.env['PATH'] = process.env['PATH'] + ':' +
            process.env['LAMBDA_TASK_ROOT'];

        // Keep track of the output result
        let output = '';
        const server = net.createServer(function(socket) {
            socket.on('data', chunk => output += chunk);
        });

        const address = await new Promise(
            resolve =>
                server.listen({port: 0, host: "127.0.0.1"}, () => resolve(server.address())));
        const port = address.port;

        process.env['SERVERLESS_HASKELL_COMMUNICATION_PORT'] = `${port}`;

        const main = spawn('./' + executable, execArguments, {
            stdio: ['pipe', process.stdout, process.stderr],
        });
        const stdin = main.stdin;

        // Send the event to the process
        stdin.end(JSON.stringify(event) + '\n', 'utf8');

        let exited = false;

        const result = new Promise((resolve, reject) => {

            main.on('exit', function (code) {
                if (!exited) {
                    exited = true;
                    if (code == 0) {
                        try {
                            const result = JSON.parse(output);
                            resolve(result);
                        } catch (err) {
                            reject('child process output bad JSON: ' + output);
                        }
                    }
                    else {
                        reject('child process exited with code ' + code);
                    }
                }
            });

            main.on('error', function (err) {
                if (!exited) {
                    exited = true;
                    reject(err, 'child process exited with error: ' + err);
                }
            });
        });

        return await result;
    };
}

// exports such as below will be added here by the plugin
// exports['EXECUTABLENAME'] = wrapper({
//   executable: 'EXECUTABLENAME',
//   arguments: ['--arg1', '--arg2'],
// });
