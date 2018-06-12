// Parse output of ldd and ldconfig

'use strict';

// Parse output of ldd or ldconfig and return a map of library names to paths
function parseLdOutput(output) {
    const libraryList = output.split('\n').filter(ln => ln.includes('=>'));

    const result = {};
    libraryList.forEach(s => {
        const [name, _, libPath] = s.trim().split(' ');
        result[name] = libPath;
    });

    return result;
}

module.exports.parseLdOutput = parseLdOutput;
