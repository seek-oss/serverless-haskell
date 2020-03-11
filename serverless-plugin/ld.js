// Parse output of ldd and ldconfig

'use strict';

const version = require('./version');

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

// Parse output of objdump -T and return minimum glibc version required
function parseObjdumpOutput(output) {
    const glibcPattern = /\bGLIBC_([0-9.]+)\b/g;

    let maxVersion = null;
    let match;
    while (match = glibcPattern.exec(output)) {
        const thisVersion = version.parse(match[1]);
        if (!maxVersion || version.greater(thisVersion, maxVersion)) {
            maxVersion = thisVersion;
        }
    }
    return maxVersion;
}

module.exports.parseLdOutput = parseLdOutput;
module.exports.parseObjdumpOutput = parseObjdumpOutput;
