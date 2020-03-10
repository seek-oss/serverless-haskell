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

const glibcPattern = /\bGLIBC_([0-9.]+)\b/gm;

// Parse output of objdump -T and return minimum glibc version required
function parseObjdumpOutput(output) {
    const versions = output.matchAll(glibcPattern).map(match => version.parse(match[1]));
    if (versions.length > 0) {
        return versions.reduce(version.max);
    } else {
        return null;
    }
}

module.exports.parseLdOutput = parseLdOutput;
module.exports.parseObjdumpOutput = parseObjdumpOutput;
