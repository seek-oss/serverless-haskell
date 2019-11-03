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
    const glibcPrefix = 'GLIBC_';
    const glibcReferences = output.split('\n').flatMap(
        ln => ln.split(/\s+/).filter(p => p.indexOf(glibcPrefix) === 0));
    const versions = glibcReferences.map(s => version.parse(s.substring(glibcPrefix.length)));
    if (versions.length > 0) {
        return versions.reduce(version.max);
    } else {
        return null;
    }
}

module.exports.parseLdOutput = parseLdOutput;
module.exports.parseObjdumpOutput = parseObjdumpOutput;
