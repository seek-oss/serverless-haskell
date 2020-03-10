// Parse output of ldd and ldconfig

import * as version from './version';

export type Paths = { [path: string]: string };

// Parse output of ldd or ldconfig and return a map of library names to paths
export function parseLdOutput(output: string): Paths {
    const libraryList = output.split('\n').filter(ln => ln.includes('=>'));

    const result: Paths = {};
    libraryList.forEach(s => {
        const [name, _, libPath] = s.trim().split(' ');
        result[name] = libPath;
    });

    return result;
}

// Parse output of objdump -T and return minimum glibc version required
export function parseObjdumpOutput(output: string): version.Version | null {
    const glibcPattern = /\bGLIBC_([0-9.]+)\b/g;

    let maxVersion = null;
    let match;
    do {
        match = glibcPattern.exec(output);
        if (match) {
            const thisVersion = version.parse(match[1]);
            if (!maxVersion || version.greater(thisVersion, maxVersion)) {
                maxVersion = thisVersion;
            }
        }
    } while (match);
    return maxVersion;
}
