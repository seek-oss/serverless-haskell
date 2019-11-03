function parse(str) {
    return str.split('.').map(n => +n);
}

function format(version) {
    return version.join('.');
}

// Compares two arrays as versions and returns:
// -1 if x < y
// 0 if x == y
// 1 if x > y
function compare(x, y) {
    for (let i = 0; i < x.length && i < y.length; i++) {
        if (x[i] < y[i]) {
            return -1;
        }
        if (x[i] > y[i]) {
            return 1;
        }
    }
    if (x.length < y.length) {
        return -1;
    }
    if (x.length > y.length) {
        return 1;
    }
    return 0;
}

function greater(x, y) {
    return compare(x, y) > 0;
}

function max(x, y) {
    return greater(x, y) ? x : y;
}

module.exports.compare = compare;
module.exports.format = format;
module.exports.greater = greater;
module.exports.max = max;
module.exports.parse = parse;
