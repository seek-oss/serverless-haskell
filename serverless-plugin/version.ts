export type Version = number[];

export function parse(str: string): Version {
    return str.split('.').map(n => +n);
}

export function format(version: Version): string {
    return version.join('.');
}

// Compares two arrays as versions and returns:
// -1 if x < y
// 0 if x == y
// 1 if x > y
export function compare(x: Version, y: Version): number {
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

export function greater(x: Version, y: Version): boolean {
    return compare(x, y) > 0;
}

export function max(x: Version, y: Version): Version {
    return greater(x, y) ? x : y;
}
