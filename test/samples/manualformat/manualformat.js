"use strict";

/**
 * @param {number} a a first number
 * @param {number} b a second number
 * @param {number} c a third number
 * @param {number} n This one on a new line
 * @param {Array.<number>} onePerLine an integer list, one per line
 * @returns {void}
 */
function manualFormat(a, b, c, n, onePerLine) {
    /* TODO From the function perspective, this is just 4 integers */
}

function main(stdin) {
    let line = 0;

    const [a, b, c] = stdin[line++].split(" ").map(Number);
    const n = Number(stdin[line++]);
    const onePerLine = [];
    for (let i = 0; i < 3; i++) {
        const j = Number(stdin[line++]);
        onePerLine.push(j);
    }
    manualFormat(a, b, c, n, onePerLine);
}

let stdin = "";
process.stdin.on("data", data => stdin += data.toString())
             .on("end", () => main(stdin.split("\n")));
