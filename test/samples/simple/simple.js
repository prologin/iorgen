"use strict";

/**
 * @param {number} n the first number
 * @param {number} otherNumber the second number
 * @returns {void}
 */
function simple(n, otherNumber) {
    /* TODO Just do what you want with these numbers, like sum them. */
}

function main(stdin) {
    let line = 0;

    const n = Number(stdin[line++]);
    const otherNumber = Number(stdin[line++]);
    simple(n, otherNumber);
}

let stdin = "";
process.stdin.on("data", data => stdin += data.toString())
             .on("end", () => main(stdin.split("\n")));
