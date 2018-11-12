"use strict";
const fs = require("fs");

/**
 * @param {number} n the first number
 * @param {number} otherNumber the second number
 * @returns {void}
 */
function simple(n, otherNumber) {
    /* TODO Just do what you want with these numbers, like sum them. */
}

{
    const stdin = fs.readFileSync("/dev/stdin").toString().split("\n");
    let line = 0;

    const n = Number(stdin[line++]);
    const otherNumber = Number(stdin[line++]);
    simple(n, otherNumber);
}
