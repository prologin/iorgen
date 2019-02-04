"use strict";
const fs = require("fs");

/**
 * @param {number} n the first list's size
 * @param {Array.<number>} listInt a list containing ints
 * @param {number} size an other size
 * @param {Array.<string>} listChar a list of char
 * @param {Array.<string>} listString4 a list of strings of size 4
 * @param {Array.<Array.<number>>} matrix a matrix of int
 * @returns {void}
 */
function lists(n, listInt, size, listChar, listString4, matrix) {
    /* TODO Aren't these lists beautifull? */
}

{
    const stdin = fs.readFileSync(0).toString().split("\n");
    let line = 0;

    const n = Number(stdin[line++]);
    const listInt = stdin[line++].split(" ", n).map(Number);
    const size = Number(stdin[line++]);
    const listChar = stdin[line++].split("");
    const listString4 = [];
    for (let i = 0; i < size; i++) {
        const j = stdin[line++];
        listString4.push(j);
    }
    const matrix = [];
    for (let i = 0; i < size; i++) {
        const j = stdin[line++].split(" ", size).map(Number);
        matrix.push(j);
    }
    lists(n, listInt, size, listChar, listString4, matrix);
}
