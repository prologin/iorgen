"use strict";

/**
 * @param {number} n the first list's size
 * @param {Array.<number>} listInt a list containing ints
 * @param {number} size an other size
 * @param {Array.<string>} listChar a list of char
 * @param {string} string a string
 * @param {Array.<string>} listString4 a list of strings of size 4
 * @param {Array.<Array.<string>>} listListString2 a list of list of strings of size 2 of size 2 of size 2
 * @param {Array.<Array.<number>>} matrix a matrix of int
 * @returns {void}
 */
function lists(n, listInt, size, listChar, string, listString4, listListString2, matrix) {
    /* TODO Aren't these lists beautifull? */
}

function main(stdin) {
    let line = 0;

    const n = Number(stdin[line++]);
    const listInt = stdin[line++].split(" ", n).map(Number);
    const size = Number(stdin[line++]);
    const listChar = stdin[line++].split("");
    const string = stdin[line++];
    const listString4 = [];
    for (let i = 0; i < size; i++) {
        const j = stdin[line++];
        listString4.push(j);
    }
    const listListString2 = [];
    for (let i = 0; i < 2; i++) {
        const j = [];
        for (let k = 0; k < 2; k++) {
            const l = stdin[line++];
            j.push(l);
        }
        listListString2.push(j);
    }
    const matrix = [];
    for (let i = 0; i < size; i++) {
        const j = stdin[line++].split(" ", size).map(Number);
        matrix.push(j);
    }
    lists(n, listInt, size, listChar, string, listString4, listListString2, matrix);
}

let stdin = "";
process.stdin.on("data", data => stdin += data.toString())
             .on("end", () => main(stdin.split("\n")));
