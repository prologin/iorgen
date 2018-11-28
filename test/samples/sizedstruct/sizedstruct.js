"use strict";
const fs = require("fs");

/**
 * @param {number} n the size of the lists
 * @param {Array.<Object>} lists a list of list of different sizes
 * @param {Array.<Object>} strings a list of strings of different sizes
 * @param {Array.<Object>} matrices a list of matrices of different sizes
 * @param {Array.<Object>} same a list of list of same sizes
 * @returns {void}
 */
function sizedStruct(n, lists, strings, matrices, same) {
    /* TODO The is a special case. */
}

{
    const stdin = fs.readFileSync("/dev/stdin").toString().split("\n");
    let line = 0;

    const n = Number(stdin[line++]);
    const lists = [];
    for (let i = 0; i < n; i++) {
        const j = {};
        j.size1 = Number(stdin[line++]);
        j.intList = stdin[line++].split(" ", j.size1).map(Number);
        lists.push(j);
    }
    const strings = [];
    for (let i = 0; i < n; i++) {
        const j = {};
        j.size2 = Number(stdin[line++]);
        j.stringList = stdin[line++];
        strings.push(j);
    }
    const matrices = [];
    for (let i = 0; i < 2; i++) {
        const j = {};
        j.size3 = Number(stdin[line++]);
        j.listList = [];
        for (let k = 0; k < j.size3; k++) {
            const l = stdin[line++].split(" ", 2).map(Number);
            j.listList.push(l);
        }
        matrices.push(j);
    }
    const same = [];
    for (let i = 0; i < n; i++) {
        const j = {};
        j.size4 = Number(stdin[line++]);
        j.intListN = stdin[line++].split(" ", n).map(Number);
        same.push(j);
    }
    sizedStruct(n, lists, strings, matrices, same);
}
