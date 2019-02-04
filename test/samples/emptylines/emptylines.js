"use strict";
const fs = require("fs");


/**
 * @param {Array.<number>} emptyList an empty list
 * @param {string} bufferString here to check correct parsing of empty line above
 * @param {number} n an integer, will be 0 in the sample input
 * @param {Array.<number>} emptyInSample an empty list (only in the sample)
 * @param {string} emptyString an empty string
 * @param {string} main an other buffer string
 * @param {Array.<string>} emptyCharList an empty char list
 * @param {Array.<string>} nonEmptyCharList an char list, non empty
 * @param {Object} structWithEmptyLine a struct containing an empty line, then a struct
 * @param {Object} aSizedStruct a sized struct containing an empty line
 * @param {string} finish a string to finish
 * @returns {void}
 */
function emptyLines(emptyList, bufferString, n, emptyInSample, emptyString, main, emptyCharList, nonEmptyCharList, structWithEmptyLine, aSizedStruct, finish) {
    /* TODO Wow, lots of empty lines! */
}

function main(stdin) {
    let line = 0;

    const emptyList = stdin[line++].split(" ", 0).map(Number);
    const bufferString = stdin[line++];
    const n = Number(stdin[line++]);
    const emptyInSample = stdin[line++].split(" ", n).map(Number);
    const emptyString = stdin[line++];
    const main = stdin[line++];
    const emptyCharList = stdin[line++].split("");
    const nonEmptyCharList = stdin[line++].split("");
    const structWithEmptyLine = {};
    structWithEmptyLine.listInStruct = stdin[line++].split(" ", n).map(Number);
    const words = stdin[line++].split(" ");
    structWithEmptyLine.structInStruct = {
        char1: words[0],
        int2: Number(words[1])
    };
    const aSizedStruct = {};
    aSizedStruct.size = Number(stdin[line++]);
    aSizedStruct.stringInStruct = stdin[line++];
    const finish = stdin[line++];
    emptyLines(emptyList, bufferString, n, emptyInSample, emptyString, main, emptyCharList, nonEmptyCharList, structWithEmptyLine, aSizedStruct, finish);
}

let stdin = "";
process.stdin.on("data", data => stdin += data.toString())
             .on("end", () => main(stdin.split("\n")));
