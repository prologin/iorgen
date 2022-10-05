"use strict";

/**
 * @param {number} f a float
 * @param {number} g a float, greater than f
 * @param {{x: number, y: number, z: number}} point some coordinates
 * @param {number} n a number
 * @param {Array.<number>} floatList a list of floats
 * @param {Array.<number>} otherList a list of floats
 * @param {Array.<{integer: number, char: string, float: number}>} inlined some inlined structs
 * @param {{'integer 2': number, string: string, 'float 2': number}} multiline a multiline struct
 * @returns {void}
 */
function floats(f, g, point, n, floatList, otherList, inlined, multiline) {
    /* TODO Parsing is often easy, reprint mode is harder */
}

function main(stdin) {
    let line = 0;

    const f = Number(stdin[line++]);
    const g = Number(stdin[line++]);
    const words = stdin[line++].split(" ");
    const point = {
        x: Number(words[0]),
        y: Number(words[1]),
        z: Number(words[2])
    };
    const n = Number(stdin[line++]);
    const floatList = stdin[line++].split(" ", n).map(Number);
    const otherList = stdin[line++].split(" ", 9).map(Number);
    const inlined = [];
    for (let i = 0; i < 3; i++) {
        const words1 = stdin[line++].split(" ");
        const j = {
            integer: Number(words1[0]),
            char_: words1[1],
            float_: Number(words1[2])
        };
        inlined.push(j);
    }
    const multiline = {};
    multiline.integer2 = Number(stdin[line++]);
    multiline.string = stdin[line++];
    multiline.float2 = Number(stdin[line++]);
    floats(f, g, point, n, floatList, otherList, inlined, multiline);
}

let stdin = "";
process.stdin.on("data", data => stdin += data.toString())
             .on("end", () => main(stdin.split("\n")));
