"use strict";

/**
 * @param {number} if_ not a condition
 * @param {string} class_ not a class
 * @param {string} i just a string
 * @param {Object} in_ not in
 * @param {Array.<number>} for_ not a loop
 * @param {Array.<Object>} words contains lots of things
 * @returns {void}
 */
function keywords(if_, class_, i, in_, for_, words) {
    /* TODO If this compiles, it is already a good step! */
}

function main(stdin) {
    let line = 0;

    const if_ = Number(stdin[line++]);
    const class_ = stdin[line++];
    const i = stdin[line++];
    const words1 = stdin[line++].split(" ");
    const in_ = {
        a: Number(words1[0]),
        static_: Number(words1[1])
    };
    const for_ = stdin[line++].split(" ", if_).map(Number);
    const words = [];
    for (let j = 0; j < 2; j++) {
        const k = {};
        k.int_ = {};
        k.int_.return_ = Number(stdin[line++]);
        k.int_.void_ = stdin[line++].split(" ", 3).map(Number);
        k.ifTrue = Number(stdin[line++]);
        words.push(k);
    }
    keywords(if_, class_, i, in_, for_, words);
}

let stdin = "";
process.stdin.on("data", data => stdin += data.toString())
             .on("end", () => main(stdin.split("\n")));
