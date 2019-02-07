"use strict";

/**
 * @param {number} n a number, used as a size
 * @param {Array.<{integer: number, character: string}>} list a list of structs
 * @returns {void}
 */
function example(n, list) {
    /* TODO In a real life scenario, you will describe here what you want the
    end user to do with this generated code */
}

function main(stdin) {
    let line = 0;

    const n = Number(stdin[line++]);
    const list = [];
    for (let i = 0; i < n; i++) {
        const words = stdin[line++].split(" ");
        const j = {
            integer: Number(words[0]),
            character: words[1]
        };
        list.push(j);
    }
    example(n, list);
}

let stdin = "";
process.stdin.on("data", data => stdin += data.toString())
             .on("end", () => main(stdin.split("\n")));
