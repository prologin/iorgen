"use strict";
const fs = require("fs");

/**
 * @param {number} n a number, used as a size
 * @param {Array.<Object>} list a list of structs
 * @returns {void}
 */
function example(n, list) {
    /* TODO In a real life scenario, you will describe here what you want the
    end user to do with this generated code */
}

{
    const stdin = fs.readFileSync(0).toString().split("\n");
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
