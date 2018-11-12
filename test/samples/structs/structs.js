"use strict";
const fs = require("fs");

/**
 * @param {Object} struct a struct 1 instance
 * @param {number} n a number
 * @param {Array.<Object>} structList a list a struct 1
 * @param {Array.<Object>} triangle a triangle
 * @returns {void}
 */
function structs(struct, n, structList, triangle) {
    /* TODO Look at them structs. */
}

{
    const stdin = fs.readFileSync("/dev/stdin").toString().split("\n");
    let line = 0;

    const words = stdin[line++].split(" ");
    const struct = {
        foo: Number(words[0]),
        bar: Number(words[1])
    };
    const n = Number(stdin[line++]);
    const structList = [];
    for (let i = 0; i < n; i++) {
        const words1 = stdin[line++].split(" ");
        const structListElem = {
            foo: Number(words1[0]),
            bar: Number(words1[1])
        };
        structList.push(structListElem);
    }
    const triangle = [];
    for (let i = 0; i < 3; i++) {
        const triangleElem = {};
        triangleElem.name = stdin[line++];
        const words1 = stdin[line++].split(" ");
        triangleElem.pos = {
            x: Number(words1[0]),
            y: Number(words1[1]),
            z: Number(words1[2])
        };
        triangle.push(triangleElem);
    }
    structs(struct, n, structList, triangle);
}
