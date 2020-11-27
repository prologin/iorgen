<?php
/**
 * @param (array{"foo": int, "bar": int}) $struct a struct 1 instance
 * @param int $n a number
 * @param (array{"foo": int, "bar": int})[] $struct_list a list a struct 1
 * @param (array{"name": string, "description": string, "pos": (array{"x": int, "y": int, "z": int})})[] $triangle a triangle
 * @param (array{"first char": string, "second char": string, "third char": string}) $struct_chars a struct of chars
 */
function structs(&$struct, $n, &$struct_list, &$triangle, &$struct_chars) {
    /* TODO Look at them structs. */
}

$struct = array_combine(["foo", "bar"], array_map('intval', explode(' ', fgets(STDIN))));
$n = intval(trim(fgets(STDIN)));
$struct_list = [];
for ($i = 0; $i < $n; $i++) {
    $struct_list[$i] = array_combine(["foo", "bar"], array_map('intval', explode(' ', fgets(STDIN))));
}
$triangle = [];
for ($i = 0; $i < 3; $i++) {
    $triangle[$i] = [];
    $triangle[$i]["name"] = fgets(STDIN)[0];
    $triangle[$i]["description"] = trim(fgets(STDIN));
    $triangle[$i]["pos"] = array_combine(["x", "y", "z"], array_map('intval', explode(' ', fgets(STDIN))));
}
$struct_chars = array_combine(["first char", "second char", "third char"], explode(' ', trim(fgets(STDIN))));
structs($struct, $n, $struct_list, $triangle, $struct_chars);
