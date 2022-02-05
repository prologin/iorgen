<?php
/**
 * @param (array{"foo": int, "bar": int}) $struct a struct 1 instance
 * @param int $n a number
 * @param (array{"foo": int, "bar": int})[] $struct_list a list a struct 1
 * @param (array{"name": string, "description": string, "pos": (array{"x": int, "y": int, "z": int})})[] $triangle a triangle
 * @param (array{"first char": string, "second char": string, "third char": string}) $struct_chars a struct of chars
 * @param (array{"int": int, "big list": int[][][]}) $big_list_struct the big list struct
 */
function structs(&$struct, $n, &$struct_list, &$triangle, &$struct_chars, &$big_list_struct) {
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
$big_list_struct = [];
$big_list_struct["int"] = intval(trim(fgets(STDIN)));
$big_list_struct["big list"] = [];
for ($i = 0; $i < 2; $i++) {
    $big_list_struct["big list"][$i] = [];
    for ($j = 0; $j < 2; $j++) {
        $big_list_struct["big list"][$i][$j] = array_map('intval', preg_split('/ /', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY));
    }
}
structs($struct, $n, $struct_list, $triangle, $struct_chars, $big_list_struct);
