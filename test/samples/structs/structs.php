<?php
/**
 * @param $struct a struct 1 instance
 * @param $n a number
 * @param $struct_list a list a struct 1
 * @param $triangle a triangle
 */
function structs(&$struct, $n, &$struct_list, &$triangle) {
    /* TODO Look at them structs. */
}

$struct = array_combine(["foo", "bar"], array_map('intval', explode(' ', fgets(STDIN))));
$n = intval(trim(fgets(STDIN)));
$struct_list = array_map(function() { return array_combine(["foo", "bar"], array_map('intval', explode(' ', fgets(STDIN)))); }, range(1, $n));
$triangle = array_map(function() { return array("name" => fgets(STDIN)[0], "pos" => array_combine(["x", "y", "z"], array_map('intval', explode(' ', fgets(STDIN))))); }, range(1, 3));
structs($struct, $n, $struct_list, $triangle);
