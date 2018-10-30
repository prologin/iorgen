<?php
/**
 * @param $n the first list's size
 * @param $list_int a list containing ints
 * @param $size an other size
 * @param $list_char a list of char
 * @param $list_string4 a list of strings of size 4
 * @param $matrix a matrix of int
 */
function lists(&$n, &$list_int, &$size, &$list_char, &$list_string4, &$matrix) {
    /* TODO Aren't these lists beautifull? */
}

$n = intval(trim(fgets(STDIN)));
$list_int = array_map('intval', explode(' ', fgets(STDIN)));
$size = intval(trim(fgets(STDIN)));
$list_char = str_split(trim(fgets(STDIN)));
$list_string4 = array_map(function() { return trim(fgets(STDIN)); }, range(1, $size));
$matrix = array_map(function() { return array_map('intval', explode(' ', fgets(STDIN))); }, range(1, $size));
lists($n, $list_int, $size, $list_char, $list_string4, $matrix);
