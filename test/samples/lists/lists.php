<?php
/**
 * @param $n the first list's size
 * @param $list_int a list containing ints
 * @param $size an other size
 * @param $list_char a list of char
 * @param $list_string4 a list of strings of size 4
 * @param $matrix a matrix of int
 */
function lists($n, &$list_int, $size, &$list_char, &$list_string4, &$matrix) {
    /* TODO Aren't these lists beautifull? */
}

$n = intval(trim(fgets(STDIN)));
$list_int = array_map('intval', preg_split('/ /', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY));
$size = intval(trim(fgets(STDIN)));
$list_char = preg_split('//', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY);
$list_string4 = [];
for ($i = 0; $i < $size; $i++) {
    $list_string4[$i] = trim(fgets(STDIN));
}
$matrix = [];
for ($i = 0; $i < $size; $i++) {
    $matrix[$i] = array_map('intval', preg_split('/ /', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY));
}
lists($n, $list_int, $size, $list_char, $list_string4, $matrix);
