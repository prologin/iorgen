<?php
/**
 * @param int $n the first list's size
 * @param int[] $list_int a list containing ints
 * @param int $size an other size
 * @param string[] $list_char a list of char
 * @param string $string a string
 * @param string[] $list_string4 a list of strings of size 4
 * @param string[][] $list_list_string2 a list of list of strings of size 2 of size 2 of size 2
 * @param int[][] $matrix a matrix of int
 */
function lists($n, &$list_int, $size, &$list_char, &$string, &$list_string4, &$list_list_string2, &$matrix) {
    /* TODO Aren't these lists beautifull? */
}

$n = intval(trim(fgets(STDIN)));
$list_int = array_map('intval', preg_split('/ /', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY));
$size = intval(trim(fgets(STDIN)));
$list_char = preg_split('//', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY);
$string = trim(fgets(STDIN));
$list_string4 = [];
for ($i = 0; $i < $size; $i++) {
    $list_string4[$i] = trim(fgets(STDIN));
}
$list_list_string2 = [];
for ($i = 0; $i < 2; $i++) {
    $list_list_string2[$i] = [];
    for ($j = 0; $j < 2; $j++) {
        $list_list_string2[$i][$j] = trim(fgets(STDIN));
    }
}
$matrix = [];
for ($i = 0; $i < $size; $i++) {
    $matrix[$i] = array_map('intval', preg_split('/ /', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY));
}
lists($n, $list_int, $size, $list_char, $string, $list_string4, $list_list_string2, $matrix);
