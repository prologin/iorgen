<?php
/**
 * @param int $n the size of the lists
 * @param (array{"size1": int, "int list": int[]})[] $lists a list of list of different sizes
 * @param (array{"size2": int, "string list": string})[] $strings a list of strings of different sizes
 * @param (array{"size3": int, "list list": int[][]})[] $matrices a list of matrices of different sizes
 * @param (array{"size4": int, "int list n": int[]})[] $same a list of list of same sizes
 */
function sized_struct($n, &$lists, &$strings, &$matrices, &$same) {
    /* TODO The is a special case. */
}

$n = intval(trim(fgets(STDIN)));
$lists = new SplFixedArray($n);
for ($i = 0; $i < $n; $i++) {
    $j = [];
    $j["size1"] = intval(trim(fgets(STDIN)));
    $j["int list"] = array_map('intval', preg_split('/ /', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY));
    $lists[$i] = $j;
}
$strings = new SplFixedArray($n);
for ($i = 0; $i < $n; $i++) {
    $j = [];
    $j["size2"] = intval(trim(fgets(STDIN)));
    $j["string list"] = trim(fgets(STDIN));
    $strings[$i] = $j;
}
$matrices = new SplFixedArray(2);
for ($i = 0; $i < 2; $i++) {
    $j = [];
    $j["size3"] = intval(trim(fgets(STDIN)));
    $j["list list"] = new SplFixedArray($j["size3"]);
    for ($k = 0; $k < $j["size3"]; $k++) {
        $j["list list"][$k] = array_map('intval', preg_split('/ /', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY));
    }
    $matrices[$i] = $j;
}
$same = new SplFixedArray($n);
for ($i = 0; $i < $n; $i++) {
    $j = [];
    $j["size4"] = intval(trim(fgets(STDIN)));
    $j["int list n"] = array_map('intval', preg_split('/ /', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY));
    $same[$i] = $j;
}
sized_struct($n, $lists, $strings, $matrices, $same);
