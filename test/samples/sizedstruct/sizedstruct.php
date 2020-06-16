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
$lists = [];
for ($i = 0; $i < $n; $i++) {
    $lists[$i] = [];
    $lists[$i]["size1"] = intval(trim(fgets(STDIN)));
    $lists[$i]["int list"] = array_map('intval', preg_split('/ /', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY));
}
$strings = [];
for ($i = 0; $i < $n; $i++) {
    $strings[$i] = [];
    $strings[$i]["size2"] = intval(trim(fgets(STDIN)));
    $strings[$i]["string list"] = trim(fgets(STDIN));
}
$matrices = [];
for ($i = 0; $i < 2; $i++) {
    $matrices[$i] = [];
    $matrices[$i]["size3"] = intval(trim(fgets(STDIN)));
    $matrices[$i]["list list"] = [];
    for ($j = 0; $j < $matrices[$i]["size3"]; $j++) {
        $matrices[$i]["list list"][$j] = array_map('intval', preg_split('/ /', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY));
    }
}
$same = [];
for ($i = 0; $i < $n; $i++) {
    $same[$i] = [];
    $same[$i]["size4"] = intval(trim(fgets(STDIN)));
    $same[$i]["int list n"] = array_map('intval', preg_split('/ /', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY));
}
sized_struct($n, $lists, $strings, $matrices, $same);
