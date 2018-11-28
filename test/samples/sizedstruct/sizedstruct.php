<?php
/**
 * @param $n the size of the lists
 * @param $lists a list of list of different sizes
 * @param $strings a list of strings of different sizes
 * @param $matrices a list of matrices of different sizes
 * @param $same a list of list of same sizes
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
    $matrices[$i]["list list"] = array_map(function() { return array_map('intval', preg_split('/ /', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY)); }, range(1, $matrices[$i]["size3"]));
}
$same = [];
for ($i = 0; $i < $n; $i++) {
    $same[$i] = [];
    $same[$i]["size4"] = intval(trim(fgets(STDIN)));
    $same[$i]["int list n"] = array_map('intval', preg_split('/ /', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY));
}
sized_struct($n, $lists, $strings, $matrices, $same);
