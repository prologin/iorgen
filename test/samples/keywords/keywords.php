<?php
/**
 * @param int $if not a condition
 * @param string $class not a class
 * @param string $i just a string
 * @param (array{"a": int, "static": int}) $in not in
 * @param int[] $for not a loop
 * @param (array{"int": (array{"return": int, "void": int[]}), "if true": int})[] $words contains lots of things
 */
function keywords($if, $class, &$i, &$in, &$for, &$words) {
    /* TODO If this compiles, it is already a good step! */
}

$if = intval(trim(fgets(STDIN)));
$class = fgets(STDIN)[0];
$i = trim(fgets(STDIN));
$in = array_combine(["a", "static"], array_map('intval', explode(' ', fgets(STDIN))));
$for = array_map('intval', preg_split('/ /', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY));
$words = [];
for ($j = 0; $j < 2; $j++) {
    $words[$j] = [];
    $words[$j]["int"] = [];
    $words[$j]["int"]["return"] = intval(trim(fgets(STDIN)));
    $words[$j]["int"]["void"] = array_map('intval', preg_split('/ /', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY));
    $words[$j]["if true"] = intval(trim(fgets(STDIN)));
}
keywords($if, $class, $i, $in, $for, $words);
