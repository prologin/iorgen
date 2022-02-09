<?php
/**
 * @param int $a a first number
 * @param int $b a second number
 * @param int $c a third number
 * @param int $n This one on a new line
 * @param int[] $one_per_line an integer list, one per line
 */
function manual_format($a, $b, $c, $n, &$one_per_line) {
    /* TODO From the function perspective, this is just 4 integers */
}

list($a, $b, $c) = array_map('intval', preg_split('/ /', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY));
$n = intval(trim(fgets(STDIN)));
$one_per_line = new SplFixedArray(3);
for ($i = 0; $i < 3; $i++) {
    $one_per_line[$i] = intval(trim(fgets(STDIN)));
}
manual_format($a, $b, $c, $n, $one_per_line);
