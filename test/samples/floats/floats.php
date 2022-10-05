<?php
/**
 * @param double $f a float
 * @param double $g a float, greater than f
 * @param (array{"x": double, "y": double, "z": double}) $point some coordinates
 * @param int $n a number
 * @param double[] $float_list a list of floats
 * @param double[] $other_list a list of floats
 * @param (array{"integer": int, "char": string, "float": double})[] $inlined some inlined structs
 * @param (array{"integer 2": int, "string": string, "float 2": double}) $multiline a multiline struct
 */
function floats($f, $g, &$point, $n, &$float_list, &$other_list, &$inlined, &$multiline) {
    /* TODO Parsing is often easy, reprint mode is harder */
}

$f = floatval(trim(fgets(STDIN)));
$g = floatval(trim(fgets(STDIN)));
$point = array_combine(["x", "y", "z"], array_map('floatval', explode(' ', fgets(STDIN))));
$n = intval(trim(fgets(STDIN)));
$float_list = array_map('floatval', preg_split('/ /', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY));
$other_list = array_map('floatval', preg_split('/ /', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY));
$inlined = new SplFixedArray(3);
for ($i = 0; $i < 3; $i++) {
    $inlined[$i] = array_combine(["integer", "char", "float"], array_map(fn($f, $x) => $f($x), ['intval', 'strval', 'floatval'], explode(' ', trim(fgets(STDIN)))));
}
$multiline = [];
$multiline["integer 2"] = intval(trim(fgets(STDIN)));
$multiline["string"] = trim(fgets(STDIN));
$multiline["float 2"] = floatval(trim(fgets(STDIN)));
floats($f, $g, $point, $n, $float_list, $other_list, $inlined, $multiline);
