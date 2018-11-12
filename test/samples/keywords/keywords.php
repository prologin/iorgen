<?php
/**
 * @param $if not a condition
 * @param $class not a class
 * @param $i just a string
 * @param $in not in
 * @param $for not a loop
 * @param $words contains lots of things
 */
function keywords($if, $class, &$i, &$in, &$for, &$words) {
    /* TODO If this compiles, it is already a good step! */
}

$if = intval(trim(fgets(STDIN)));
$class = fgets(STDIN)[0];
$i = trim(fgets(STDIN));
$in = array_combine(["a", "static"], array_map('intval', explode(' ', fgets(STDIN))));
$for = array_map('intval', explode(' ', fgets(STDIN)));
$words = array_map(function() { return array("int" => array("return" => intval(trim(fgets(STDIN))), "void" => array_map('intval', explode(' ', fgets(STDIN)))), "if true" => intval(trim(fgets(STDIN)))); }, range(1, 2));
keywords($if, $class, $i, $in, $for, $words);
