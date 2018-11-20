<?php
/**
 * @param $n a number, used as a size
 * @param $list a list of structs
 */
function example($n, &$list) {
    /* TODO In a real life scenario, you will describe here what you want the
    end user to do with this generated code */
}

$n = intval(trim(fgets(STDIN)));
$list = array_map(function() { return array_combine(["integer", "character"], explode(' ', trim(fgets(STDIN)))); }, range(1, $n));
example($n, $list);
