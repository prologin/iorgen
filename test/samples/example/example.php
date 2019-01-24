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
$list = [];
for ($i = 0; $i < $n; $i++) {
    $list[$i] = array_combine(["integer", "character"], explode(' ', trim(fgets(STDIN))));
}
example($n, $list);
