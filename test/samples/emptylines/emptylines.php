<?php
/**
 * @param $empty_list an empty list
 * @param $buffer_string here to check correct parsing of empty line above
 * @param $n an integer, will be 0 in the sample input
 * @param $empty_in_sample an empty list (only in the sample)
 * @param $empty_string an empty string
 * @param $main an other buffer string
 * @param $empty_char_list an empty char list
 * @param $non_empty_char_list an char list, non empty
 * @param $struct_with_empty_line a struct containing an empty line, then a struct
 * @param $a_sized_struct a sized struct containing an empty line
 * @param $finish a string to finish
 */
function empty_lines(&$empty_list, &$buffer_string, $n, &$empty_in_sample, &$empty_string, &$main, &$empty_char_list, &$non_empty_char_list, &$struct_with_empty_line, &$a_sized_struct, &$finish) {
    /* TODO Wow, lots of empty lines! */
}

$empty_list = array_map('intval', preg_split('/ /', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY));
$buffer_string = trim(fgets(STDIN));
$n = intval(trim(fgets(STDIN)));
$empty_in_sample = array_map('intval', preg_split('/ /', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY));
$empty_string = trim(fgets(STDIN));
$main = trim(fgets(STDIN));
$empty_char_list = preg_split('//', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY);
$non_empty_char_list = preg_split('//', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY);
$struct_with_empty_line = [];
$struct_with_empty_line["list in struct"] = array_map('intval', preg_split('/ /', trim(fgets(STDIN)), -1, PREG_SPLIT_NO_EMPTY));
$struct_with_empty_line["struct in struct"] = array_combine(["char1", "int2"], explode(' ', trim(fgets(STDIN))));
$a_sized_struct = [];
$a_sized_struct["size"] = intval(trim(fgets(STDIN)));
$a_sized_struct["string in struct"] = trim(fgets(STDIN));
$finish = trim(fgets(STDIN));
empty_lines($empty_list, $buffer_string, $n, $empty_in_sample, $empty_string, $main, $empty_char_list, $non_empty_char_list, $struct_with_empty_line, $a_sized_struct, $finish);
