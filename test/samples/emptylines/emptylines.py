from dataclasses import dataclass
from typing import List


@dataclass
class StructWithAChar:
    """a char struct"""

    char1: str  # a char
    int2: int  # an integer


@dataclass
class A:
    """a struct"""

    list_in_struct: List[int]  # a list in a struct
    struct_in_struct: StructWithAChar  # a struct in a struct


@dataclass
class SizedStruct:
    """a sized struct"""

    size: int  # the size
    string_in_struct: str  # the string


def empty_lines(
    empty_list: List[int],
    buffer_string: str,
    n: int,
    empty_in_sample: List[int],
    empty_string: str,
    main: str,
    empty_char_list: List[str],
    non_empty_char_list: List[str],
    struct_with_empty_line: A,
    a_sized_struct: SizedStruct,
    finish: str,
) -> None:
    """
    :param empty_list: an empty list
    :param buffer_string: here to check correct parsing of empty line above
    :param n: an integer, will be 0 in the sample input
    :param empty_in_sample: an empty list (only in the sample)
    :param empty_string: an empty string
    :param main: an other buffer string
    :param empty_char_list: an empty char list
    :param non_empty_char_list: an char list, non empty
    :param struct_with_empty_line: a struct containing an empty line, then a struct
    :param a_sized_struct: a sized struct containing an empty line
    :param finish: a string to finish
    """
    # TODO Wow, lots of empty lines!
    pass


if __name__ == "__main__":
    empty_list = list(map(int, input().split()))
    buffer_string = input()
    n = int(input())
    empty_in_sample = list(map(int, input().split()))
    empty_string = input()
    main = input()
    empty_char_list = list(input())
    non_empty_char_list = list(input())
    struct_with_empty_line = A(
        list(map(int, input().split())),
        StructWithAChar(*map(lambda x, y: int(y) if x else y, (0, 1), input().split())),
    )
    a_sized_struct = (lambda i: SizedStruct(
        i,
        input()
    ))(int(input()))
    finish = input()
    empty_lines(
        empty_list,
        buffer_string,
        n,
        empty_in_sample,
        empty_string,
        main,
        empty_char_list,
        non_empty_char_list,
        struct_with_empty_line,
        a_sized_struct,
        finish,
    )
