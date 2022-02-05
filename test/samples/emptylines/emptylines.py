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


def empty_lines(empty_list, buffer_string, n, empty_in_sample, empty_string, main, empty_char_list, non_empty_char_list, struct_with_empty_line, a_sized_struct, finish):
    """
    :param empty_list: an empty list
    :type empty_list: List[int]
    :param buffer_string: here to check correct parsing of empty line above
    :type buffer_string: str
    :param n: an integer, will be 0 in the sample input
    :type n: int
    :param empty_in_sample: an empty list (only in the sample)
    :type empty_in_sample: List[int]
    :param empty_string: an empty string
    :type empty_string: str
    :param main: an other buffer string
    :type main: str
    :param empty_char_list: an empty char list
    :type empty_char_list: List[str]
    :param non_empty_char_list: an char list, non empty
    :type non_empty_char_list: List[str]
    :param struct_with_empty_line: a struct containing an empty line, then a struct
    :type struct_with_empty_line: A
    :param a_sized_struct: a sized struct containing an empty line
    :type a_sized_struct: SizedStruct
    :param finish: a string to finish
    :type finish: str
    """
    # TODO Wow, lots of empty lines!
    pass


if __name__ == '__main__':
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
    empty_lines(empty_list, buffer_string, n, empty_in_sample, empty_string, main, empty_char_list, non_empty_char_list, struct_with_empty_line, a_sized_struct, finish)
