from dataclasses import dataclass
from typing import List


@dataclass
class List_:
    """contains a list"""

    size1: int  # the list's size
    int_list: List[int]  # the integer list


@dataclass
class String:
    """contains a string"""

    size2: int  # the list's size
    string_list: str  # the string list


@dataclass
class Matrix:
    """contains a matrix"""

    size3: int  # the list's size
    list_list: List[List[int]]  # the list list


@dataclass
class NotASizedStruct:
    """this is not a 'sized struct', but a regular one!"""

    size4: int  # not the list's size
    int_list_n: List[int]  # the integer list


def sized_struct(n: int, lists: List[List_], strings: List[String], matrices: List[Matrix], same: List[NotASizedStruct]) -> None:
    """
    :param n: the size of the lists
    :param lists: a list of list of different sizes
    :param strings: a list of strings of different sizes
    :param matrices: a list of matrices of different sizes
    :param same: a list of list of same sizes
    """
    # TODO The is a special case.
    pass


if __name__ == '__main__':
    n = int(input())
    lists = [
        (lambda i: List_(
            i,
            list(map(int, input().split()))
        ))(int(input()))
        for _ in range(n)
        ]
    strings = [
        (lambda i: String(
            i,
            input()
        ))(int(input()))
        for _ in range(n)
        ]
    matrices = [
        (lambda i: Matrix(
            i,
            [list(map(int, input().split())) for _ in range(i)]
        ))(int(input()))
        for _ in range(2)
        ]
    same = [
        NotASizedStruct(
            int(input()),
            list(map(int, input().split())),
        ) for _ in range(n)]
    sized_struct(n, lists, strings, matrices, same)
