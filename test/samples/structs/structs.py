from dataclasses import dataclass
from typing import List


@dataclass
class Struct1:
    """A simple struct"""

    foo: int  # a field
    bar: int  # a field


@dataclass
class Position:
    """Represents a position"""

    x: int  # X
    y: int  # Y
    z: int  # Z


@dataclass
class Point:
    """A point's name and position"""

    name: str  # the point's name (single character)
    description: str  # the point's description
    pos: Position  # the point's position


@dataclass
class Chars:
    """a struct of chars"""

    first_char: str  # a first char
    second_char: str  # a second char
    third_char: str  # a third char


@dataclass
class WithList:
    """contains a big list inside"""

    int: int  # int
    big_list: List[List[List[int]]]  # list nested 3 times!


def structs(
    struct: Struct1,
    n: int,
    struct_list: List[Struct1],
    triangle: List[Point],
    struct_chars: Chars,
    big_list_struct: WithList,
) -> None:
    """
    :param struct: a struct 1 instance
    :param n: a number
    :param struct_list: a list a struct 1
    :param triangle: a triangle
    :param struct_chars: a struct of chars
    :param big_list_struct: the big list struct
    """
    # TODO Look at them structs.
    pass


if __name__ == "__main__":
    struct = Struct1(*map(int, input().split()))
    n = int(input())
    struct_list = [Struct1(*map(int, input().split())) for _ in range(n)]
    triangle = [
        Point(
            input()[0],
            input(),
            Position(*map(int, input().split())),
        )
        for _ in range(3)
    ]
    struct_chars = Chars(*input().split())
    big_list_struct = WithList(
        int(input()),
        [[list(map(int, input().split())) for _ in range(2)] for _ in range(2)],
    )
    structs(struct, n, struct_list, triangle, struct_chars, big_list_struct)
