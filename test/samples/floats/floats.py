from dataclasses import dataclass
from typing import List


@dataclass
class Coordinates:
    """Represents coordinates"""

    x: float  # X
    y: float  # Y
    z: float  # Z


@dataclass
class InlinedMix:
    """Mix of fields that go on one line"""

    integer: int  # an integer
    char: str  # a char
    float: float  # a float


@dataclass
class MultilineMix:
    """a struct of chars"""

    integer_2: int  # an other integer
    string: str  # a string of size 5
    float_2: float  # an other float


def floats(
    f: float,
    g: float,
    point: Coordinates,
    n: int,
    float_list: List[float],
    other_list: List[float],
    inlined: List[InlinedMix],
    multiline: MultilineMix,
) -> None:
    """
    :param f: a float
    :param g: a float, greater than f
    :param point: some coordinates
    :param n: a number
    :param float_list: a list of floats
    :param other_list: a list of floats
    :param inlined: some inlined structs
    :param multiline: a multiline struct
    """
    # TODO Parsing is often easy, reprint mode is harder
    pass


if __name__ == "__main__":
    f = float(input())
    g = float(input())
    point = Coordinates(*map(float, input().split()))
    n = int(input())
    float_list = list(map(float, input().split()))
    other_list = list(map(float, input().split()))
    inlined = [
        InlinedMix(*map(lambda f, x: f(x), (int, str, float), input().split()))
        for _ in range(3)
    ]
    multiline = MultilineMix(
        int(input()),
        input(),
        float(input()),
    )
    floats(f, g, point, n, float_list, other_list, inlined, multiline)
