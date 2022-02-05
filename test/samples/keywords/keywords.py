from dataclasses import dataclass
from typing import List


@dataclass
class Console:
    """may conflict in c#"""

    a: int  # the first letter of the alphabet
    static: int  # an integer


@dataclass
class System:
    """may conflict in c#"""

    return_: int  # not the end of the function
    void: List[int]  # not nothing


@dataclass
class Main:
    """not the main function"""

    int: System  # not an integer
    if_true: int  # should not cause conflict


def keywords(if_, class_, i, in_, for_, words):
    """
    :param if_: not a condition
    :type if_: int
    :param class_: not a class
    :type class_: str
    :param i: just a string
    :type i: str
    :param in_: not in
    :type in_: Console
    :param for_: not a loop
    :type for_: List[int]
    :param words: contains lots of things
    :type words: List[Main]
    """
    # TODO If this compiles, it is already a good step!
    pass


if __name__ == '__main__':
    if_ = int(input())
    class_ = input()[0]
    i = input()
    in_ = Console(*map(int, input().split()))
    for_ = list(map(int, input().split()))
    words = [
        Main(
            System(
                int(input()),
                list(map(int, input().split())),
            ),
            int(input()),
        ) for _ in range(2)]
    keywords(if_, class_, i, in_, for_, words)
