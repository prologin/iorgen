from dataclasses import dataclass
from typing import List


@dataclass
class AStruct:
    """A struct for the example"""

    integer: int  # an integer
    character: str  # a char


def example(n, list):
    """
    :param n: a number, used as a size
    :type n: int
    :param list: a list of structs
    :type list: List[AStruct]
    """
    # TODO In a real life scenario, you will describe here what you want the
    # end user to do with this generated code
    pass


if __name__ == '__main__':
    n = int(input())
    list = [
        AStruct(*map(lambda x, y: int(y) if x else y, (1, 0), input().split()))
        for _ in range(n)
        ]
    example(n, list)
