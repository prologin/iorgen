from typing import List


def manual_format(a: int, b: int, c: int, n: int, one_per_line: List[int]) -> None:
    """
    :param a: a first number
    :param b: a second number
    :param c: a third number
    :param n: This one on a new line
    :param one_per_line: an integer list, one per line
    """
    # TODO From the function perspective, this is just 4 integers
    pass


if __name__ == "__main__":
    a, b, c = map(int, input().split())
    n = int(input())
    one_per_line = [int(input()) for _ in range(3)]
    manual_format(a, b, c, n, one_per_line)
