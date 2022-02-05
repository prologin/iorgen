from typing import List


def lists(n: int, list_int: List[int], size: int, list_char: List[str], string: str, list_string4: List[str], list_list_string2: List[List[str]], matrix: List[List[int]]) -> None:
    """
    :param n: the first list's size
    :param list_int: a list containing ints
    :param size: an other size
    :param list_char: a list of char
    :param string: a string
    :param list_string4: a list of strings of size 4
    :param list_list_string2: a list of list of strings of size 2 of size 2 of size 2
    :param matrix: a matrix of int
    """
    # TODO Aren't these lists beautifull?
    pass


if __name__ == '__main__':
    n = int(input())
    list_int = list(map(int, input().split()))
    size = int(input())
    list_char = list(input())
    string = input()
    list_string4 = [input() for _ in range(size)]
    list_list_string2 = [[input() for _ in range(2)] for _ in range(2)]
    matrix = [list(map(int, input().split())) for _ in range(size)]
    lists(n, list_int, size, list_char, string, list_string4, list_list_string2, matrix)
