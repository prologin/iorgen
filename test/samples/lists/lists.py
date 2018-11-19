def lists(n, list_int, size, list_char, list_string4, matrix):
    """
    :param n: the first list's size
    :param list_int: a list containing ints
    :param size: an other size
    :param list_char: a list of char
    :param list_string4: a list of strings of size 4
    :param matrix: a matrix of int
    """
    # TODO Aren't these lists beautifull?
    pass


if __name__ == '__main__':
    n = int(input())
    list_int = list(map(int, input().split()))
    size = int(input())
    list_char = list(input())
    list_string4 = [input() for _ in range(size)]
    matrix = [list(map(int, input().split())) for _ in range(size)]
    lists(n, list_int, size, list_char, list_string4, matrix)
