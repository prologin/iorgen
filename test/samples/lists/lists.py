def lists(n, list_int, size, list_char, string, list_string4, list_list_string2, matrix):
    """
    :param n: the first list's size
    :type n: int
    :param list_int: a list containing ints
    :type list_int: List[int]
    :param size: an other size
    :type size: int
    :param list_char: a list of char
    :type list_char: List[str]
    :param string: a string
    :type string: str
    :param list_string4: a list of strings of size 4
    :type list_string4: List[str]
    :param list_list_string2: a list of list of strings of size 2 of size 2 of size 2
    :type list_list_string2: List[List[str]]
    :param matrix: a matrix of int
    :type matrix: List[List[int]]
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
