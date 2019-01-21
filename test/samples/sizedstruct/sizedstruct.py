def sized_struct(n, lists, strings, matrices, same):
    """
    :param n: the size of the lists
    :type n: int
    :param lists: a list of list of different sizes
    :type lists: list[dict["size1": int, "int list": list[int]]]
    :param strings: a list of strings of different sizes
    :type strings: list[dict["size2": int, "string list": str]]
    :param matrices: a list of matrices of different sizes
    :type matrices: list[dict["size3": int, "list list": list[list[int]]]]
    :param same: a list of list of same sizes
    :type same: list[dict["size4": int, "int list n": list[int]]]
    """
    # TODO The is a special case.
    pass


if __name__ == '__main__':
    n = int(input())
    lists = [
        (lambda i: {
            "size1": i,
            "int list": list(map(int, input().split()))
        })(int(input()))
        for _ in range(n)
        ]
    strings = [
        (lambda i: {
            "size2": i,
            "string list": input()
        })(int(input()))
        for _ in range(n)
        ]
    matrices = [
        (lambda i: {
            "size3": i,
            "list list": [list(map(int, input().split())) for _ in range(i)]
        })(int(input()))
        for _ in range(2)
        ]
    same = [{
        "size4": int(input()),
        "int list n": list(map(int, input().split()))
    } for _ in range(n)]
    sized_struct(n, lists, strings, matrices, same)
