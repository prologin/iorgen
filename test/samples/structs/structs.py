def structs(struct, n, struct_list, triangle, struct_chars):
    """
    :param struct: a struct 1 instance
    :type struct: dict["foo": int, "bar": int]
    :param n: a number
    :type n: int
    :param struct_list: a list a struct 1
    :type struct_list: list[dict["foo": int, "bar": int]]
    :param triangle: a triangle
    :type triangle: list[dict["name": str, "pos": dict["x": int, "y": int, "z": int]]]
    :param struct_chars: a struct of chars
    :type struct_chars: dict["first char": str, "second char": str, "third char": str]
    """
    # TODO Look at them structs.
    pass


if __name__ == '__main__':
    struct = dict(zip(("foo", "bar"), map(int, input().split())))
    n = int(input())
    struct_list = [dict(zip(("foo", "bar"), map(int, input().split()))) for _ in range(n)]
    triangle = [{
        "name": input()[0],
        "pos": dict(zip(("x", "y", "z"), map(int, input().split())))
    } for _ in range(3)]
    struct_chars = dict(zip(("first char", "second char", "third char"), input().split()))
    structs(struct, n, struct_list, triangle, struct_chars)
