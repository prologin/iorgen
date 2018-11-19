def structs(struct, n, struct_list, triangle):
    """
    :param struct: a struct 1 instance
    :param n: a number
    :param struct_list: a list a struct 1
    :param triangle: a triangle
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
    structs(struct, n, struct_list, triangle)
