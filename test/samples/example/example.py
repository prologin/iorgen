def example(n, list):
    """
    :param n: a number, used as a size
    :type n: int
    :param list: a list of structs
    :type list: list[dict["integer": int, "character": str]]
    """
    # TODO In a real life scenario, you will describe here what you want the
    # end user to do with this generated code
    pass


if __name__ == '__main__':
    n = int(input())
    list = [
        dict(map(lambda x, y, z : (x, int(z) if y else z), ("integer", "character"), (1, 0), input().split()))
        for _ in range(n)
        ]
    example(n, list)
