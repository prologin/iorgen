def keywords(if_, class_, i, in_, for_, words):
    """
    :param if_: not a condition
    :type if_: int
    :param class_: not a class
    :type class_: str
    :param i: just a string
    :type i: str
    :param in_: not in
    :type in_: dict["a": int, "static": int]
    :param for_: not a loop
    :type for_: list[int]
    :param words: contains lots of things
    :type words: list[dict["int": dict["return": int, "void": list[int]], "if true": int]]
    """
    # TODO If this compiles, it is already a good step!
    pass


if __name__ == '__main__':
    if_ = int(input())
    class_ = input()[0]
    i = input()
    in_ = dict(zip(("a", "static"), map(int, input().split())))
    for_ = list(map(int, input().split()))
    words = [{
        "int": {
            "return": int(input()),
            "void": list(map(int, input().split()))
        },
        "if true": int(input())
    } for _ in range(2)]
    keywords(if_, class_, i, in_, for_, words)
