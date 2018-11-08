def keywords(if_, class_, i, in_, for_, words):
    """
    :param if_: not a condition
    :param class_: not a class
    :param i: just a string
    :param in_: not in
    :param for_: not a loop
    :param words: contains lots of things
    """
    # TODO If this compiles, it is already a good step!
    pass

if __name__ == '__main__':
    if_ = int(input())
    class_ = input()[0]
    i = input()
    in_ = dict(zip(["a", "static"], map(int, input().split())))
    for_ = list(map(int, input().split()))
    words = [{"int": {"return": int(input()), "void": list(map(int, input().split()))}, "if true": int(input())} for _ in range(2)]
    keywords(if_, class_, i, in_, for_, words)
