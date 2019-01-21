def empty_lines(empty_list, buffer_string, n, empty_in_sample, empty_string, main, empty_char_list, non_empty_char_list, struct_with_empty_line, a_sized_struct, finish):
    """
    :param empty_list: an empty list
    :type empty_list: list[int]
    :param buffer_string: here to check correct parsing of empty line above
    :type buffer_string: str
    :param n: an integer, will be 0 in the sample input
    :type n: int
    :param empty_in_sample: an empty list (only in the sample)
    :type empty_in_sample: list[int]
    :param empty_string: an empty string
    :type empty_string: str
    :param main: an other buffer string
    :type main: str
    :param empty_char_list: an empty char list
    :type empty_char_list: list[str]
    :param non_empty_char_list: an char list, non empty
    :type non_empty_char_list: list[str]
    :param struct_with_empty_line: a struct containing an empty line, then a struct
    :type struct_with_empty_line: dict["list in struct": list[int], "struct in struct": dict["char1": str, "int2": int]]
    :param a_sized_struct: a sized struct containing an empty line
    :type a_sized_struct: dict["size": int, "string in struct": str]
    :param finish: a string to finish
    :type finish: str
    """
    # TODO Wow, lots of empty lines!
    pass


if __name__ == '__main__':
    empty_list = list(map(int, input().split()))
    buffer_string = input()
    n = int(input())
    empty_in_sample = list(map(int, input().split()))
    empty_string = input()
    main = input()
    empty_char_list = list(input())
    non_empty_char_list = list(input())
    struct_with_empty_line = {
        "list in struct": list(map(int, input().split())),
        "struct in struct": dict(map(lambda x, y, z : (x, int(z) if y else z), ("char1", "int2"), (0, 1), input().split()))
    }
    a_sized_struct = (lambda i: {
        "size": i,
        "string in struct": input()
    })(int(input()))
    finish = input()
    empty_lines(empty_list, buffer_string, n, empty_in_sample, empty_string, main, empty_char_list, non_empty_char_list, struct_with_empty_line, a_sized_struct, finish)
