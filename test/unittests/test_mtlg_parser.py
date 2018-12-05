import sys
sys.path.insert(0, ' ../..')
import unittest

from iorgen.parser_mtlg import ParserMetalang
from iorgen.types import TypeEnum, Type, Input, List, Struct, Variable

def gen_input(name: str = '', structs: List[Struct] = [],
              inputs: List[Variable] = [], subject: str = '', output: str = ''
              ) -> Input:
    return Input(name, structs, inputs, subject, output)

class TestParserMtlgTypeStr(unittest.TestCase):
    def setUp(self):
        self.pm = ParserMetalang(gen_input())

    def test_type_str_int(self):
        type_ = Type(TypeEnum.INT)
        self.assertEqual(self.pm.get_type_str(type_), 'int')

    def test_type_str_char(self):
        type_ = Type(TypeEnum.CHAR)
        self.assertEqual(self.pm.get_type_str(type_), 'char')

    def test_type_str_str(self):
        type_ = Type(TypeEnum.STR)
        self.assertEqual(self.pm.get_type_str(type_), 'array<char>')

    def test_type_str_list(self):
        type_ = Type(TypeEnum.LIST, encapsulated=Type(TypeEnum.INT))
        self.assertEqual(self.pm.get_type_str(type_), 'array<int>')

    def test_type_str_struct(self):
        type_ = Type(TypeEnum.STRUCT, struct_name='some_struct')
        self.assertEqual(self.pm.get_type_str(type_), '@some_struct')

class TestParserMtlgFunProto(unittest.TestCase):
    def setUp(self):
        self.pm = ParserMetalang(gen_input())

    def test_fun_prototype_empty(self):
        self.assertEqual(self.pm.get_fun_prototype('test', []), 'def void test()')

    def test_fun_prototype_empty_different_name(self):
        self.assertEqual(self.pm.get_fun_prototype('a_name', []), 'def void a_name()')

    def test_fun_prototype_empty_wrong_name(self):
        self.assertEqual(self.pm.get_fun_prototype('A nAmE', []), 'def void a_name()')

    def test_fun_prototype_int(self):
        var = Variable('n', '', Type(TypeEnum.INT))
        inputs = [var]
        res = self.pm.get_fun_prototype('test', inputs)
        self.assertEqual(res, "def void test(int n)")

    def test_fun_prototype_multiple_ints(self):
        var1 = Variable('n', '', Type(TypeEnum.INT))
        var2 = Variable('m', '', Type(TypeEnum.INT))
        inputs = [var1, var2]
        res = self.pm.get_fun_prototype('test', inputs)
        self.assertEqual(res, "def void test(int n, int m)")

    def test_fun_prototype_char(self):
        var = Variable('c', '', Type(TypeEnum.CHAR))
        inputs = [var]
        res = self.pm.get_fun_prototype('test', inputs)
        self.assertEqual(res, "def void test(char c)")

    def test_fun_prototype_int_char(self):
        var1 = Variable('n', '', Type(TypeEnum.INT))
        var2 = Variable('c', '', Type(TypeEnum.CHAR))
        inputs = [var1, var2]
        res = self.pm.get_fun_prototype('test', inputs)
        self.assertEqual(res, "def void test(int n, char c)")

    def test_fun_prototype_wrong_name(self):
        var = Variable('ThIs Is A TeSt', '', Type(TypeEnum.INT))
        inputs = [var]
        res = self.pm.get_fun_prototype('test', inputs)
        self.assertEqual(res, "def void test(int this_is_a_test)")

class TestParserMtlgFunDecl(unittest.TestCase):
    def setUp(self):
        self.pm = ParserMetalang(gen_input())

    def test_fun_decl_empty(self):
        self.assertEqual(self.pm.get_fun_decl('test', []), 'def void test()\nend')

    def test_fun_decl_args(self):
        var1 = Variable('n', '', Type(TypeEnum.INT))
        var2 = Variable('c', '', Type(TypeEnum.CHAR))
        inputs = [var1, var2]
        res = self.pm.get_fun_decl('test', inputs)
        self.assertEqual(res, "def void test(int n, char c)\nend")

class TestParserMtlgFunCall(unittest.TestCase):
    def setUp(self):
        self.pm = ParserMetalang(gen_input())

    def test_fun_call_empty(self):
        self.assertEqual(self.pm.get_fun_call('test', []), 'test()')

    def test_fun_call_args(self):
        var1 = Variable('n', '', Type(TypeEnum.INT))
        var2 = Variable('c', '', Type(TypeEnum.CHAR))
        inputs = [var1, var2]
        res = self.pm.get_fun_call('test', inputs)
        self.assertEqual(res, "test(n, c)")

    def test_fun_call_wrong_name(self):
        var = Variable('ThIs Is A TeSt', '', Type(TypeEnum.INT))
        inputs = [var]
        res = self.pm.get_fun_call('Test LoL', inputs)
        self.assertEqual(res, "test_lol(this_is_a_test)")

class TestParserMtlgVarDecl(unittest.TestCase):
    def setUp(self):
        self.pm = ParserMetalang(gen_input())

    def test_var_decl_int(self):
        var = Variable('n', '', Type(TypeEnum.INT))
        self.assertEqual(self.pm.get_var_decl(var), 'def int n = read_int()')

    def test_var_decl_char(self):
        var = Variable('c', '', Type(TypeEnum.CHAR))
        self.assertEqual(self.pm.get_var_decl(var), 'def array<char> c_tmp = read_char_line(1)\ndef char c = c_tmp[0]')

    def test_var_decl_list_int(self):
        type_ = Type(TypeEnum.LIST, 'N', encapsulated=Type(TypeEnum.INT))
        var = Variable('list', '', type_)
        self.assertEqual(
            self.pm.get_var_decl(var),
            "def array<int> list = read_int_line(n)"
        )

    def test_var_decl_list_char(self):
        type_ = Type(TypeEnum.LIST, 'N', encapsulated=Type(TypeEnum.CHAR))
        var = Variable('list', '', type_)
        self.assertEqual(
            self.pm.get_var_decl(var),
            "def array<char> list = read_char_line(n)"
        )

    def test_var_decl_str(self):
        type_ = Type(TypeEnum.STR, '5')
        var = Variable('str', '', type_)
        self.assertEqual(
            self.pm.get_var_decl(var),
            "def array<char> str = read_char_line(5)"
        )

    def test_var_decl_matrix_int(self):
        type_ = Type(TypeEnum.LIST, 'N',
            encapsulated=Type(TypeEnum.LIST, 'M',
                encapsulated=Type(TypeEnum.INT)
            )
        )
        var = Variable('mat', '', type_)
        self.assertEqual(
            self.pm.get_var_decl(var),
            "def array<array<int>> mat = read_int_matrix(n, m)"
        )

    def test_var_decl_matrix_char(self):
        type_ = Type(TypeEnum.LIST, 'N',
            encapsulated=Type(TypeEnum.LIST, 'M',
                encapsulated=Type(TypeEnum.CHAR)
            )
        )
        var = Variable('mat', '', type_)
        self.assertEqual(
            self.pm.get_var_decl(var),
            "def array<array<char>> mat = read_char_matrix(n, m)"
        )

    def test_var_decl_list_str(self):
        type_ = Type(TypeEnum.LIST, 'N',
            encapsulated=Type(TypeEnum.STR, '4')
        )
        var = Variable('list', '', type_)
        self.assertEqual(
            self.pm.get_var_decl(var),
            "def array<array<char>> list[n] with i do\nreturn read_char_line(4)\nend"
        )

    def test_var_decl_list_multiple(self):
        type_ = Type(TypeEnum.LIST, 'N',
            encapsulated=Type(TypeEnum.STR, '4')
        )
        var = Variable('list', '', type_)
        self.pm.get_var_decl(var)
        self.assertEqual(
            self.pm.get_var_decl(var),
            "def array<array<char>> list[n] with j do\nreturn read_char_line(4)\nend"
        )

    def test_parse_wrong_name(self):
        var = Variable('ThIs Is A TeSt', '', Type(TypeEnum.INT))
        self.assertEqual(self.pm.get_var_decl(var), 'def int this_is_a_test = read_int()')

    def test_parse_var_i_list(self):
        i = Variable('i', '', Type(TypeEnum.INT))
        self.pm.get_var_decl(i)
        type_ = Type(TypeEnum.LIST, 'N',
            encapsulated=Type(TypeEnum.STR, '4')
        )
        var = Variable('list', '', type_)
        self.assertEqual(
            self.pm.get_var_decl(var),
            "def array<array<char>> list[n] with i do\nreturn read_char_line(4)\nend"
        )

class TestParserMtlgStruct(unittest.TestCase):
    def setUp(self):
        self.pm = ParserMetalang(gen_input())

    def test_get_struct_decl_empty(self):
        struct = Struct('some_struct', '', [])
        self.assertEqual(self.pm.get_struct_decl(struct), 'record @some_struct\nend')

    def test_get_struct_decl_filled(self):
        var1 = Variable('i', '', Type(TypeEnum.INT))
        var2 = Variable('c', '', Type(TypeEnum.CHAR))
        fields = [var1, var2]
        struct = Struct('some_struct', '', fields)
        self.assertEqual(
            self.pm.get_struct_decl(struct),
            'record @some_struct\ni : int\nc : char\nend'
        )

    def test_get_struct_parsing(self):
        var1 = Variable('i', '', Type(TypeEnum.INT))
        var2 = Variable('c', '', Type(TypeEnum.CHAR))
        fields = [var1, var2]
        struct = Struct('some_struct', '', fields)
        self.pm = ParserMetalang(gen_input(structs=[struct]))
        var = Variable('stru', '', Type(TypeEnum.STRUCT, struct_name='some_struct'))
        self.assertEqual(
            self.pm.get_var_decl(var),
            'def int i = read_int()\ndef array<char> c_tmp = read_char_line(1)\ndef char c = c_tmp[0]\n' \
            + 'def @some_struct stru = record i = i; c = c; end'
        )

    def test_get_list_struct(self):
        var = Variable('i', '', Type(TypeEnum.INT))
        fields = [var]
        struct = Struct('some_struct', '', fields)
        self.pm = ParserMetalang(gen_input(structs=[struct]))
        type_ = Type(TypeEnum.LIST, 'N', encapsulated=Type(TypeEnum.STRUCT, struct_name='some_struct'))
        var = Variable('list', '', type_)
        self.assertEqual(
            self.pm.get_var_decl(var),
            'def array<@some_struct> list[n] with i do\n' \
            + 'def int i = read_int()\n' \
            + 'def @some_struct some_struct_i = record i = i; end\n'
            + 'return some_struct_i\nend'
        )

    def test_get_struct_decl_wrong_name(self):
        var1 = Variable('I', '', Type(TypeEnum.INT))
        var2 = Variable('C', '', Type(TypeEnum.CHAR))
        fields = [var1, var2]
        struct = Struct('SoMe StRucT', '', fields)
        self.assertEqual(
            self.pm.get_struct_decl(struct),
            'record @some_struct\ni : int\nc : char\nend'
        )

    def test_get_struct_parsing_wrong_name(self):
        var1 = Variable('I', '', Type(TypeEnum.INT))
        var2 = Variable('C', '', Type(TypeEnum.CHAR))
        fields = [var1, var2]
        struct = Struct('SoMe StRucT', '', fields)
        self.pm = ParserMetalang(gen_input(structs=[struct]))
        var = Variable('struct', '', Type(TypeEnum.STRUCT, struct_name='some_struct'))
        self.assertEqual(
            self.pm.get_var_decl(var),
            'def int i = read_int()\ndef array<char> c_tmp = read_char_line(1)\ndef char c = c_tmp[0]\n' \
            + 'def @some_struct struct_ = record i = i; c = c; end'
        )

    def test_get_struct_parsing_multiple(self):
        var1 = Variable('I', '', Type(TypeEnum.INT))
        var2 = Variable('C', '', Type(TypeEnum.CHAR))
        fields = [var1, var2]
        struct = Struct('SoMe StRucT', '', fields)
        self.pm = ParserMetalang(gen_input(structs=[struct]))
        var = Variable('stru', '', Type(TypeEnum.STRUCT, struct_name='some_struct'))
        self.pm.get_var_decl(var)
        self.assertEqual(
            self.pm.get_var_decl(var),
            'def int i_ = read_int()\ndef array<char> c__tmp = read_char_line(1)\ndef char c_ = c__tmp[0]\n' \
            + 'def @some_struct stru = record i = i_; c = c_; end'
        )

class TestParserMtlgIndent(unittest.TestCase):
    def setUp(self):
        self.pm = ParserMetalang(gen_input())

    def test_var_decl_int_indent(self):
        var = Variable('n', '', Type(TypeEnum.INT))
        self.assertEqual(self.pm.get_var_decl(var, 4), '    def int n = read_int()')

    def test_var_decl_char_indent(self):
        var = Variable('c', '', Type(TypeEnum.CHAR))
        self.assertEqual(self.pm.get_var_decl(var, 4), '    def array<char> c_tmp = read_char_line(1)\n    def char c = c_tmp[0]')

    def test_var_decl_list_int_indent(self):
        type_ = Type(TypeEnum.LIST, 'N', encapsulated=Type(TypeEnum.INT))
        var = Variable('list', '', type_)
        self.assertEqual(
            self.pm.get_var_decl(var, 4),
            "    def array<int> list = read_int_line(n)"
        )

    def test_var_decl_str_indent(self):
        type_ = Type(TypeEnum.STR, '5')
        var = Variable('str', '', type_)
        self.assertEqual(
            self.pm.get_var_decl(var, 4),
            "    def array<char> str = read_char_line(5)"
        )

    def test_var_decl_list_str_indent(self):
        type_ = Type(TypeEnum.LIST, 'N',
            encapsulated=Type(TypeEnum.STR, '4')
        )
        var = Variable('list', '', type_)
        self.pm = ParserMetalang(gen_input(), 4)
        self.assertEqual(
            self.pm.get_var_decl(var, 4),
            "    def array<array<char>> list[n] with i do\n        return read_char_line(4)\n    end"
        )

    def test_var_decl_matrix_int_indent(self):
        type_ = Type(TypeEnum.LIST, 'N',
            encapsulated=Type(TypeEnum.LIST, 'M',
                encapsulated=Type(TypeEnum.INT)
            )
        )
        var = Variable('mat', '', type_)
        self.assertEqual(
            self.pm.get_var_decl(var, 4),
            "    def array<array<int>> mat = read_int_matrix(n, m)"
        )

    def test_get_struct_indent(self):
        var1 = Variable('i', '', Type(TypeEnum.INT))
        var2 = Variable('c', '', Type(TypeEnum.CHAR))
        fields = [var1, var2]
        struct = Struct('some_struct', '', fields)
        self.pm = ParserMetalang(gen_input(structs=[struct]))
        var = Variable('stru', '', Type(TypeEnum.STRUCT, struct_name='some_struct'))
        self.assertEqual(
            self.pm.get_var_decl(var, 4),
            '    def int i = read_int()\n    def array<char> c_tmp = read_char_line(1)\n    def char c = c_tmp[0]\n' \
            + '    def @some_struct stru = record i = i; c = c; end'
        )

    def test_get_list_struct_indent(self):
        var = Variable('i', '', Type(TypeEnum.INT))
        fields = [var]
        struct = Struct('some_struct', '', fields)
        self.pm = ParserMetalang(gen_input(structs=[struct]), 4)
        type_ = Type(TypeEnum.LIST, 'N', encapsulated=Type(TypeEnum.STRUCT, struct_name='some_struct'))
        var = Variable('list', '', type_)
        self.assertEqual(
            self.pm.get_var_decl(var, 4),
            '    def array<@some_struct> list[n] with i do\n' \
            + '        def int i = read_int()\n' \
            + '        def @some_struct some_struct_i = record i = i; end\n'
            + '        return some_struct_i\n    end'
        )

    def test_get_struct_decl_indent(self):
        var1 = Variable('i', '', Type(TypeEnum.INT))
        var2 = Variable('c', '', Type(TypeEnum.CHAR))
        fields = [var1, var2]
        struct = Struct('some_struct', '', fields)
        self.assertEqual(
            self.pm.get_struct_decl(struct, 4),
            'record @some_struct\n    i : int\n    c : char\nend'
        )

    def test_fun_call_empty(self):
        self.assertEqual(self.pm.get_fun_call('test', [], 4), '    test()')

    def test_fun_call_args(self):
        var1 = Variable('n', '', Type(TypeEnum.INT))
        var2 = Variable('c', '', Type(TypeEnum.CHAR))
        inputs = [var1, var2]
        res = self.pm.get_fun_call('test', inputs, 4)
        self.assertEqual(res, "    test(n, c)")

class TestParserMtlgComment(unittest.TestCase):
    def setUp(self):
        self.pm = ParserMetalang(gen_input())

    def test_get_var_comment(self):
        var = Variable('n', 'some int', Type(TypeEnum.INT))
        res = self.pm.get_var_comment(var)
        self.assertEqual(res, "/* some int */")

    def test_get_param_comment(self):
        var = Variable('n', 'some int', Type(TypeEnum.INT))
        res = self.pm.get_param_comment(var)
        self.assertEqual(res, "/* int n: some int */")

    def test_get_fun_comment(self):
        self.assertEqual(self.pm.get_fun_comment('test'), '/* TODO test */')

    def test_get_struct_attr_comment(self):
        var = Variable('n', 'some int', Type(TypeEnum.INT))
        res = self.pm.get_struct_attr_comment(var)
        self.assertEqual(res, '/* n: some int */')

    def test_get_struct_comment(self):
        var1 = Variable('i', 'some int', Type(TypeEnum.INT))
        var2 = Variable('c', 'some char', Type(TypeEnum.CHAR))
        fields = [var1, var2]
        struct = Struct('some_struct', 'some struct', fields)
        res = self.pm.get_struct_comment(struct)
        self.assertEqual(res, '/* some struct */\n/* i: some int */\n/* c: some char */')

    def test_get_var_decl_comment(self):
        var = Variable('n', 'some int', Type(TypeEnum.INT))
        res = self.pm.get_var_decl(var)
        self.assertEqual(res, "def int n = read_int() /* some int */")

    def test_get_fun_decl_comment(self):
        var1 = Variable('n', 'some int', Type(TypeEnum.INT))
        var2 = Variable('c', 'some char', Type(TypeEnum.CHAR))
        inputs = [var1, var2]
        self.pm = ParserMetalang(gen_input(output='output something'))
        res = self.pm.get_fun_decl('test', inputs)
        self.assertEqual(res, '/* int n: some int */\n/* char c: some char */\n' \
                            + 'def void test(int n, char c)\n/* TODO output something */\nend')

    def test_get_struct_decl_comment(self):
        var1 = Variable('i', 'some int', Type(TypeEnum.INT))
        var2 = Variable('c', 'some char', Type(TypeEnum.CHAR))
        fields = [var1, var2]
        struct = Struct('some_struct', 'some struct', fields)
        self.assertEqual(
            self.pm.get_struct_decl(struct),
            '/* some struct */\n/* i: some int */\n/* c: some char */\nrecord @some_struct\ni : int\nc : char\nend'
        )

class TestParserMtlgPrint(unittest.TestCase):
    def setUp(self):
        self.pm = ParserMetalang(gen_input())

    def test_var_print_int(self):
        var = Variable('n', '', Type(TypeEnum.INT))
        self.assertEqual(self.pm.get_var_print(var), 'print n')

    def test_var_print_char(self):
        var = Variable('c', '', Type(TypeEnum.CHAR))
        self.assertEqual(self.pm.get_var_print(var), 'print c')

    def test_var_print_list_int(self):
        type_ = Type(TypeEnum.LIST, 'n', encapsulated=Type(TypeEnum.INT))
        var = Variable('list', '', type_)
        self.assertEqual(
            self.pm.get_var_print(var),
            'print list[0]\nfor i = 1 to n - 1 do\nprint " " print list[i]\nend'
        )

    def test_var_print_str(self):
        type_ = Type(TypeEnum.STR, '5', encapsulated=Type(TypeEnum.STR))
        var = Variable('s', '', type_)
        self.assertEqual(
            self.pm.get_var_print(var),
            'for i = 0 to 4 do\nprint s[i]\nend'
        )

    def test_var_print_matrix_int(self):
        type_ = Type(TypeEnum.LIST, 'N',
            encapsulated=Type(TypeEnum.LIST, 'M',
                encapsulated=Type(TypeEnum.INT)
            )
        )
        var = Variable('mat', '', type_)
        self.assertEqual(
            self.pm.get_var_print(var),
            'print mat[0][0]' \
            + '\nfor i = 1 to m - 1 do' \
            + '\nprint " " print mat[0][i]' \
            + '\nend' \
            + '\nfor j = 1 to n - 1 do' \
            + '\nprint "\\n" print mat[j][0]' \
            + '\nfor k = 1 to m - 1 do' \
            + '\nprint " " print mat[j][k]' \
            + '\nend' \
            + '\nend'
        )

    def test_var_print_struct(self):
        var1 = Variable('i', '', Type(TypeEnum.INT))
        var2 = Variable('c', '', Type(TypeEnum.CHAR))
        fields = [var1, var2]
        struct = Struct('some_struct', '', fields)
        self.pm = ParserMetalang(gen_input(structs=[struct]))
        var = Variable('stru', '', Type(TypeEnum.STRUCT, struct_name='some_struct'))
        self.assertEqual(
            self.pm.get_var_print(var),
            'print stru.i print " " print stru.c'
        )