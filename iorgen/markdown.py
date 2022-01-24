# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018-2020 Sacha Delanoue
# Copyright 2021 Kenji Gaillac
"""Generate the markdown describing the subject"""

import textwrap
from typing import Dict, List, Optional

from iorgen.types import Input, Type, TypeEnum, Variable

LANG = {
    "en": {
        ":": ":",
        "and": "and ",
        "char": "a char",
        "char list": "a list of **{}** chars next to each other",
        "constraints": "Constraints",
        "constraints perf": "Performance constraints",
        "first line": "On the first line,",
        "first lines": "On the first lines,",
        "input": "Input",
        "input decl": "The input will contain:",
        "int": "an integer",
        "int list": "a list of **{}** integers separated by spaces",
        "list": "a list of **{}** elements",
        "list line": "One line per list element:",
        "list lines": "Each list element is on several lines:",
        "next line": "On the next line,",
        "next lines": "On the next lines,",
        "one line": "One line containing",
        "output": "Output",
        "str": "a string of size **{}** or less",
        "struct lines": "a struct **{}**",
        "struct oneline": "separated by spaces",
        "subject": "Subject",
    },
    "fr": {
        ":": " :",
        "and": "et ",
        "char": "un caractère",
        "char list": "une liste de **{}** caractères juxtaposés",
        "constraints": "Contraintes",
        "constraints perf": "Contraintes de performance",
        "first line": "Sur la première ligne,",
        "first lines": "Sur les premières lignes,",
        "input": "Entrée",
        "input decl": "L’entrée contiendra :",
        "int": "un entier",
        "int list": "une liste de **{}** entiers séparés par des espaces",
        "list": "une liste de **{}** éléments",
        "list line": "Une ligne par élément de la liste :",
        "list lines": "Chaque élément de la liste est sur plusieurs lignes :",
        "next line": "Sur la ligne suivante,",
        "next lines": "Sur les lignes suivantes,",
        "one line": "Une ligne avec",
        "output": "Sortie",
        "str": "une chaine de **{}** caractères ou moins",
        "struct lines": "une struct **{}**",
        "struct oneline": "séparés par des espaces",
        "subject": "Énoncé",
    },
}


def wrap_item(content: str, indent: int) -> List[str]:
    """Wrap list item to 79 characters max. Add identation and list bullet"""
    return textwrap.wrap(
        content,
        79,
        initial_indent="    " * indent + "- ",
        subsequent_indent="    " * indent + "  ",
    )


class Markdown:
    """Create the markdown describing the input"""

    def __init__(self, input_data: Input, lang: str = "en") -> None:
        self.input = input_data
        self.lang = LANG[lang]
        self.first_line = True
        self.start_list = False

    def line_description(self, plural: bool = False) -> str:
        """Return the key for a line: is it the first or not?"""
        if self.first_line:
            assert not self.start_list
            self.first_line = False
            return self.lang["first lines" if plural else "first line"]
        if self.start_list:
            self.start_list = False
            return self.lang["list lines" if plural else "list line"]
        return self.lang["next lines" if plural else "next line"]

    def describe_oneline(
        self, name: Optional[str], comment: Optional[str], type_: Type, indent: int
    ) -> List[str]:
        """Describe a line of input"""
        assert type_.fits_in_one_line(self.input.structs)
        type_str = ""
        if type_.main == TypeEnum.INT:
            type_str = self.lang["int"]
        elif type_.main == TypeEnum.CHAR:
            type_str = self.lang["char"]
        elif type_.main == TypeEnum.STR:
            type_str = self.lang["str"].format(type_.size)
        elif type_.main == TypeEnum.LIST:
            assert type_.encapsulated is not None
            assert type_.encapsulated.main in (TypeEnum.INT, TypeEnum.CHAR)
            if type_.encapsulated.main == TypeEnum.INT:
                type_str = self.lang["int list"]
            else:
                assert type_.encapsulated.main == TypeEnum.CHAR
                type_str = self.lang["char list"]
            type_str = type_str.format(type_.size)
        else:
            assert type_.main == TypeEnum.STRUCT
            struct = self.input.get_struct(type_.struct_name)
            type_str = "{}, {}".format(
                self.lang["struct oneline"],
                ", ".join(
                    "{}{} **{}** ({})".format(
                        "" if i != len(struct.fields) - 1 else self.lang["and"],
                        self.lang["int" if field.type.main == TypeEnum.INT else "char"],
                        field.name,
                        field.comment,
                    )
                    for i, field in enumerate(struct.fields)
                ),
            )
        content = ""
        if name is not None and comment is not None:
            content = "{} {}{} **{}**, {}.".format(
                self.line_description(), type_str, self.lang[":"], name, comment
            )
        else:
            content = "{} {}.".format(self.line_description(), type_str)
        return wrap_item(content, indent)

    def describe_multi(
        self, name: Optional[str], comment: Optional[str], type_: Type, indent: int = 0
    ) -> List[str]:
        """Describe a type taking several lines of input"""
        if type_.fits_in_one_line(self.input.structs):
            return self.describe_oneline(name, comment, type_, indent)
        if type_.main == TypeEnum.STRUCT:
            struct = self.input.get_struct(type_.struct_name)
            out = wrap_item(
                "{} {}.".format(
                    self.line_description(True), self.lang["struct lines"]
                ).format(struct.name),
                indent,
            )
            self.first_line = True
            for i in struct.fields:
                out.extend(self.describe_multi(i.name, i.comment, i.type, indent + 1))
            return out
        assert type_.main == TypeEnum.LIST
        assert type_.encapsulated is not None
        comment_and_name = ""
        if comment is not None and name is not None:
            comment_and_name = "{} **{}**, {}".format(self.lang[":"], name, comment)
        out = wrap_item(
            "{} {}{}.".format(
                self.line_description(True),
                self.lang["list"].format(type_.size),
                comment_and_name,
            ),
            indent,
        )
        self.start_list = True
        return out + self.describe_multi(None, None, type_.encapsulated, indent + 1)

    def content(self) -> str:
        """Return the generated content"""
        output = []
        for var in self.input.input:
            output.extend(self.describe_multi(var.name, var.comment, var.type))
        return "\n".join(output) + "\n"


def constraints(variables: List[Variable], lang: Dict[str, str]) -> str:
    """Return content of constraints sections"""
    l_simple = []
    l_perf = []
    for var in variables:
        simple = var.constraints_repr()
        if simple:
            l_simple.append(simple)
        perf = var.constraints_repr(True)
        if perf:
            l_perf.append(perf)
    output = []
    if l_simple:
        output.extend(["", "### {}".format(lang["constraints"]), ""])
        output.extend([f"- ${i}$" for i in l_simple])
    if l_perf:
        output.extend(["", "### {}".format(lang["constraints perf"]), ""])
        output.extend([f"- ${i}$" for i in l_perf])
    return "\n".join(output + [""])


def gen_markdown(input_data: Input, lang: str = "en") -> str:
    """Generate a markdown describing the input"""
    output = "## {}\n\n".format(LANG[lang]["subject"])
    if input_data.subject:
        output += (
            "\n".join(
                textwrap.wrap(
                    input_data.subject.replace("\n", "\n\n"),
                    79,
                    replace_whitespace=False,
                )
            )
            + "\n\n"
        )
    output += "### {}\n\n".format(LANG[lang]["input"])
    output += LANG[lang]["input decl"] + "\n\n"
    output += Markdown(input_data, lang).content()
    if input_data.output:
        output += "\n### {}\n\n".format(LANG[lang]["output"])
        output += "\n".join(textwrap.wrap(input_data.output, 79)) + "\n"
    variables = input_data.input.copy()
    for struct in input_data.structs:
        for var in struct.fields:
            variables.append(var)
    output += constraints(variables, LANG[lang])
    return output
