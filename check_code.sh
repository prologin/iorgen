#!/usr/bin/env bash

yapf --recursive -d iorgen/ parser.py test/test.py
mypy --strict iorgen parser.py test/test.py
pylint -d R0801 iorgen parser.py test/test.py
