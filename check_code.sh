#!/usr/bin/env bash

yapf --recursive -d iorgen/ test/test.py test/regenerate.py
mypy --strict iorgen test/test.py test/regenerate.py
pylint -d R0801 iorgen test/test.py test/regenerate.py
