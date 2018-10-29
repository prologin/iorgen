#!/usr/bin/env bash

yapf --recursive -d iorgen/ parser.py
mypy --strict iorgen parser.py
pylint -d R0801 iorgen parser.py
