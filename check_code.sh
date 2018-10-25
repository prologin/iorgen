#!/usr/bin/env bash

yapf --recursive -d iorgen/ parser.py
mypy --strict iorgen parser.py
pylint iorgen parser.py
