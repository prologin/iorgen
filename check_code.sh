#!/usr/bin/env bash

yapf -p --recursive -d iorgen/ test/test.py test/regenerate.py
r1=$?
mypy --strict iorgen test/test.py test/regenerate.py
r2=$?
pylint -j 0 -d R0801 iorgen test/test.py test/regenerate.py
r3=$?
[ $r1 -eq 0 ] && [ $r2 -eq 0 ] && [ $r3 -eq 0 ]
