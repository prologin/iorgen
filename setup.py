# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Florian Amsallem
# Copyright 2020-2022 Sacha Delanoue

from setuptools import setup, find_packages

with open("README.md", "r") as fh:
    long_description = fh.read()

setup(
    name="iorgen",
    version="1.0",
    author="Sacha Delanoue",
    author_email="shaac@prologin.org",
    description="Iorgen is a multi languages code generator to parse a "
    "predefined input template",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/prologin/iorgen",
    packages=find_packages(),
    classifiers=[
        "Development Status :: 5 - Production/Stable",
        "Environment :: Console",
        "License :: OSI Approved :: GNU General Public License v3 or later (GPLv3+)",
        "Operating System :: OS Independent",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Topic :: Software Development :: Code Generators",
        "Typing :: Typed",
    ],
    install_requires=[
        "pyyaml",
    ],
    entry_points={
        "console_scripts": [
            "iorgen=iorgen.__main__:main",
        ],
    },
    python_requires=">=3.7",
)
