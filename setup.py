# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright 2018 Florian Amsallem
from setuptools import setup, find_packages

setup(
    name='iorgen',
    version='1.0',
    description='Iorgen is a multi languages code generator to parse a '
                'predefined input template',
    author='Sacha Delanoue',
    author_email='shaac@prologin.org',
    url='https://github.com/prologin/iorgen',
    packages=find_packages(),
    install_requires=[
        'pyyaml',
    ],
    entry_points={
        'console_scripts': [
            'iorgen=iorgen.__main__:main',
        ],
    }
)
