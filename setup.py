import sys

from setuptools import setup, find_packages

script_name, rope_lib = {
    2: ('traad', 'rope'),
    3: ('traad3', 'rope_py3k'),
}[sys.version_info.major]

setup(
    name = 'traad',
    version = '0.8',
    packages = find_packages(),

    # metadata for upload to PyPI
    author = 'Austin Bingham',
    author_email = 'austin.bingham@gmail.com',
    description = 'A JSON+HTTP server for the rope Python refactoring library.',
    license = 'MIT',
    keywords = 'refactoring',
    url = 'http://github.com/abingham/traad',

    entry_points = {
        'console_scripts': [
            '{} = traad.server:main'.format(script_name),
            ],
        },

    install_requires=[
        'decorator',
        'eagertools',
        'nose',
        'pykka',
        rope_lib,
        'webtest',
        'with_fixture',
    ],
)
