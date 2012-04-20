import distribute_setup
distribute_setup.use_setuptools()

from setuptools import setup, find_packages

setup(
    name = 'traad',
    version = '0.1',
    packages = find_packages(),

    # metadata for upload to PyPI
    author = 'Austin Bingham',
    author_email = 'austin.bingham@gmail.com',
    description = 'An XMLRPC server for the rope Python refactoring library.',
    license = 'MIT',
    keywords = 'refactoring',
    url = 'http://github.com/abingham/traad',

    entry_points = {
        'console_scripts': [
            'traad = traad.server:main',
            ],
        },

    install_requires=[
        'rope'
    ],
)
