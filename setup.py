from setuptools import setup, find_packages
import sys

install_requires=[
    'rope',
]

if sys.version_info < (3, 4):
    install_requires.append('enum34')

setup(
    name='traad',
    version='3.0.1',
    packages=find_packages(),

    # metadata for upload to PyPI
    author='Austin Bingham',
    author_email='austin.bingham@gmail.com',
    description='A JSON+HTTP server for the rope Python refactoring library.',
    license='MIT',
    keywords='refactoring',
    url='http://github.com/abingham/traad',

    entry_points={
        'console_scripts': [
            'traad = traad.server:main',
            ],
        },

    install_requires=install_requires,
    extras_require={
        'test': ['pytest', 'tox', 'webtest'],
    },
)
