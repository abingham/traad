import io
import os
from setuptools import setup, find_packages
import sys

install_requires = [
    'rope',
]

if sys.version_info < (3, 4):
    install_requires.append('enum34')
else:
    # We only enable ultan stuff in 3.4+
    install_requires.append('ultan')


def local_file(*name):
    return os.path.join(
        os.path.dirname(__file__),
        *name)


def read(name, **kwargs):
    with io.open(
        name,
        encoding=kwargs.get("encoding", "utf8")
    ) as handle:
        return handle.read()


# This is unfortunately duplicated from scripts/cosmic_ray_tooling.py. I
# couldn't find a way to use the original version and still have tox
# work...hmmm...
def read_version(version_file):
    "Read the `(version-string, version-info)` from `version.py`."
    local_vars = {}
    with open(version_file) as handle:
        exec(handle.read(), {}, local_vars)  # pylint: disable=exec-used
    return (local_vars['__version__'], local_vars['__version_info__'])


setup(
    name='traad',
    version=read_version(local_file('traad', 'version.py'))[0],
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
