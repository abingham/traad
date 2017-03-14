import os
import paths
import pytest


@pytest.fixture
def proj(activate_package, start_project):
    activate_package(package='basic', into='main')
    yield start_project('main')


def test_find_occurrences(proj):
    # Find occurrences of the Foo class
    occ = proj.find_occurrences(
        8,
        'basic/foo.py').get()

    assert len(occ) == 3


def test_find_implementations(proj):
    impls = proj.find_implementations(
        33,
        'basic/overrides.py').get()
    assert len(impls) == 1


def test_find_definition(proj):
    path = os.path.join(
        paths.active('main'),
        'basic', 'bar.py')

    with open(path, 'r') as f:
        code = f.read()

    loc = proj.find_definition(
        code,
        142,
        os.path.join('basic', 'bar.py')).get()

    assert loc == (os.path.join('basic', 'bar.py'),
                   (91, 100),
                   91,
                   False,
                   7)
