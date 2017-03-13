import os
import paths


def test_find_occurrences(copy_project, start_project):
    copy_project('basic', 'main')
    proj = start_project('main')

    # Find occurrences of the Foo class
    occ = proj.find_occurrences(
        8,
        'basic/foo.py').get()

    assert len(occ) == 3


def test_find_implementations(copy_project, start_project):
    copy_project('basic', 'main')
    proj = start_project('main')
    impls = proj.find_implementations(
        33,
        'basic/overrides.py').get()
    assert len(impls) == 1


def test_find_definition(copy_project, start_project):
    copy_project('basic', 'main')
    proj = start_project('main')

    path = os.path.join(
        paths.activated_path('main'),
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
