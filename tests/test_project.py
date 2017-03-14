def test_get_all_resources(activate_package, start_project):
    activate_package(package='basic', into='main')
    proj = start_project('main')

    assert (sorted(proj.get_all_resources().get()) ==
            [('', True),
             ('basic', True),
             ('basic/__init__.py', False),
             ('basic/bar.py', False),
             ('basic/foo.py', False),
             ('basic/overrides.py', False)])


def test_get_children(activate_package, start_project):
    activate_package(package='basic', into='main')
    proj = start_project('main')

    assert (sorted(proj.get_children('basic').get()) ==
            [('basic/__init__.py', False),
             ('basic/bar.py', False),
             ('basic/foo.py', False),
             ('basic/overrides.py', False)])
