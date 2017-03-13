def test_find_occurrences(copy_project, start_project):
    copy_project('basic', 'main')
    proj = start_project('main')

    # Find occurrences of the Foo class
    occ = proj.find_occurrences(
        8,
        'basic/foo.py').get()

    assert len(occ) == 3


# @common.use_project('basic', 'main')
# def test_find_implementations(self):
#     impls = self.proj.find_implementations(
#         33,
#         'basic/overrides.py').get()
#     self.assertEqual(len(impls), 1)


# @common.use_project('basic', 'main')
# def test_find_definition(self):
#     path = os.path.join(
#         common.activated_path('main'),
#         'basic', 'bar.py')
#     with open(path, 'r') as f:
#         code = f.read()
#     loc = self.proj.find_definition(
#         code,
#         142,
#         os.path.join('basic', 'bar.py')).get()
#     self.assertEqual(
#         loc,
#         (os.path.join('basic', 'bar.py'),
#             (91, 100), 91, False, 7))
