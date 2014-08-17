import os
import unittest

import with_fixture

from traad.test import common


class FinditTests(with_fixture.TestCase):
    def withFixture(self):
        with common.use_project({'main': ['basic']}) as self.proj:
            yield

    def test_find_occurrences(self):
        # Find occurrences of the Foo class
        occ = self.proj.find_occurrences(
            8,
            'basic/foo.py').get()
        self.assertEqual(len(occ), 3)

    def test_find_implementations(self):
        impls = self.proj.find_implementations(
            33,
            'basic/overrides.py').get()
        self.assertEqual(len(impls), 1)

    def test_find_definition(self):
        path = os.path.join(
            common.activated_path('main'),
            'basic', 'bar.py')
        with open(path, 'r') as f:
            code = f.read()
        loc = self.proj.find_definition(
            code,
            142,
            os.path.join('basic', 'bar.py')).get()
        self.assertEqual(
            loc,
            (os.path.join('basic', 'bar.py'),
             (91, 100), 91, False, 7))

if __name__ == '__main__':
    unittest.main()
