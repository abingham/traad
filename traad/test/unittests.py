import logging
import os
import unittest


def run():
    suite = unittest.TestLoader().discover(
        start_dir=os.path.split(__file__)[0],
        pattern='*_test.py')

    unittest.TextTestRunner(verbosity=1).run(suite)

if __name__ == '__main__':
    logging.basicConfig(
        level=logging.CRITICAL)
    run()
