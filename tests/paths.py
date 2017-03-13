import os

_THIS_DIR = os.path.abspath(os.path.split(__file__)[0])
ACTIVE_DIR = os.path.join(_THIS_DIR, 'active')
PROJECT_DIR = os.path.join(_THIS_DIR, 'projects')


def activated_path(mod):
    return os.path.join(ACTIVE_DIR, mod)
