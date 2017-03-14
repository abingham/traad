import os

_THIS_DIR = os.path.abspath(os.path.split(__file__)[0])
ACTIVE_DIR = os.path.join(_THIS_DIR, 'active')
APPROVED_DIR = os.path.join(_THIS_DIR, 'approved')
PACKAGES_DIR = os.path.join(_THIS_DIR, 'packages')


def active(*mods):
    return os.path.join(ACTIVE_DIR, *mods)


def approved(*mods):
    return os.path.join(APPROVED_DIR, *mods)


def packages(*mods):
    return os.path.join(PACKAGES_DIR, *mods)
