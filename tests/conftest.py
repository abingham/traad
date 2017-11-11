import logging
import os
import pytest
import shutil
from traad.rope.workspace import Workspace

from paths import ACTIVE_DIR, PACKAGES_DIR

# We don't want to see logging output for the most part. We intentionally
# trigger cases where traad will log to error, and those just mess up the
# output.
logging.basicConfig(level=logging.CRITICAL)


@pytest.fixture
def make_workspace():
    def workspace_factory(main, *cross):
        proj = Workspace(
            os.path.join(ACTIVE_DIR, main),
            cross_project_dirs=[
                os.path.join(ACTIVE_DIR, project)
                for project in cross])
        return proj

    return workspace_factory


@pytest.fixture
def activate_package():
    def f(package, into):
        dest_dir = os.path.join(ACTIVE_DIR, into)

        shutil.rmtree(dest_dir, ignore_errors=True)
        try:
            os.makedirs(dest_dir)
        except OSError:
            pass

        shutil.copytree(
            os.path.join(PACKAGES_DIR, package),
            os.path.join(dest_dir, package))

    try:
        yield f
    except:
        pass

    try:
        shutil.rmtree(ACTIVE_DIR)
    except OSError:
        pass
