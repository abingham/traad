import contextlib
from filecmp import dircmp
from itertools import chain
import os
import shutil
import sys

try:
    from io import StringIO
except ImportError:
    from StringIO import StringIO

from traad.rope.project import Project


THIS_DIR = os.path.split(__file__)[0]
ACTIVE_DIR = os.path.join(THIS_DIR, 'active')
PROJECT_DIR = os.path.join(THIS_DIR, 'projects')


def activate(projects,
             active_dir=ACTIVE_DIR,
             project_dir=PROJECT_DIR):
    """Copy projects from the ``project_dir`` to the ``active_dir``.

    ``projects`` is a dict mapping top-level names in the active
    directory to projects that go there. So, for example::

      {'main' : ['foo', bar']}

    would copy ``project_dir/foo`` and ``project_dir/bar`` to
    ``active_dir/main``.
    """
    try:
        shutil.rmtree(active_dir)
    except OSError:
        pass

    os.makedirs(active_dir)

    for tl_dir, prjs in projects.items():
        tl_dir = os.path.join(active_dir, tl_dir)

        try:
            os.makedirs(tl_dir)
        except OSError:
            pass

        for prj in prjs:
            shutil.copytree(
                os.path.join(project_dir, prj),
                os.path.join(tl_dir, prj))


def activate_project(projects,
                     main_dir='main',
                     active_dir=ACTIVE_DIR,
                     project_dir=PROJECT_DIR):
    activate(projects, active_dir, project_dir)

    return Project.start(
        os.path.join(active_dir, main_dir),
        cross_project_dirs=[os.path.join(active_dir, n)
                            for n in projects if n != main_dir]).proxy()


def activated_path(top_level_name):
    return os.path.join(ACTIVE_DIR, top_level_name)


def deactivate(active_dir=ACTIVE_DIR):
    shutil.rmtree(active_dir)

@contextlib.contextmanager
def use_project(projects,
                main_dir='main',
                active_dir=ACTIVE_DIR,
                project_dir=PROJECT_DIR):
    """A context-manager that calls `activate_project`, yields the
    project, and then on exit stops and deactivates the project.
    """
    try:
        proj = activate_project(projects, main_dir, active_dir, project_dir)
        yield proj
    finally:
        proj.stop()
        deactivate(active_dir)

@contextlib.contextmanager
def use_proxy(proxy):
    """A context-manager to simplify using Pykka actors in with-statement.

    This yields `proxy` and then stops it on exit.
    """
    try:
        yield proxy
    finally:
        proxy.stop()


def diff_report(dc):
    '''Generate a report of differences.

    Stolen directly from `filecmp.dircmp` and modified to be non-insane.

    Args:
        dc: A `dircmp` object.

    Returns: a string with the report.
    '''
    # Output format is purposely lousy
    sio = StringIO()

    if dc.left_only:
        dc.left_only.sort()
        sio.write(u' '.join(chain(['Only in', dc.left, ':'], s.left_only)))
    if dc.right_only:
        dc.right_only.sort()
        sio.write(u' '.join(chain(['Only in', dc.right, ':'], dc.right_only)))
    if dc.same_files:
        dc.same_files.sort()
        sio.write(u' '.join(chain(['Identical files:'], dc.same_files)))
    if dc.diff_files:
        dc.diff_files.sort()
        sio.write(u' '.join(chain(['Differing files:'], dc.diff_files)))
    if dc.funny_files:
        dc.funny_files.sort()
        sio.write(u' '.join(chain(['Trouble with common files:'], dc.funny_files)))
    if dc.common_dirs:
        dc.common_dirs.sort()
        sio.write(u' '.join(chain(['Common subdirectories:'], dc.common_dirs)))
    if dc.common_funny:
        dc.common_funny.sort()
        sio.write(u' '.join(chain(['Common funny cases:'], dc.common_funny)))

    sio.seek(0)
    return sio.read()

def compare_projects(canned_project,
                     top_level_dir,
                     project,
                     active_dir=ACTIVE_DIR,
                     project_dir=PROJECT_DIR):
    dc = dircmp(os.path.join(project_dir,
                             canned_project),
                os.path.join(active_dir,
                             top_level_dir,
                             project))

    if any([dc.left_only,
            dc.right_only,
            dc.diff_files]):
        dr = diff_report(dc)
        raise ValueError(dr)
