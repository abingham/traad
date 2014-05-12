from filecmp import dircmp
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


def diff_report(dc):
    try:
        old_stdout = sys.stdout
        sys.stdout = StringIO()
        dc.report()
        return sys.stdout.getvalue()
    finally:
        sys.stdout = old_stdout


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
        raise ValueError(diff_report(dc))
