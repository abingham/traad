import os
import shutil

from traad.rope.interface import RopeInterface

THIS_DIR = os.path.split(__file__)[0]
ACTIVE_DIR = os.path.join(THIS_DIR, 'active')
PROJECT_DIR = os.path.join(THIS_DIR, 'projects')

def activate(names,
             active_dir=ACTIVE_DIR,
             project_dir=PROJECT_DIR):
    """Copy `project_dir/name` to `active_dir/name` and return a
    `RopeInterface` on the target directory.
    """
    try:
        shutil.rmtree(active_dir)
    except OSError:
        pass

    os.mkdir(active_dir)

    for name in names:
        shutil.copytree(
            os.path.join(project_dir, name),
            os.path.join(active_dir, name))

    return RopeInterface(
        os.path.join(active_dir, names[0]),
        cross_project_dirs = [os.path.join(active_dir, n) for n in names[1:]])

def deactivate(active_dir=ACTIVE_DIR):
    shutil.rmtree(active_dir)
