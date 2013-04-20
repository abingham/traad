import os

from eagertools import emap
import rope.base.project
from rope.refactor import multiproject

import traad.trace
from traad.rope.change_signature import ChangeSignatureFunctions
from traad.rope.codeassist import CodeAssistFunctions
from traad.rope.extract import ExtractFunctions
from traad.rope.findit import FinditFunctions
from traad.rope.history import HistoryFunctions
from traad.rope.importutil import ImportUtilFunctions
from traad.rope.log import log
from traad.rope.rename import RenameFunctions
from traad.rope.validate import validate


def get_all_resources(proj):
    '''Generate a sequence of (path, is_folder) tuples for all
    resources in a project.

    Args:
      proj: The rope Project to scan.

    Returns: An iterable over all resources in a Project, with a tuple
      (path, is_folder) for each.
    '''
    todo = ['']
    while todo:
        res_path = todo[0]
        todo = todo[1:]
        res = proj.get_resource(res_path)
        yield(res.path, res.is_folder())

        if res.is_folder():
            todo.extend((child.path for child in res.get_children()))

class MultiProjectRefactoring(object):
    """Support class for performing with multi-project refactorings.
    """
    def __init__(self, interface, ref, *args):
        cross_ref = multiproject.MultiProjectRefactoring(
            ref,
            list(interface.cross_projects.values()))
        self.ref = cross_ref(*args)

    def get_changed_resources(self, *args):
        """Generate the sequence of Resources that will be changed
        when *args is applied.
        """
        for proj, cset in self.ref.get_all_changes(*args):
            for res in cset.get_changed_resources():
                yield res

    def perform(self, *args):
        """Perform the refactoring with *args.
        """
        multiproject.perform(
            self.ref.get_all_changes(*args))

class RopeInterface(ChangeSignatureFunctions,
                    CodeAssistFunctions,
                    ExtractFunctions,
                    FinditFunctions,
                    HistoryFunctions,
                    ImportUtilFunctions,
                    RenameFunctions):
    def __init__(self,
                 project_dir,
                 cross_project_dirs=[]):
        self.proj = rope.base.project.Project(project_dir)

        self.cross_projects = dict()

        cross_dirs = set(cross_project_dirs)
        cross_dirs.discard(project_dir)
        emap(self.add_cross_project, cross_dirs)

    def close(self):
        self.proj.close()

    def add_cross_project(self, directory):
        """Add a cross project rooted at `directory`."""
        self.cross_projects[directory] = rope.base.project.Project(directory)

    def remove_cross_project(self, directory):
        """Remove the cross project rooted at `directory`."""
        del self.cross_projects[directory]

    def cross_project_directories(self):
        """Get a list of root directories for all cross projects."""
        return list(self.cross_projects.keys())

    @traad.trace.trace
    @validate
    def get_children(self, path):
        '''Get a list of all child resources of a given path.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          path: The path of the file/directory to query.

        Returns: A list of tuples of the form (path,
          is_folder).

        '''

        path = self._to_relative_path(path)

        children = self.proj.get_resource(path).get_children()
        return [(child.path, child.is_folder()) for child in children]

    @traad.trace.trace
    def get_all_resources(self):
        '''Get a list of all resources in the project.

        Returns: A list of tuples of the form (path,
            is_folder).
        '''
        return list(get_all_resources(self.proj))

    @traad.trace.trace
    def get_root(self):
        return self.proj.root.real_path

    def _to_relative_path(self, path):
        '''Get a version of a path relative to the project root.

        If ``path`` is already relative, then it is unchanged. If
        ``path`` is absolute, then it is made relative to the project
        root.

        Args:
          path: The path to make relative.

        Returns: ``path`` relative to the project root.

        '''
        if os.path.isabs(path):
            path = os.path.relpath(
                os.path.realpath(path),
                self.proj.root.real_path)
        return path



    def multi_project_refactoring(self, ref, *args):
        """Create a MultiProjectRefactoring object for the refactoring `ref`
        with the constructor `*args`.
        """

        return MultiProjectRefactoring(self, ref, *args)

    def __repr__(self):
        return 'RopeInterface("{}")'.format(
            self.proj.root.real_path)

    def __str__(self):
        return repr(self)
