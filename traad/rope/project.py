import contextlib
import os

from eagertools import emap

import pykka

import rope.base.project
from rope.refactor import multiproject

from traad.rope.change_signature import ChangeSignatureMixin
from traad.rope.codeassist import CodeAssistMixin
from traad.rope.extract import ExtractMixin
from traad.rope.findit import FinditMixin
from traad.rope.history import HistoryMixin
from traad.rope.inline import InlineMixin
from traad.rope.importutil import ImportUtilsMixin
from traad.rope.rename import RenameMixin


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


class Change:
    """This represents a single, fully-specified change that can be
    performed.

    This includes both the refactoring type as well as the arguments
    to the refactoring, the resources, etc. You can direcly call
    perform() on this object to run the refactoring.
    """
    def __init__(self, refactoring, *args):
        self.refactoring = refactoring
        self.args = args

        self.changes = self.refactoring.get_all_changes(*args)
        self._performed = False

    @property
    def descriptions(self):
        """An iterable of descriptions of the changes that this
        refactoring will make.
        """
        for proj, cset in self.changes:
            yield cset.get_description()

    @property
    def resources(self):
        """An iterable of resources that this refactoring will modify.
        """
        for proj, cset in self.changes:
            for res in cset.get_changed_resources():
                yield res

    def perform(self):
        "Perform the refactoring."
        assert not self._performed

        multiproject.perform(self.changes)
        self._performed = True


class MultiProjectRefactoring:
    """Support class for performing multi-project refactorings.
    """
    def __init__(self, project, refactoring_type, *args):
        cross_ref = multiproject.MultiProjectRefactoring(
            refactoring_type,
            list(project.cross_projects.values()))
        self.rope_ref = cross_ref(project.proj, *args)

    def get_change(self, *args):
        return Change(self.rope_ref, *args)


class Project(ChangeSignatureMixin,
              CodeAssistMixin,
              ExtractMixin,
              FinditMixin,
              InlineMixin,
              HistoryMixin,
              ImportUtilsMixin,
              RenameMixin,
              pykka.ThreadingActor):
    """An actor that controls access to an underlying Rope project.
    """
    def __init__(self, project_dir, cross_project_dirs=[]):
        super(Project, self).__init__()

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

    def to_relative_path(self, path):
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

    def get_resource(self, path):
        return self.proj.get_resource(path)

    def make_refactoring(self, refactoring_type, *args):
        return MultiProjectRefactoring(
            self,
            refactoring_type,
            *args)

    def get_children(self, path):
        '''Get a list of all child resources of a given path.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          path: The path of the file/directory to query.

        Returns: A list of tuples of the form (path,
          is_folder).

        '''

        path = self.to_relative_path(path)

        children = self.proj.get_resource(path).get_children()
        return [(child.path, child.is_folder()) for child in children]

    def get_all_resources(self):
        '''Get a list of all resources in the project.

        Returns: A list of tuples of the form (path,
            is_folder).
        '''
        return list(get_all_resources(self.proj))

    def get_root(self):
        return self.proj.root.real_path

    def __repr__(self):
        return 'Project("{}")'.format(
            self.proj.root.real_path)

    def __str__(self):
        return repr(self)
