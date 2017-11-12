import os

import rope.base.project
import rope.refactor.inline
import rope.refactor.multiproject
import rope.refactor.rename
from rope.base.change import ChangeToData, DataToChange

from .change_signature import ChangeSignatureMixin
from .code_assist import CodeAssistMixin
from .extract import ExtractMixin
from .history import HistoryMixin
from .imports import ImportsMixin
from .validate import validate


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


class Workspace(ChangeSignatureMixin,
                CodeAssistMixin,
                ExtractMixin,
                HistoryMixin,
                ImportsMixin):
    """An actor that controls access to an underlying Rope project.
    """
    def __init__(self,
                 root_project_dir,
                 cross_project_dirs=[]):
        self._root_project = rope.base.project.Project(root_project_dir)

        self._cross_projects = dict()

        cross_dirs = set(cross_project_dirs)
        cross_dirs.discard(root_project_dir)
        for cross_dir in cross_dirs:
            self.add_cross_project(cross_dir)

    def close(self):
        self.root_project.close()

    def add_cross_project(self, directory):
        """Add a cross project rooted at `directory`."""
        self._cross_projects[directory] = rope.base.project.Project(directory)

    def remove_cross_project(self, directory):
        """Remove the cross project rooted at `directory`."""
        del self._cross_projects[directory]

    @property
    def root_project(self):
        return self._root_project

    @property
    def cross_projects(self):
        return self._cross_projects.values()

    @property
    def projects(self):
        yield self.root_project
        for cp in self.cross_projects:
            yield cp

    def to_relative_path(self, path, project=None):
        '''Get a version of a path relative to the project root.

        If ``path`` is already relative, then it is unchanged. If
        ``path`` is absolute, then it is made relative to the project
        root.

        Args:
          path: The path to make relative.
          project: The project to use as the root directory [default: root project]

        Returns: ``path`` relative to the project root.

        '''
        project = project or self.root_project

        if os.path.isabs(path):
            path = os.path.relpath(
                os.path.realpath(path),
                project.root.real_path)
        return path

    def get_resource(self, path):
        return self.root_project.get_resource(
            self.to_relative_path(path))

    def get_changes(self,
                    refactoring_type,
                    path,
                    refactoring_args,
                    change_args):
        """Calculate the changes for a specific refactoring.

        Args:
          refactoring_type: The class of the refactoring to perform (e.g.
            `rope.refactor.rename.Rename`)
          path: The path to the resource in the project.
          refactoring_args: The sequence of args to pass to the
            `refactoring_type` constructor.
          change_args: The sequence of args to pass to
            `MultiProjectRefactoring.get_all_changes`.

        Returns: All changes that would be performed by the refactoring. A list
          of the form `[[<project>, [<change set>]]`.
        """
        refactoring = refactoring_type(
            self.root_project,
            self.get_resource(
                self.to_relative_path(
                    path)),
            *refactoring_args)
        multi_project_refactoring = rope.refactor.MultiProjectRefactoring(
            refactoring, self.cross_projects)
        return multi_project_refactoring(
            self.root_project,
            *change_args).get_all_changes(*change_args)

    def perform(self, changes):
        self.root_project.do(changes)

    @validate
    def rename(self, path, offset, name):
        ref = rope.refactor.rename.Rename(
            self.root_project,
            self.get_resource(path),
            offset)
        return ref.get_changes(name)

    @validate
    def inline(self, path, offset):
        ref = rope.refactor.inline.create_inline(
            self.root_project,
            self.get_resource(path),
            offset)
        return ref.get_changes()

    def __repr__(self):
        return 'Project("{}")'.format(
            self.root_project.root.real_path)

    def __str__(self):
        return repr(self)

    def _root_to_project(self, root):
        if root == self.root_project.root.real_path:
            return self.root_project
        return self.cross_projects[root]


def changes_to_data(changes):
    return ChangeToData()(changes)


def data_to_changes(workspace, data):
    return DataToChange(workspace.root_project)(data)
