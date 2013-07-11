import contextlib
import os
import threading

from eagertools import emap

import rope.base.project
from rope.refactor import multiproject


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

        # TODO: Perhaps this can be delayed until required.
        self.changes = self.refactoring.get_all_changes(*args)

    @property
    def descriptions(self):
        """An iterable of descriptions of the changes that this
        refactoring will make.
        """
        for proj, cset in self.changes:
            for desc in cset.get_description():
                yield desc

    @property
    def resources(self):
        """An iterable of resources that this refactoring will modify.
        """
        for proj, cset in self.changes:
            for res in cset.get_changed_resources():
                yield res

    def perform(self):
        "Perform the refactoring."
        multiproject.perform(self.changes)


class MultiProjectRefactoring:
    """Support class for performing multi-project refactorings.
    """
    def __init__(self, project, refactoring_type, *args):
        # TODO: Why do we only pass in cross_project.values()? What
        # about the "base" project? Strange...
        cross_ref = multiproject.MultiProjectRefactoring(
            refactoring_type,
            list(project.cross_projects.values()))
        self.rope_ref = cross_ref(project.proj, *args)

    def get_change(self, *args):
        return Change(self.rope_ref, *args)


class Project:
    """Manages an underlying rope.base.Project, providing a lock to
    serialize access to it, and is a factory for creating refactoring
    objects.
    """
    def __init__(self, project_dir, cross_project_dirs=[]):
        self.proj = rope.base.project.Project(project_dir)

        self.cross_projects = dict()

        cross_dirs = set(cross_project_dirs)
        cross_dirs.discard(project_dir)
        emap(self.add_cross_project, cross_dirs)

        self._lock = threading.Lock()

    def close(self):
        self.proj.close()

    def add_cross_project(self, directory):
        """Add a cross project rooted at `directory`."""
        self.cross_projects[directory] = rope.base.project.Project(directory)

    @contextlib.contextmanager
    def lock(self):
        self._lock.acquire()
        try:
            yield
        finally:
            self._lock.release()

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
