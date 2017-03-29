import sys

import rope.refactor.change_signature

from traad.rope.validate import validate
from traad.state import task_state_monitor
import traad.trace


class ChangeSignatureMixin:
    @validate
    @task_state_monitor
    def change_sig(self, state, path, offset, refactoring):
        """Common implementation for change-signature refactorings.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          self: The Project on which this operates.
          state: The TaskState for this refactoring.
          path: The path of the file/directory to query.
          offset: The offset in the resource of the method signature
            (i.e., the distance into the file of the cursor).
          refactoring: The refactoring job to run.
        """

        path = self.to_relative_path(path)

        ref = self.make_refactoring(
            rope.refactor.change_signature.ChangeSignature,
            self.get_resource(path),
            offset)

        change = ref.get_change(
            [refactoring])

        state.update(
            {'description': list(change.descriptions),
             'changed_resources': [r.name for r in change.resources],
             })

        change.perform()

    @traad.trace.trace
    def normalize_arguments(self, state, path, offset):
        """Normalize arguments for a method.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          self: The Project on which this operates.
          state: The TaskState for this refactoring.
          path: The path of the file/directory to query.
          offset: The offset in the resource of the method signature.
        """
        self.change_sig(
            state,
            path,
            offset,
            rope.refactor.change_signature.ArgumentNormalizer())


    @traad.trace.trace
    def remove_argument(self,
                        state,
                        arg_index,
                        path,
                        offset):
        """Remove an argument from a method.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          self: The Project on which this operates.
          state: The TaskState for this refactoring.
          arg_index: The index of the argument to remove.
          path: The path of the file/directory to query.
          offset: The offset in the resource of the method signature.
        """
        self.change_sig(
            state,
            path,
            offset,
            rope.refactor.change_signature.ArgumentRemover(arg_index))


    @traad.trace.trace
    def add_argument(self,
                     state,
                     path,
                     offset,
                     index,
                     name,
                     default=None,
                     value=None):
        """Add an argument to a method.

        Args:
          self: The Project on which this operates.
          state: The TaskState for this refactoring.
          path: The path of the file/directory to query.
          offset: The offset in the resource of the method signature.
          index: The position of the new argument in the signature.
          name: The name of the new argument.
          default: The default argument value (optional).
          value: The argument value (optional).
        """
        self.change_sig(
            state,
            path,
            offset,
            rope.refactor.change_signature.ArgumentAdder(index, name,
                                                         default=default,
                                                         value=value))
