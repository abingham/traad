import sys

import rope.refactor.change_signature

import traad.trace
from traad.rope.validate import validate


def change_sig(project, state, path, offset, refactoring):
    """Common implementation for change-signature refactorings.

    ``path`` may be absolute or relative. If ``path`` is relative,
    then it must to be relative to the root of the project.

    Args:
      project: The Project on which this operates.
      state: The TaskState for this refactoring.
      path: The path of the file/directory to query.
      offset: The offset in the resource of the method signature.
      refactoring: The refactoring job to run.
    """

    path = project.to_relative_path(path)

    ref = project.make_refactoring(
        rope.refactor.change_signature.ChangeSignature,
        project.get_resource(path),
        offset)

    change = ref.get_change(
        [refactoring])

    state.update(
        {'description': list(change.descriptions),
         'changed_resources': [r.name for r in change.resources],
         })

    change.perform()

@traad.trace.trace
@validate
def normalize_arguments(project, state, path, offset):
    """Normalize arguments for a method.

    ``path`` may be absolute or relative. If ``path`` is relative,
    then it must to be relative to the root of the project.

    Args:
      project: The Project on which this operates.
      state: The TaskState for this refactoring.
      path: The path of the file/directory to query.
      offset: The offset in the resource of the method signature.
    """
    change_sig(
        project,
        state,
        path,
        offset,
        rope.refactor.change_signature.ArgumentNormalizer())


@traad.trace.trace
@validate
def remove_argument(project,
                    state,
                    arg_index,
                    path,
                    offset):
    """Remove an argument from a method.

    ``path`` may be absolute or relative. If ``path`` is relative,
    then it must to be relative to the root of the project.

    Args:
      project: The Project on which this operates.
      state: The TaskState for this refactoring.
      arg_index: The index of the argument to remove.
      path: The path of the file/directory to query.
      offset: The offset in the resource of the method signature.
    """
    change_sig(
        project,
        state,
        path,
        offset,
        rope.refactor.change_signature.ArgumentRemover(arg_index))
