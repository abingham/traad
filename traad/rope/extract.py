import rope.refactor.extract

import traad.trace
from traad.rope.validate import validate


@traad.trace.trace
@validate
def _extract(project, state, method, name, path, start_offset, end_offset):
    """Shared implementation of extract_method and _variable.
    """
    path = project.to_relative_path(path)

    ref = project.make_refactoring(
        method,
        project.get_resource(path),
        start_offset,
        end_offset)

    change = ref.get_change(name)

    state.update(
        {'description': list(change.descriptions),
         'changed_resources': [r.name for r in change.resources],
     })

    change.perform()


@traad.trace.trace
def extract_method(project, state, name, path, start_offset, end_offset):
    """Extract a method.

    ``path`` may be absolute or relative. If ``path`` is relative,
    then it must to be relative to the root of the project.

    Args:
      new_name: The name for the new method.
      path: The path of the resource containing the code.
      start_offset: The starting offset of the region to extract.
      end_offset: The end (one past the last character) of the
        region to extract.
    """
    _extract(project,
             state,
             rope.refactor.extract.ExtractMethod,
             name,
             path,
             start_offset,
             end_offset)


@traad.trace.trace
def extract_variable(project, state, name, path, start_offset, end_offset):
    """Extract a variable.

    ``path`` may be absolute or relative. If ``path`` is relative,
    then it must to be relative to the root of the project.

    Args:
      new_name: The name for the new variable.
      path: The path of the resource containing the code.
      start_offset: The starting offset of the region to extract.
      end_offset: The end (one past the last character) of the
        region to extract.
    """
    _extract(project,
             state,
             rope.refactor.extract.ExtractVariable,
             name,
             path,
             start_offset,
             end_offset)
