import sys

import rope.refactor.change_signature

import traad.trace
from traad.rope.validate import validate


@traad.trace.trace
@validate
def normalize_arguments(project, state, path, offset):
    """Normalize arguments for a method.

    ``path`` may be absolute or relative. If ``path`` is relative,
    then it must to be relative to the root of the project.

    Args:
      path: The path of the file/directory to query.
      offset: The offset in the resource of the method signature.
    """

    path = project.to_relative_path(path)

    ref = project.make_refactoring(
        rope.refactor.change_signature.ChangeSignature,
        project.get_resource(path),
        offset)

    change = ref.get_change(
        [rope.refactor.change_signature.ArgumentNormalizer()])

    state.update(
        {'description': list(change.descriptions),
         'changed_resources': [r.name for r in change.resources],
         })

    change.perform()


class ChangeSignatureFunctions:
    """The change-signature related functions of the rope interface.

    A base for RopeInterface.

    """

    def change_sig(self, path, offset, operation):
        ref = self.multi_project_refactoring(
            rope.refactor.change_signature.ChangeSignature,
            self.proj,
            self.proj.get_resource(
               self._to_relative_path(path)),
            offset)

        ref.perform([operation])

        files = [res.real_path for res in ref.get_changed_resources([operation])]
        return {'files': files }


    @traad.trace.trace
    @validate
    def remove_argument(self,
                        arg_index,
                        path,
                        offset):
        """Remove an argument from a method.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          arg_index: The index of the argument to remove.
          path: The path of the file/directory to query.
          offset: The offset in the resource of the method signature.
        """
        self.change_sig(
            path, offset,
            rope.refactor.change_signature.ArgumentRemover(arg_index))
