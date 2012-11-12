import rope.refactor.change_signature

import traad.trace
from traad.rope.validate import validate


class ChangeSignatureFunctions:
    """The change-signature related functions of the rope interface.

    A base for RopeInterface.

    """

    @traad.trace.trace
    @validate
    def remove_argument(self, arg_index, path, offset):
        """Remove an argument from a method.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          arg_index: The index of the argument to remove.
          path: The path of the file/directory to query.
          offset: The offset in the resource of the method signature.
        """

        path = self._to_relative_path(path)

        change_sig = rope.refactor.change_signature.ChangeSignature(
            self.proj,
            self.proj.get_resource(path),
            offset)

        self.proj.do(
            change_sig.get_changes(
                [rope.refactor.change_signature.ArgumentRemover(arg_index)]))