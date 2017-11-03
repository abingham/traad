import rope.contrib.codeassist

import traad.trace
from traad.rope.validate import validate


class CodeAssistMixin:
    @traad.trace.trace
    @validate
    def get_calltip(self, code, offset, path):
        """Get the calltip of a function.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must be relative to the root of the project.

        Args:
          self: The Project to use.
          code: The source code.
          offset: An offset into ``code`` of the object to query.
          path: The path to the resource in which the search is
            being done.

        Returns: A calltip string.

        """

        path = self.to_relative_path(path)
        return rope.contrib.codeassist.get_calltip(
            self.proj,
            code,
            offset,
            self.get_resource(path))


    @traad.trace.trace
    @validate
    def get_doc(self, code, offset, path):
        '''Get docstring for an object.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          self: The Project to work against.
          code: The source code.
          offset: An offset into ``code`` of the object to query.
          path: The path to the resource in which the completion is
            being done.

        Returns: The docstring for the object, or None if there is no such
            documentation.
        '''

        path = self.to_relative_path(path)
        return rope.contrib.codeassist.get_doc(
            self.proj,
            code,
            offset,
            self.get_resource(path))


# class CodeAssistFunctions:
#     """The codeassist related functions of the rope interface.

#     A base for RopeInterface.

#     """

#     @traad.trace.trace
#     @validate
#     def get_definition_location(self, code, offset, path):
#         '''Get location of definition for a symbol.

#         ``path`` may be absolute or relative. If ``path`` is relative,
#         then it must to be relative to the root of the project.

#         Args:
#           code: The source code.
#           offset: An offset into ``code`` of the object to query.
#           path: The path to the resource in which the search is
#             being done.

#         Returns: A tuple of the form (path, lineno). If no definition
#           can be found, then (None, None) is returned.
#         '''
#         path = self._to_relative_path(path)
#         rslt = rope.contrib.codeassist.get_definition_location(
#             self.proj,
#             code,
#             offset,
#             self.proj.get_resource(path))

#         if rslt[1] is None:
#             return rslt

#         if rslt[0] is None:
#             return (path, rslt[1])

#         return (rslt[0].real_path, rslt[1])
