import rope.contrib.codeassist

import traad.trace
from traad.rope.validate import validate


class CodeAssistFunctions:
    """The codeassist related functions of the rope interface.

    A base for RopeInterface.

    """

    @traad.trace.trace
    @validate
    def code_assist(self, code, offset, path):
        '''Get code-assist completions for a point in a file.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          code: The source code in which the completion should
            happen. Note that this may differ from the contents of the
            resource at ``path``.
          offset: The offset into ``code`` where the completion should
            happen.
          path: The path to the resource in which the completion is
            being done.

        Returns: A list of tuples of the form (name, documentation,
          scope, type) for each possible completion.
        '''

        path = self._to_relative_path(path)
        results = rope.contrib.codeassist.code_assist(
            self.proj,
            code,
            offset,
            self.proj.get_resource(path))
        return [(r.name, r.get_doc(), r.scope, r.type) for r in results]

    @traad.trace.trace
    @validate
    def get_doc(self, code, offset, path):
        '''Get docstring for an object.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          code: The source code.
          offset: An offset into ``code`` of the object to query.
          path: The path to the resource in which the completion is
            being done.

        Returns: The docstring for the object.
        '''

        path = self._to_relative_path(path)
        return rope.contrib.codeassist.get_doc(
            self.proj,
            code,
            offset,
            self.proj.get_resource(path))

    @traad.trace.trace
    @validate
    def get_calltip(self, code, offset, path):
        """Get the calltip of a function.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must be relative to the root of the project.

        Args:
          code: The source code.
          offset: An offset into ``code`` of the object to query.
          path: The path to the resource in which the search is
            being done.

        Returns: A calltip string.

        """

        path = self._to_relative_path(path)
        return rope.contrib.codeassist.get_calltip(
            self.proj,
            code,
            offset,
            self.proj.get_resource(path))

    @traad.trace.trace
    @validate
    def get_definition_location(self, code, offset, path):
        '''Get location of definition for a symbol.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          code: The source code.
          offset: An offset into ``code`` of the object to query.
          path: The path to the resource in which the search is
            being done.

        Returns: A tuple of the form (path, lineno). If no definition
          can be found, then (None, None) is returned.
        '''
        path = self._to_relative_path(path)
        rslt = rope.contrib.codeassist.get_definition_location(
            self.proj,
            code,
            offset,
            self.proj.get_resource(path))

        if rslt[1] is None:
            return rslt

        if rslt[0] is None:
            return (path, rslt[1])

        return (rslt[0].real_path, rslt[1])

    # TODO: calltip, definition location, occurences/findit