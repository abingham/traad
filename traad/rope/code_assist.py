import rope.contrib.codeassist


class CodeAssistMixin:
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

        results = rope.contrib.codeassist.code_assist(
            self.root_project,
            code,
            offset,
            self.get_resource(path))
        rslt = [(r.name, r.get_doc(), r.scope, r.type) for r in results]
        return rslt

    def get_doc(self, code, offset, path):
        '''Get docstring for an object.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          code: The source code.
          offset: An offset into ``code`` of the object to query.
          path: The path to the resource in which the completion is
            being done.

        Returns: The docstring for the object, or None if there is no such
            documentation.
        '''

        return rope.contrib.codeassist.get_doc(
            self.root_project,
            code,
            offset,
            self.get_resource(path))

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

        return rope.contrib.codeassist.get_calltip(
            self.root_project,
            code,
            offset,
            self.get_resource(path))
