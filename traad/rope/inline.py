import rope.refactor.inline


class InlineMixin:
    def inline(self, state, path, offset):
        path = self.to_relative_path(path)
        ref = rope.refactor.inline.create_inline(
            self.proj,
            self.get_resource(path),
            offset)
        change_set = ref.get_changes()

        state.update(
            {'description':
             change_set.get_description(),
             'changed_resources':
             [r.name for r
              in change_set.get_changed_resources()]
            })

        self.proj.do(change_set)
