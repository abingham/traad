import rope.refactor.extract

from .validate import validate


class ExtractMixin:
    @validate
    def extract_method(self, path, start_offset, end_offset, name):
        ref = rope.refactor.extract.ExtractMethod(
            self.root_project,
            self.get_resource(path),
            start_offset,
            end_offset)
        return ref.get_changes(name)

    @validate
    def extract_variable(self, path, start_offset, end_offset, name):
        ref = rope.refactor.extract.ExtractVariable(
            self.root_project,
            self.get_resource(path),
            start_offset,
            end_offset)
        return ref.get_changes(name)
