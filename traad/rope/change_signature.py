from rope.refactor.change_signature import (ArgumentAdder,
                                            ArgumentNormalizer,
                                            ArgumentRemover,
                                            ChangeSignature)

from .validate import validate


class ChangeSignatureMixin:
    @validate
    def normalize_arguments(self, path, offset):
        changers = [ArgumentNormalizer()]
        ref = ChangeSignature(
            self.root_project,
            self.get_resource(path),
            offset)
        return ref.get_changes(changers)

    @validate
    def remove_argument(self, path, offset, index):
        changers = [ArgumentRemover(index)]
        ref = ChangeSignature(
            self.root_project,
            self.get_resource(path),
            offset)
        return ref.get_changes(changers)

    @validate
    def add_argument(self, path, offset, index, name, default, value):
        changers = [ArgumentAdder(index, name, default, value)]
        ref = ChangeSignature(
            self.root_project,
            self.get_resource(path),
            offset)
        return ref.get_changes(changers)
