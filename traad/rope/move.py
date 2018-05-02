from rope.refactor.move import MoveGlobal, MoveModule
from .validate import validate


class MoveMixin:
    @validate
    def move_global(self, path, offset, dest_file):
        dest_res = self.get_file(dest_file)
        if not dest_res.exists():
            dest_res.create()

        mover = MoveGlobal(self.root_project,
                           self.get_resource(path),
                           offset)

        return mover.get_changes(dest_res)

    @validate
    def move_module(self, path, dest_folder):
        dest_res = self.get_folder(dest_folder)
        if not dest_res.exists():
            dest_res.create()

        mover = MoveModule(self.root_project,
                           self.get_resource(path))

        return mover.get_changes(dest_res)
