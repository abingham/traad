from rope.refactor.move import create_move
from .validate import validate


class MoveMixin:
    @validate
    def move_global(self, path, offset, dest_file):
        mover = create_move(self.root_project,
                            self.get_resource(path),
                            offset)
        dest_res = self.get_file(dest_file)
        if not dest_res.exists():
            dest_res.create()
        changes = mover.get_changes(dest_res)
        return changes
