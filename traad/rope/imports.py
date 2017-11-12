from rope.refactor.importutils import ImportOrganizer

from .validate import validate


class ImportsMixin:
    def _organize_imports(self, operation, path):
        organizer = ImportOrganizer(self.root_project)
        return getattr(organizer, operation)(
            self.get_resource(path))

    @validate
    def organize_imports(self, path):
        return self._organize_imports("organize_imports", path)

    @validate
    def expand_star_imports(self, path):
        return self._organize_imports("expand_star_imports", path)

    @validate
    def froms_to_imports(self, path):
        return self._organize_imports("froms_to_imports", path)

    @validate
    def relatives_to_absolutes(self, path):
        return self._organize_imports("relatives_to_absolutes", path)

    @validate
    def handle_long_imports(self, path):
        return self._organize_imports("handle_long_imports", path)
