from rope.base.worder import get_name_at
from rope.contrib.autoimport import AutoImport


class AutoImportMixin:
    def __init__(self):
        self._auto_import = None

    @property
    def auto_import(self):
        if self._auto_import is None:
            self._auto_import = AutoImport(self.root_project)

        # TODO: Is this too heavyweight?
        self._auto_import.generate_cache()
        return self._auto_import

    def get_imports(self, path, offset):
        for r in self.root_project.get_python_files():
            print(r.path)
        res = self.get_resource(path)
        name = get_name_at(res, offset)
        imports = self.auto_import.import_assist(name)
        location = self.auto_import.find_insertion_line(res.read())
        print(res, name, imports, location)
        return {
            'imports': imports,
            'location': location
        }
