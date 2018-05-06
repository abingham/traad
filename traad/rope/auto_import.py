from logging import getLogger

from rope.base.worder import get_name_at
from rope.contrib.autoimport import AutoImport

log = getLogger()


# We use different name-fetching backends depending on whether ultan is
# available or not.
try:
    from ultan.name_index import NameIndex

    class NameGetter:
        def __init__(self):
            self.name_index = NameIndex()

        def get_names(self, name):
            return self.name_index.get_names(name)

except ImportError:
    log.info('Ultan not available. Falling back to rope auto-import.')

    # We only really come down this path if we're on python 2.7, so we inherit
    # from object to make super work later. This is a little shady, but that's
    # how I roll.
    class NameGetter(object):
        def get_names(self, name):
            return self.auto_import.import_assist(name)


class AutoImportMixin(NameGetter):
    def __init__(self):
        super(AutoImportMixin, self).__init__()
        self._auto_import = None
        self.name_getter = NameGetter()

    @property
    def auto_import(self):
        if self._auto_import is None:
            self._auto_import = AutoImport(self.root_project)
        # No need to generate the cache...we don't use it.
        return self._auto_import

    def get_imports(self, path, offset):
        for r in self.root_project.get_python_files():
            print(r.path)
        res = self.get_resource(path)
        name = get_name_at(res, offset)
        imports = tuple(self.get_names(name))
        location = self.auto_import.find_insertion_line(res.read())
        return {
            'imports': imports,
            'location': location
        }
