import logging
import os

import rope.base.project
import rope.refactor.extract
import rope.refactor.rename

from .util import cmap
from .xmlrpc import SimpleXMLRPCServer

log = logging.getLogger(__name__)

def get_all_resources(proj):
    '''Generate a sequence of (path, is_folder) tuples for all
    resources in a project.

    Args:
      proj: The rope Project to scan.

    Returns: An iterable over all resources in a Project, with a tuple
      (path, is_folder) for each.
    '''
    todo = ['']
    while todo:
        res_path = todo[0]
        todo = todo[1:]
        res = proj.get_resource(res_path)
        yield(res.path, res.is_folder())

        if res.is_folder():
            todo.extend((child.path for child in res.get_children()))

class ProjectServer(SimpleXMLRPCServer):
    def __init__(self,
                 project_dir,
                 *args,
                 **kwargs):
        self.proj = rope.base.project.Project(project_dir)

        SimpleXMLRPCServer.__init__(
            self,
            allow_none=True,
            *args, **kwargs)

        exported_functions = [
            self.extract_method,
            self.get_all_resources,
            self.get_children,
            self.undo,
            self.redo,
            self.rename,
            ]
        cmap(self.register_function, exported_functions)

    def get_children(self, path):
        '''Get a list of all child resources of a given path.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          path: The path of the file/directory to query.

        Returns: A list of tuples of the form (path,
          is_folder).

        '''

        path = self._to_relative_path(path)
        children = self.proj.get_resource(path).get_children()
        return [(child.path, child.is_folder()) for child in children]

    def get_all_resources(self):
        '''Get a list of all resources in the project.

        Returns: A list of tuples of the form (path,
            is_folder).
        '''
        return list(get_all_resources(self.proj))

    def undo(self):
        '''Undo the last operation.
        '''
        self.proj.history.undo()

    def redo(self):
        '''Redo the last undone operation.
        '''
        self.proj.history.redo()

    def extract_method(self, name, path, start_offset, end_offset):
        '''Extract a method.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          new_name: The name for the new method.
          path: The path of the resource containing the code.
          start_offset: The starting offset of the region to extract.
          end_offset: The end (one past the last character) of the
            region to extract.
        '''

        path = self._to_relative_path(path)

        extractor = rope.refactor.extract.ExtractMethod(
            self.proj,
            self.proj.get_resource(path),
            start_offset,
            end_offset)

        self.proj.do(extractor.get_changes(name))

    def rename(self, new_name, path, offset=None):
        '''Rename a resource.

        ``path`` may be absolute or relative. If ``path`` is relative,
        then it must to be relative to the root of the project.

        Args:
          path: The path of the file/directory to query.
        '''

        path = self._to_relative_path(path)

        renamer = rope.refactor.rename.Rename(
            self.proj,
            self.proj.get_resource(path),
            offset)
        self.proj.do(renamer.get_changes(new_name))

    def _to_relative_path(self, path):
        '''Get a version of a path relative to the project root.

        If ``path`` is already relative, then it is unchanged. If
        ``path`` is absolute, then it is made relative to the project
        root.

        Args:
          path: The path to make relative.

        Returns: ``path`` relative to the project root.

        '''
        if os.path.isabs(path):
            path = os.path.relpath(
                path,
                self.proj.root.real_path)
        return path

def run_server(port, project):
    log.info(
        'Running traad server for project "{}" on port {}'.format(
            project, port))

    server = ProjectServer(
        project,
        ('127.0.0.1', port))

    server.serve_forever()

def main():
    import argparse

    parser = argparse.ArgumentParser(
        description='Run a traad server.')

    parser.add_argument(
        '-p, --port', metavar='N', type=int,
        dest='port', default=6942,
        help='the port on which the server will listen')

    parser.add_argument(
        '-V, --verbose',
        dest='verbose', default=False, action='store_true',
        help='print debug information')

    parser.add_argument(
        'project', metavar='P', type=str,
        help='the directory containing the project to server')

    args = parser.parse_args()

    # Configure logging
    level = logging.INFO
    if args.verbose:
        level = logging.DEBUG
    logging.basicConfig(
        level=level)

    run_server(args.port, args.project)

if __name__ == '__main__':
    main()