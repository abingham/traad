"""Using rope to figure out 'kind' of thing at a point in a resource.
"""

from enum import Enum

from rope.base import evaluate, pyobjects


class ThingType(str, Enum):
    Module = 'module'
    Package = 'package'
    Function = 'function'
    Method = 'method'
    Class = 'class'


def thing_at(project, resource, offset=None):
    """Determine the kind of thing at an offset in a resource.

    This is based heavily on the logic in `rope.refactor.move.create_move`.

    Args:
        project: A `rope.base.project.Project` instance
        resource: The `Resource` to inspect
        offset: The offset into `Resource` of the thing to inspect

    Returns: A `ThingType` or `None`.
    """
    if offset is None:
        return ThingType.Module

    this_pymodule = project.get_pymodule(resource)
    pyname = evaluate.eval_location(this_pymodule, offset)

    if pyname is not None:
        obj = pyname.get_object()
        if isinstance(obj, pyobjects.PyModule):
            return ThingType.Module
        elif isinstance(obj, pyobjects.PyPackage):
            return ThingType.Package
        elif isinstance(obj, pyobjects.PyFunction):
            if isinstance(obj.parent, pyobjects.PyClass):
                return ThingType.Method
            else:
                return ThingType.Function
        elif isinstance(obj, pyobjects.PyClass):
            return ThingType.Class

        # elif isinstance(pyobject, pyobjects.PyDefinedObject) and \
        #      isinstance(pyobject.parent, pyobjects.PyModule) or \
        #      isinstance(pyname, pynames.AssignedName):
        #     return ThingType.AssignedName

    return None
