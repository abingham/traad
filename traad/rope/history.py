import traad.trace
from traad.rope.validate import validate


@traad.trace.trace
@validate
def undo(project, index=0):
    project.proj.history.undo(
        project.proj.history.undo_list[index])


@traad.trace.trace
@validate
def redo(project, idx=0):
    '''Redo the last undone operation.
    '''
    project.proj.history.redo(
        project.proj.history.redo_list[idx])


@traad.trace.trace
def undo_history(project):
    '''Get a list of undo-able changes.

    Returns:
      A list of descriptions of undoable changes/refactorings.
    '''
    return [cs.description for cs in project.proj.history.undo_list]


@traad.trace.trace
def redo_history(project):
    '''Get a list of redo-able changes.

    Returns:
      A list of descriptions of redoable changes/refactorings.
    '''
    return [cs.description for cs in project.proj.history.redo_list]


def _history_info(seq, idx):
    def contents(c):
        return {
            'resource': c.resource.path,
            'change': c.get_description(),
        }

    c = seq[idx]
    return {
        'description': c.description,
        'time': c.time,
        'full_change': c.get_description(),
        'changes': [contents(x) for x in c.changes],
    }


@traad.trace.trace
def undo_info(project, idx):
    '''Get information about a single undoable operation.

    Args:
      idx: An index in the undo history.

    Raises:
      IndexError: ``idx`` is out of range.

    Returns:
       A dict of information about the undoable change.
    '''
    return _history_info(project.proj.history.undo_list, idx)


@traad.trace.trace
def redo_info(project, idx):
    '''Get information about a single redoable operation.

    Args:
      idx: An index in the redo history.

    Raises:
      IndexError: ``idx`` is out of range.

    Returns:
       A dict of information about the redoable change.
    '''
    return _history_info(project.proj.history.redo_list, idx)
