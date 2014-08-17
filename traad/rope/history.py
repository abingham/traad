import traad.trace
from traad.rope.validate import validate


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


class HistoryMixin:
    @traad.trace.trace
    @validate
    def undo(self, index=0):
        self.proj.history.undo(
            self.proj.history.undo_list[index])


    @traad.trace.trace
    @validate
    def redo(self, idx=0):
        '''Redo the last undone operation.
        '''
        self.proj.history.redo(
            self.proj.history.redo_list[idx])


    @traad.trace.trace
    def undo_history(self):
        '''Get a list of undo-able changes.

        Returns:
          A list of descriptions of undoable changes/refactorings.
        '''
        return [cs.description for cs in self.proj.history.undo_list]


    @traad.trace.trace
    def redo_history(self):
        '''Get a list of redo-able changes.

        Returns:
          A list of descriptions of redoable changes/refactorings.
        '''
        return [cs.description for cs in self.proj.history.redo_list]


    @traad.trace.trace
    def undo_info(self, idx):
        '''Get information about a single undoable operation.

        Args:
          idx: An index in the undo history.

        Raises:
          IndexError: ``idx`` is out of range.

        Returns:
           A dict of information about the undoable change.
        '''
        return _history_info(self.proj.history.undo_list, idx)


    @traad.trace.trace
    def redo_info(self, idx):
        '''Get information about a single redoable operation.

        Args:
          idx: An index in the redo history.

        Raises:
          IndexError: ``idx`` is out of range.

        Returns:
           A dict of information about the redoable change.
        '''
        return _history_info(self.proj.history.redo_list, idx)
