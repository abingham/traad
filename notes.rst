Goals
=====

Asynchronous refactoring
------------------------

Handle long-running things (e.g. most refactorings) in a separate thread.

We only need one background thread, handling the refactoring requests
in order.

These background jobs need to periodically update some state
information - progress, task description, etc - which can be accessed
by other threads.

Synchronous IDE requests
------------------------

Some requests should be handled synchronously and with priority. These
include things like calltip requests, completion requests, etc...these
are things for which the user needs immediate feedback.

Some of these things will access the state that's updated by
asynchronous tasks. For example, progress requests will access the
progress information which is updated by the asynchronous refactoring jobs.

Concurrency
-----------

We need to make sure that the shared job information is protected.

We need to make sure that the rope Project is not accessed
reentrantly. So a synchronous calltip request needs to make sure that
some asynchronous rename isn't working with the Project.

Simple model: only one task at a time; synchronous tasks take priority
over all asynchronous tasks.

Even simpler: only one task at a time. synchronous tasks wait for all
async tasks to finish. Problem: no priority for sync tasks.

The state data object can probably just be designed to be re-entrant
entirely. So this is probably not an issue. The project is a bit
trickier. Perhaps we just provide a lock_project() context manager
which we require to be used...simple enough...

Options
-------

Gevent/eventlet/etc
~~~~~~~~~~~~~~~~~~~

None of these seem to work on python3 :\ Maybe concurrence, but I'm
not sure.

Threads
~~~~~~~

This seems straightforward enough. We just need a queue with a
consumer thread that takes job after job...easy peasy, right?

State
=====

State maps task-ids to a dict of information.

Whatever's in State must be json-able since any or all of it may be
sent to clients via json.

Each task must have the following entries:

  'status': ['pending', 'started', 'success', 'failure']

optionally it can have a 'message' field.

All other fields are up to the various refactorings to fill in as
desired. Common ones might include:

  'changed_resources': list of resource paths
  'description': A description of the change

Protocol
========

What is the basic protocol for async and sync jobs?

Async
-----

E.g. rename
REQ: {'name': 'foobar', path='/something', offset=1}
RSP: {'task_id': 3, 'result': 'ok'}
RSP: {'result': 'fail', 'message': 'Failed for some reason...'}

So the response includes 'result' indicating 'ok' or 'fail'. This is
the base information for all async responses.

On success it includes a 'task_id' which can be used for status
queries.

On failure, it includes a 'message' indicating the reason. This seems
like enough for now.
