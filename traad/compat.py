"""Functions for managing Python 2/3 compatibility.
"""

import inspect
import sys

version = sys.version_info.major
if version == 2:
    getargspec = inspect.getargspec
elif version == 3:
    getargspec = inspect.getfullargspec
else:
    raise ValueError('Unexpected Python major version: {}'.format(version))
