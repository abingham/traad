import paths

from filecmp import dircmp
from itertools import chain
import os

try:
    from io import StringIO
except ImportError:
    from StringIO import StringIO


def diff_report(dc):
    '''Generate a report of differences.

    Stolen directly from `filecmp.dircmp` and modified to be non-insane.

    Args:
        dc: A `dircmp` object.

    Returns: a string with the report.
    '''
    # Output format is purposely lousy
    sio = StringIO()

    if dc.left_only:
        dc.left_only.sort()
        sio.write(u' '.join(chain(['Only in', dc.left, ':'], dc.left_only)))
    if dc.right_only:
        dc.right_only.sort()
        sio.write(u' '.join(chain(['Only in', dc.right, ':'], dc.right_only)))
    if dc.same_files:
        dc.same_files.sort()
        sio.write(u' '.join(chain(['Identical files:'], dc.same_files)))
    if dc.diff_files:
        dc.diff_files.sort()
        sio.write(u' '.join(chain(['Differing files:'], dc.diff_files)))
    if dc.funny_files:
        dc.funny_files.sort()
        sio.write(u' '.join(chain(['Trouble with common files:'], dc.funny_files)))
    if dc.common_dirs:
        dc.common_dirs.sort()
        sio.write(u' '.join(chain(['Common subdirectories:'], dc.common_dirs)))
    if dc.common_funny:
        dc.common_funny.sort()
        sio.write(u' '.join(chain(['Common funny cases:'], dc.common_funny)))

    sio.seek(0)
    return sio.read()


def compare_projects(approved, active):
    dc = dircmp(approved, active)

    if any([dc.left_only,
            dc.right_only,
            dc.diff_files]):
        dr = diff_report(dc)
        raise ValueError(dr)
