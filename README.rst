`Travis CI <https://travis-ci.org/abingham/traad>`_ |build-status|

=======================================
 Traad: Client-server Rope refactoring
=======================================

Traad is a client-server approach to using the
`rope <http://rope.sourceforge.net/>`_ Python refactory library. It
involves two basic components:

 1. An XMLRPC server exposing the rope API, and
 2. Client libraries (currently just emacs lisp) for talking to the
    server

The hope is that this approach will make it easier - at least in some
cases - to use rope from various other tools.

Rationale
=========

I (the author of traad) use emacs for most of my Python development,
and I've often been jealous of the cool refactoring tools that my
colleagues get with their fancy IDEs. Not jealous enough to actually
*switch*, of course. I'm way to stubborn for that. But I was jealous
enough that I investigated the options available for emacs.

One of the best options available is the *rope* Python refactoring
library. Rope is very powerful and does all of the things I'd
like. Unfortunately, I could never find a satisfactory way of
integrating it into emacs. The pymacs-based approaches never quite
worked for me, and in any case that approach struck me as somehow
incorrect.

So, in the spirit of open-source development, I decided to try my own
approach. I wanted a way to access rope functionality without having
to contort either emacs or Python in unnatural ways. Thus the idea of
using a client-server approach was born. It may strike others as odd
or incorrect, but it works well for me.

Installation
============

Traad installation involves both server and client components. Each is
discussed separately.

Python server components
------------------------

To install the Python parts of traad, just use the standard setup.py::

  cd <traad source directory>
  python setup.py install

This will install the server components, including the server program
``traad``.

emacs client components
-----------------------

To install the elisp components, copy ``elisp/traad.el`` into your
emacs load-path and call ``(require 'traad)`` somewhere in your emacs
startup. You can get more details in the documentation in ``traad.el``
itself.

You will also need to install ``xml-rpc.el``, which you can get `here
<http://emacswiki.org/emacs/xml-rpc.el>`_. ``traad`` was last tested
with version 1.6.8.

Usage
=====

The generally intended use of traad is via some client environment,
e.g. from emacs. The traad server can, of course, be run on its own,
but in general your interaction with traad will be via some other
higher-level tool which manages a traad process on your behalf.

Python server
-------------

The traad XMLRPC server is written in Python. When traad is installed,
it installs a program called (shockingly) ``traad`` which is the
server program. The basic usage of the server, should you choose to
run it standalone, is:

  usage: traad [-h] [-p, --port N] [-V, --verbose] P

  positional arguments:
    P              the directory containing the project to server

  optional arguments:
    -h, --help     show this help message and exit
    -p, --port N   the port on which the server will listen
    -V, --verbose  print debug information

You can get this information by running ``traad --help``.

emacs
-----

The first thing you need to do to use traad (once it's installed) is
to open a project. A traad project is exactly analogous to a rope
project, in that it's just a collection of Python files under some
top-level directory. To open a traad project, use ``traad-open``::

  (traad-open "~/my_python_project")

This will start a traad server. Once the project is open, you can
start interacting with it using other traad functions. For example, to
rename a Python file (and thus a module), switch to that file's buffer
and run ``traad-rename-current-file``. This will run the rope
``rename`` refactoring, it will kill the buffer you were visiting
(since it's been renamed), and it will open a new buffer visiting the
new file.

You can also rename sub-file elements like classes, functions, etc. To
do that, just put the point (i.e. your cursor) over the name you want
to change, and then run ``traad-rename``. You will be prompted
for the new name, after which rope will run that refactoring.

In general, you'll probably want to configure emacs to automatically
refresh buffers for you. This way the changes caused by the
refactorings will appear in your buffers.

Using ``virtualenv`` with ``python-environment.el``
===================================================

If you plan to use traad in a ``virtualenv`` virtual environment, you 
might look into using Takafumi Arakaki's `python-environment.el
<https://github.com/tkf/emacs-python-environment>`_. 

Using this package, you can create a new virtual environment like this::

  (python-environment-run (list "pip" "install" <path to traad source>))
  
You can then set your ``traad-server-program`` variable using ``python-environment-bin``::

  (setq traad-server-program (python-environment-bin "traad"))
  
or if you're using python3::

  (setq traad-server-program (python-environment-bin "traad3"))
  
.. Build status badge
.. |build-status|
   image:: https://secure.travis-ci.org/abingham/traad.png
           ?branch=master
   :target: http://travis-ci.org/abingham/traad
   :alt: Build Status
