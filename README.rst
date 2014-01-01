`Travis CI <https://travis-ci.org/abingham/traad>`_ |build-status|

=======================================
 Traad: Client-server Rope refactoring
=======================================

Traad is a client-server approach to using the
`rope <http://rope.sourceforge.net/>`_ Python refactory library. It
involves two basic components:

 1. A HTTP server exposing the rope API via JSON, and
 2. Client libraries (currently just emacs lisp) for talking to the
    server

The hope is that this approach will make it easier - at least in some
cases - to use rope from various other tools.

Since rope is written in Python, any tool that wants to use rope needs
to either embed Python, or it needs to find a way to communicate with
a Python process running rope. The embedding approach is difficult if
the target environment is not written in Python, and it also faces
challenges when dealing with more than one Python version.

So traad aims to simplify communication with rope running in an
independent process. HTTP communication and JSON data handling is well
supported in many, many languages, so any environment that wants to
use rope should be able to easily communicate with traad.

Most of the documentation for traad can be found on the `wiki
<http://github.com/abingham/traad/wiki>`_.

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

.. Build status badge
.. |build-status|
   image:: https://secure.travis-ci.org/abingham/traad.png
           ?branch=master
   :target: http://travis-ci.org/abingham/traad
   :alt: Build Status
