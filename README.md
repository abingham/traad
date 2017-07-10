[![Build Status](https://travis-ci.org/abingham/traad.svg)](https://travis-ci.org/abingham/traad)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

# Traad: A Python refactoring server

Traad is an refactoring server for Python code. It listens for HTTP requests to
perform refactorings, performs them, and allows clients to query for the status.
There is also talk from EuroPython 2014 with design description and some live demo: https://www.youtube.com/watch?v=NvV5OrVk24c

## Setup

To use traad you'll normally need both the server and a client.

### Emacs

The only client code (that I know of) is the
[emacs-traad](https://github.com/abingham/emacs-traad) package for Emacs.
`emacs-traad` is able to install the server for you, so if you're using it you
should just need to follow its setup instruction.

### Installing the server with pip

If you just want to install the most recent release of the Python server
components (i.e. this project), you can use pip:
```
pip install traad
```

### Installing the server from source

If you want to install the server from source - perhaps because you're doing development on it - you should first clone the repository:
```
git clone https://github.com/abingham/traad
```

Then you can install everything with `setup.py`:
```
cd traad
python setup.py install
```

### Python 2 vs. Python 3

Note that if you install `traad` into a Python 3 environment, the server name
will be `traad3`. If you install it into a Python 2 environment it will simply
be `traad`. This makes it somewhat simpler to have system-wide installations of
both on systems with both Python 2 and 3.

## Tests

`traad` has a suite of tests in the `tests` directory. They are based on [`pytest`](http://docs.pytest.org). In order to run them, go to the project's root directory (i.e. the one containing this README) and use:

```
pytest tests
```

## Rationale

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

## Design

Traad is a client-server approach to using the
[rope](https://github.com/python-rope/rope) Python refactory library. It
involves two basic components:

 1. A HTTP server exposing the rope API via JSON, and
 2. Client libraries (currently just [emacs
    lisp](https://github.com/abingham/emacs-traad)) for talking to the server

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
