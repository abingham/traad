import timeit

from traad.test.acceptance.json_api import json_request

filename = '/Users/abingham/projects/pylons/pyramid/pyramid/request.py'

with open(filename, 'rt') as f:
    data = f.read()

def func():
    json_request('http://localhost:23445/code_assist/doc',
                 {'offset': 4683,
                  'path': filename,
                  'code': data},
                 method='GET')

print(
    timeit.timeit('func()', setup='from __main__ import func', number=100))