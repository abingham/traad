import common
import os
import paths
import pytest
import time

import webtest

import traad.app


def wait_for_task(task_id, app):
    while True:
        resp = app.get('/task/{}'.format(task_id))

        if resp.json['status'] == 'success':
            return True
        elif resp.json['status'] == 'failure':
            return False

        time.sleep(0.01)


@pytest.fixture
def app(activate_package):
    activate_package(package='basic', into='main')

    with traad.app.using_project(paths.active('main')) as app:
        yield webtest.TestApp(app)


def test_rename(app):
    resp = app.post_json(
        '/refactor/rename',
        {
            'name': 'Llama',
            'path': 'basic/foo.py',
            'offset': 8
        })

    assert resp.json['result'] == 'success'
    task_id = resp.json['task_id']

    assert wait_for_task(task_id, app)

    common.compare_projects(
        paths.approved('basic_rename_llama'),
        paths.active('main', 'basic'))


def test_find_occurrences(app):
    resp = app.post_json(
        '/findit/occurrences',
        {
            'path': 'basic/foo.py',
            'offset': 8,
        })
    assert len(resp.json['data']) == 3


def test_find_implementations(app):
    resp = app.post_json(
        '/findit/implementations',
        {
            'path': 'basic/overrides.py',
            'offset': 33,
        })
    assert len(resp.json['data']) == 1


def test_find_definition(app):
    path = os.path.join(
        paths.active('main'),
        'basic', 'bar.py')

    resp = app.post_json(
        '/findit/definition',
        {
            'path': path,
            'offset': 142,
        })

    assert (resp.json['data'] ==
            [os.path.join('basic', 'bar.py'),
             [91, 100],
             91,
             False,
             7])


def test_undo_undoes_changes(app):
    resp = app.post_json(
        '/refactor/rename',
        {
            'name': 'Llama',
            'path': 'basic/foo.py',
            'offset': 8,
        })

    if resp.json['result'] != 'success':
        print(resp.json['message'])
    assert resp.json['result'] == 'success'
    task_id = resp.json['task_id']

    assert wait_for_task(task_id, app)

    with pytest.raises(ValueError):
        common.compare_projects(
            paths.packages('basic'),
            paths.active('main', 'basic'))

    resp = app.post_json(
        '/history/undo',
        {'index': 0})

    assert resp.json['result'] == 'success'

    common.compare_projects(
        paths.packages('basic'),
        paths.active('main', 'basic'))
