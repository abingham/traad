import common
import os
import paths
import pytest

import webtest

import traad.app


@pytest.fixture
def app(activate_package):
    activate_package(package='basic', into='main')

    with traad.app.using_workspace(paths.active('main')) as app:
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

    app.post_json(
        '/refactor/perform',
        resp.json)

    common.compare_workspaces(
        paths.approved('basic_rename_llama'),
        paths.active('main', 'basic'))


@pytest.mark.skip()
def test_find_occurrences(app):
    resp = app.post_json(
        '/findit/occurrences',
        {
            'path': 'basic/foo.py',
            'offset': 8,
        })
    assert len(resp.json['data']) == 3


@pytest.mark.skip()
def test_find_implementations(app):
    resp = app.post_json(
        '/findit/implementations',
        {
            'path': 'basic/overrides.py',
            'offset': 33,
        })
    assert len(resp.json['data']) == 1


@pytest.mark.skip()
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

    resp = app.post_json(
        '/refactor/perform',
        resp.json)

    with pytest.raises(ValueError):
        common.compare_workspaces(
            paths.packages('basic'),
            paths.active('main', 'basic'))

    resp = app.post_json(
        '/history/undo',
        {'index': 0})

    assert resp.json['result'] == 'success'

    common.compare_workspaces(
        paths.packages('basic'),
        paths.active('main', 'basic'))
