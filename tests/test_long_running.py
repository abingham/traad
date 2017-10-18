import paths
import pytest
import time

import webtest

import traad.app


@pytest.fixture
def app(activate_package):
    activate_package(package='basic', into='main')
    with traad.app.using_workspace(paths.active('main')) as app:
        yield webtest.TestApp(app)


@pytest.mark.slowtest
def test_long_running(app):
    resp = app.post_json(
        '/test/long_running',
        {'message': 'Test message!'})

    task_id = resp.json['task_id']

    results = set()

    start_time = time.time()
    while time.time() - start_time < 15:
        resp = app.get('/task/{}'.format(task_id))
        results.add(resp.body)
        time.sleep(0.1)

    assert len(results) == 11
