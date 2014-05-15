import time

import webtest

import traad.server


try:
    traad_app = traad.server.make_app('.')
    app = webtest.TestApp(traad_app)

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
finally:
    traad.server.stop_app(traad_app)
