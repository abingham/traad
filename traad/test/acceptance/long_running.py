import time
import unittest

import webtest

import traad.app


class LongRunningTest(unittest.TestCase):
    def test_long_running(self):
        with traad.app.bind_to_project('.') as traad_app:
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

            self.assertEqual(len(results), 11)

if __name__ == '__main__':
    unittest.main()
