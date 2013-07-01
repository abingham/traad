import json
import urllib.request

jdata = json.dumps(
    {'llama': 42}).encode('utf-8')

req = urllib.request.Request(
    url='http://localhost:8080/refactor/rename',
    data=jdata,
    method='POST',
    headers={
        'Content-Type': 'application/json;charset=utf-8',
        }
    )
rslt = urllib.request.urlopen(req)
print(rslt.read())
