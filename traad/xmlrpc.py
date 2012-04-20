import sys

major_version = sys.version_info.major

if major_version == 2:
    from SimpleXMLRPCServer import SimpleXMLRPCServer
    from xmlrpclib import ServerProxy

elif major_version == 3:
    from xmlrpc.client import ServerProxy
    from xmlrpc.server import SimpleXMLRPCServer

else:
    assert False, 'Only supported on Python 2 and 3. Your version = {}'.format(
        sys.version)