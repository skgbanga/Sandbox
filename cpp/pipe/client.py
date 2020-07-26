import os
import struct
import sys

class Request:
    fmt = 'ii'

class Response:
    fmt = 'i'


client_queue_name = f'/tmp/client_queue_{os.getpid()}'
os.mkfifo(client_queue_name)

# write to server
with open('/tmp/server_queue', 'wb') as f:
    buf = struct.pack(Request.fmt, os.getpid(), int(sys.argv[1]))
    f.write(buf)

# wait for response
# with open(client_queue_name, 'rb') as client_queue:
#     buf = client_queue.read(struct.calcsize(Response.fmt))
#     seqnum, = struct.unpack(Response.fmt, buf)
#     print(f'Got {seqnum} as the start seqnum')

while True:
    pass

os.unlink(client_queue_name)
