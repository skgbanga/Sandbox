import os
import struct
from contextlib import suppress

server_queue_name = "/tmp/server_queue"
seqnum_file = "/tmp/seqnum"

with suppress(FileExistsError):
    os.mkfifo(server_queue_name)


class Request:
    # pid
    # seqnum len
    fmt = "ii"


class Response:
    # starting seqnum
    fmt = "i"


class Seqnum:
    fmt = "i"


def opener(path, flags):
    # hijack flags here to remove O_CREAT
    return os.open(path, flags=os.O_WRONLY)


# check if seqnum file is there
seqnum = 0
if os.path.exists(seqnum_file):
    with open(seqnum_file, mode="rb") as f:
        buf = f.read()
        seqnum, = struct.unpack(Seqnum.fmt, buf)

seqnum_handle = open(seqnum_file, mode="wb", buffering=0)

with open(server_queue_name, "rb") as f:  # will block till first client
    w = open(server_queue_name, "wb")  # so that we don't get EOF while reading

    while True:
        buf = f.read(struct.calcsize(Request.fmt))
        if not buf:  # should never happen
            break
        pid, seqlen = struct.unpack(Request.fmt, buf)
        print(f"Read {seqlen} from client with pid {pid}")

        # write back to client
        client_fifo_name = f"/tmp/client_queue_{pid}"

        client_fifo = open(client_fifo_name, "wb", opener=opener)
        buf = struct.pack(Response.fmt, seqnum)
        client_fifo.write(buf)
        client_fifo.close()

        seqnum += seqlen

        # always write in the beginning
        seqnum_handle.seek(0)
        buf = struct.pack(Seqnum.fmt, seqnum)
        seqnum_handle.write(buf)


os.unlink(server_queue_name)
seqnum_handle.close()
