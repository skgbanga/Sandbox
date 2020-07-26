# bidirectional communication between a parent and child

import os
import struct


class Header:
    fmt = 'i'


r1, w1 = os.pipe()   # parent --> child
r2, w2 = os.pipe()   # child  --> parent


cpid = os.fork()
if cpid == 0:  # child
    os.close(r2)
    os.close(w1)

    while True:
        buf = os.read(r1, struct.calcsize(Header.fmt))
        size, = struct.unpack(Header.fmt, buf)

        data = os.read(r1, size)
        os.write(w2, data.upper())



os.close(r1)
os.close(w2)

while True:
    # read a line from input
    data = input()
    size = len(data)

    os.write(w1, struct.pack(Header.fmt, size))
    os.write(w1, data.encode())

    newdata = os.read(r2, len(data))
    print(newdata.decode())
