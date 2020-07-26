import os
import sys
import time


def convert_size(value):
    suffix = value[-2:]
    if suffix == "KB":
        numeric = int(value[:-2])
        return numeric * 1024

    if suffix == "MB":
        numeric = int(value[:-2])
        return numeric * 1024 * 1024

    return int(value)


num_blocks = int(sys.argv[1])
size_block = convert_size(sys.argv[2])


r, w = os.pipe()
cpid = os.fork()


if cpid == 0:
    data = "0" * size_block
    for _ in range(num_blocks):
        written = os.write(w, data.encode())
        if written != len(data):
            raise ValueError("Error writing data to file")
else:
    start = time.time()
    for _ in range(num_blocks):
        data = os.read(r, size_block)
        if len(data) != size_block:
            raise ValueError("Read less data that was written")
    end = time.time()

    time_taken = end - start
    print("Elapsed time: {}".format(time_taken))
    size_mb = (num_blocks * size_block) / (1024 * 1024)
    print("Data Rate: {}".format(size_mb / time_taken))

    os.waitpid(cpid, 0)
