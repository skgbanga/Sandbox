import os
import sys
import select
import time
import curio

if __name__ == "__main__":
    os.set_blocking(sys.stdin.fileno(), False)  # magic sauce

    now = time.time()
    while True:
        r, _, _ = select.select([sys.stdin], [], [], 0)
        if sys.stdin in r:
            incoming = sys.stdin.read()  # terminal will still buffer
            print(incoming, end='')
            now = time.time()

        if time.time() - now > 5:
            print('No incoming data for 5 seconds')
            now = time.time()

# Using Curio:
# 
# async def userinput():
#     stdin = curio.io.FileStream(sys.stdin.buffer)
#     while stdin:
#         print("> ", end="", flush=True)
#         line = await stdin.read()
#         if not line:
#             break
#         text = line.decode()
#         print(text, end='')
# 
# if __name__ == "__main__":
#     curio.run(userinput)
