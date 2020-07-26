import os
import threading
import time


os.mkfifo('foo')

def threadFunc():
    print(f'{threading.get_ident()} waiting for writer to open the file')
    with open('foo', 'w') as f:
        print('opened the file')
        f.write('what is up')

threading.Thread(target=threadFunc).start()

print('parent: started the thread. sleeping now')
time.sleep(5)

print('opening the file for reading now')
with open('foo', 'r') as f:
    msg = f.read()
    print(msg)


os.unlink('foo')
