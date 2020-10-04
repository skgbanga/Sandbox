# https://www.youtube.com/watch?v=Y4Gt3Xjd7G8
# https://gist.github.com/dabeaz/f86ded8d61206c757c5cd4dbb5109f74

import time


def countdown(n):
    while n > 0:
        print(f"Down {n}")
        time.sleep(1)
        n -= 1


def countup(stop):
    n = 0
    while n < stop:
        print(f"Up {n}")
        time.sleep(1)
        n += 1


# 1. sequential execution

# countdown(5)
# countup(5)


# 2. concurrent execution
# import threading

# threading.Thread(target=countdown, args=(5,)).start()
# threading.Thread(target=countup, args=(5,)).start()


# => how to do the above concurrently

from collections import deque
import heapq


class Scheduler:
    def __init__(self):
        self.ready = deque()
        self.sleeping = []
        self.sequence = 0

    def call_soon(self, func):
        self.ready.append(func)

    def call_later(self, delay, func):
        self.sequence += 1
        deadline = time.time() + delay
        heapq.heappush(self.sleeping, (deadline, self.sequence, func))

    def run(self):
        while self.ready or self.sleeping:
            if not self.ready:
                deadline, _, func = heapq.heappop(self.sleeping)
                delta = deadline - time.time()
                if delta > 0:
                    time.sleep(delta)
                self.ready.append(func)

            while self.ready:
                func = self.ready.popleft()
                func()


scheduler = Scheduler()


# version 1
def countdown(n):
    if n > 0:
        print(f"Down {n}")
        time.sleep(1)
        scheduler.call_soon(lambda: countdown(n - 1))


def countup(stop, n=0):
    if n < stop:
        print(f"Up {n}")
        time.sleep(1)
        scheduler.call_soon(lambda: countup(stop, n + 1))


# version 2
# which avoids the sleeping problem
def countdown(n):
    if n > 0:
        print(f"Down {n}")
        scheduler.call_later(4, lambda: countdown(n - 1))


def countup(stop, n=0):
    if n < stop:
        print(f"Up {n}")
        scheduler.call_later(1, lambda: countup(stop, n + 1))


# scheduler.call_soon(lambda: countdown(5))
# scheduler.call_soon(lambda: countup(20))
# scheduler.run()


# producer consumer problem

import threading
import queue
import time


def producer(q, count):
    for n in range(count):
        print("Producing ", n)
        q.put(n)
        time.sleep(1)
    print("Producer done")
    q.put(None)


def consumer(q):
    while True:
        item = q.get()
        if item is None:
            break
        print(f"Consuming {item}")
    print("Consumer done")


#  threading attempt
# q = queue.Queue()
# threading.Thread(target=producer, args=(q, 10)).start()
# threading.Thread(target=consumer, args=(q,)).start()


class QueueClosed(Exception):
    pass


class Result:  # < -- future
    def __init__(self, value=None, exc=None):
        self.value = value
        self.exc = exc

    def result(self):
        if self.exc:
            raise self.exc
        else:
            return self.value


class AsyncQueue:
    def __init__(self):
        self.items = deque()
        self.waiting = deque()
        self._closed = False

    def close(self):
        self._closed = True
        if self.waiting and not self.items:
            for func in self.waiting:
                scheduler.call_soon(func)

    def put(self, item):
        if self._closed:
            raise QueueClosed()

        self.items.append(item)
        if self.waiting:
            func = self.waiting.popleft()
            scheduler.call_soon(func)  # why are we not calling it right away?

    def get(self, callback):
        if self.items:
            callback(Result(value=self.items.popleft()))
        else:
            if self._closed:
                callback(Result(exc=QueueClosed()))
            self.waiting.append(lambda: self.get(callback))


def producer(q, count):
    def _run(n):
        if n == 0:
            q.close()
            print("Producer Done")
        else:
            print("Producing ", n)
            q.put(n)
            scheduler.call_later(1, lambda: _run(n - 1))

    _run(count)


def consumer(q):
    def _run(result):
        try:
            item = result.result()
            print("Consuming ", item)
            q.get(_run)
        except QueueClosed:
            print("Consumer done")

    q.get(_run)


# q = AsyncQueue()
# scheduler.call_soon(lambda: producer(q, 10))
# scheduler.call_soon(lambda: consumer(q))
# scheduler.run()


###### ###### ######
## use generators/coroutines


from select import select


class Scheduler:
    def __init__(self):
        self.ready = deque()
        self.current = None
        self.sleeping = []
        self.sequence = 0
        self._read_waiting = {}
        self._write_waiting = {}

    def new_task(self, coro):
        self.ready.append(coro)

    async def sleep(self, delay):
        dealine = time.time() + delay
        self.sequence += 1
        heapq.heappush(self.sleeping, (dealine, self.sequence, self.current))
        self.current = None
        await switch()  # switch task

    def run(self):
        while self.ready or self.sleeping:
            if not self.ready:
                deadline, _, coro = heapq.heappop(self.sleeping)
                delta = deadline - time.time()
                if delta > 0:
                    time.sleep(delta)
                self.ready.append(coro)

            self.current = self.ready.popleft()
            try:
                self.current.send(None)
                if self.current:
                    self.ready.append(self.current)
            except StopIteration:
                pass


scheduler = Scheduler()


class Awaitable:
    def __await__(self):
        yield


def switch():
    return Awaitable()


async def countdown(n):
    while n > 0:
        print("Down", n)
        await scheduler.sleep(4)
        n -= 1


async def countup(stop):
    n = 0
    while n < stop:
        print("Up", n)
        await scheduler.sleep(1)
        n += 1


scheduler.new_task(countdown(5))
scheduler.new_task(countup(20))
scheduler.run()


####### ####### #######
# We can do similar thing for queues

# And then for IO
