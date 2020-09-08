def compact(iterable):
    iterable = iter(iterable)
    try:
        previous = next(iterable)
        yield previous
        while True:
            value = next(iterable)
            if value != previous:
                previous = value
                yield previous
            else:
                continue

    except StopIteration:
        pass


# trey's solution
def compact(iterable):
    previous = object()
    for item in iterable:
        if item != previous:
            yield item
            previous = item


# another solution
from itertools import groupby


def compact(iterable):
    return (item for item, _ in groupby(iterable))
