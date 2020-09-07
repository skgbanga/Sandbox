def equal(iterable, key=lambda x: x):
    iterable = iter(iterable)
    try:
        first = next(iterable)
    except StopIteration:
        return True

    return all(key(first) == key(rest) for rest in iterable)
