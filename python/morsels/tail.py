def tail(iterable, n):
    if n <= 0:
        return []

    items = []
    for item in iterable:
        if len(items) == n:
            items.pop(0)
        items.append(item)

    return items


from collections import deque
def tail(iterable, n):
    if n <= 0:
        return []

    return list(deque(iterable, maxlen=n))
