def parse_ranges(data):
    ranges = data.split(",")
    for r in ranges:
        if "->" in r:
            a, b = r.split("->")
            yield int(a)
        else:
            tokens = [int(i) for i in r.split("-")]
            if len(tokens) == 1:
                yield tokens[0]
            elif len(tokens) == 2:
                start, stop = tokens
                yield from range(start, stop + 1)


# another method
def parse_ranges(ranges):
    for group in ranges.split(","):
        start, sep, end = group.partition("-")
        if end.startswith(">") or not sep:
            yield int(start)
        else:
            yield from range(int(start), int(end) + 1)
