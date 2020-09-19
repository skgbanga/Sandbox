# my solution
from utils import equal

def add(*matrices):
    if not equal(matrices, len):
        raise ValueError("Given matrices are not the same size.")

    s = []
    for rows in zip(*matrices):
        if not equal(rows, len):
            raise ValueError("Given matrices are not the same size.")

        t = []
        for elems in zip(*rows):
            t.append(sum(elems))
        s.append(t)
    return s


# trey solution
def add1(*matrices):
    matrix_shapes = {tuple(len(r) for r in matrix) for matrix in matrices}
    if len(matrix_shapes) > 1:
        raise ValueError("Given matrices are not the same size.")

    return [[sum(elems) for elems in zip(*rows)] for rows in zip(*matrices)]


from itertools import zip_longest

def add2(*matrices):
    """Add corresponding numbers in given 2-D matrices."""
    try:
        return [
            [sum(values) for values in zip_longest(*rows)]
            for rows in zip_longest(*matrices)
        ]
    except TypeError as e:
        raise ValueError("Given matrices are not the same size.") from e
