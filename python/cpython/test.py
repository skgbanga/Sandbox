import sys
import numpy as np

sys.path.append("build/lib")

import derived  # pylint: disable=import-error

# objs = {
#     "a": derived.frodo(),
#     "b": derived.sam(),
#     "c": np.array(derived.gandalf(), copy=False),
#     "d": derived.aragorn(),
# }

# for key, value in objs.items():
#     print(key)
#     print(type(value))
#     print(value)
#     print()


# hobbits = derived.build().hobbits
# print(type(hobbits))
# print(hobbits)
# print()


# mordor = derived.evil()
# orcs = mordor.orcs()
# print(type(orcs))
# print(orcs)

import numpy as np
import time
def run(n):
    c = []
    py = []
    x = np.arange(100_000, dtype=np.float32)
    for _ in range(n):
        start = time.time()
        t, _ = derived.cumsum(x)
        end = time.time()
        c.append(t)
        py.append(end - start)
    return f'{np.mean(c)}us', f'{np.mean(py) * 1000000:.3f}us'

print(run(100))
