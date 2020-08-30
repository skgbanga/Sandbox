import sys
import numpy as np

sys.path.append("build/lib")

import derived  # pylint: disable=import-error

objs = {
    "a": derived.frodo(),
    "b": derived.sam(),
    "c": np.array(derived.gandalf(), copy=False),
    "d": derived.aragorn(),
}

for key, value in objs.items():
    print(key)
    print(type(value))
    print(value)
    print()


hobbits = derived.build().hobbits
print(type(hobbits))
print(hobbits)
print()


mordor = derived.evil()
orcs = mordor.orcs()
print(type(orcs))
print(orcs)
