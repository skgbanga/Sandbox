def extract_word(token):
    start = 0
    for idx, c in enumerate(token):
        if c.isalnum():
            start = idx
            break
    else:
        return ''

    end = 0
    for idx, c in enumerate(reversed(token)):
        if c.isalnum():
            end = idx
            break
    else:
        assert False

    if end == 0:
        end = None
    else:
        end = -end

    return token[start:end]




def count_words(msg):
    tokens = [extract_word(token).lower() for token in msg.split()]
    d = {}
    for token in tokens:
        d[token] = d.get(token, 0) + 1

    return d


# trey's solution
import re
from collections import Counter

def count_words(msg):
    return Counter(re.findall(r"\b[\w'-]+\b", msg.lower()))

