#!/usr/bin/env python3

def length_of_longest_substring(s):
    occurs = dict()
    begin, end = 0, 1
    longest = 0

    def length():
        return end - begin - 1

    for idx, c in enumerate(s):
        if c in occurs:
            longest = max(longest, length())

            previous = occurs[c]
            for i in range(begin, previous + 1):
                occurs.pop(s[i], None)

            begin = previous + 1

        occurs[c] = idx
        end += 1

    return max(longest, length())
