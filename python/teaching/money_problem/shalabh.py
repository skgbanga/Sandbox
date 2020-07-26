f = open('data', 'r')
f1, names, entries = f.readlines(), [], []

for x in f1:
    newname, newval = x.split()[0], int(x.split()[1])
    '''
    try to use of less [0] in python, and directly bind things into meaningful names
    so I would have written the above as:

    newname, newval = x.split()
    newval = int(newval)
    '''

    if newname not in names:
        names.append(newname)
        entries.append([1, newval, 0, 0, 0, 0])
        '''
        when we get to the point of how to create classes in python, this can be better modeled as a class
        class Entry:
            def __init__(self, name, value):
                self.name = name
                self.values = [value, 0, 0, 0, 0]

        Normally list is used to contain similar kind of things i.e. they all look/feel alike
        '''
    else:
        index = names.index(newname)
        entries[index][0] += 1
        '''
        So what you have done in the loop below is a very classical 'insertion sort'
        https://en.wikipedia.org/wiki/Insertion_sort

        you take a "new" number, and see where that number fits into an already sorted list
        once you have found its location, you insert it into that place and remove the last number
        '''
        for ind in range(1, 6):
            if newval > entries[index][ind]:
                entries[index].insert(ind, newval)
                entries[index].pop()
                break

        '''
        Here you have used another classical sorting algorithm ('bubble sort')
        https://en.wikipedia.org/wiki/Bubble_sort

        you pick a number and "bubble" that number up to its right place by "swapping"
        that number with its neighbour
        '''
        for ind in range(index, 0, -1):
            if entries[ind][0] > entries[ind-1][0]:
                entries[ind], entries[ind-1] = entries[ind-1], entries[ind]
                names[ind], names[ind-1] = names[ind-1], names[ind]
            else:
                break

for ind in range(len(names)):
    print(names[ind], ' ', entries[ind][0], entries[ind][1:min(entries[ind][0],5)+1])

'''
so this loop can be better written as:

for name, (cnt, *values) in zip(names, entries):
    print(name, cnt, values[:cnt])

There are few very powerful python concepts in the above two liner, and I recommend reading about:
    1. what is zip?
    2. a, b = [1, 2] vs a, *b = [1, 2, 3]
    3. how does list indexing work if the index is more than length of list i.e.
      for a list A = [1, 2] what will A[0:100] print
'''

f.close()
