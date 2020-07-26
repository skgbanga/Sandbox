records = {}
with open('data', 'r') as f:
    for line in f:
        name, cnt = line.split(' ')
        records.setdefault(name, []).append(int(cnt))


ordered = sorted(records.items(), reverse=True, key=lambda x: len(x[1]))
for name, cnts in ordered:
    print(name, len(cnts), sorted(cnts, reverse=True)[:5])
