import random

names = [
        'sandeep',
        'kalyani',
        'sonu',
        'aanya',
        'sanjeev',
        'mitali',
        'shalabh',
        'nidhi',
        'chaman',
        'vijay',
        'neha',
        'vishesh'
        ]
weights = [random.randint(10, 50) for _ in names]

entries = random.choices(names, weights, k=100)
with open('data', 'w') as f:
    for entry in entries:
        f.write('{} {}\n'.format(entry, random.randint(10, 100)))
