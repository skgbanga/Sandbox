import math

class Circle:
    def __init__(self, r=1):
        self.radius = r

    @property
    def radius(self):
        return self._radius

    @radius.setter
    def radius(self, r):
        if r < 0:
            raise ValueError('Radius cannot be negative')

        self._radius = r

    @property
    def diameter(self):
        return self.radius * 2

    @diameter.setter
    def diameter(self, d):
        self.radius = d / 2

    @property
    def area(self):
        return math.pi * self.radius * self.radius

    def __repr__(self):
        return f'Circle({self.radius})'
