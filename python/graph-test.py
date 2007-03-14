
class Edge:
    LEFT = 'left'
    RIGHT = 'right'

    def __init__(self, side=None, label=None):
        self.side = side
        self.label = label

    def __eq__(self, other):
        equal = True
        has_k = False
        for k in set(self.map.keys()).intersection(set(other.map.keys())):
            equal = equal and (self.map[k] == other.map[k])
            has_k = True
        return (equal and has_k)


# x = Edge({Edge.SIDE: Edge.LEFT})
# y = Edge({Edge.LABEL: Edge.LEFT})

z = Edge(side=Edge.LEFT, label='teste')
print z.side, z.label

z = Edge()
print z.side, z.label

print dir(z)

# print x == y

