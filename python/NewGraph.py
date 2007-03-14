from networkx import *

SIDE =  'side'
ORDER = 'order'
LABEL = 'label'

LEFT = 'left'
RIGHT = 'right'
DERIVATION = 'derivation'

class Node:
    id = 0
    def __init__(self, label, sign=None):
	self.graph = None
	self.label = label
        self.id = Node.id
        self.sign = sign
        Node.id += 1

    def __str__(self):
	return "Node(%s,%s)" %(self.id,self.label)

    def add_child(self, nodes, side=None, label=None):
        ns = nodes
        if isinstance(nodes, Node):
            ns = [nodes]
	for n in ns:
            assert isinstance(n, Node)
            self.graph.add_node(n)
	    self.graph.add_edge(self, n, Edge(side=side, label=label))

    def get_childs(self, side=None):
        edges = self.graph.out_edges(self)
        if side:
            edges = filter(lambda x : x[2].side == side , edges)
        if edges:
            return [x[1] for x in edges]
	else:
	    return []

    def get_out_edges(self, side=None):
        edges = self.graph.out_edges(self)
        if side:
            edges = filter(lambda x : x[2].side == side , edges)
        if edges:
            return [(x[1], x[2]) for x in edges]
	else:
	    return []


# class Edge:
#     def __init__(self, side = None, label=None, order=None):
#         self.side = side
#         self.label = label
#         self.order = order

#     def __str__(self):
#         return "(side=%s, %s)" %(self.side,self.label)


class MyGraph(XDiGraph):
    def create_node(self, label):
        n = Node(label)
        self.add_node(n)
        return n

    def add_node(self, n):
	try:
	    assert isinstance(n, Node)
	    n.graph = self
	    XDiGraph.add_node(self, n)
	except AssertionError:
	    raise TypeError, "Object is not a Node instance"

    def add_edge(self, n1, n2, edge):
        try:
            assert isinstance(n1, Node)
            if isinstance(n2, Node):
                XDiGraph.add_edge(self, n1, n2, edge)
            elif type(n2) == list:
                for i,r in enumerate(n2):
                    assert isinstance(r, Node)
                    e = edge.copy()
                    e[ORDER] = i 
                    XDiGraph.add_edge(self, n1, r, e)
            else:
                raise TypeError, 'Wrong type!'
        except AssertionError:
            print 'Error: ', n1, n2
            raise TypeError, "Wrong type!"

