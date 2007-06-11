from copy import *

class Node:
    def __init__(self, label):
        self.graph = None
        self.label = label

    def __str__(self):
        return self.__repr__()

    def __repr__(self):
        return self.label

    def add_child(self, node, edge):
        self.graph.add_node(node)
        self.graph.add_edge(self, node, edge)

    def add_childs(self, nodes, edge):
        for n in nodes:
            self.add_child(n, edge)

    def get_childs(self, f=None):
        edges = self.get_out_edges(func=f)
        if edges:
            return [x[0] for x in edges]
        else:
            return []

    def get_out_edges(self, func=None):
        edges = self.graph.out_edges(self)
        if func:
            # note that edges is a list of tuples with 2 element
            edges = filter(lambda x: func(x[1]), edges)
        if edges:
            return edges
	else:
	    return []

    def copy_childs_from(self, node):
        A = node.get_out_edges()
        f = lambda e: e[1].type != Edge.DERIVATION
        for (n, e) in filter(f, A):
            self.add_child(n, e)

    def remove_child(self, node):
        self.graph.delete_edge(self, node)


class Edge:
    LEFT = 'Left'
    RIGHT = 'Right'
    STRUCT = 'S'
    DERIVATION = 'D'
    FAILURE = 'F'

    def __init__(self, type='S', side = None, label=None, sign=None):
        self.type = type   # Strutural, Deduction, Failure 
        self.side = side   # Left, Right
        self.sign = sign   # True, False
        self.label = label # connector, proposition, predicate or function name...

    def __str__(self):
        return self.__repr__()

    def __repr__(self):
        return "%s" % (self.__dict__)

    def __getitem__(self, name):
        return self.__dict__[name]

    def __setitem__(self, name, value):
        self.__dict__[name] = value


class Graph(object):
    '''
    The data attribute will hold a dictionary of lists:
    { node : [ (child1, edge) , (child2, edge) , ... ] , ... }

    In the original XDiGraph implementation:
    { node : { child : edge , child : edge , ... } , ... }
    '''
    def __init__(self):
        self._data = {}

    def create_node(self, label):
        n = Node(label)
        self._data[n] = []
        return n

    def add_node(self, n):
	try:
	    assert isinstance(n, Node)
	    n.graph = self
            if not self._data.has_key(n):
                self._data[n] = []
	except AssertionError:
	    raise TypeError, 'Object is not a Node instance'


    def add_edge(self, n1, n2, edge):
        try:
            assert isinstance(edge, Edge)
            self.add_node(n1)
            self.add_node(n2)
            self._data[n1].append( (n2,copy(edge)) )
        except AssertionError:
            raise TypeError, 'Edge is not a Edge instance'

    def add_edges(self, n1, lst, edge):
        if type(lst) != list:
            lst = [lst]
        for r in lst:
            self.add_edge(n1, r, edge)

    def out_edges(self, n):
        return self._data[n]

    def delete_edge(self, n1, n2):
        if self._data.has_key(n1):
            for x in self._data[n1]:
                if n2 == x[0]:
                    self._data[n1].remove(x)
        else:
            print 'nao achei %s' % n1

    def write(self):
        for k in self._data.keys():
            print "%s: %s" % (k, [(x[0].__str__(), x[1].__str__()) for x in self._data[k]])


def teste():
    g = Graph()
    a = Node('a')
    b = Node('b')
    g.add_node(a)
    a.add_child(b, Edge(label=Edge.DERIVATION))
    g.write()
    print '---'
    a.remove_child(b)
    g.write()
