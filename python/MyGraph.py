from copy import *

class Node:
    def __init__(self, label):
	self.graph = None
	self.label = label

    def __str__(self):
	return self.label

    def add_child(self, node, edge):
        self.graph.add_node(node)
        self.graph.add_edge(self, node, edge)

    def add_childs(self, nodes, edge):
        for n in nodes:
            self.add_child(n, edge)

    def get_childs(self, func):
        edges = self.get_out_edges(f_filter=func)
        if edges:
            return [x[0] for x in edges]
        else:
            return []
            
    def get_out_edges(self, func=None):
        edges = self.graph.out_edges(self)
        if f_filter:
            edges = filter(func, edges)
        if edges:
            return edges
	else:
	    return []

    def copy_childs_from(self, node):
        A = node.get_out_edges()
        f = lambda e: e[1].type != 'D'
        for (n, e) in filter(f, A):
            self.add_child(n, e)

    def remove_child(self, node):
        self.graph.delete_edge(self, node)


class Edge:
    def __init__(self, type, side = None, label=None, sign=None):
        self.type = type   # Strutural, Deduction, Failure 
        self.side = side   # Left, Right
        self.sign = sign   # True, False
        self.label = label # connector, proposition, predicate or function name...

    def __str__(self):
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
        self.data = {}

    def create_node(self, label):
        n = Node(label)
        self.data[n] = []
        return n

    def add_node(self, n):
	try:
	    assert isinstance(n, Node)
	    n.graph = self
            if not self.data.has_key(n):
                self.data[n] = []
	except AssertionError:
	    raise TypeError, 'Object is not a Node instance'


    def add_edge(self, n1, n2, edge):
        try:
            assert isinstance(edge, Edge)
            self.add_node(n1)
            self.add_node(n2)
            self.data[n1].append( (n2,copy(edge)) )
        except AssertionError:
            raise TypeError, "Wrong type!"

    def add_edges(self, n1, lst, edge):
        if type(lst) != list:
            lst = [lst]
        for r in lst:
            self.add_edge(n1, r, edge)

    def out_edges(self, n):
        return self.data[n]

    def delete_edge(self, n1, n2):
        if self.data.has_key(n1):
            if n2 in set(self.data[n1]):
                self.data[n1].remove(n2)

    def write(self):
        for k in self.data.keys():
            print "%s: " % k
            for c in self.data[k]:
                print "(%s,%s)" % (c[0],c[1]), 
            print ""


def teste():
    g = Graph()
    n0 = Node('|-')
    nr = Node('a')
    n1 = Node('-->')	

    g.add_edges(n0, [n1], Edge('S',side='left'))
    n1.add_child(nr, Edge('S',side='left'))
    n1.add_child(Node('b'), Edge('S',side='right'))

    n = g.create_node('|-')
    n1.copy_childs_from(n0)
    g.write()

    n0.remove_child(nr)
    n1.remove_child(Node('aa'))
    g.write()

