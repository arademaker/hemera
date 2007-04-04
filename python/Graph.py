from networkx import *
from copy import *

LEFT = 'left'
RIGHT = 'right'
DERIVATION = 'derivation'

class Node:
    def __init__(self, label):
	self.graph = None
	self.label = label

    def __str__(self):
	return self.label

    def add_child(self, node, edge):
        assert isinstance(node, Node) and isinstance(edge, Edge)
        self.graph.add_node(node)
        self.graph.add_edge(self, node, copy(edge))

    def add_childs(self, nodes, edge):
        for n in nodes:
            self.add_child(n, edge)

    def get_childs(self, side=None, sign=None):
        edges = self.get_out_edges(side=side, sign=sign)
        if edges:
            return [x[0] for x in edges]
        else:
            return []
            
    def get_out_edges(self, side=None, sign=None):
        edges = self.graph.out_edges(self)
        if side:
            edges = filter(lambda x : x[2].side == side, edges)
        if sign:
            edges = filter(lambda x : x[2].sign == sign, edges)
        if edges:
            edges.sort(key = lambda x: x[2], cmp = lambda x,y : cmp(x.order,y.order))
            return [(x[1], x[2]) for x in edges]
	else:
	    return []

    def copy_childs_from(self, node):
        A = node.get_out_edges()
        f = lambda e: e[1].side != DERIVATION
        for (n, e) in filter(f, A):
            self.add_child(n, copy(e))

    def remove_child(self, node):
        self.graph.delete_edge(self, node)


class Edge:
    def __init__(self, type, side = None, label=None, sign=None):
        self.type = type # Strutural, Deduction, Failure 
        self.side = side
        self.sign = sign
        self.label = label

    def __str__(self):
        return "%s" % (self.__dict__)

    def __getitem__(self, name):
        return self.__dict__[name]

    def __setitem__(self, name, value):
        self.__dict__[name] = value


class MyGraph(XDiGraph):
    def __init__(self):
        XDiGraph.__init__(self)
        self.__adjl__ = {}

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
	    raise TypeError, 'Object is not a Node instance'

    def append_edge(self, n1, n2):
        if not self.__adjl__.has_key(n1):
           self.__adjl__[n1] = []
        self.__adjl__[n1].append(n2)


    def add_edge(self, n1, n2, edge):
        try:
            assert isinstance(edge, Edge)
            self.add_node(n1)
            self.add_node(n2)
            XDiGraph.add_edge(self, n1, n2, copy(edge))
            self.append_edge(n1,n2)
        except AssertionError:
            raise TypeError, "Wrong type!"

    def add_edges(self, n1, lst, edge):
        if type(lst) != list:
            lst = [lst]
        for r in lst:
            self.add_edge(n1, r, edge)



def teste():
    g = MyGraph()
    n0 = Node('|-')
    nr = Node('a')
    n1 = Node('-->')	

    g.add_edges(n0, [n1], Edge('S',side=LEFT))
    n1.add_child(nr, Edge('S', side=LEFT))
    n1.add_child(Node('b'), Edge('S',side=RIGHT))

    n = g.create_node('|-')
    n1.copy_childs_from(n0)
    write_dot(g)

    n0.remove_child(nr)
    n1.remove_child(Node('aa'))

    write_dot(g)
