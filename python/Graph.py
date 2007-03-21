from networkx import *
from copy import *

LEFT = 'left'
RIGHT = 'right'
DERIVATION = 'derivation'

class Node:
    id = 0
    def __init__(self, label):
	self.graph = None
	self.label = label
        self.id = Node.id
        Node.id += 1

    def __getitem__(self, aname):
        if(aname == "label"):
            return self.label
        elif(aname == "sign"):
            return self.sign
        else:
            raise AttributeError, aname

    def __str__(self):
	return "Node(%s,%s)" %(self.id,self.label)

    def add_child(self, node, edge):
        assert isinstance(node, Node) and isinstance(edge, Edge)
        self.graph.add_node(node)
        self.graph.add_edge(self, node, edge)

    def add_childs(self, nodes, edge):
        for n in nodes:
            e = copy(edge)
            self.add_child(n, e)

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
    def __init__(self, side = None, label=None, sign=None):
        self.side = side
        self.sign = sign
        self.label = label

    def __getitem__(self, aname):
        if(aname == "label"):
            return self.label
        elif(aname == "side"):
            return self.side
        elif(aname == "order"):
            return self.order
        else:
            raise AttributeError, aname

    def __str__(self):
        return "(%s, %s, %s, %s)" %(self.side,self.order,self.sign,self.label)


class MyGraph(XDiGraph):
    def __init__(self):
        XDiGraph.__init__(self)
        self._edgenum = 0

    def create_node(self, label):
        n = Node(label)
        self.add_node(n)
        return n

    def _nedge(self):
        self._edgenum = self._edgenum + 1
        return self._edgenum
    
    def add_node(self, n):
	try:
	    assert isinstance(n, Node)
	    n.graph = self
	    XDiGraph.add_node(self, n)
	except AssertionError:
	    raise TypeError, 'Object is not a Node instance'

    def add_edge(self, n1, n2, edge):
        try:
            assert isinstance(edge, Edge)
            self.add_node(n1)
            self.add_node(n2)
            edge.order = self._nedge()
            XDiGraph.add_edge(self, n1, n2, copy(edge))
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

    n = Node('a')

    g.add_edges(n0, [Node('-->')], Edge(side=LEFT))
    n0.add_child(n, Edge(side=LEFT))
    n0.add_child(Node('b'), Edge(side=RIGHT))

    n1 = g.create_node('|-')
    n1.copy_childs_from(n0)

    write_dot(g)

    n0.remove_child(n)
    n1.remove_child(Node('aa'))

    write_dot(g)
