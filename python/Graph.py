from networkx import *
from copy import *

LEFT = 'left'
RIGHT = 'right'
DERIVATION = 'derivation'

class Node:
    id = 0
    def __init__(self, label, sign=None, cost=None, used=None):
	self.graph = None
	self.label = label
        self.cost = cost
        self.id = Node.id
        self.used = used
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


    def add_child(self, node, side=None, label=None):
        assert isinstance(node, Node)
        self.graph.add_node(node)
        self.graph.add_edge(self, node, Edge(side=side, label=label))


    def add_childs(self, nodes, side=None, label=None):
        for n in nodes:
            self.add_child(n, side=side, label=label)


    def get_childs(self, side=None, sign=None):
        edges = self.get_out_edges(side=side, sign=sign)
        if edges:
            return [x[1] for x in edges]
        else:
            return []
            

    def get_out_edges(self, side=None, sign=None):
        edges = self.graph.out_edges(self)
        if side:
            edges = filter(lambda x : x[2].side == side, edges)
        elif sign:
            edges = filter(lambda x : x[2].sign == sign, edges)
        if edges:
            return [(x[1], x[2]) for x in edges]
	else:
	    return []


class Edge:
    def __init__(self, side = None, label=None, order=None, sign=None):
        self.side = side
        self.sign = sign
        self.label = label
        self.order = order


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
        return "(side=%s, %s)" %(self.side,self.label)


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
            assert isinstance(n1, Node) and isinstance(edge, Edge)
            if isinstance(n2, Node):
                XDiGraph.add_edge(self, n1, n2, edge)
            elif type(n2) == list:
                for i,r in enumerate(n2):
                    assert isinstance(r, Node)
                    e = copy(edge)
                    e.order = i 
                    XDiGraph.add_edge(self, n1, r, e)
            else:
                raise TypeError, 'Wrong type!'
        except AssertionError:
            print 'Error: ', n1, n2
            raise TypeError, "Wrong type!"

