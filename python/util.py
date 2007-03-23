from Graph import *
from grammar import *

ISSUE = '''
Provador de Teoremas 0.01
TecMF - 2007

Authors: Alexandre Rademaker, Edward Hermann

Calculus: 
 - Sequent Calculus
 - Tableaux


'''


def exp2graph(graph, term):
    "recebe um  termo resultado do parser da formula e retorna (grafo, raiz)"
    if graph == None: 
        graph = MyGraph()
    if term == None:
        return (graph, None)

    if type(term) not in [tuple, list]:
        r = graph.create_node(term)
        return (graph, r)
    elif type(term) == list:
        g, rl = (graph, [])
        for n in term:
            g, rsub = exp2graph(g, n)
            rl.append(rsub)
        return (g, rl)
    elif len(term) == 2 and type(term) == tuple:
        r = graph.create_node(term[0])
        g, r_sub = exp2graph(graph, term[1])
        if r_sub != None:
            graph.add_edges(r, r_sub, Edge(side=LEFT))
        return (graph, r)
    elif len(term) == 3 and type(term) == tuple:
        r = graph.create_node(term[0])
        g, r1 = exp2graph(graph, term[1])
        g, r2 = exp2graph(graph, term[2])
        if r1 != None:
            graph.add_edges(r, r1, Edge(side=LEFT))
        if r2 != None:
            graph.add_edges(r, r2, Edge(side=RIGHT))
        return (g, r)


def teste():
    cmds = ['read a, b |- a --> b | c, ab',
            'read a --> b | ab & c']
    for c in cmds:
        term = yacc.parse(c)
        print term
        graph, root = exp2graph(None, term[1])
        write_dot(graph)
