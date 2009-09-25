from util import *

ROOT = None
GOALS = []
GRAPH = None

class NoMoreGoals(Exception):
    pass

class REDUCE(Exception):
    pass


def is_atomic(formula):
    if len(formula.get_childs()) == 0: return True
    else: return False


def unify(n1, n2):
    ' trying to close the branch '
    if is_atomic(n1) and is_atomic(n2):
        return (n1.label == n2.label)
    else:
        return False


def split_goal(goal):
    Gamma = goal.get_childs(sign=True)
    Delta = goal.get_childs(sign=False)
    return (Gamma, Delta)


def solve_goal(goal):
    As, Bs = split_goal(goal)
    for x in As:
        for y in Bs:
            if unify(x, y):
                return [x]
    return []


def insert_goals(goals, new_goals):
    success = []
    for g in new_goals:
        f = solve_goal(g)
        if f:
            success.append( f[0] )
        else:
            goals.append(g)
    return (success, goals)


def cost(sign, conn):
    if   sign==True  and conn == '&':   return 1
    elif sign==False and conn == '|':   return 1
    elif sign==False and conn == '-->': return 1
    elif sign==False and conn == '&':   return 2
    elif sign==True  and conn == '|':   return 2
    elif sign==True  and conn == '-->': return 2
    elif conn == '~':   return 1
    else: return 4


def cost_formulas(goal):
    paircost = lambda pair: (pair[0], pair[1].sign, cost(pair[1].sign, pair[0].label))
    return map(paircost, goal.get_out_edges())


def new_goal(goal, pair, pairs):
    global GRAPH
    ng = GRAPH.create_node('|-')
    nlabel = '%s.%s' % (pair[1],pair[0].label)
    goal.add_child(ng, Edge(label=nlabel, side=DERIVATION))

    ng.copy_childs_from(goal)
    ng.remove_child(pair[0])

    for p in pairs:
        ng.add_child(p[0], Edge(sign=p[1]))
    return ng


def new_goals(goal, pair, pairslist):
    ngs = []
    for pairs in pairslist:
        ng = new_goal(goal, pair, pairs)
        ngs.append(ng)
    return ngs


def reduce_goal(goal):
    ''' construcao da lista de formulas do goal recebido e escolha da
        formula para aplicar regra '''
    lst = cost_formulas(goal)
    lst.sort(key = lambda x: x[2], cmp = lambda x,y : cmp(x,y))
    f_pair = lst.pop(0)

    a = f_pair[0].get_childs(side=LEFT)
    b = f_pair[0].get_childs(side=RIGHT)
    if len(a) == 1 and len(b) == 0:
        a = a[0]
    elif len(a) == len(b) == 1:
        a, b = a[0], b[0]
    else:
        raise REDUCE

    if   f_pair[1] == True  and f_pair[0].label == '-->': pairsl = [[(a,False)],[(b,True)]]
    elif f_pair[1] == False and f_pair[0].label == '-->': pairsl = [[(a,True),(b,False)]]
    elif f_pair[1] == True  and f_pair[0].label == '&':   pairsl = [[(a,True),(b,True)]]
    elif f_pair[1] == False and f_pair[0].label == '&':   pairsl = [[(a,False)],[(b,False)]]
    elif f_pair[1] == True  and f_pair[0].label == '|':   pairsl = [[(a,True)],[(b,True)]]
    elif f_pair[1] == False and f_pair[0].label == '|':   pairsl = [[(a,False),(b,False)]]
    elif f_pair[1] == False and f_pair[0].label == '~':   pairsl = [[(a,True)]]
    elif f_pair[1] == True  and f_pair[0].label == '~':   pairsl = [[(a,False)]]
    return f_pair, new_goals(goal, f_pair, pairsl)


def proof_step(goals):
    try:
        # escolha do proximo goal a ser tentado, confirmar se lista
        # nao vazia!
        assert goals
        goal = goals.pop()
        pair, new_goals = reduce_goal(goal)
        sucess, goals = insert_goals(goals, new_goals)
        print_step(goal, sucess)
    except (IndexError, AssertionError):
        raise NoMoreGoals
    else:
        return goals

def proof_steps(n, goals):
    if goals == [] or goals == None:
        return []
    if n == 0:
        return goals
    else:
        try:
            goals = proof_step(goals)
            return proof_steps(n-1, goals)
        except REDUCE:
            print "==> No proof rules applicable"


def read_goal(exp):
    graph, root = exp2graph(None, exp)
    if root.label != '|-':
        goal = graph.create_node('|-')
        goal.add_child(root, Edge(sign=False))
    else:
        raise Error
    return (graph, [goal])


def string_from_node(n):
    cl = n.get_childs()
    if   len(cl) == 2:
        l = n.get_childs(side=LEFT)[0]
        s1 = string_from_node(l)
        r = n.get_childs(side=RIGHT)[0]
        s2 = string_from_node(r)
        return "(%s %s %s)" % (s1, n.label, s2)
    elif len(cl) == 1:
        s1 = string_from_node( cl[0] )
        return "%s %s" % (n.label, s1)
    elif len(cl) == 0:
        return n.label


def string_from_nodes(As):
    s = []
    for a in As:
        s.append( string_from_node(a) )
    return ', '.join(s)


def string_from_goal(goal):
    edges = goal.get_out_edges()
    s = []
    f = lambda e: e[1].side != DERIVATION
    for (n,e) in filter(f, edges):
        str = '%s (%s)' %(e.sign, string_from_node(n))
        s.append(str)
    return ' |- ' + ', '.join(s) 


def print_step(last_goal, sucess):
    e = last_goal.get_out_edges(side=DERIVATION)[0]
    print 'Last rule: %s' %(e[1].label)
    print 'Sucess: ', string_from_nodes(sucess)


def print_goals(goals):
    s = ''
    if (goals == [] or goals == None):
        print "No more goals: proof finished"
    else:
        print 'Goals: '
        for g in goals:
            print string_from_goal(g)


def set_goals(goals):
    global GOALS
    GOALS = goals
    print_goals(goals)


def step():
    set_goals( proof_step(GOALS) )

def steps(n):
    set_goals( proof_steps(max(int(n),0), GOALS) )

def run():
    set_goals( proof_steps(-1, GOALS) )


def print_proof(out, goal):
    n1 = string_from_goal(goal)
    out.add_node(n1)
    childs = goal.get_out_edges(side=DERIVATION)
    for c,edge in childs:
        n2 = string_from_goal(c)
        out.add_edge(n1, n2, edge.label)
        out = print_proof(out, c)
    return out


def eval(input):
    global GRAPH, GOALS, ROOT
    if input[0] == 'step': 
        step()
    elif input[0] == 'steps':
        print 'Lets try %s times!' % input[1]
        steps(input[1])
    elif input[0] == 'read':
        print 'New proof initialized!'
        GRAPH, GOALS = read_goal(input[1])
        ROOT = GOALS[0]
    elif input[0] == 'run':
        print 'I will do my best...'
        run()
    elif input[0] == 'quit':
        print 'Bye.'
        sys.exit(0)
    elif input[0] == 'print':
        proof = XDiGraph()
        proof = print_proof(proof, ROOT)
        write_dot(proof)


