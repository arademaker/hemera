from util import *
import pygraphviz as pyg

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
    if is_atomic(n1) and is_atomic(n2):
        return (n1.label == n2.label)
    else:
        return False


def split_goal(goal):
    Gamma = goal.get_childs(lambda x: x.side==Edge.LEFT)
    Delta = goal.get_childs(lambda x: x.side==Edge.RIGHT)
    return (Gamma, Delta)


def solve_goal(goal):
    As, Bs = split_goal(goal)
    for x in filter(is_atomic, As):
        for y in filter(is_atomic, Bs):
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


def cost(side, conn):
    if   side==Edge.LEFT  and conn == '&':   return 1
    elif side==Edge.RIGHT and conn == '|':   return 1
    elif side==Edge.RIGHT and conn == '-->': return 1
    elif side==Edge.RIGHT and conn == '&':   return 2
    elif side==Edge.LEFT  and conn == '|':   return 2
    elif side==Edge.LEFT  and conn == '-->': return 2
    elif conn == '~':   return 1
    else: return 4


def cost_formulas(goal):
    lst = []
    for x in goal.get_childs(lambda x: x.side==Edge.LEFT):
        lst.append( (x, Edge.LEFT, cost(Edge.LEFT, x.label)) )
    for x in goal.get_childs(lambda x: x.side==Edge.RIGHT):
        lst.append( (x, Edge.RIGHT, cost(Edge.RIGHT, x.label)) )
    return lst


def new_goal(goal, pair, pairs):
    ng = GRAPH.create_node('|-')
    s = '%s-%s' % (pair[0].label, string_from_side(pair[1]))
    goal.add_child(ng, Edge(label=s, type=Edge.DERIVATION))

    ng.copy_childs_from(goal)
    ng.remove_child(pair[0])

    for p in pairs:
        ng.add_child(p[0], Edge(side=p[1]))
    return ng


def new_goals(goal, pair, pairslist):
    ngs = []
    for pairs in pairslist:
        ng = new_goal(goal, pair, pairs)
        ngs.append(ng)
    return ngs


def reduce_goal(goal):
    # construcao da lista de formulas do goal recebido e escolha da
    # formula para aplicar regra
    lst = cost_formulas(goal)
    lst.sort(key = lambda x: x[2], cmp = lambda x,y : cmp(x,y))
    f_pair = lst.pop(0)

    a = f_pair[0].get_childs(lambda x: x.side==Edge.LEFT)
    b = f_pair[0].get_childs(lambda x: x.side==Edge.RIGHT)
    if len(a) == 1 and len(b) == 0:
        a = a[0]
    elif len(a) == len(b) == 1:
        a, b = a[0], b[0]
    else:
        raise REDUCE

    if   f_pair[1] == Edge.LEFT  and f_pair[0].label == '-->': pairsl = [[(a,Edge.RIGHT)],[(b,Edge.LEFT)]]
    elif f_pair[1] == Edge.RIGHT and f_pair[0].label == '-->': pairsl = [[(a,Edge.LEFT),(b,Edge.RIGHT)]]
    elif f_pair[1] == Edge.LEFT  and f_pair[0].label == '&':   pairsl = [[(a,Edge.LEFT),(b,Edge.LEFT)]]
    elif f_pair[1] == Edge.RIGHT and f_pair[0].label == '&':   pairsl = [[(a,Edge.RIGHT)],[(b,Edge.RIGHT)]]
    elif f_pair[1] == Edge.LEFT  and f_pair[0].label == '|':   pairsl = [[(a,Edge.LEFT)],[(b,Edge.LEFT)]]
    elif f_pair[1] == Edge.RIGHT and f_pair[0].label == '|':   pairsl = [[(a,Edge.RIGHT),(b,Edge.RIGHT)]]
    elif f_pair[1] == Edge.LEFT  and f_pair[0].label == '~':   pairsl = [[(a,Edge.RIGHT)]]
    elif f_pair[1] == Edge.RIGHT and f_pair[0].label == '~':   pairsl = [[(a,Edge.LEFT)]]
    else:
        raise REDUCE
    return f_pair, new_goals(goal, f_pair, pairsl)


def proof_step(goals):
    try:
        # escolha do proximo goal a ser tentado, confirmar se lista
        # nao vazia  ATENCAO!! Talvez usar pop(0)
        goal = goals[0]
        pair, new_goals = reduce_goal(goal)
        sucess, goals = insert_goals(goals, new_goals)
        print_step(pair, goals, sucess)
    except (IndexError, AttributeError):
        raise NoMoreGoals
    except (REDUCE):
        raise REDUCE
    else:
        goals.remove(goal)
        return goals


def string_from_side(s):
    if   s == Edge.LEFT or s == Edge.RIGHT: return s
    else: raise Exception
    

def print_step(pair, goals, sucess):
    print 'Last rule: %s-%s' %(pair[0].label, string_from_side(pair[1]))
    print 'Sucess: ', string_from_nodes(sucess)


def proof_steps(n, goals):
    if n == 0:
        return goals
    else:
        try:
            return proof_steps(n-1, proof_step(goals))
        except REDUCE:
            print "No proof rules applicable."
            return goals
        except NoMoreGoals:
            return goals


def read_goal(exp):
    graph, root = exp2graph(None, exp)
    goal = root
    if root.label != '|-':
        goal = graph.create_node("|-")
        goal.add_child(root, Edge(side=RIGHT))
    return (graph, [goal])


def string_from_node(n):
    cl = n.get_childs()
    if   len(cl) == 2:
        l = n.get_childs(lambda x: x.side==Edge.LEFT)[0]
        s1 = string_from_node(l)
        r = n.get_childs(lambda x: x.side==Edge.RIGHT)[0]
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
    As, Bs = split_goal(goal)
    return string_from_nodes(As) + ' |- ' + string_from_nodes(Bs)


def print_goals(goals):
    if (goals == [] or goals == None):
        print "No more goals."
    else:
        for n, g in enumerate(goals):
            print 'Goal #%s: %s' %(n, string_from_goal(g))


def set_goals(goals):
    global GOALS
    GOALS = goals
    print_goals(goals)


def step():
    set_goals( proof_steps(1, GOALS) )

def steps(n):
    set_goals( proof_steps(max(int(n),0), GOALS) )

def run():
    set_goals( proof_steps(-1, GOALS) )


def print_proof(out, goal):
    ''' receive an intance of MyGraph.Graph and produce an instance of pyparsing.AGraph '''
    n1 = string_from_goal(goal)
    out.add_node(n1)
    childs = goal.get_out_edges(lambda x: x.type==Edge.DERIVATION)
    for c,edge in childs:
        n2 = string_from_goal(c)
        out.add_edge(n1, n2)
        e = out.get_edge(n1, n2)
        e.attr['label'] = edge.label
        out = print_proof(out, c)
    return out


def eval(input):
    global GRAPH, GOALS, ROOT
    if input[0] == 'step': 
        step()
    elif input[0] == 'steps':
        print input
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
        proof = pyg.AGraph()
        proof = print_proof(proof, ROOT)
        print proof.string()


print ISSUE
while 1:
    try:
        s = raw_input('(sequent prover)> ')
        if not s: 
            continue
        else:
            cmd = yacc.parse(s)
            eval(cmd)
    except EOFError:
        break
    except NoMoreGoals:
        print 'No more goals.'
