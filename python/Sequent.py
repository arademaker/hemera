from util import *
import pygraphviz as pyg

class NoMoreGoals(Exception):
    pass

class REDUCE(Exception):
    pass

class SequentProver:
    __NO_CURRENT_RULE = -1
    
    def __init__(self):
        self._ROOT = None
        self._GOALS = []
        self._GRAPH = None
        self._currentGoal = 0
        self._currentRule = self.__NO_CURRENT_RULE        
        self._currentRules = []
        self.finalStatus = "" 
        
        
    def is_atomic(self, formula):
        if len(formula.get_childs()) == 0: return True
        else: return False
    
    
    def unify(self, n1, n2):
        if self.is_atomic(n1) and self.is_atomic(n2):
            return (n1.label == n2.label)
        else:
            return False
    
    
    def split_goal(self, goal):
        Gamma = goal.get_childs(lambda x: x.side==Edge.LEFT)
        Delta = goal.get_childs(lambda x: x.side==Edge.RIGHT)
        return (Gamma, Delta)
    
    
    def solve_goal(self, goal):
        As, Bs = self.split_goal(goal)
        for x in filter(self.is_atomic, As):
            for y in filter(self.is_atomic, Bs):
                if self.unify(x, y):
                    return [x]
        return []
    
    
    def insert_goals(self, goals, new_goals):
        success = []
        for g in new_goals:
            f = self.solve_goal(g)
            if f:
                success.append( f[0] )
            else:
                goals.append(g)
        return (success, goals)
    
    
    def cost(self, side, conn):
        if   side==Edge.LEFT  and conn == '&':   return 1
        elif side==Edge.RIGHT and conn == '|':   return 1
        elif side==Edge.RIGHT and conn == '-->': return 1
        elif side==Edge.RIGHT and conn == '&':   return 2
        elif side==Edge.LEFT  and conn == '|':   return 2
        elif side==Edge.LEFT  and conn == '-->': return 2
        elif conn == '~':   return 1
        else: return 4
    
    
    def cost_formulas(self, goal):
        lst = []
        for x in goal.get_childs(lambda x: x.side==Edge.LEFT):
            lst.append( (x, Edge.LEFT, self.cost(Edge.LEFT, x.label)) )
        for x in goal.get_childs(lambda x: x.side==Edge.RIGHT):
            lst.append( (x, Edge.RIGHT, self.cost(Edge.RIGHT, x.label)) )
        return lst
    
    
    def new_goal(self, goal, pair, pairs):
        ng = self._GRAPH.create_node('|-')
        s = '%s-%s' % (pair[0].label, self.string_from_side(pair[1]))
        goal.add_child(ng, Edge(label=s, type=Edge.DERIVATION))
    
        ng.copy_childs_from(goal)
        ng.remove_child(pair[0])
    
        for p in pairs:
            ng.add_child(p[0], Edge(side=p[1]))
        return ng
    
    
    def new_goals(self, goal, pair, pairslist):
        ngs = []
        for pairs in pairslist:
            ng = self.new_goal(goal, pair, pairs)
            ngs.append(ng)
        return ngs
    
    
    def reduce_goal(self, goal):
        # construcao da lista de formulas do goal recebido e escolha da
        # formula para aplicar regra
        if self._currentRule == self.__NO_CURRENT_RULE:
            lst = self.cost_formulas(goal)
            lst.sort(key = lambda x: x[2], cmp = lambda x,y : cmp(x,y))
            f_pair = lst.pop(0)            
        else:
            f_pair = self._currentRules[self._currentRule]
            
                
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
        return f_pair, self.new_goals(goal, f_pair, pairsl)
    
    
    def proof_step(self, goals):
        try:
            # escolha do proximo goal a ser tentado, confirmar se lista
            # nao vazia  ATENCAO!! Talvez usar pop(0)
            goal = goals[self._currentGoal]
            pair, new_goals = self.reduce_goal(goal)
            sucess, goals = self.insert_goals(goals, new_goals)
            self.print_step(pair, goals, sucess)
        except (IndexError, AttributeError):
            raise NoMoreGoals
        except (REDUCE):
            raise REDUCE
        else:
            goals.remove(goal)
            return goals
    
    
    def string_from_side(self, s):
        if   s == Edge.LEFT or s == Edge.RIGHT: return s
        else: raise Exception
        
    
    def print_step(self, pair, goals, sucess):
        print 'Last rule: %s-%s' %(pair[0].label, self.string_from_side(pair[1]))
        print 'Sucess: ', self.string_from_nodes(sucess)
    
    
    def proof_steps(self, n, goals):
        if n == 0:
            return goals
        else:
            try:
                return self.proof_steps(n-1, self.proof_step(goals))
            except REDUCE:
                self.finalStatus = "No proof rules applicable."
                print self.finalStatus                 
                return goals
            except NoMoreGoals:
                return goals
    
    
    def read_goal(self, exp):
        graph, root = exp2graph(None, exp)
        goal = root
        if root.label != '|-':
            goal = graph.create_node("|-")
            goal.add_child(root, Edge(side=Edge.RIGHT))
        return (graph, [goal])
    
    
    def string_from_node(self, n):
        cl = n.get_childs()
        if   len(cl) == 2:
            l = n.get_childs(lambda x: x.side==Edge.LEFT)[0]
            s1 = self.string_from_node(l)
            r = n.get_childs(lambda x: x.side==Edge.RIGHT)[0]
            s2 = self.string_from_node(r)
            return "(%s %s %s)" % (s1, n.label, s2)
        elif len(cl) == 1:
            s1 = self.string_from_node( cl[0] )
            return "%s %s" % (n.label, s1)
        elif len(cl) == 0:
            return n.label
    
    
    def string_from_nodes(self, As):
        s = []
        for a in As:
            s.append( self.string_from_node(a) )
        return ', '.join(s)
    
    
    def string_from_goal(self, goal):
        As, Bs = self.split_goal(goal)
        return self.string_from_nodes(As) + ' |- ' + self.string_from_nodes(Bs)
    
    
    def print_goals(self, goals):
        if (goals == [] or goals == None):
            self.finalStatus = "No more goals."
            print self.finalStatus
        else:
            for n, g in enumerate(goals):
                print 'Goal #%s: %s' %(n, self.string_from_goal(g))
                
        
    def set_goals(self, goals):
        self._GOALS = goals
        self.print_goals(goals)
        
        
    def step(self):
        self.set_goals( self.proof_steps(1, self._GOALS) )
        
    
    def steps(self, n):
        self.set_goals( self.proof_steps(max(int(n),0), self._GOALS) )
        
    
    def run(self):
        self.set_goals( self.proof_steps(-1, self._GOALS) )
        
        
    def print_proof(self, out, goal):
        ''' 
        receive an intance of MyGraph.Graph and produce an instance of pygraphviz.AGraph 
        '''
    
        out, n1 = self.add_graphviz_node(out, goal)                   
        childs = goal.get_out_edges(lambda x: x.type==Edge.DERIVATION)
    
        for c,edge in childs:
            out, n2 = self.add_graphviz_node(out, c)
            out.add_edge(n1, n2)
            e = out.get_edge(n1, n2)
            
            e.attr['label'] = str(edge.label)
            out = self.print_proof(out, c)
        return out
    
     
    def add_graphviz_node(self, aGraph, node):
        n = self.string_from_goal(node)
        n = str(n)
                
        if node in self._GOALS:
            aGraph.add_node(n, shape='ellipse')
        else:
            aGraph.add_node(n, shape='box')         
        return aGraph, n
    
    
    def get_goal_id(self, goalStr):
        for n, g in enumerate(self._GOALS):
            gStr = self.string_from_goal(g)
            if (goalStr == gStr):
                return n 
            
            
    def get_goal_applicable_rules(self, goalStr):
        n = self.get_goal_id(goalStr)
        if (n == None):
            return []
        
        goal = self._GOALS[n]
        
        self._currentRules = self.cost_formulas(goal)
        self._currentRules.sort(key = lambda x: x[2], cmp = lambda x,y : cmp(x,y))
        
        rules = []
        for n, rule in enumerate(self._currentRules):
            if (rule[2] != 4):
                node = rule[0]
                nodeStr = self.string_from_node(node)
                ruleStr = nodeStr + ' ' + rule[1]
                rules.append(ruleStr)
            
        return rules
             
    
    def apply_rule_to_goal(self, goalStr, ruleId):
        self._currentGoal = self.get_goal_id(goalStr)
        self._currentRule = ruleId
        self.step()

        
    def eval(self, input): 
        proofRepr = ""
        if input[0] == 'step': 
            self.step()
        elif input[0] == 'steps':
            print input
            print 'Lets try %s times!' % input[1]
            self.steps(input[1])
        elif input[0] == 'read':
            print 'New proof initialized!'
            self.finalStatus = ""
            self._GRAPH, self._GOALS = self.read_goal(input[1])
            self._ROOT = self._GOALS[0]
        elif input[0] == 'run':
            print 'I will do my best...'
            self.run()
        elif input[0] == 'quit':
            print 'Bye.'
            sys.exit(0)
        elif input[0] == 'print':
            proof = pyg.AGraph(center='true')
            self.print_proof(proof, self._ROOT)
            proofRepr = proof.string()
            print proofRepr

        return proofRepr
