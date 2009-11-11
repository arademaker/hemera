from Sequent import	*

print ISSUE

prover = SequentProver()

while 1:
    try:
        s = raw_input('(sequent prover)> ')
        if not s: 
            continue
        else:
            if (s == 'apply'):
                prover.get_goal_applicable_rules('(p1 --> p2), (p1 --> (p2 --> p3)), p1, (p3 --> p4) |- p4')
            else:
                cmd = yacc.parse(s)
                prover.eval(cmd)
    except EOFError:
        break
    except NoMoreGoals:
        print 'No more goals.'
