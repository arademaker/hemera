from Sequent import	*

print ISSUE

prover = SequentProver()

while 1:
    try:
        s = raw_input('(sequent prover)> ')
        if not s: 
            continue
        else:
            cmd = yacc.parse(s)
            prover.eval(cmd)
    except EOFError:
        break
    except NoMoreGoals:
        print 'No more goals.'
