from Tableaux import    *

while 1:
    try:
        s = raw_input('(tableaux-prover)> ')
        if not s: 
            continue
        else:
            cmd = yacc.parse(s)
            eval(cmd)
    except EOFError:
        break
    except NoMoreGoals:
        print 'No more goals.'
    except REDUCE:
        print 'No rule aplicable.'