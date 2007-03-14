from util import *

### Testes

s1 = '(((((((((((((((((((Aa --> (~ Ac & ~ Ab)) & ((~ Ac & ~ Ab) --> Aa)) & (Ab --> (~ Aa & ~ Ac))) & ((~ Aa & ~ Ac) --> Ab)) & (Ac --> (~ Aa & ~ Ab))) & ((~ Aa & ~ Ab) --> Ac)) & (Ba --> (~ Bc & ~ Bb))) & ((~ Bc & ~ Bb) --> Ba)) & (Bb --> (~ Ba & ~ Bc))) & ((~ Ba & ~ Bc) --> Bb)) & (Bc --> (~ Ba & ~ Bb))) & ((~ Ba & ~ Bb) --> Bc)) & (Ca --> (~ Cc & ~ Cb))) & ((~ Cc & ~ Cb) --> Ca)) & (Cb --> (~ Ca & ~ Cc))) & ((~ Ca & ~ Cc) --> Cb)) & (Cc --> (~ Ca & ~ Cb))) & ((~ Ca & ~ Cb) --> Cc)) & ((((Aa | Ba | Ca) & (Ab | Cb | Bb)) & (Ac | Bc | Cc)) & (~ Ab & ~ Cb & ~ Cc))) |- (((Aa | Ab | Ac) & (Ba | Bb | Bc)) & (Ca | Cb | Cc))'

s2 = '''(Aa --> (~ Ac & ~ Ab)) & 
((~ Ac & ~ Ab) --> Aa) & 
(Ab --> (~ Aa & ~ Ac)) & 
((~ Aa & ~ Ac) --> Ab) & 
(Ac --> (~ Aa & ~ Ab)) & 
((~ Aa & ~ Ab) --> Ac) & 
(Ba --> (~ Bc & ~ Bb)) & 
((~ Bc & ~ Bb) --> Ba) & 
(Bb --> (~ Ba & ~ Bc)) & 
((~ Ba & ~ Bc) --> Bb) & 
(Bc --> (~ Ba & ~ Bb)) & 
((~ Ba & ~ Bb) --> Bc) & 
(Ca --> (~ Cc & ~ Cb)) & 
((~ Cc & ~ Cb) --> Ca) & 
(Cb --> (~ Ca & ~ Cc)) & 
((~ Ca & ~ Cc) --> Cb) & 
(Cc --> (~ Ca & ~ Cb)) & 
((~ Ca & ~ Cb) --> Cc) & 
(Aa | Ba | Ca) & 
(Ab | Cb | Bb) & 
(Ac | Bc | Cc) & 
(~ Ab & ~ Cb & ~ Cc) |- (Aa | Ab | Ac) & (Ba | Bb | Bc) & (Ca | Cb | Cc)'''

teste = ['Aa --> (~ Ac & ~ Ab) |- Aa',
         '(~ Aa & ~ Ab) |- a',
         '(Aa --> Ab) | Ac |- a',
         'a & a & ab |- a',
         'a | t --> y & q, a, b & x, aa --> cd |- d', s2, s1,
         '|- (p | q) & (p | r) --> p | (q & r)']

# for data in teste:
#     print 'Inicio do parser:'
#     lexer.input(data)
#     while 1:
#         tok = lexer.token()
#         if not tok: break
#         print tok, 
#     print '\n'

# while 1:
#    try:
#        s = raw_input('calc > ')
#    except EOFError:
#        break
#    if not s: continue
#    result = yacc.parse(s)
#    print result

for data in teste:
    print 'Inicio do parser para: [%s] ' % data
    result = yacc.parse('read ' + data)
    print result
    g, goal = exp2graph(None, result[1])
    write_dot(g)

