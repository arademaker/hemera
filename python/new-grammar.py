import ply.lex as lex, ply.yacc as yacc, sys

tokens = ('ID',
          'IMPLICATION',
          'SEQUENT',
          'AND',
          'OR',
          'NOT',
          'LPAR',
          'RPAR',
          'CMD')

# seria possivel tambem definir os operadores como literais? Qual a
# vantagem?
# literals = '& | ~'

t_IMPLICATION  = r'-->'
t_SEQUENT      = r'\|-'
t_AND          = r'&'
t_OR           = r'\|'
t_NOT          = r'~'
t_LPAR         = r'\('
t_RPAR         = r'\)'
t_ignore       = ' \t\n'

literals = ","

reserved = {'read'  : 'CMD',
            'print' : 'CMD',
            'step'  : 'CMD',
            'steps' : 'CMD',
            'quit'  : 'CMD'}

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'ID') # Check for reserved words
    return t

# Error handling rule
def t_error(t):
    print "Illegal character '%s'" % t.value[0]
    t.skip(1)


# Build the lexer
lexer = lex.lex()


"""
lang : CMD SEQUENT 
     | CMD

WFF : WFF '&' WFF
    | WFF '|' WFF
    | WFF '-->' WFF
    | '~' WFF
    | atomic
    | '(' WFF ')'

LWFF : WFF ',' LWFF
     | WFF

SEQUENT : LWFF '|-' LWFF
        | '|-' LWFF
        | LWFF '|-'
"""


def p_lang_cmd_sequent(p):
    'lang : CMD sequent'
    p[0] = (p[1], p[2])

def p_lang_cmd(p):
    'lang : CMD'
    p[0] = (p[1],)


def p_wff_sequent_wff(p):
    'sequent : lwff SEQUENT lwff'
    p[0] = (p[2], p[1], p[3])

def p_wff_sequent(p):
    'sequent : SEQUENT lwff'
    p[0] = (p[1], None, p[2])

def p_sequent_wff(p):
    'sequent : lwff SEQUENT'
    p[0] = (p[2], p[1], None)


def p_lwff_wff_lwff(p):
    ''' lwff : wff ',' lwff '''
    p[3].append(p[1])
    # print 'DEBUG1: ', p[1], p[3]
    p[0] = p[3]

def p_lwff_wff(p):
    ''' lwff : wff '''
    # print 'DEBUG2: ', p[1]
    p[0] = [p[1]]


def p_wff_and_or(p):
    ''' wff : wff AND wff
            | wff OR wff
            | wff IMPLICATION wff '''
    p[0] = (p[2], p[1], p[3])

def p_wff_not(p):
    ''' wff : NOT wff '''
    p[0] = (p[1], p[2])

def p_wff_atomic(p):
    ''' wff : ID '''
    p[0] = (p[1])

def p_wff_wff(p):
    ''' wff : LPAR wff RPAR '''
    p[0] = p[2]


precedence = (
    ('left', 'AND', 'OR'),
    ('left', 'IMPLICATION'),
    ('right', 'NOT')
)

def p_error(p):
    print "Syntax error in input!"


# Build the parser
yacc.yacc()



### Testes

teste = ['Aa --> (~ Ac & ~ Ab) |- Aa',
         '(~ Aa & ~ Ab) |- a',
         '(Aa --> Ab) | Ac |- a',
         'a & a & ab |- a',
         'a | t --> y & q, a, b & x, aa --> cd |- d']

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


