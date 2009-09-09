import ply.lex as lex, ply.yacc as yacc, sys

class SyntaxError(Exception):
    def __init__(self, value):
        self.value = value
    def __str__(self):
        return repr(self.value)

tokens = ('ID',
          'IMPLICATION',
          'SEQUENT',
          'AND',
          'OR',
          'NOT',
          'NUMBER',
          'LPAR',
          'RPAR',
          'CMD')

t_IMPLICATION  = r'-->'
t_SEQUENT      = r'\|-'
t_AND          = r'&'
t_OR           = r'\|'
t_NOT          = r'~'
t_LPAR         = r'\('
t_RPAR         = r'\)'
t_ignore       = ' \t\n'
t_NUMBER       = r'[0-9]+'

literals = ","

reserved = {'read'  : 'CMD',
            'print' : 'CMD',
            'step'  : 'CMD',
            'run'   : 'CMD',
            'steps' : 'CMD',
            'quit'  : 'CMD'}

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'ID') # Check for reserved words
    return t

# def t_FILENAME(t):
#     r'[a-zA-Z_0-9]*'
#     t.type = reserved.get(t.value,'ID') # Check for reserved words
#     return t

# Error handling rule
def t_error(t):
    errorMsg = "Illegal character '%s'" % t.value[0] 
    print errorMsg
    raise SyntaxError(errorMsg)    

    t.skip(1)


# Build the lexer
lexer = lex.lex()


"""
lang : CMD SEQUENT 
     | CMD WFF
     | CMD ID
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

def p_lang_cmd_wff(p):
    'lang : CMD wff'
    p[0] = (p[1], p[2])

def p_lang_cmd(p):
    'lang : CMD'
    p[0] = (p[1],)

def p_lang_cmd_num(p):
    'lang : CMD NUMBER'
    p[0] = (p[1],p[2])


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
    p[0] = p[3]

def p_lwff_wff(p):
    ''' lwff : wff '''
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
    ('left', 'IMPLICATION'),
    ('left', 'AND', 'OR'),
    ('right', 'NOT')
)

def p_error(p):
    msgError = "Syntax error in input!"
    print msgError
    raise SyntaxError(msgError);


# Build the parser
yacc.yacc()

