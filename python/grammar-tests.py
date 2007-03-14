from grammar import *

formulas = ["c | a --> (b --> a)",
            "(~ p) | (q & r)", 
            "(p & q ) | ( r --> a )", 
            "~ ((p & q ) | r) --> a", 
            "a --> a --> a",
            "a -> a --> c"]

# ultimo caso acima propositalmente errado!

S = ' |- '
for f in formulas:
    print '==> formula: ', f
    print wff.parseString(f)
    seqs = [f + S + f, f + S, S + f]
    for s in seqs:
        print language.parseString("read " + s)
