##################################################
# HemeraServiceContainer.py
#
##################################################

#!/usr/bin/python2.4
# -*- coding: utf-8 -*-

# *** ZSI 2.1 ***
from optparse import OptionParser
# Import the ZSI stuff you'd need no matter what
from ZSI.wstools import logging
from ZSI.ServiceContainer import AsServer
# Import the generated Server Object
from HemeraService_server import HemeraService
# Import libs that implements the prover machine
from Sequent import *
#from Tableaux import *
# Import the routines to generate SVG well formed XML
from svg import *
#
from grammar import SyntaxError

# Create a Server implementation by inheritance
class HemeraServiceContainer(HemeraService):

    # Make WSDL available for HTTP GET
    _wsdl = "".join(open("HemeraService.wsdl").readlines())

    # Represents the prove service. This method will be invoked
    # from the ZSI infrastructure
    #   ps - parsed soap, representing the request
    def soap_prove(self, ps, **kw):
        # Call the generated base class method to get appropriate
        # input/output data structures
        request, response = HemeraService.soap_prove(self, ps, **kw)
        response._return = self.prove(request._formula)
        return request, response

    # The prove service algorithm.
    #  formula - a string representations of a formula
    #            in the hemera sintax.
    def prove(self, formula):
        print "formula to prove: ", formula
        
        ret = ""
        
        try:
            s = "read " + formula  
            cmd = yacc.parse(s)
            eval(cmd)
        except SyntaxError:
            ret = "Error: Syntax Error" 
        else:
            s = "run"
            cmd = yacc.parse(s)
            eval(cmd)
        
            s = "print"
            cmd = yacc.parse(s)
            proof = eval(cmd)
        
            ret = parseToSVG(proof)       
           
        return ret

# Setup log information
op = OptionParser(usage="%prog [options]")
op.add_option("-l", "--loglevel", help="loglevel (DEBUG, WARN)", metavar="LOGLEVEL")
op.add_option("-p", "--port", help="HTTP port", metavar="PORT", default=8080, type="int")
options, args = op.parse_args()

# set the loglevel according to cmd line arg
if options.loglevel:
    loglevel = eval(options.loglevel, logging.__dict__)
    logger = logging.getLogger("")
    logger.setLevel(loglevel)

# Run the server with a given list services
AsServer(port=options.port, services=[HemeraServiceContainer(),])