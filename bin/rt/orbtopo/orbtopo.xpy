#
#   Copyright (c) 2008 Lindquist Consulting, Inc.
#   All rights reserved. 
#                                                                     
#   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
#
#   This software is licensed under the New BSD license: 
#
#   Redistribution and use in source and binary forms,
#   with or without modification, are permitted provided
#   that the following conditions are met:
#   
#   * Redistributions of source code must retain the above
#   copyright notice, this list of conditions and the
#   following disclaimer.
#   
#   * Redistributions in binary form must reproduce the
#   above copyright notice, this list of conditions and
#   the following disclaimer in the documentation and/or
#   other materials provided with the distribution.
#   
#   * Neither the name of Lindquist Consulting, Inc. nor
#   the names of its contributors may be used to endorse
#   or promote products derived from this software without
#   specific prior written permission.
#
#   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
#   CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
#   WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
#   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
#   PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
#   THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY
#   DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
#   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
#   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
#   USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
#   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
#   IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
#   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
#   USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
#   POSSIBILITY OF SUCH DAMAGE.
#

from antelope.datascope import *
from antelope.stock import *

try:
    import networkx as NX
except:
    print "orbtopo requires the python 'networkx' module (not found). " + \
    "See https://networkx.lanl.gov to download and install 'networkx'. Bye."
    raise SystemExit

try:
    import pylab as P
except:
    print "orbtopo requires the python 'pylab' module (not found). " + \
    "See http://matplotlib.sourceforge.net to download and install 'pylab'. Bye."
    raise SystemExit

#SCAFFOLD elog_init

pfname = 'orbtopo'

#SCAFFOLD needs getopt and -p for pfname
#SCAFFOLD need -i for interactive-plotting mode
 
if len(sys.argv) != 3:

    #SCAFFOLD elog_die

    print "Usage: orbtopo dbname targetdir\n"
    raise SystemExit

else:

    dbname = sys.argv[1]
    targetdir = sys.argv[2]

output_figure = pfget(pfname, 'output_figure')

G = NX.DiGraph()

db = dbopen( dbname )
db.lookup( table = 'servers' )
nrecs = db.query( dbRECORD_COUNT )

for db.record in range(nrecs):
    G.add_node(db.getv('serveraddress'))

db.lookup( table = 'connections' )
nrecs = db.query( dbRECORD_COUNT )

for db.record in range(nrecs):
    G.add_edge(db.getv('fromaddress'), db.getv('toaddress'))

NX.draw_graphviz(G, with_labels=False)
P.savefig(output_figure)
