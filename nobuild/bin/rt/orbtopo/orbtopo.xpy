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
import socket
import string
import getopt

try:

    import pygraphviz as PG

except:

    print "orbtopo requires the python 'pygraphviz' module (not found). " + \
    "See https://networkx.lanl.gov to download and install 'pygraphviz'. Bye."
    raise SystemExit

try:

    import pylab as P

except:

    print "orbtopo requires the python 'pylab' module (not found). " + \
    "See http://matplotlib.sourceforge.net to download and install 'pylab'. Bye."
    raise SystemExit

def portnum_to_name(portnum):

    if( orbserver_portnames.has_key(portnum) ):

        portname = orbserver_portnames[portnum]

    else:

        portname = str(portnum)

    return portname

def serverip_to_name(ip):

    if( not servernames.has_key(ip) ):

        try:

            servernames[ip] = socket.gethostbyaddr(ip)[0]

	except:

            servernames[ip] = ip

    servername = servernames[ip]

    return servername

def server_description(ip, port):

    description = serverip_to_name(ip) + ':' + portnum_to_name(port)

    return description

def set_nodecolor(desc):

    n = G.get_node(desc)

    if( directmonitor.has_key(desc) and directmonitor[desc] ):

        fillcolor = pf['colors']['monitored']

    else:

        fillcolor = pf['colors']['fringe']

    n.attr['fillcolor'] = fillcolor

def set_edgecolor(latency,fromdesc,todesc):

    n = G.get_edge(fromdesc,todesc)

    if( latency > pf['latency_warning_levels']['high'] ):

        linecolor = pf['latency_warning']['high']

    elif( latency > pf['latency_warning_levels']['warning'] ):

        linecolor = pf['latency_warning']['warning']

    else:

        linecolor = pf['latency_warning']['normal']

    n.attr['fontcolor'] = linecolor
    n.attr['color'] = linecolor

def set_penwidth(packets,fromdesc,todesc):

    n = G.get_edge(fromdesc,todesc)

    if( packets > 0 ):

        penwidth = 1 + ( math.log10( packets ) ) 
        arrowsize = 1 + ( math.log10( packets ) ) 

    else:
        penwidth = 1 
        arrowsize = 1 

    n.attr['arrowSize'] = '%s' % (arrowsize)

    # Note that setlinewidth is deprecated as of 2008-01-31, use 'penwidth'=W instead
    n.attr['style'] = 'setlinewidth(%s)' % (penwidth)

usage = "Usage: orbtopo [-v] [-p pfname] dbname targetdir\n"

elog_init( sys.argv )

try:
    opts, pargs = getopt.getopt(sys.argv[1:], "p:v")

except getopt.GetoptError, err:

    print str(err) 
    elog_die( usage );

if len(pargs) != 2:

    elog_die( usage )

else:

    dbname = pargs[0]
    targetdir = pargs[1]

    pfname = 'orbtopo'
    verbose = False

    for o, a in opts:
        if o == "-v":

	    verbose = True

        elif o == "-p":

	    pfname = a
        
	else:

	    elog_die( usage )

pf = pfget(pfname)

orbserver_names = pfget('orbserver_names')
orbserver_portnames = dict(zip(orbserver_names.values(), orbserver_names.keys()))

G = PG.AGraph()

for key in pf['graph_attributes'].keys():
    G.graph_attr[key] = str(pf['graph_attributes'][key]).lower()

for key in pf['node_attributes'].keys():
    G.node_attr[key] = str(pf['node_attributes'][key]).lower()

for key in pf['edge_attributes'].keys():
    G.edge_attr[key] = str(pf['edge_attributes'][key]).lower()

db = dbopen( dbname )

servernames = {};
directmonitor = {};

db.lookup( table = 'servers' )

for db.record in range(db.query(dbRECORD_COUNT)):

    (serveraddress, serverport, host) = db.getv('serveraddress', 'serverport', 'host')

    (machine, rtdir) = string.split( host, ':' )

    servernames[serveraddress] = machine

    # If it's in the servers table, it's directly monitored, and vice-versa:

    serverdesc =  server_description(serveraddress,serverport)

    directmonitor[serverdesc] = True

db.lookup( table = 'connections' )

if( pf['valid_twin_sec'] > 0 ):
    db.subset( 'when >= ' + str(str2epoch( "now" ) - float(pf['valid_twin_sec'])) )

db.join('clients', outer=True)

for db.record in range(db.query(dbRECORD_COUNT)):

    (fromaddress, fromport, toaddress, toport, closeorb, o2omachine, clientid, latency) = \
            db.getv( 'fromaddress', 'fromport', 'toaddress', 'toport', 'closeorb', \
            'o2omachine', 'clientid', 'latency_sec')

    fromdesc = server_description(fromaddress, fromport)
    todesc = server_description(toaddress, toport)

    if(not G.has_node(fromdesc)):
        G.add_node(fromdesc)
        set_nodecolor(fromdesc)

    if(not G.has_node(todesc)):
        G.add_node(todesc)
        set_nodecolor(todesc)

    G.add_edge(fromdesc, todesc, label=strtdelta(latency).strip())

    set_edgecolor(latency, fromdesc, todesc)
    set_penwidth(latency, fromdesc, todesc)

for ofile in pf['outputs']:
    opath = os.path.join(targetdir, ofile)
    G.draw(opath, prog=pf['graph_program'])
