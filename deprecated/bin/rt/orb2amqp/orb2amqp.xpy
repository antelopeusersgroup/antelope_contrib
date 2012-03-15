#
#   Copyright (c) 2010 Lindquist Consulting, Inc.
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

import sys
import getopt

import antelope.orb as o
import antelope.stock as s

from amqplib import client_0_8 as amqp

pfname			= 'orb2amqp'

orbname			= ''
amqphost		= ''
userid			= ''
password 		= ''
exchange		= ''
routing_key		= ''
match			= ''
reject			= ''
statefile		= ''
npkts 			= -1
port 			= 5672
time_encoding_format 	= '%17.5lf'
delivery_mode		= 2
verbose			= False
veryverbose		= False
virtual_host		= '/'

orbamqp_repr_version = '1.00'

def usage():

    print "Usage: orb2amqp [-p pfname] [-S statefile] [-P port] [-v] [-V] [-m match] [-r reject] [-n npkts] [-u userid] [-s password] orbname amqphost"

def configure(args):
    try:

        opts, pargs = getopt.getopt(sys.argv[1:], 'm:n:p:P:r:s:S:u:vV')

    except getopt.GetoptError:

        usage()
        sys.exit(-1)
    
    if( len(pargs) != 2):

        usage()
        sys.exit(-1)

    else:

        globals()['orbname'] = pargs[0]
        globals()['amqphost'] = pargs[1]

    userid_from_pf = True
    password_from_pf = True
    port_from_pf = True

    for option, value in opts:

        if option in ('-m'):
            globals()['match'] = value

        elif option in ('-n'):
            globals()['npkts'] = int(value)

        elif option in ('-p'):
            globals()['pfname'] = value

        elif option in ('-P'):
            globals()['port'] = int(value)
	    port_from_pf = False

        elif option in ('-r'):
            globals()['reject'] = value

        elif option in ('-s'):
            globals()['password'] = value
	    password_from_pf = False

        elif option in ('-S'):
            globals()['statefile'] = value

        elif option in ('-u'):
            globals()['userid'] = value
	    userid_from_pf = False

        elif option in ('-v'):
            globals()['verbose'] = True

        elif option in ('-V'):
            globals()['verbose'] = True
            globals()['veryverbose'] = True

    pfname = globals()['pfname'] 

    if( userid_from_pf ):
        globals()['userid'] = s.pfget_string( pfname, "userid" )

    if( password_from_pf ):
        globals()['password'] = s.pfget_string( pfname, "password" )

    if( port_from_pf ):
        globals()['port'] = s.pfget_int( pfname, "port" )

    globals()['delivery_mode']        = s.pfget_int( pfname, "delivery_mode" )
    globals()['virtual_host']         = s.pfget_string( pfname, "virtual_host" )
    globals()['exchange']             = s.pfget_string( pfname, "exchange" )
    globals()['routing_key']          = s.pfget_string( pfname, "routing_key" )
    globals()['time_encoding_format'] = s.pfget_string( pfname, "time_encoding_format" )


configure(sys.argv)

conn = amqp.Connection( host = amqphost,
                        userid = userid,
                        password = password,
			virtual_host = virtual_host,
			insist = False )

chan = conn.channel()

orb = o.orbopen( orbname, "r" )

if( match != '' ):
    nsources = orb.select(match)
    if( verbose ):
        print "Orb Select request to select sources matching '%s' returned %d sources\n" % (match, nsources)

if( reject != '' ):
    nsources = orb.reject(reject)
    if( verbose ):
        print "Orb Reject request to reject sources matching '%s' returned %d sources\n" % (reject, nsources)

if( statefile != '' ):

    rc = o.orbexhume(statefile)

    if( verbose ):
        if rc < 0:
            print "Failed to exhume and/or create state-file '%s'. Continuing\n" % (statefile)
        elif rc == 0:
            print "Did not find state-file '%s'; Generating it\n" % (statefile)
        elif rc == 1:
            print "Recovered state information from state-file '%s'\n" % (statefile)
        elif rc == 2:
            print "Recovered corrupt state information from state-file '%s'\n" % (statefile)

    if( rc < 0 ):
        statefile = ''
    else:
        try:
            (pktid, time) = orb.resurrect()
        except s.ElogException, e:
           print "Received exception: %s\n" % (e.string)
           print "Repositioned the orb to pktid %d, pkttime '%s'\n" % (pktid, s.strtime(time))
           #SCAFFOLD statefile = ''


while npkts != 0:
    try:
        (pktid, srcname, time, packet, nbytes) = orb.reap()
    except s.ElogException, e:
        print e.string
	continue

    if( veryverbose ):
        try:
            print "Got an orb message with srcname: %s time: '%s' pktid: %d bytes: %d\n" % ( srcname, s.strtime(time), pktid, nbytes )
        except s.ElogException, e: 
           print "Received exception: %s\n" % (e.string)

    appheaders = { 'srcname' : srcname, 
		   'time'    : time_encoding_format % ( time ),
                   'nbytes'  : nbytes,
		   'oarvers' : orbamqp_repr_version }

    msg = amqp.Message( packet, application_headers = appheaders )

    msg.properties["delivery_mode"] = delivery_mode

    chan.basic_publish( msg, exchange = exchange, routing_key = routing_key )

    if statefile != '':
        orb.bury( pktid, time )

    if( npkts > 0 ):
        npkts = npkts - 1

chan.close()

conn.close()

orb.close()
