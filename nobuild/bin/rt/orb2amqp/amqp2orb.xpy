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

pfname = 'amqp2orb'

orbname = ''
amqphost = ''
userid = ''
password = ''
routing_key = ''
exchange = '' 
exchange_type = '' 
queue = '' 
consumer_tag = '' 
npkts = -1
virtual_host = '/'
verbose = False
veryverbose = False

def usage():

    print "Usage: [-p pfname] [-v] [-V] [-n npkts] [-u userid] [-s password] amqphost orbname"

def configure(args):
    try:

        opts, pargs = getopt.getopt(sys.argv[1:], 'n:p:s:u:vV')

    except getopt.GetoptError:

        usage()
	sys.exit(-1)

    if( len(pargs) != 2 ):

        usage()
	sys.exit(-1)

    else:

        globals()['amqphost'] = pargs[0]
        globals()['orbname'] = pargs[1]

    userid_from_pf = True
    password_from_pf = True


    for option, value in opts:

        if option in ('-n'):
	    globals()['npkts'] = int(value)

        elif option in ('-p'):
	    globals()['pfname'] = value

        elif option in ('-s'):
	    globals()['password'] = value
	    password_from_pf = False

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

    globals()['virtual_host']         = s.pfget_string( pfname, "virtual_host" )
    globals()['exchange']             = s.pfget_string( pfname, "exchange" )
    globals()['exchange_type']        = s.pfget_string( pfname, "exchange_type" )
    globals()['queue']                = s.pfget_string( pfname, "queue" )
    globals()['consumer_tag']         = s.pfget_string( pfname, "consumer_tag" )
    globals()['routing_key']          = s.pfget_string( pfname, "routing_key" )

configure(sys.argv)

conn = amqp.Connection( host = amqphost,
                        userid = userid,
                        password = password,
			virtual_host = virtual_host,
			insist = False )

chan = conn.channel()

chan.queue_declare( queue = queue,
                    durable = True,
		    exclusive = False,
		    auto_delete = False )

chan.exchange_declare( exchange = exchange,
                       type = exchange_type,
		       durable = True,
		       auto_delete = False )

chan.queue_bind( queue = queue,
                 exchange = exchange,
		 routing_key = routing_key )

orb = o.orbopen( orbname, "w" )

def recv_callback( msg ):

    oarvers_needed = '1.00'

    try:
        orb2amqp_repr_version = msg.application_headers['oarvers']
    except KeyError:
        print "Received a packet without an orb2amqp representation version in header (need oarvers = '%s'). Skipping packet\n" % (oarvers_needed)
	return

    if( orb2amqp_repr_version != '1.00' ):
        print "Received orb2amqp representation version %s; need %s. Skipping packet\n" % (orb2amqp_repr_version, oarvers_needed)
	return

    srcname = msg.application_headers['srcname']
    time = float(msg.application_headers['time'])
    nbytes = msg.application_headers['nbytes']

    orb = globals()['orb']

    if( verbose ):
        print 'Received message with srcname %s, time %f, nbytes %d\n' % ( srcname, time, nbytes )

    rc = orb.put( srcname, time, msg.body, nbytes )

    if( veryverbose ):
        print "orbput returned %d\n" % (rc)

chan.basic_consume( queue = queue,
                    no_ack = True, 
		    callback = recv_callback, 
		    consumer_tag = consumer_tag )

while npkts != 0: 

    chan.wait()

    if( npkts > 0 ):
        npkts = npkts - 1

chan.basic_cancel( consumer_tag )

orb.close()

chan.close()

conn.close()
