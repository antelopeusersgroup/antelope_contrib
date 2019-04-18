import os, sys, re, time
import argparse
import webbrowser
import threading
from http.server import HTTPServer
import SimpleHTTPServer


description = '''

Simple HTTP server on Python that will serve all requests
for help documentation in the Antelope distribution.

Depends on the environment variable $ANTELOPE to be set
before running the script. This variable can be set by
sourcing the setup.sh or setup.csh file in your antelope
distribution.


The service will configure a simple HTTP server on the
host and will be bound to the localhost (127.0.0.1) interface
only to avoid exposing the directory to external requests.
This can be configured to all interfaces by setting the
value to (0.0.0.0) using the --bind or -b flags.

WARNING:
BINDING TO ANYTHING ELSE THAN LOCALHOST WILL BE A SECURITY RISK.
NO ACCESS RESTRICTIONS OF ANY KIND TO THE FILESYSTEM.

The system will not verify if the port is already in use by a different
process. If needed the port can be set using the --port or -p flags.

The process will automatically attempt to open a browser window
with the system's default browser on the main antelope.html page
for the distribution configured in the $ANTELOPE system variable.
If you want to run as a service then you can use the -n or
--nobrowser flags to avoid opening the initial page.

There is no verbose flag. The script will print every request
to STDOUT. Run with -h or --help flag to get the list of
options and default values.


'''

epilog = '''
MISSING:
    Option to run as server and respond to external requests.

EXAMPLE:
    antelope_help -p 8888 --bind=0.0.0.0

HELP:
    antelope_help -h


Report bugs to Juan Reyes <reyes@ucsd.edu>.
'''

version = '''
%(prog)s 1.0

Copyright (c) 2017, The Regents of the University of California
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:
 1. Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation and/or
    other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


Written by Juan Reyes <reyes@ucsd.edu>
'''

parser = argparse.ArgumentParser( prog='antelope_help',
                    formatter_class=argparse.RawTextHelpFormatter,
                    description=description, epilog=epilog)

parser.add_argument('-V', '--version', action='version', version=version)

parser.add_argument('-p', '--port', action='store', dest='port', type=int,
            default=8000, help='Bind to this port.(default: %(default)s)')

parser.add_argument('-b', '--bind', action='store', dest='bind', type=str, default='127.0.0.1',
            help='Bind to this interface. Open to all with 0.0.0.0 (default: %(default)s)')

parser.add_argument('-n', '--nobrowser', action='store_true', dest='nobrowser',
            default=False, help='Do not open browser.(default: %(default)s)')

config = parser.parse_args()

antelope_path = os.environ['ANTELOPE']
if not antelope_path:
    sys.exit( '\n\t$ANTELOPE variable not set!!!\n' )

version = antelope_path.split('/')[-1]
os.chdir('/opt/antelope')


class myHandler(SimpleHTTPServer.SimpleHTTPRequestHandler):
   def do_GET(self):

       original = self.path

       if re.match( r'^/(python|perl|tcltk|qt)', self.path):
           pass
       elif re.match( r'^/opt/antelope/([\w\d_.-]+)/(.*)', self.path):
           m = re.match( r'^/opt/antelope/([\w\d_.-]+)/(.*)', self.path)
           self.send_response(301)
           self.send_header('Location','/%s' % m.group(2) )
           self.end_headers()
           return
       else:
           self.path = '/%s%s' % ( version, self.path )

       print '[%s] => %s' % (original, self.path )

       return SimpleHTTPServer.SimpleHTTPRequestHandler.do_GET(self)

def start_server(path,interface='127.0.0.1', port=8000):
    '''Start a simple webserver serving path on port'''
    os.chdir(path)
    try:
        httpd = HTTPServer((interface, port), myHandler)
        httpd.serve_forever()
    except Exception,e:
        print '\n%s: %s\n' % (Exception, e)
        sys.exit(2)


# Start the server in a new thread
daemon = threading.Thread(name='daemon_server',
                          target=start_server,
                          args=('.', config.bind, config.port))

daemon.setDaemon(True)
daemon.start()

# Open the web browser
if not config.nobrowser:
    webbrowser.open('http://localhost:{}/antelope.html'.format(config.port))

while True:
    try:
        time.sleep(0.1)
    except KeyboardInterrupt:
        print '\nStop Server on port %s\n' % config.port
        sys.exit()
    except Exception,e:
        print '\n%s: %s\n' % (Exception, e)
        sys.exit(1)
