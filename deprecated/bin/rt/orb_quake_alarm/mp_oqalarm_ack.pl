
#   Copyright (c) 2006 Lindquist Consulting, Inc.
#   All rights reserved. 
#                                                                     
#   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
#
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
#   KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
#   WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR 
#   PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
#   OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
#   OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
#   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
#   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#
#   This software may be used freely in any way as long as 
#   the copyright statement above is not removed. 

sub mp_oqalarm_ack_handler {
	my( $message, $pfarray ) = @_;

	$database = %{$pfarray}->{database};

	my( @body ) = @{$message->body()};
	my( $from ) = $message->get("From");

	if( $from =~ /.*<(.*)>/ ) {

		$from = $1;
	}

	if( grep( /ACK\s+(\d+)\s*$/i && ( $alarmid = $1 ), @body ) ) {
		
		if( $opt_v ) {
			
			elog_notify( "Received acknowledgment message for " .
				     "alarmid $alarmid\n" );
		}

	} else {
		
		elog_complain( "Failed to interpret message from '$from'; " .
			       "No acknowledgment recorded! Message body is:\n" .
			       join( "", @body ) );
		return;
	}

	@db = dbopen( $database, "r+" );

	@db = dblookup( @db, "", "alarms", "alarmid", "$alarmid" );

	if( $db[1] < 0 ) {

		elog_complain( "Didn't find alarms table in '$database'!\n" );

		dbclose( @db );
		return;

	} elsif( $db[3] < 0 ) {
		
		elog_complain( "No record found for alarmid $alarmid " .
			       "in '$database'\n" );

		dbclose( @db );
		return;

	} else {
		
		( $oldack, $oldacktime, $oldackauth ) = dbgetv( @db, 
							"acknowledged", 
							"acktime",
							"ackauth" );

		$acktime = now();
		$ackauth = $from;

		if( $oldack eq "y" ) {

			if( $opt_v ) {

				elog_notify( "alarmid $alarmid has already been acknowledged " .
				     "by '$oldackauth' at " . strtime( $oldacktime ) .
				     "; ignoring duplicate acknowledgement from '$ackauth'" .
				     "at " . strtime( $acktime ) . "\n" );
			}

		} else {

			if( $opt_v ) {

				elog_notify( "Recording acknowledgment for alarmid " .
				     	"$alarmid from '$ackauth' at " .
				     	strtime( $acktime ) . "\n" );
			}
	
			$rc = dbputv( @db, "acknowledged", "y",
			     	"acktime", $acktime,
			     	"ackauth", $ackauth );

			if( $rc < 0 ) {
			
				clear_register( 1 );
	
				elog_complain( "Failed to record acknowledgment " .
				       	"in database for alarmid $alarmid!\n" );
			}
		}
	}

	dbclose( @db );

	return;
}

1;

