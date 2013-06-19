
########
#IMPORT#
########
import sys
import os
import argparse
import re

sys.path.append( os.environ['ANTELOPE'] + '/data/python' )

from antelope.datascope import *
from antelope.stock import *

##############
#SUB-ROUTINES#
##############

def create_view( db, subset_expr=None ):

	"""This method creates a Datascope view with the pertinent information for\
the generation of a ceef file.

Returns db pointer"""

	db_return = dbopen( db, 'r' )
	db_return = db_return.lookup( table='origin' )
	db_return = db_return.join( 'event' )
	db_return = db_return.join( 'netmag', outer=True )

	if subset_expr:
		db_return = db_return.subset( subset_expr )

	db_return = db_return.sort( 'time' )
	db_return[3] = 0

	return db_return
####
	
def db2ceef( db, file, mode ):

	"""This method writes properly formatted output to the ouput file for all events\
in the input database.

Returns nothing"""

	outFile = open( file, mode )

	outFile.write( '%6sDate%4sTime%10sLat%6sLon%4sDepth%25sMagnitudes%27sSrc\n' % ( '','','','','','','' ) )
	outFile.write( '  yyyy mm dd hhmm ss.s%24skm%8smb    Mn    ML    MS    MC    Mw   Othr  Pref   Mw       \n' % ( '', '' ) )
	outFile.write( '*'*114 + '\n' )

	if display_output:
		sys.stdout.write( '%6sDate%4sTime%10sLat%6sLon%4sDepth%25sMagnitudes%27sSrc\n' % ( '','','','','','','' ) )
		sys.stdout.write( '  yyyy mm dd hhmm ss.s%24skm%8smb    Mn    ML    MS    MC    Mw   Othr  Pref   Mw       \n' % ( '', '' ) )
		sys.stdout.write( '*'*114 + '\n' )
		


	processed_evids = []

	for db[3] in range( db.nrecs() ):
		evid = str( db.getv( 'evid' )[0] )

		if evid not in processed_evids:
			db_subset = db.subset(  'evid == ' + evid )

			event_blob = process_event( db_subset )

			outFile.write( event_blob )

			if display_output:
				sys.stdout.write( event_blob )

			processed_evids.append( evid )

	outFile.close()
####

def process_event( db ):

	"""This method will return the necessary number of properly formatted rows for\
a given event.

Returns string""" 

	db.sort( 'abs( orid - prefor )' )

	pref_flag = True
	event_blob = ''
	processed_orids = []

	pref_mag = get_prefmag( db )

	for db[3] in range( db.nrecs() ):
		orid = str( db.getv( 'orid' )[0] )

		if orid not in processed_orids:
			magnitudes = get_all_magnitudes( db, orid )

			event_blob += write_row( db, magnitudes, pref_flag, pref_mag )

			pref_flag = False

			processed_orids.append( orid )
		
	return event_blob
####

def get_all_magnitudes( db, orid ):

	"""This method will return all magnitude data available for a give origin.\
CAVEAT: Only one magnitude value of type 'other' will be stored per origin as the ceef\
format only has one available field to hold this data. Which value is being kept is\ 
currently arbitrary.

Returns dictionary"""

	db_orid = db.subset( 'orid == ' + orid )

	magnitudes = {}

	for db_orid[3] in range( db_orid.nrecs() ):
		magnitude, magtype = db_orid.getv( 'magnitude', 'magtype' )

		if not magtype == '-':
			if not ( magtype == 'mb' or magtype == 'mn' or magtype == 'ml' or magtype == 'ms'\
			or magtype == 'mc' or magtype == 'mw' or magtype == 'mw\'' ):
				magnitudes[ 'other' ] = ( '%5.2f' % magnitude )

			magnitudes[ magtype.lower() ] = ( '%5.2f' % magnitude ) 

	return magnitudes
####

def get_prefmag( db ):

	"""This method will return the magnitude, associated with the\
preferred origin and with the highest priority, as specified in the\
parameter file.

Returns float"""

	db[3] = 0

	magnitudes = get_all_magnitudes( db, str( db.getv( 'orid' )[0] ) )

	lowest_priority = 0
	prefmag = ''

	for key in magnitudes:
		if int( magtype_priorities[ key ] ) > lowest_priority:
			lowest_priority, prefmag = magtype_priorities[ key ], magnitudes[ key ]

	return prefmag
####

def write_row( db,magnitudes, pref_flag, pref_mag ):

	"""This method will return the properly formatted string for a given\
origin.

Returns string"""

	time, lat, lon, depth, auth = db.getv( 'time', 'lat', 'lon', 'depth', 'auth' )

	lead = '+' if pref_flag else ''

	time = strtime( time ).split()
	year = int( time[0].split('-')[0] )
	month = int( time[0].split('-')[1] )
	day = int( time[0].split('-')[2] )
	hhmm = int( time[1].split(':')[0] + time[1].split(':')[1] )
	ss = float( time[1].split(':')[2] )

	src = auth2src( auth )

	mb = magnitudes[ 'mb' ] if 'mb' in magnitudes else ''
	mn = magnitudes[ 'mn' ] if 'mn' in magnitudes else ''
	ml = magnitudes[ 'ml' ] if 'ml' in magnitudes else ''
	ms = magnitudes[ 'ms' ] if 'ms' in magnitudes else ''
	mc = magnitudes[ 'mc' ] if 'mc' in magnitudes else ''
	mw = magnitudes[ 'mw' ] if 'mw' in magnitudes else ''
	mw_prime = magnitudes[ 'mw\'' ] if 'mw\'' in magnitudes else ''
	m_othr = magnitudes[ 'other' ] if 'other' in magnitudes else ''

	row = ( '%1s %4d %02d %02d %04d %04.1f    %7.3f %8.3f %6.2f     %5s %5s %5s %5s %5s %5s %5s %5s %5s    %3s\n' \
		       % (lead, year, month, day, hhmm, ss, lat, lon, depth, mb, mn, ml, ms, mc, mw, m_othr, pref_mag, mw_prime, src) )

	return row
####

def auth2src( auth ):

	"""This method will return the three letter source code appropriate for a give author.
Author to source code mapping is done via the parameter file. This method will return
the default value, as specified in the parameter file, for unmapped authors.

Returns string"""

	global warning_blob

	for key in auth_src:
		pattern = re.compile( key )

		if pattern.match( auth ):
			return auth_src[ key ]
	
	if display_output:
		warning_blob += 'WARNING: Author \'' + auth + '\' could not be matched to a source. Please edit parameter file.'
	else:
		print 'WARNING: Author \'', auth, '\' could not be matched to a source. Please edit paramter file.'

	return src_dflt

######
#MAIN#
######
def main():

	"""This is the main method and returns nothing. This method serves to parse command line and parameter/
file arguments. This method also serves as a staging to make method calls to workhorse routines.

Returns nothing
"""
	parser = argparse.ArgumentParser()
	parser.add_argument( 'dbin', help='input database' )
	parser.add_argument( 'output', help='output ceef file' )
	parser.add_argument( '-p', dest='pf', nargs=1, help='optional parameter file' )
	parser.add_argument( '-s', dest='subset_expr', nargs=1, help='optional subset expression' )
	parser.add_argument( '-d', '--display_output', action='store_true', help='display output as generated' )

	args = parser.parse_args()

	#use program name as pf name if -p option not specified
	#pf = args.pf[0] if args.pf else os.path.splitext( sys.argv[0] )[0]

	#above may be uneccessarily complicated and be undesirable for debugging - hard code default pf name
	pf = args.pf[0] if args.pf else os.environ['PFPATH'] + '/db2ceef'

	global auth_src, src_dflt, magtype_priorities, display_output, warning_blob

	auth_src = pfget_arr( pf, 'auth_sources' )
	magtype_priorities = pfget_arr( pf, 'magtype_priorities' )
	src_dflt = pfget_string( pf, 'source_default' )

	subset_expr = args.subset_expr[0] if args.subset_expr else None
	display_output = args.display_output

	warning_blob = ''

	db = create_view( args.dbin, subset_expr )

	db2ceef( db, args.output, 'w' )

	if display_output:
		print warning_blob


main()



