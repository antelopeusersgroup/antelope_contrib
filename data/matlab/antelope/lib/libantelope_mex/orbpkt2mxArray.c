/*
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#include <stdio.h>
#include "antelope_mex.h"
#include "mex_orb.h"

mxArray *orbpkt2mxArray( char *srcname, double time, char *packet, int nbytes, char *pkttype )
{
	mxArray	*array = NULL;
	Packet  *pkt = NULL;
	PktChannel *pktchan = NULL;
	int	type;	
	mxArray *in[2], *out[1];
	int	dims[2];
	char	*pdata = NULL;
	Dbptr	tr;
	Trsample *trdata = NULL;
	double	endtime;
	char    *tr_path = NULL;
	char    *schema_name = NULL;
	char    errmsg[STRSZ];
	int	ichannel;
	int	idata;

	type = unstuffPkt( srcname, time, packet, nbytes, &pkt );

	switch( type ) 
	{
	case Pkt_wf:
		sprintf( pkttype, "waveform" );
		tr = trnew( tr_path, schema_name );
		antelope_mex_clear_register( 1 );

		if( tr.database == dbINVALID )
		{
			array = (mxArray *) NULL;
			mexErrMsgTxt( "failed to create new trace database" );
		}
		else
		{
			tr = dblookup( tr, 0, "trace", 0, 0 );
			antelope_mex_clear_register( 1 );
		}

		for( ichannel = 0; ichannel < pkt->nchannels; ichannel++ )
		{
			pktchan = (PktChannel *) gettbl( pkt->channels, ichannel );

			/* trace library mallocs outside of Matlab context, so we will too: */
			allot( Trsample *, trdata, pktchan->nsamp );

			for( idata = 0; idata < pktchan->nsamp; idata++ )
			{
				trdata[idata] = (Trsample) pktchan->data[idata];
			}

			endtime = ENDTIME( pktchan->time, pktchan->samprate, pktchan->nsamp );

			tr.record = dbaddv( tr, NULL, 
					  "net", pktchan->net,
					  "sta", pktchan->sta,
					  "chan", pktchan->chan,
					  "segtype", pktchan->segtype,
					  "time", pktchan->time,
					  "samprate", pktchan->samprate,
					  "calib", pktchan->calib,
					  "calper", pktchan->calper,
					  "nsamp", (long) pktchan->nsamp,
					  "endtime", endtime,
					  "data", trdata,
					  NULL );

			antelope_mex_clear_register( 1 );
		}

		/* This is hopefully convenient for single-channel waveform packets */
		if( pkt->nchannels == 1 ) {
			tr.record = 0;
		}

		array = CreateDbptrStructFromDbptr( tr );
		if( array == NULL )
		{
			trfree( tr );
			antelope_mex_clear_register( 1 );

			sprintf( errmsg, "failed to create database-pointer " );
			strcat( errmsg, "structure for result" );
			mexErrMsgTxt( errmsg );
		}
		break;

	case Pkt_db:
		sprintf( pkttype, "database" );
		array = CreateDbptrStructFromDbptr( pkt->db );
		break;

	case Pkt_pf:
		sprintf( pkttype, "parameterfile" );

		/* Call dbpf constructor */
		/* Can't use Pf2mxArray() because we're not in dbpf context */
		/* Compile pf directly into new dbpf to avoid memory-management
		 * and static-library problems */

		mexCallMATLAB( 1, out, 0, 0, "dbpf" );
		in[0] = out[0];
		in[1] = mxCreateString( pf2string( pkt->pf ) );
		mexCallMATLAB( 1, out, 2, in, "dbpf" );
		mxDestroyArray( in[0] );
		mxDestroyArray( in[1] );
		array = out[0];
		break;

	default:
		sprintf( pkttype, "unknown" );
		mexWarnMsgTxt( "this packet-type is not currently handled! Returning raw byte array\n" );
		array = mxCreateNumericArray( 2, dims, mxUINT8_CLASS, mxREAL );
		pdata = mxGetData( array );
		memcpy( pdata, packet, nbytes );
		break;
	}

	if( pkt ) freePkt( pkt );
	antelope_mex_clear_register( 1 );

	return array;
}
