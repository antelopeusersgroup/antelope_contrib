/************************************************************************
  *
  *  
  *
***********************************************************************/
#include "mrc.h"       

int offscale( 
    Packet *packet, 
    double pkttime, 
    char *srcid, 
    int tm_period,
    int *dasid )

{

  PktChannel	*achan;
  ChRec		*ch;
  ChPipe	*chpipe;
  int 		i, npkts;  
  Ch 		*crnt_chan=0; 
  ulong_t       fullsc= 0x80000000;
  int		*data;
  int 		max_nsamp,
  		nrec;
  int 		new = 0,
  		retcode=0,
		off_scale;
  char 		key[64];

  for( nrec=0 ; nrec < packet->nchannels ; nrec++ ) {

      off_scale = 0;
      crnt_chan = 0;

      achan = (PktChannel * ) gettbl ( packet->chan, nrec );
      
      sprintf( key, "%s/%s_%s\0", achan->net, achan->sta, achan->chan );
      max_nsamp = achan->samprate * tm_period;
      
/*
  complain( 0, " %s %lf %d\n ", key, pkttime, achan->nsamp );
*/
      ch = ( ChRec *) getarr( PChan, key );
      if( ch == 0 ) 
	 ch = ( ChRec *) new_ch( key, achan->sta, pkttime, tm_period );
      
      *dasid = ch->dasid;
	  
      ch->nsamp += achan->nsamp;
      data =  (int *)achan->data;
      for( i = 0; i < achan->nsamp; i++ )  
          ch->lta += data[i];
      
      chpipe = ch->chpipe;

      chpipe->crnt_chan.time = pkttime; 
      chpipe->crnt_chan.nsamp = achan->nsamp;
      chpipe->crnt_chan.lta = ch->lta;

      npkts = maxtbl( chpipe->tbl );
      if ( npkts >= chpipe->maxtbl ) 
          crnt_chan = shifttbl( chpipe->tbl);
      
      pushtbl( chpipe->tbl, &chpipe->crnt_chan);
  
      if( crnt_chan != 0 )  {

	  ch->nsamp -= crnt_chan->nsamp;
	  ch->lta -= crnt_chan->lta;

      }
      
      if( ch->nsamp >= max_nsamp )  {
	  if( labs(ch->lta / ch->nsamp) >= MaxOff )  {
              complain( 0, " %s_%s LTA is to high - %ld \n ", 
	                achan->sta, achan->chan, ch->lta );
	      off_scale = 1;
	      ch->lta = 0;
	      ch->nsamp = 0;
	  } else off_scale = 0;
	
      }
  
      setarr( PChan, key, ch );
      retcode += off_scale;

   }

   return retcode;

}

