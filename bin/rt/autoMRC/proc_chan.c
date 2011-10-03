/************************************************************************
  *
  *  offscale.c
  * 
  *  calculate LTA; compare LTA value with MAX allowed value;
  *  
  *
  *
  *  Author: Marina Harkins-Glushko
  *  	    UCSD, IGPP
  *	    glushko@ucsd.edu
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
  unsigned long fullsc= 0x80000000;
  long 		lta;
  int		*data;
  int 		max_nsamp,
  		nrec;
  int 		mrcnum = 0, new = 0;
  char 		key[64];

  for( nrec=0 ; nrec < packet->nchannels ; nrec++ ) {

      crnt_chan = 0;

      achan = (PktChannel * ) gettbl ( packet->channels, nrec );
      
      sprintf( key, "%s/%s_%s\0", achan->net, achan->sta, achan->chan );
      max_nsamp = achan->samprate * tm_period;
      
      ch = ( ChRec *) getarr( PChan, key );
      if( ch == 0 ) { 
	 ch = ( ChRec *) new_ch( key, achan->sta, pkttime, tm_period );
      } 
      *dasid = ch->dasid;
	  
      ch->nsamp += achan->nsamp;
      data =  (int *)achan->data;
      ch->pktlta = 0;
      for( i = 0; i < achan->nsamp; i++ )
          ch->pktlta += data[i];
     
      ch->lta += ch->pktlta; 
      chpipe = ch->chpipe;

      chpipe->crnt_chan.time = pkttime; 
      chpipe->crnt_chan.nsamp = achan->nsamp;
      chpipe->crnt_chan.lta = ch->pktlta;

      npkts = maxtbl( chpipe->tbl );
      if ( npkts >= chpipe->maxtbl ) 
          crnt_chan = shifttbl( chpipe->tbl);
     
  
      if( crnt_chan != 0 )  {

	  ch->nsamp -= crnt_chan->nsamp;
	  ch->lta -= crnt_chan->lta;

      } 
      pushtbl( chpipe->tbl, &(chpipe->crnt_chan) );
/*     
lta = (ch->lta / ch->nsamp) ;
fprintf(stderr, "%s %ld %d - %ld\n", key, ch->lta, ch->nsamp, lta );
*/
      if( pkttime - ch->time >= tm_period )  {
	  lta = (ch->lta / ch->nsamp) ;
	  if( labs(lta) >= MaxOff )  {
              elog_complain( 0, " %s_%s LTA is to high - %ld \n ", 
	                achan->sta, achan->chan, lta );
	      ch->lta = 0;
	      ch->nsamp = 0;
	      (ch->mrcnum)++ ;
	      freetbl(chpipe->tbl, 0 );
	      chpipe->tbl = inittbl( 0, chpipe->maxtbl, 1, 0, sizeof(Ch) );

	  } else {
	      ch->mrcnum = 0;
	  }
	  ch->time = pkttime;
	
          if( ch->mrcnum > mrcnum ) mrcnum = ch->mrcnum; 
      } else mrcnum = 0;
      
      setarr( PChan, key, ch );

   }

   return mrcnum;

}

