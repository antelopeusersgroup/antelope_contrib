#include "ref2db.h"

Arr *wantcmp = 0;

int wrt2db ( double pkttime,
	char *srcname,
	char *packet,
	SpecPar *par)
{
    int i, unstuff, code ; 
    PktChannel *achan ;
    static Packet *unstuffed=0 ;
    Ch_data *buf ;
    char key[128];
    int  *data;

    if( wantcmp == 0 ) wantcmp = newarr( 0 );

    switch( code = 
          (unstuffpkt (pkttime, srcname, packet, &unstuffed) ) ) {
    
          case 1:
              for (i = 0; i < unstuffed->nchannels; i++) {
    	          achan = (PktChannel *) gettbl (unstuffed->chan, i);
                  sprintf (key, "%s_%s_%s", achan->net, achan->sta, achan->chan);
	          buf = (Ch_data *) getarr ( wantcmp, key );
                  if( buf == 0 )  {
	              buf = new_chan (achan, par) ;
                      setarr( wantcmp, key, buf );
                  } 
		      record (buf, achan ) ;
	      }
	      return 1;
            
            case 2:
                
                /* INFO packet  */
                return 1;
         
            default:
               
                elog_complain( 0, "unstuff error\n");
	        return 0;
    }

}

