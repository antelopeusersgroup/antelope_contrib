/**************************************************************************
 *
 *
 *
 *************************************************************************/
#include "extrd.h"
#define DEFINST "RefTek"
#define DEFSEG  "V"

extern Steim *init_steim();
extern int save_seed();

int update_segdata( SegData *segment , SegData *new )
{


        segment->time = 0.0;
        segment->endtime = 0.0;
        segment->calib = new->calib;
        segment->calper = new->calper;
        segment->samprate = new->samprate;
        segment->nsamp = 0;               
        segment->foff = Foff;              
        segment->dcode = new->dcode;
        strcpy( segment->net, new->net);
        strcpy( segment->sta, new->sta);
        strcpy( segment->chan, new->chan);
        strcpy( segment->datatype, new->datatype);
       
/*
        new_db( segment, new ); 
*/
        segment->new = 1;
        segment->dbname = Data_file;
        segment->fp = Df; 
        if( segment->dcode == trSEED)  {
           if( segment->steim != 0 ) freesteim(segment->steim);
           segment->steim =  init_steim(segment);
        }
return 1;
}

int check_param( SegData *segment , SegData *new )
{

   int code;
   int nsamp;



#ifdef DEBUG
fprintf( stderr, "check %lf - %lf * %lf \n", 
new->time, segment->endtime, segment->samprate);
#endif

        nsamp = (int) (new->time - segment->endtime) * segment->samprate;
        if( segment->calib != new->calib ||
            segment->calper != new->calper ||       
            segment->samprate != new->samprate ||
            segment->dcode != new->dcode  ||
            nsamp > 1 )   {
            
            flush_db( segment );
            update_segdata( segment, new );
            
       } 
return 1;
}

int new_db( SegData *segment )

{
    double epoch;


    epoch = now();
 
   
     dbout.record = dbaddnull( dbout );
     dbputv( dbout, 0,
             "time", segment->time,
             "endtime", segment->endtime,
             "calib", segment->calib,
             "calper", segment->calper,
             "samprate", segment->samprate,
             "datatype", segment->datatype,
             "nsamp", segment->nsamp,
             "foff", segment->foff,
             "sta", segment->sta, 
             "chan", segment->chan, 
             "dir", ".",
             "dfile", Dfile,
             "jdate", yearday( segment->time),
             "lddate", epoch,
             0);
    if( segment->dcode != trSEED)
       dbputv( dbout, 0, "datatype", "s4", 0);
       
    if( strncmp(segment->segtype, "A", 1 ) == 0  ||
        strncmp(segment->segtype, "D", 1 ) == 0  ||
        strncmp(segment->segtype, "V", 1 ) == 0  )  
       dbputv( dbout, 0, "segtype", segment->segtype, 0);
    else 
       dbputv( dbout, 0, "segtype", DEFSEG, 0);
/* 
    if( strncmp(segment->instype, " ", 1 ) != 0 )
       dbputv( dbout, 0, "instype", segment->instype, 0);
    else
       dbputv( dbout, 0, "instype",DEFINST, 0);
*/ 
        
    segment->db = dbout;

return 1;
}

int flush_db( SegData *segment )

{
 
     int code;


#ifdef DEBUG
fprintf( stderr, "flush: %lf %lf %d\n", segment->time, segment->endtime, segment->nsamp);
#endif
    
     if( segment->new )  {
        new_db( segment );
        segment->new = 0;
     }
     if( segment->dcode == trSEED )  {
         if( (code = csteim ( segment->steim, save_seed, 0, 0) )  < 0 ) 
              elog_die( 1, " steim compression error for %s_%s_%s at last record\n", 
                    segment->net, segment->sta, segment->chan  ) ;
 	else if ( code > 0 ) 
	      elog_complain( 1, " steim compression problems for %s_%s_%s at last record\n", 
	            segment->net, segment->sta, segment->chan ) ;
     }  else  {
	if( dbputv( dbout, 0,
             "time", segment->time,
             "endtime", segment->endtime,
             "nsamp", segment->nsamp,
             0)  < 0 ) 
             elog_die( 0, "can't write dbrecord\n");
	     
     }
 
return 1;
}

