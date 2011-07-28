/*************************************************************************
 *
 * 
 *   dbtc.c                        
 *
 *
 ***********************************************************************/
#include "dbtc.h"

Dbptr Dbtc ;
int Log = 0 ;
long Foff = 0 ;
 
main(argc, argv)
int argc;
char **argv;
 
{
 
    extern char *optarg;
    extern int  optind;
    Pf          *pf;
    Dset        good_set, bad_set;
    Param	par;
    double      dtg, dtb;
    double      otime, timerr, etime, stime=0.0;
    float       *resamp = 0;
    int         retcode, npts; 
    int         c, err=0, done=0;
    char        *timstr=0;
    char        *bad, *good;
    char        *pfile = "dbtc";
 
    Log = 0;
    Program_Name = argv[0];
   
    while ((c = getopt (argc, argv, "p:t:v")) != -1)
        switch (c) {
        case 'p':
            pfile = strdup( optarg);
            break;
 
        case 't':
            timstr = strdup( optarg);
            stime = str2epoch( timstr);
            break;
 
        case 'v':
            Log=1;
            break;
 
        case '?':
            err++;
        }
 
    if (err || argc - optind != 2 )
        usage ();
 
    good = strdup( argv[optind++]); 
    bad = strdup( argv[optind++]); 
 
    if( !parse_key( good, &good_set ))
       elog_die( 0, 
          "bad name of a control db - %s.\n (Should be in form of db:sta:ch.)\n",
           good );
 
    if( !parse_key( bad, &bad_set ))
       elog_die( 0, 
          "bad name of a corrupted db - %s.\n(Should be in form of db:sta:ch.)\n", 
          bad );
 
    if( pfread ( pfile, &pf) != 0 )
         elog_die(0, "Can't read %s parameter file\n", pfile );
 
    par.datseg  = pfget_int( pf, "data_segment_window");
    par.err_win = pfget_int( pf, "allowed_time_err_offset");
    par.cor_win = pfget_int( pf, "correlation_window");

    fprintf( stdout, "The following parameters will be used for time correction:\n\n");
    fprintf( stdout, "\tcontrol:\tDB=%s\tSTA=%s\tCHAN=%s\n", 
                     good_set.dbname, good_set.sta, good_set.chan);
    fprintf( stdout, "\tcorrupted:\tDB=%s\tSTA=%s\tCHAN=%s\n", 
                     bad_set.dbname, bad_set.sta, bad_set.chan);
    fprintf( stdout, "\tdata segment = %d seconds\n", par.datseg );
    fprintf( stdout, "\tcorrelation window = %d seconds\n", par.cor_win );
    fprintf( stdout, "\terror offset window = %d seconds\n", par.err_win );
    fflush(stdout);

  /*  Open  a control & corrupted databases.  */
 
    opendb ( &good_set );           
    opendb ( &bad_set );           
    Dbtc = dblookup ( bad_set.db, 0, "tcorrection", 0, 0);
    if (Dbtc.table < 0)  
        elog_die(0, "Can't open tcorrection table '%s'\n", bad_set.dbname );

    etime = stime + par.datseg ;
 
    while ( !done )  { 

       bad_set.stime = stime ;
       bad_set.etime = etime ;
   
       switch( (retcode = get_data( &bad_set, &par )) )  {
	  case 0:
             dbclose( bad_set.db); 
             dbclose( good_set.db); 
             elog_die(0, "can't get %s data for %lf - %lf time period.\n", 
	             bad, stime, etime );
          case -1:
             done = 1; 
             break;
	  default:
            stime = bad_set.etime + 1.0/bad_set.srate;
            etime = bad_set.etime + par.datseg ;
       
	    good_set.stime = bad_set.stime + par.err_win;
       	    good_set.etime = good_set.stime + par.cor_win;

       	    if( !get_data( &good_set, &par ))
          	    elog_die(0, "can't get %s data for %lf - %lf time period.\n", 
	          	    good, good_set.stime, good_set.etime );


  /* Check if sample rates are the same for 'good' and 'bad'
     data sets.  If not then 'resample' 'bad' data set  */
		      

       	    if( fabs( good_set.srate - bad_set.srate ) >= 1.0 )  {
	     
           	    if( Log )  {
      	     	       elog_complain( 0, "data sets have a different sample rate - %lf != %lf\n",
                 	       good_set.srate, bad_set.srate );
       	    	       elog_complain( 0, "will resample \'bad\' data set from %lf to %lf...",
                 	       bad_set.srate, good_set.srate );
	   	    }
	   	    dtg = 1.0/good_set.srate;
	   	    dtb = 1.0/bad_set.srate;
	   	    npts = 0;
	   	    resmpl( bad_set.npts, dtb, bad_set.stime, bad_set.data, 
	           	    &npts, dtg, bad_set.stime, &otime, &resamp );
             	     
           	    free(bad_set.data);
	   	    bad_set.npts = npts;
	   	    bad_set.data = resamp ; 
       	    }  

       	    if( bad_set.npts <= good_set.npts )  {
           	  dbclose( bad_set.db); 
           	  dbclose( good_set.db); 
           	  elog_die(0, 
		  "can't process data.\nNot enough data points (%d) for corrupted db %s.\n",
	           bad_set.npts, bad );
       	    }
       	    par.ncorr = good_set.npts;

       	    if( Log )  { 
           	 fprintf(stdout, "control data: stime=%lf endtime=%lf nsamp=%d\n", 
                    good_set.stime, good_set.etime, good_set.npts);
           	 fprintf(stdout, "corrupted data: stime=%lf endtime=%lf nsamp=%d\n", 
                    bad_set.stime, bad_set.etime, bad_set.npts);
           	 fflush(stdout);
       	    } 
       	    if( !correlation( &good_set, &bad_set, &par, &timerr ) ) {
	  	 if( Log ) { 
	      	    fprintf(stdout, 
                        "\tbad data segment at %s (%lf-%lf).\n", 
	                timstr=strtime(bad_set.stime), bad_set.stime, bad_set.etime );
              	    free(timstr);
	      	    fflush(stdout);
          	 }
	  	 continue;
       	    }
       	    timerr = good_set.stime - bad_set.stime - timerr;
 
       	    if( Log )  {
	   	    fprintf(stdout, 
              	       "\t%lf seconds time shift will be applied to corrupted DB at %lf.\n", 
	      	       timerr, bad_set.stime);
           	    fflush(stdout);
       	    }
       
       	    free(bad_set.data); free(good_set.data);
  
      	    save_tcorr( &bad_set, &good_set, timerr );
            break;
       }
    }
    exit(0);                             
}

