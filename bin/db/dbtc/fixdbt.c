/*************************************************************************
 *
 * 
 *   fixdbt.c                        
 *
 *
 ***********************************************************************/
#include "dbtc.h"
 
main(argc, argv)
int argc;
char **argv;
 
{
 
    extern char *optarg;
    extern int  optind;
    Pf          *pf;
    Dset        dset;
    double      timerr, stime=0.0;
    char        *timstr=0;
    char        *dbname;
    char        *pfile = 0;
    int         c, err=0;
 
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
 
    if (err || argc - optind != 1 )
        usage ();
 
    dbname = strdup( argv[optind++]); 
 
    if( pfread ( pfile, &pf) != 0 )
         elog_die(0, "Can't read %s parameter file\n", pfile );
 
    timoff = pfget_int( pf, "allowed_time_err_offset");
    cor_win = pfget_int( pf, "correlation_window");

    fprintf( stdout, "The following parameters will be used for time correction:\n\n");
    fprintf( stdout, "\tcontrol:\tDB=%s\tSTA=%s\tCHAN=%s\n", 
                     good_set.dbname, good_set.sta, good_set.chan);
    fprintf( stdout, "\tcorrupted:\tDB=%s\tSTA=%s\tCHAN=%s\n", 
                     bad_set.dbname, bad_set.sta, bad_set.chan);
    fprintf( stdout, "\tcorrelation window = %d seconds\n", cor_win );
    fprintf( stdout, "\toffset window = %d seconds\n", timoff );
    fflush(stdout);

    good_set.cor_win = cor_win;
 
    bad_set.cor_win = cor_win + 2*timoff ;
    bad_set.stime = stime;
    bad_set.etime = stime + bad_set.cor_win;
 
  /*  Open  a control & corrupted databases.  */
 
    opendb ( &good_set );           
    opendb ( &bad_set );           
    
    if( !get_data( &bad_set ))
       elog_die(0, "can't process corrupted data set - %s.\n", bad );
    dbclose( bad_set.db); 
/*
    bad_set.npts -= cor_win*bad_set.srate;
    good_set.stime = bad_set.stime + timoff;
    good_set.etime = good_set.stime + good_set.cor_win;
*/ 
    good_set.stime = bad_set.stime + timoff;
    good_set.etime = bad_set.etime;

    if( !get_data( &good_set ))
       elog_die(0, "can't process control data set - %s.\n", good );
 
    dbclose( good_set.db );
    
    bad_set.extra_pts = (good_set.stime - bad_set.stime) * bad_set.srate;
 
    
    if( Log )  { 
        fprintf(stdout, "control data: stime=%lf endtime=%lf nsamp=%d\n", 
                good_set.stime, good_set.etime, good_set.npts);
        fprintf(stdout, "corrupted data: stime=%lf endtime=%lf nsamp=%d\n", 
                         bad_set.stime, bad_set.etime, bad_set.npts);
        fflush(stdout);
    } 
    timerr = correlation( &good_set, &bad_set ); 
    timerr = good_set.stime - bad_set.stime - timerr;
 
    fprintf(stdout, 
            "\t%lf seconds time shift will be applied to corrupted DB.\n", timerr);
    fflush(stdout);
  
    change_wfd( &bad_set, timerr );
    change_arr( &bad_set, timerr );
 
    exit(0);                             
}

