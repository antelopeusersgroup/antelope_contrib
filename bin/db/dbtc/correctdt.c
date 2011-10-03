/*************************************************************************
 *
 * 
 *   correctdt.c                        
 *
 *
 ***********************************************************************/
#include "dbtc.h"

#define NEW "new"
#define TIM_OFF  20
#define DATA_OFF 44

int Log = 0 ;
long Foff = 0 ;


Dbptr	dbc, dba, dbw;
regex_t srcmatch;


void usage ()
{
    fprintf (stderr, "usage: %s [-m srcmatch] [-t stime] [-v] dbname\n",Program_Name);
    exit (1);
}
 
int wrt_data( 
    FILE *fp,
    char *data,
    int npts
)
{
     long wrt_pnts = 0;
     int code;
     
             
     wrt_pnts = fwrite( data, 1, npts,  fp );
     if ( wrt_pnts != npts )  {
               elog_complain(  0, "write %ld samples insted of %ld \n", wrt_pnts, npts);
               return 0;
     }
     Foff += wrt_pnts;
     fflush( fp );
 
  return 1;
}
 
double seedtime( char *buf)
{
 
   double epoch;
   char str[48], *tmp;
   short val;
   int yr, day, hr, min, sec, msec;
   
   memcpy( str, buf+20, 10 );
   tmp = &str[0];
   memcpy( &val, tmp, 2 );
   yr = val;
   tmp += 2;
   
   memcpy( &val, tmp, 2 );
   day = val;
   tmp += 2;
 
   val = *tmp;
   hr = val;
   tmp += 1;
   
   val = *tmp;
   min = val;
   tmp += 1;
 
   val = *tmp;
   sec = val;
   tmp += 2;
 
   memcpy( &val, tmp, 2 );
   msec = val;
   tmp += 2;
 
   sprintf( str, "%4d%03d:%02d:%02d:%02d.%03d\0", yr, day, hr, min, sec, msec);
   epoch = str2epoch( str );
 
   return epoch;

}

int change_time( char *buf0, 
                 double epoch, 
                 int nbytes, 
                 char **buf1 )
{
 
   double dsec;
   char *tmp;
   short val;
   int yr, day, hr, min, sec, msec;
   
   e2h( epoch, &yr, &day, &hr, &min, &dsec);   
   sec = (int) dsec;
   msec = (int) ( ( dsec - sec) * 1.0e4 + 0.5 );
 
   tmp = *buf1;
   memcpy( tmp, buf0, 20 );
   tmp += 20;
   nbytes -= 20;
 
   val = yr;
   memcpy (tmp, &val, 2);
   tmp += 2;
   val = day;
   memcpy (tmp, &val, 2);
   tmp += 2;
   val = hr;
   *tmp = val;
   tmp++;
   val = min;
   *tmp = val;
   tmp++;
   val = sec;
   *tmp = val ;  
   tmp++;
   memset (tmp, 0, 1);
   tmp++;
   val = msec;
   memcpy (tmp, &val, 2);
   tmp += 2;
   nbytes -= 10;
 
   memcpy( tmp, buf0 + 30, nbytes); 
   
   return 1;
 
}
double *newtim() 
{
    double *new;
    allot( double *, new, 1 );
    return new;
}

int change_name( char *dbname )
{
    struct stat stat_buf;
    char newname[512], str[512];


    /* change original wfdisc&arrival tables in tables
       with '-' at the end of a name  */

    sprintf( newname, "%s.wfdisc\0", NEW );
    if( stat( newname, &stat_buf) != 0 ) {
         if(!ENOENT) elog_die( 1, "can't stat %s.\n", newname );
    }  else {
       sprintf( str, "mv %s.wfdisc %s.wfdisc_orig\0", dbname, dbname );
       if( system(str) == -1 ) 
           elog_die( 1, "can't save an original wfdisc file - %s.wfdisc\n", dbname );
       sprintf( str, "mv %s %s.wfdisc\0", newname, dbname );
       if( system(str) == -1 ) 
           elog_die( 1, "can't save a new wfdisc file - %s\n", newname );
    }

    sprintf( newname, "%.arrival\0", NEW );
    if( stat( newname, &stat_buf) != 0 ) {
         if( !ENOENT) elog_die( 1, "can't stat %s.\n", newname );
    }  else {
       sprintf( str, "mv %s.arrival %s.arrival_orig\0", dbname, dbname );
       if( system(str) == -1 ) 
           elog_die( 1, "can't save an original arrival file - %s.arrival\n", dbname );
       sprintf( str, "mv %s %s.arrival\0", newname, dbname );
       if( system(str) == -1 ) 
           elog_die( 1, "can't save a new arrival file - %s\n", newname );
    }
   
    return 1; 
}


int change_arr( int dbrec, int arec )
{
    double stime, ctime, tcorr;
    double prev_tcor=0.0;
    char newname[512], key[32], arrec[512];
    char sta[8], chan[12], net[12];
    Dbptr dbout;
    int recnum; 
     
    if (dbopen_database ( NEW, "r+", &dbout ) == dbINVALID )
         elog_die(0, "Can't open database %s\n",  NEW );
 
    dbout = dblookup (dbout, 0, "arrival", 0, 0);
    if (dbout.table < 0)  {
        elog_die(0, "Can't open arrival table '%s'\n", NEW );
    }
    
    for( dba.record = 0, dbout.record=0; dba.record < arec; dba.record++, dbout.record++)   {
 
       if( (dbgetv( dba, 0, "time", &stime, 0 )) == dbINVALID )  
           elog_die( 0, "dbgetv faild for record #%d\n", dba.record );  
      
       if ( dbget ( dba, arrec ) == dbINVALID )   {
             elog_die( 1,"Can't get arrival record #%d\n", dba.record );
       }

       dbout.record = dbaddnull( dbout );
       dbput( dbout, arrec);

       for( dbc.record = 0; dbc.record < dbrec; dbc.record++) {
	            
	    if( (dbgetv( dbc, 0,
	         "time", &ctime,
	         "tcor", &tcorr,
	         "bchan", chan,
	         "bnet", net,
	         "bsta", sta, 0 )) == dbINVALID )
	          elog_die( 0, "dbgetv faild for record #%d\n", dbc.record );
	        
	    sprintf( key, "%s_%s_%s\0", net, sta, chan );

	    if( !regexec( &srcmatch, key, (size_t) 0, NULL, 0 ) )  {
	          if( prev_tcor == 0.0 )  prev_tcor = tcorr;
	          if( stime > ctime )  {
			dbputv( dbout, 0, "time", stime + prev_tcor, 0);
			break;
		  }
	          if( stime == ctime )  {
			dbputv( dbout, 0, "time", stime + tcorr, 0);
			break;
		  }
		  prev_tcor = tcorr;
	    }
       }
    }
  
    dbclose(dbout);
  
  return 1;
}


 
int change_wfd( char *dbname, int dbrec, int wfrec )
{
    double endtime, stime, ctime, tcorr;
    double timerr, prev_tcor=0.0;
    double samprate, crnt_time, rectime;
    Dbptr dbout;
    struct stat stat_buf;
    unsigned short doff;
    int dcode, foff, wfdoff; 
    int seed=0;
    char datatype[4], sta[8], ssta[8], chan[12], schan[4];
    char old_fname[256], newname[512], key[32];
    char csta[8], cchan[12], cnet[12];
    char  dfname[512], wrec[512]; 
    char tmpname[512], str[512];
    char Dfile[64];
    char newnm[132];
    int done=0, nread,nleft, i, recnum;
    char *seed_buf=0, *rec=0;
    FILE *infp=0, *outfp=0;
 
 
     
    if (dbopen_database ( NEW, "r+", &dbout ) == dbINVALID )
         elog_die(0, "Can't open database %s\n",  NEW );
 
    dbout = dblookup (dbout, 0, "wfdisc", 0, 0);
    if (dbout.table < 0)  {
        elog_die(0, "Can't open wfdisc table '%s'\n", NEW );
    }
    
   
    wfdoff = 0;
    for( dbw.record = 0, dbout.record=0; dbw.record < wfrec; dbw.record++, dbout.record++)   {
 
       if( (dbgetv( dbw, 0,
          "time", &stime,
          "endtime", &endtime,
          "foff", &foff,
          "samprate", &samprate,
          "sta", sta,
          "chan", chan,
          "datatype", datatype,
           0 )) == dbINVALID )
           elog_die( 0, "dbgetv faild for record #%d\n", dbw.record );

       if ( dbget ( dbw, wrec ) == dbINVALID )   {
             elog_die( 1,"Can't get wfdisc record #%d\n", dbw.record );
       }

       dbout.record = dbaddnull( dbout );
       dbput( dbout, wrec);
 
       dcode = trdatacode ( datatype);
       if( Log )  {
           fprintf( stdout, "fixing db records for %s_%s (%lf %lf) ", 
                                sta, chan, stime, endtime );
           if( dcode == trSEED) 
              fprintf( stdout, "and time in a SEED header.\n");

           fflush(stdout);
       }

       timerr = 0.0;
       for( dbc.record = 0; dbc.record < dbrec; dbc.record++) {
	            
	    if( (dbgetv( dbc, 0,
	         "time", &ctime,
	         "tcor", &tcorr,
	         "bchan", cchan,
	         "bnet", cnet,
	         "bsta", csta, 0 )) == dbINVALID )
	          elog_die( 0, "dbgetv faild for record #%d\n", dbc.record );
	        
	    sprintf( key, "%s_%s_%s\0", cnet, sta, chan );

	    if( !regexec( &srcmatch, key, (size_t) 0, NULL, 0 ) )  {
	          if( prev_tcor == 0.0 ) prev_tcor = tcorr;
	          if( stime < ctime )  {
			timerr =  prev_tcor;
			break;
		  }
	          if( stime = ctime )  {
			timerr =  tcorr;
			break;
		  }
	    }
       }

       if( timerr != 0.0 )  {
           if( dcode == trSEED )  {
          
              if( rec == 0 ) allot( char *, rec, 64 );
              if( seed_buf == 0 ) allot( char *, seed_buf, 4096 );
 
              if(!seed )  {
                 sprintf( tmpname, "new.w\0" );
                 outfp = fopen (tmpname, "w");
                 if (outfp == 0) elog_die( 1, "Unable to open '%s'\n", tmpname);
                 seed = 1;
              }
              if (dbfilename (dbw, dfname) < 1) 
                      elog_die(1, "Unable to find input file '%s'\n", dfname);
              if( strncmp( old_fname, dfname, sizeof(dfname) ) ) {
                 if(infp != 0 )  fclose(infp);
                 infp = zopen (dfname);
                 if (infp == 0) 
                      elog_die( 1, "Unable to open '%s'\n", dfname);
              } 
              strcpy( old_fname, dfname);
              if (fseek(infp, foff, 0) < 0) {
                    fclose (infp);
                    elog_die( 0, "fseek() error on '%s'\n", dfname);
              }  
     
          
              while( !done )  {
     
                  if ( (nread=fread ( seed_buf, 1, 48, infp)) != 48 ) {
                        if( nread == 0 ) break;
                        else  elog_die( 1, "Can't read first 48 bytes of a SEED record.\n" );
                  }
                  memcpy(ssta,  seed_buf+8, 5 ) ;
                  TRIM(ssta,6);
         
                  memcpy(schan,  seed_buf + 15, 3 ) ;
                  TRIM(schan,4);
                        
                  crnt_time = seedtime( seed_buf );
                  rectime = crnt_time + timerr ;
        
                  if( strncmp(sta, ssta, sizeof(sta) ) || 
                        strncmp(chan, schan, sizeof(chan) ) ||  
                        ( crnt_time - endtime ) > 1.0/samprate ) {
                        break;        
                  } else { 
                        memcpy( (char *) &doff, seed_buf+DATA_OFF, 2);
                        change_time( seed_buf, rectime, nread, &rec );
      
                        if( !wrt_data( outfp, rec, 48) )  
                            elog_die( 1, "write error for %s_%s SEEDHDR. \n", sta, chan );  
                    
                        nleft = doff - 48;
                        if( doff <= 0 ) break;   
                        if ( (nread=fread ( seed_buf, 1, nleft, infp)) != nleft ) 
                              elog_die( 1, "Can't read SEED headers.\n" );
                        if( !wrt_data( outfp, seed_buf, nread ) )  
                              elog_die( 1, "write error for %s_%s SEEDHDR.\n", sta, chan);  
                   
                        nleft = 4096 - doff; 
                        if ( (nread=fread ( seed_buf, 1, nleft, infp)) != nleft ) 
                              elog_die( 1, "Can't read SEED headers.\n" );
                        if( !wrt_data( outfp, seed_buf, nread) )  
                              elog_die( 1, "write error for %s_%s at %lf.\n", sta, chan, rectime );  
                 
                  }
    
                  if( Log )  {
                      fprintf( stderr, "sta=%s chan=%s time=%lf databytes:%d total_bytes:%ld\n", 
                                        ssta, schan, rectime, nread, Foff );
                      fflush(stdout);
                  } 
              }
     
         } else wfdoff = foff; 
	 dbputv( dbout, 0,
             "foff", wfdoff,
             "time", stime + timerr,
             "endtime", endtime + timerr, 0);
     }  else {
         
         dbputv( dbout, 0,       
             "foff", wfdoff,
             "time", stime ,
             "endtime", endtime , 0);
     }
     wfdoff = Foff;
  }
  
  fclose( outfp );
  fclose(infp);
 
  dbclose( dbw); dbclose(dbout);
  
  if(seed )  {
 
    sprintf( str, "mv %s.w %s.w_orig\0", dbname, dbname );
    if( system(str) == -1 ) 
          elog_die( 1, "can't save an original data file - %s.w\n", dbname );
    
 
    sprintf( str, "mv %s %s.w\0", tmpname, dbname );
    if( system(str) == -1 ) 
       elog_die( 1, "can't rename new data file - %s\n", Dfile);
    
  }
  return 1;
}

main(argc, argv)
int argc;
char **argv;
 
{
 
    extern char *optarg;
    extern int  optind;
    Dbptr	db;
    double 	stime, *ttim;
    char        *dbname;
    Tbl 	*sort_sta_ch_tm=0;
    int 	dbrec, i, code;
    int 	arrec, wfrec;
    int         c, err=0, done=0;
    char        *match=".*", *timstr=0;
 
    Log = 0;
    Program_Name = argv[0];
   
    while ((c = getopt (argc, argv, "m:t:v")) != -1)
        switch (c) {
 
        case 'm':
            match = strdup( optarg);
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
            
    if ( (code = regcomp( &srcmatch, match, REG_EXTENDED|REG_NOSUB)) != 0)
          elog_die( 1, "regcomp error #%d for %s\n", code, match );

  /*  Open  a corrupted databases.  */

    if (dbopen_database ( dbname, "r", &db ) == dbINVALID )
        elog_die(0, "Can't open database %s\n",  dbname );
	      
    dbc = dblookup ( db, 0, "tcorrection", 0, 0);
    if (dbc.table < 0)  
        elog_die(0, "Can't open tcorrection table '%s'\n", dbname );

    sort_sta_ch_tm = strtbl("bsta", "bchan", "time", 0 ) ;
    dbc = dbsort ( dbc, sort_sta_ch_tm, 0, 0 ) ;
    dbquery ( dbc, dbRECORD_COUNT, &dbrec );
    if( dbrec <= 0 )
       elog_die( 0, " no record in %s \'tcorrection\' table.\n", dbname ); 
   
    dbw = dblookup ( db, 0, "wfdisc", 0, 0);
    if ( dbw.table == dbINVALID )
       elog_complain(0, "Can't open '%s' wfdisc table.\n", dbname );
    dbquery ( dbw, dbRECORD_COUNT, &wfrec );
    if( wfrec <= 0 )
       elog_complain( 0, " no record in %s \'wfdisc\' table.\n", dbname ); 
			     
    dba = dblookup ( db, 0, "arrival", 0, 0);
    dbquery ( dba, dbRECORD_COUNT, &arrec );
    if( arrec <= 0 )
       elog_complain( 0, " no record in %s \'arrival\' table.\n", dbname); 

    if( wfrec <= 0 && arrec <= 0 ) 
       elog_die(0, "wfdisc&arrival tables contain no records. Nothing to fix.\n");

    if( wfrec > 0 )  change_wfd( dbname, dbrec, wfrec );
    if( arrec > 0 ) change_arr(  dbrec, arrec );
    
    if( !change_name( dbname ) )
       elog_die(0, "can't save new wfdisc/arrival tables.\n"); 
    
    exit(0);                             
}

