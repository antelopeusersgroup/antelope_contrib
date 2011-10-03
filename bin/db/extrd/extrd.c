/**********************************************************************
 *
 * 
 *  extrd/main.c 
 *  
 *  Program extract portion of the data form data file in css format.
 *  Program write in the current directory modified wfdisc file
 *  make directory with start time and write there portion of the data.
 *
 *
 *********************************************************************/
#include <signal.h>
#include "extrd.h"

regex_t sta_match;
regex_t chan_match;
FILE *Df;
char *Data_file;
char Outdir[132];
char Dfile[132];
long Foff;
Dbptr db, dbout;
Trdata mydata;

extern void sig_hdlr();
char *Network = 0;
int Seq;
char in_dbase[132];

static void usage()
{ 
        fprintf(stderr,
	"\nusage:  %s [-c chan] [-n net] [-o outdir] [-s sta] dbname|wfd_dir|\"wfd_fls\" stime etime \n\n", 
	Program_Name);
        exit(1);
}

main(argc, argv)
int argc;
char *argv[];

{

 extern int optind;
 extern char *optarg; 
 struct stat buf;
 Dbptr     tmpaf, dbaf, dbsub, tmpdb, wfddb;
 Tbl       *sort_sta_ch_tm;
 FILE      *fd;
 double    stime, etime;
 char      *out_dbase;
 char      odir[132], exten[132];
 char      path[132], name[132];
 char      wfd_name[256], *fname;
 char      *str;
 char      afname[256], inname[256]; 
 char      newdir[256], *tmpname;
 char      rec[256];
 int       nrec, oldsize, 
           record_size, i, j;
 int       wfdrec;
 int       err_in, id;
 int 	   affil = 0, gotone;
 char      subset_condition[80];
 char      *out_dir = 0;
 char      *sta = ".*" ;
 char      *chan = ".*" ;

    err_in = 0; 

    while( ( id = getopt( argc, argv, "c:n:o:s:") ) != -1 )
      switch( id )  {

        case 'c':
          chan = strdup( optarg );
          break;
        
        case 'n':
          Network = strdup( optarg );
          break;
        
        case 'o':
          if( (out_dir = (char *) malloc(256) ) == NULL )  {
             elog_die( 1, "extrd/main(): malloc");
          }
          strcpy(out_dir, optarg);
          break;
 
        case 's':
          sta = strdup( optarg );
          break;
        
        case '?':
          err_in++;
      }

    elog_init (argc, argv) ;
    elog_notify (0, "%s v2.0\n", argv[0] ) ;
    Program_Name = argv[0];
    
    if( err_in || argc - optind < 3 )
      usage();

    strcpy(inname,argv[optind++]);

    stime = str2epoch(argv[optind++]);
    etime = str2epoch(argv[optind]);
   
    
    if( (out_dbase = (char *) malloc(256) ) == NULL )  {
       elog_die( 1, "extrd/main(): malloc");
    }

    if( (fname = (char *) malloc(256) ) == NULL )  {
       elog_die(1, "extrd/main(): malloc");
    }

    if( (str = (char *) malloc(256) ) == NULL )  {
       elog_die(1, "extrd/main(): malloc");
    }


/*  Get input parameters from comand line and initialized its */

   if(out_dir != 0) 
       if(!create_dir(out_dir))
          elog_die( 1, "Can't create an output directory: %s\n", out_dir); 
       
   signal(SIGINT, sig_hdlr );
   signal(SIGBUS, sig_hdlr );
   signal(SIGSEGV, sig_hdlr );

   tmpname = mktemp(strdup("extrdXXXXXX") );
   sprintf( in_dbase, "%s.wfdisc\0",  tmpname );  
   sprintf( afname, "%s.affiliation\0",  tmpname );  
   
   if(stat(inname, &buf) == 0) {
     if(S_ISDIR(buf.st_mode) != 0)  {
        strcpy(path, inname );
        sprintf(str, "ls %s/*.wfdisc > .tmp \0",inname );
     }  else if(S_ISREG(buf.st_mode) != 0)  {

         namefrpath( inname , name);
         fexten(name, exten);
         if( strlen(exten) <= 0)  {
             get_wfd_name(inname , wfd_name);
                 sprintf(str, "ls %s > .tmp \0",wfd_name);
         } else if( strncmp(exten, "wfdisc", strlen("wfdisc")) == 0)  {
            sprintf(str, "ls %s > .tmp\0",inname );
         }
     }
   } else  if(ENOENT) {
       fexten(inname , exten);
       if( strlen(exten) > 0 && strncmp(exten,"wfdisc", strlen("wfdisc") ) == 0)  {
           pathfrname(inname , path);
           sprintf(str, "ls %s > .tmp \0", inname );
       }  else if(strlen(exten) <= 0)  {
           sprintf(wfd_name,"%s.wfdisc\0", inname );
           if(stat(wfd_name, &buf) == 0) { 
               pathfrname(wfd_name, path);
               sprintf(str, "ls %s > .tmp\0",wfd_name );
           } else {
               elog_die( 0, "Can't find wfdisc file.\n");
           }
       } else { 
	  elog_die( 0, "Can't find wfdisc file.\n"); 
       } 
   } else { 
       elog_die( 1, "Can't stat!\n");
   }
   system(str); 

   /*  Get file name from list file  */
 
   if ((fd = fopen(".tmp","r") ) == NULL) {
         unlink( ".tmp" ); 
         elog_die( 1,"Can't open file .tmp \n");
    } 

    
    if (dbopen_database (in_dbase, "r+", &db) < 0)  {
         unlink( ".tmp" ); 
         elog_die(0, "Can't open database %s\n", in_dbase);
    } 

    db = dblookup (db, 0, "wfdisc", 0, 0);
    if (db.table < 0)  {
         unlink( ".tmp" ); 
        elog_die(0, "Can't open wfdisc table '%s'\n", in_dbase );
    }

    dbaf = dblookup (db, 0, "affiliation", 0, 0);
    if (dbaf.table < 0)  {
         unlink( ".tmp" ); 
        elog_die(0, "Can't open wfdisc table '%s'\n", in_dbase );
    }


    while((fname = fgets(fname, 132,fd)) != NULL)  {
      gotone = 1;
      for(i = strlen(fname); i >= 0; i--) {
         if( fname[i] == '\n' )
         break;
      }
      fname[i] = '\0';
      fname[ i - strlen(".wfdisc")] = '\0';
 
      if (dbopen( fname, "r+", &tmpdb) < 0)  {
         unlink( ".tmp" ); 
         elog_die(0, "Can't open database %s\n", fname );
      }
 
      tmpdb = dblookup (tmpdb, 0, "wfdisc", 0, 0);
      if (tmpdb.table < 0)  {
        unlink( ".tmp" ); 
        elog_die(0, "Can't open wfdisc table '%s'\n", fname );
      }

     /* subset db to only look at waveform segments that matter */
      sprintf(subset_condition,"(endtime >= %lf && time <= %lf)",
                        stime, etime);
      dbsub = dbsubset(tmpdb,subset_condition,0);
      dbquery (dbsub, dbRECORD_COUNT, &nrec);
      if( nrec <= 0 )  {
	  elog_complain(0, "No records in  %s after '%s' subset\n", 
	                fname, subset_condition );
	  gotone = 0;
	  continue;
      } 
      sprintf(subset_condition,"(chan =~ /%s/ && sta =~ /%s/)", chan, sta);
      dbsub = dbsubset(dbsub,subset_condition,0);
      dbquery (dbsub, dbRECORD_COUNT, &nrec);
      if( nrec <= 0 )  {
	  elog_complain(0, "No records in  %s after '%s' subset\n", 
	                fname, subset_condition );
	  gotone = 0;
	  continue;
      } 
      for( dbsub.record = 0; dbsub.record < nrec; dbsub.record++ )  {
  
          if ( dbgetv ( dbsub, 0, "wfdisc", &wfddb, 0 ) == dbINVALID )   {
               elog_complain( 1,"Can't get wfddb\n" );
               break;
          } 
          if ( ( wfdrec = dbget ( wfddb, rec )) == dbINVALID )   {
             elog_complain( 1,"Can't get dbsub record #%d\n", wfddb.record );
             break;
          }

          db.record = dbaddnull( db ); 
          dbput ( db, rec );
          
          dbgetv( db, 0,
	          "dir", odir, 0 );
		  
	  pathfrname( fname, newdir );
          if( odir[0] != '/' )  {
	     if( strlen( newdir ) > 0 )  {
	        strcat( newdir, "/" );
	        strcat( newdir, odir );
                if( Find_path( newdir, &str ) < 0 ) {
                    elog_complain(0, "can't find a data path for record #%d\n",
                                  dbsub.record);
                    continue;
                }
                dbputv( db, 0, "dir", str, 0 );
	     }
	  }

       }

      if( !Network && gotone )  {

          tmpaf = dblookup (tmpdb, 0, "affiliation", 0, 0);
          if (tmpaf.table < 0)  {
              unlink( ".tmp" ); 
              elog_die(0, "Can't open affiliation table '%s'. Use '-n' option.\n", tmpdb );
          }
          dbquery (tmpaf, dbRECORD_COUNT, &nrec);
          if( nrec <= 0 )
              elog_die( 0, "No records in affiliation table. Use '-n' option.\n");
      
	  for( tmpaf.record = 0; tmpaf.record < nrec; tmpaf.record++ )  {
  
             if ( ( wfdrec = dbget ( tmpaf, rec )) == dbINVALID )   {
                elog_complain( 1,"Can't get affiliation record #%d\n", tmpaf.record );
                break;
             }

             dbaf.record = dbaddnull( dbaf ); 
             dbput ( dbaf, rec );
          
          }
          db = dbjoin( dbaf, db, 0, 0, 0, 0, 0);
          dbquery (db, dbRECORD_COUNT, &nrec);
          if( nrec <= 0 )
              elog_die( 0, "No records in wfdisc&affiliation join.\n");
	  affil = 1;
      }
   }
   unlink( ".tmp" ); 

   if( gotone )  {
      dbquery (db, dbRECORD_COUNT, &nrec);
      if( nrec <= 0 ) {
         elog_complain( 0, "No records in dbin.\n");
      }  else  {
         sort_sta_ch_tm = strtbl("sta", "chan", "time", 0 ) ;
         db = dbsort (db, sort_sta_ch_tm, 0, 0 ) ; 

         if(!mkfname( &out_dbase, stime, out_dir))  {
              elog_die( 1, " can't make outdb\n");
         } 
   
         if (dbopen_database ( out_dbase, "r+", &dbout) < 0)
              elog_die(1, "Can't open output database %s\n", out_dbase);
         if (dbout.table < 0) {
              dbout = dblookup (dbout, 0, "wfdisc", 0, 0);
              if (dbout.table < 0)
                  elog_die(1, "Can't open wfdisc table '%s'\n", out_dbase);
         }
 

         Seq = 1;
         if(!get_data( stime, etime, nrec ))  {
             elog_complain(0, "error in get_data()\n");
             system(CLEAN); 
         }
      }

      if( affil)
        if( unlink( afname ) < 0 )  
           elog_complain( 1, "can't remove %s\n", afname);
      if( unlink( in_dbase ) < 0 )  
           elog_complain( 1, "can't remove %s\n", in_dbase);
   }

  exit(0);
}

get_wfd_name(dbname, wfd_name)
char *dbname;
char wfd_name[256];
{

   Dbptr db;
   Dbvalue dbv;
   struct stat buf;
   char name[256];

   if (dbopen (dbname, "r", &db) == dbINVALID) {
      elog_complain(0, "Unable to open database %s.\n",
                                                 dbname);
      elog_complain(0, "Trying wfdisc name %s.wfdisc\n", dbname);
      sprintf(name, "%s.wfdisc\0", dbname);
      if(stat(name, &buf) == 0)  {
         strcpy(wfd_name, name);
      }  else
         elog_die( 1, "The %s doesn't exist.\n", name); 
         
    }  else {
       db = dblookup (db, 0, "wfdisc", 0, 0);
       dbquery (db, dbTABLE_FILENAME , &dbv);
       strcpy(wfd_name, dbv.t);
    } 
}
