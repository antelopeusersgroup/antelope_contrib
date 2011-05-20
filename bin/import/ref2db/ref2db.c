/************************************************************************
  *
  * 
  *
  *
************************************************************************/
#include "ref2db.h"       
#include <regex.h>

Arr *Dases;	    /* selected Dases  */
struct PsclDk PsclDk;
int Pblcks;
FILE *timerr;

extern int PsclLog;
int VLog=0;
int Log=0;
  
Tbl		*DasList=0;

char *new_das( char *name ) {
    char *new;
 
    new=strdup( name );
    return new;
}

void usage ()
{
    fprintf (stderr, 
        "Usage: %s [-d datatype] [-e] [-l] [-m das1,das2,das3] [-n net] [-p pfile] [-s size] [-v][-w] iput dbname [start-time [end_time]] \n", 
         Program_Name);
    exit (1);
}

main(argc, argv)
int argc;
char *argv[];
{
  extern char   *optarg;
  extern int    optind;
  int    	i, dasnum, code, dev_type;
  Dbptr         db;
  RunArg        arg;
  SpecPar       par;
  char          *match=0, *st_str, *et_str;
  char 		*dasname, *tmp_name;
  char 		*pfile="ref2db";
  Pf		*pf;
  Tbl 		*daslist;

   Program_Name = argv[0];
   elog_init (argc, argv) ;
   elog_notify (0, "%s version 1.0\n", argv[0]) ;
  
   par.segsiz = 86400.0;
   par.byevent = 0;
   par.wfname = 0;
   strcpy (par.datatype, "sd");
   strcpy(par.network, "PS");
   
   arg.stime = 0.0;
   arg.etime = BIG_NUMBER;
   arg.ifp = -1;
   arg.nodata = 0;
   arg.select = 0;
   
   Dases = 0;
   
  /* Set command line parameters default values  */
 
  while ( ( i = getopt (argc, argv, "d:elm:n:p:s:vw")) != -1)
        switch (i) {
	case 'd':
	    strncpy (par.datatype, optarg, 3);
	    break;

	case 'e':
	    par.byevent = 1;
	    break;

        case 'l':
            Log = 1;
            break;
	
        case 'm':
	    match = strdup(optarg);
            arg.select = 1;
            break;

	case 'n':
            strcpy (par.network, optarg );
	    break;

	case 'p':
            pfile = strdup( optarg );
	    break;

	case 's':
            par.segsiz = atoi( optarg );
	    break;

        case 'v':
            VLog = 1;
            break;
        case 'w':
            arg.nodata = 1;
            break;
	
        default: 
            usage();
        }
      
  if( par.byevent ) par.segsiz = 0.0;

  if ( argc - optind < 2 || argc - optind > 4)
      usage ();
         
  strcpy(arg.iport, argv[optind++]);
  strcpy( arg.dbname,  argv[optind++] ); 
  if (argc - optind >= 1) {
        st_str = argv[optind++] ; 
        arg.stime = str2epoch (st_str);
        if ( argc - optind == 1 ) {
            et_str = argv[optind++] ;
            arg.etime = str2epoch ( et_str ) ; 
        }
  }
  if( pfread ( pfile, &pf) != 0 )
      elog_die(0, "Can't read parameter file %s\n", pfile );

  daslist = pfget_tbl( pf, "Site" );
  dasnum = maxtbl( daslist );
  if( dasnum <= 0 ) DasList = 0; 
  else init_daslist( daslist, dasnum ); 

  par.datacode = trdatacode (par.datatype);

  if (dbopen_database ( arg.dbname, "r+", &db) < 0)
	elog_die(0, "Can't open output database %s\n",  arg.dbname);
  if (db.table < 0) {
	db = dblookup (db, 0, "wfdisc", 0, 0);
	if (db.table < 0)
	    elog_die(0, "Can't open output table '%s'\n",  arg.dbname);
  }
  par.db = db ;

  if( match )  {
      Dases = newarr(0);
      if( (dasname = strtok( match, "," )) != 0 )  {
           tmp_name = new_das(dasname);
           setarr( Dases, tmp_name, tmp_name );
      }
      while( (dasname = strtok( NULL, "," )) != 0 )  {
           tmp_name = new_das(dasname);
           setarr( Dases, tmp_name, tmp_name );
      }

  }
  if( (arg.dev_type = open_IN_ports( &arg ) ) > 0 )   {
       read_input( &par, &arg );
  }    else 
       elog_die( 0, "can't open iport %\n", arg.iport );


}

