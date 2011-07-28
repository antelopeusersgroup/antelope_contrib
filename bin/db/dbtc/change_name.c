/*
 *
 *************************************************************************/
#include "dbtc.h"
 
int change_name( char *dbname )
{
    struct stat stat_buf;
    char newname[512], str[512];


    /* change original wfdisc&arrival tables in tables
       with '-' at the end of a name  */

    sprintf( newname, "%s.wfdisc\0", NEW );
    if( stat( newname, &stat_buf) != 0 ) {
         if(ENOENT) break;
         else   elog_die( 1, "can't stat %s.\n", newname );
    }  else {
       sprintf( str, "mv %s.wfdisc %s_orig.wfdisc\0", dbname, dbname );
       if( system(str) == -1 ) 
           elog_die( 1, "can't save an original wfdisc file - %s.wfdisc\n", dbname );
       sprintf( str, "cp %s %s.wfdisc\0", newname, dbname );
       if( system(str) == -1 ) 
           elog_die( 1, "can't save a new wfdisc file - %s\n", newname );
    }

    sprintf( newname, "%.arrival\0", NEW );
    if( stat( newname, &stat_buf) != 0 ) {
         if(ENOENT) break;
         else   elog_die( 1, "can't stat %s.\n", newname );
    }  else {
       sprintf( str, "mv %s.arrival %s_orig.arrival\0", dbname, dbname );
       if( system(str) == -1 ) 
           elog_die( 1, "can't save an original arrival file - %s.arrival\n", dbname );
       sprintf( str, "cp %s %s.arrival\0", newname, dbname );
       if( system(str) == -1 ) 
           elog_die( 1, "can't save a new arrival file - %s\n", newname );
    }
   
    return 1; 
}

