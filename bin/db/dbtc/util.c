/*************************************************************************
 *
 * 
 *   utilities          
 *
 *
 *
 ***********************************************************************/
#include "dbtc.h"  
#include "coords.h"
 
void usage ()
{
    fprintf (stderr, "usage: %s [-p pfile] [-t stime] [-v] gdb:gnet:gsta:gch bdb:bnet:bsta:bch\n",Program_Name);
    exit (1);
}
 
/* Parse db:sta:chan  string to get db & station & channel names  */
 
int parse_key(
    char *key,
    Dset *db
    ) 
    
  { 
    char *src;
    char *tmp;
 
    strcpy( db->key, key);
 
    src = strdup( key );
 
    tmp = strtok( src, ":") ;
    if( tmp == 0 ) return 0;
    strcpy( db->dbname, tmp);
    
    tmp = strtok( NULL, ":");
    if( tmp == 0 ) return 0;
    strcpy( db->net, tmp);
    
    tmp = strtok( NULL, ":");
    if( tmp == 0 ) return 0;
    strcpy( db->sta, tmp);
    
    tmp = strtok( NULL, ":" );
    if( tmp == 0 ) return 0;
    strcpy( db->chan, tmp);
 
    return 1;
 }
 
int opendb( Dset *dset )
{
 
   char key[256];
   Tbl *sort_sta_ch_tm;
 
   if (dbopen_database ( dset->dbname, "r+", &(dset->db) ) == dbINVALID )
         elog_die(0, "Can't open database %s\n",  dset->dbname );
 
    dset->db = dblookup ( dset->db, 0, "wfdisc", 0, 0);
    if ( dset->db.table == dbINVALID )
       elog_die(0, "Can't open '%s' wfdisc table.\n", dset->dbname );
 
    /* Select specified sta&chan */
 
    sprintf( key,"(chan =~ /%s/ && sta=~ /%s/)\0", dset->chan, dset->sta );
    dset->db = dbsubset(  dset->db, key, 0 );
    sort_sta_ch_tm = strtbl("sta", "chan", "time", 0 ) ;
    dset->db = dbsort ( dset->db, sort_sta_ch_tm, 0, 0 ) ;
 
    dbquery ( dset->db, dbRECORD_COUNT, &(dset->dbrec) );
    if( dset->dbrec <= 0 )
       elog_die( 0, " no record with sta == %s and chan == %s in %s.\n", 
                 dset->sta, dset->chan, dset->dbname );
 
 
    return 1;
 
}  

int save_tcorr( Dset *bset, Dset *gset, double tcorr)
{

    Dbtc.record = dbaddnull( Dbtc );
    dbputv( Dbtc, 0,
	   "time", bset->stime,
	   "tcor", tcorr,
	   "gnet", gset->net, 
	   "gsta", gset->sta, 
	   "gchan", gset->chan,
	   "gsrate", gset->srate,
	   "bnet", bset->net, 
	   "bsta", bset->sta, 
	   "bchan", bset->chan,
	   "bsrate", bset->srate,
	   "lddate", now(), 0);

    return 1 ;

}
    
 
void namefrpath(path, name)
char *path;
char *name;
{
int i, j;
 
        for(j = 0,i = strlen(path)-1; i >= 0; j++,i --)
             if (path[i] == '/') break;
        strncpy(name, path + i + 1, j);
        name[j] = '\0';
}
 
