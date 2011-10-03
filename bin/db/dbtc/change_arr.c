 
int change_arr( int dbrec, int arec )
{
    double stime, ctime, tcorr;
    double prev_tcor=0.0;
    char newname[512], key[32];
    char sta[8], chan[12], net[12];
    Dbptr dbout;
 
     
    if (dbopen_database ( NEW, "r+", &dbout ) == dbINVALID )
         elog_die(0, "Can't open database %s\n",  NEW );
 
    dbout = dblookup (dbout, 0, "arrival", 0, 0);
    if (dbout.table < 0)  {
        elog_die(0, "Can't open wfdisc table '%s'\n", dbname );
    }
    
    for( dba.record = 0, dbout.record=0; dba.record < arec; dba.record++, dbout.record++)   {
 
       if( (dbgetv( dba, 0, "time", &stime, 0 )) == dbINVALID )  
           elog_die( 0, "dbgetv faild for record #%d\n", dba.record );  
       
       for( dbc.record = 0; dbc.record < dbrec; dbc.record++) {
	            
	    if( (dbgetv( db, 0,
	         "time", &ctime,
	         "tcorr", &tcorr,
	         "bchan", chan,
	         "bnet", net,
	         "bsta", sta, 0 )) == dbINVALID )
	          elog_die( 0, "dbgetv faild for record #%d\n", dbc.record );
	        
	    sprintf( key, "%s_%s_%s\0", net, sta, chan );

	    if( !regexec( &srcmatch, key, (size_t) 0, NULL, 0 ) )  {
	          if( prev_tcor == 0.0 )  {
		    prev_tcor = tcor;
		  }
	          if( stime > ctime )  {
			dbout = dbaddnull( dbout );
			dbputv( dbout, 0, "time", stime + prev_tcor, 0);
			break;
		  }
	          if( stime == ctime )  {
			dbout = dbaddnull( dbout );
			dbputv( dbout, 0, "time", stime + tcorr, 0);
			break;
		  }
		  prev_time = ctime;
		  prev_tcor = tcorr;
	    }
       }
    }
  
    dbclose(dbout);
  
  return 1;
}

