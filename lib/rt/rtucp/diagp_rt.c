#include "diagp.h"

static int Started = 0;
static int AnzaStop=1;
static int DasStop = 1;
static int GainStop = 1;
static int DcStop = 1;
static int AnzaStoped=1;
static int DasStoped = 1;
static int DcStoped = 1;
static int GainStoped = 1;
static TclOrb *anzatcl = 0;
static TclOrb *dastcl = 0;
static TclOrb *gaintcl = 0;
static TclOrb *dctcl = 0;
int StopBeep = 0;
Arr *Das_ID_toName = 0;
Arr *Staname=0;
Arr *DPOrbs=0;
Arr *DP=0;
Arr *BatRec = 0;
extern Arr *pfnames;
regex_t dasmatch;
regex_t dcmatch;


static int 
dp_tclCmd _ANSI_ARGS_((ClientData clientData, Tcl_Interp *, int argc, char **argv));

/*
 *--------------------------------------------------------------
 *
 * dprtCmd --
 *
 *	This procedure is invoked to process the "dpv" Tcl
 *	command.
 *
 * Results:
 *	A standard Tcl result.
 *
 *--------------------------------------------------------------
 */

int dpadmCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window associated with
				 * interpreter. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{

    char *orb;

    if (argc < 3) {
	Tcl_AppendResult(interp, "wrong # args:  should be \"", argv[0], " cmd cmdpar ", (char *) NULL);
	return TCL_ERROR;
    }

    if( strncmp( argv[1], "pfclean", strlen("pfclean") ) == 0 )  {
       Tcl_SetResult ( interp, "pfclean not supported", TCL_STATIC ) ; 
       return TCL_ERROR ; 
       /* if( pfnames != 0 ) freearr( pfnames,0 );
       pfnames = 0; */
    } else if( strncmp( argv[1], "dcalarm", strlen("dcalarm") ) == 0 )  {
       orb = strdup( argv[2] );

    } else {
	Tcl_AppendResult(interp, "unknown command - ", argv[1], (char *) NULL);
	return TCL_ERROR;

    } 
    return TCL_OK;
}
/*
 *--------------------------------------------------------------
 *
 * dprtCmd --
 *
 *	This procedure is invoked to process the "dpv" Tcl
 *	command.
 *
 * Results:
 *	A standard Tcl result.
 *
 *--------------------------------------------------------------
 */

int dprtCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window associated with
				 * interpreter. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    int pktid, orb;
    TclOrb *tclorb=0;
    char *str, *cmd = 0, *orbname=0;

    if (argc < 3) {
	Tcl_AppendResult(interp, "wrong # args:  should be \"",
		argv[0], " tclcmd orbname ", (char *) NULL);
	return TCL_ERROR;
    }

    /* Open orb */

    orbname = strdup(argv[2]);
    orb = orbopen ( orbname, "r&" );
    if ( orb < 0) {
  	elog_clear_register(1);
	Tcl_AppendResult(interp, argv[0], ": orbopen() error on ", orbname, (char *) NULL);
	return TCL_ERROR;
    }
                
    if( ( pktid = orbseek (orb, ORBNEWEST) ) < 0 )  {
   	elog_clear_register(1);
	Tcl_AppendResult(interp, argv[0], ": orbseek() ORBNEWEST error", (char *) NULL);
	return TCL_ERROR;
    }
            
    /* Set name assosiated with specific orb   */

    cmd = strdup(argv[1]);
    if ( DPOrbs == 0 ) {
    	DPOrbs = newarr (0);
	if (DPOrbs == NULL) {
		Tcl_AppendResult(interp, "newarr() error.", (char *) NULL);
		return TCL_ERROR;
	}
    }
    str = getarr ( DPOrbs, cmd );
    if ( str != 0 ) {
	Tcl_AppendResult(interp, "tclorb ", cmd, " already in use.", (char *) NULL);
	return TCL_ERROR;
    }
/*printf( "set tcl %s for %s\n", cmd, orbname );  */

    tclorb = (TclOrb * ) ckalloc( sizeof(TclOrb) );

    tclorb->lcmd_time = 0.0;
    tclorb->pktid = pktid;
    tclorb->orbid = orb;
    strcpy (tclorb->cmdname, cmd );
    strcpy (tclorb->orbname, orbname );
    tclorb->verbose = 0;
    tclorb->dpars = 0;
    tclorb->parcallback	= 0;
    tclorb->alarmcall = 0;
    tclorb->balarmcall = 0;
    tclorb->lcmdcall = 0;
    tclorb->interp = interp;
    tclorb->lcmdwidget[0] = '\0';
    tclorb->timwidget[0] = '\0';
    tclorb->clcsel[0] = '\0';

    /* instance command. */

    Tcl_CreateCommand(interp, cmd, dp_tclCmd, (ClientData) tclorb, (void (*)()) NULL);

    setarr ( DPOrbs, cmd, cmd );

    return TCL_OK;
}
/*
 *--------------------------------------------------------------
 *
 * dcrtCmd --
 *
 *	This procedure is invoked to process the "dpv" Tcl
 *	command.
 *
 * Results:
 *	A standard Tcl result.
 *
 *--------------------------------------------------------------
 */

int dcrtCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window associated with
				 * interpreter. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    int orb, pktid;
    TclOrb *tclorb=0;
    char *str, *cmd=0, *orbname=0;

    if (argc < 3) {
	Tcl_AppendResult(interp, "wrong # args:  should be \"", argv[0], " tclcmd orbname ", (char *) NULL);
	return TCL_ERROR;
    }

    /* Open orb */

    orbname = strdup( argv[2]);
    orb = orbopen ( orbname, "r&" );
    if (orb < 0) {
  	elog_clear_register(1);
	Tcl_AppendResult(interp, argv[0], ": orbopen() error on ", orbname, (char *) NULL);
	return TCL_ERROR;
    }
                
    if( (pktid = orbseek (orb, ORBNEWEST)) < 0 )  {
   	elog_clear_register(1);
	Tcl_AppendResult(interp, argv[0], ": orbseek() ORBNEWEST error", (char *) NULL);
	return TCL_ERROR;
    }
            
    /* Set name assosiated with specific orb   */

    cmd = strdup(argv[1]);
    if ( DPOrbs == 0 ) {
    	DPOrbs = newarr (0);
	if ( DPOrbs == 0 ) {
		Tcl_AppendResult(interp, "newarr() error.", (char *) NULL);
		return TCL_ERROR;
	}
    }
    str = getarr ( DPOrbs, cmd );
    if ( str != 0 ) {
	Tcl_AppendResult(interp, "tclorb ", cmd, " already in use.", (char *) NULL);
	return TCL_ERROR;
    }

    tclorb = (TclOrb * ) ckalloc( sizeof(TclOrb) );
    
    tclorb->orbid = orb;
    tclorb->pktid = pktid;
    strcpy (tclorb->cmdname, cmd );
    strcpy (tclorb->orbname, orbname);
    tclorb->verbose	= 0;
    tclorb->dpars	= NULL;
    tclorb->parcallback	= 0;
    tclorb->alarmcall	= 0;
    tclorb->balarmcall	= 0;
    tclorb->lcmd_time	= 0.0;
    tclorb->lcmdcall	= 0;
    tclorb->interp	= interp;
    tclorb->timwidget[0] = '\0';
    tclorb->lcmdwidget[0] = '\0';
    tclorb->clcsel[0] = '\0';

    /* instance command. */

    Tcl_CreateCommand(interp, cmd, dp_tclCmd, (ClientData) tclorb, (void (*)()) NULL);

    setarr ( DPOrbs, cmd, cmd );

    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * dp_tclCmd --
 *
 *	This procedure is invoked to process the Tcl command
 *	that corresponds to a widget managed by this module.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */

static int
dp_tclCmd(clientData, interp, argc, argv)
    ClientData clientData;		/* Information about orb widget. */
    Tcl_Interp *interp;			/* Current interpreter. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    TclOrb *tclorb = (TclOrb *) clientData;
    DPars *dp;
    int result = TCL_OK;
    double after;
    char *stanam, *orbname, *str;
    char *command, *newname;
    uchar_t partype;
    char string[2048], 
         widget[256],
         daspkt[64], dcpkt[64],
         srcname[ORBSRCNAME_SIZE];
    int i, val, n;
    double time;
    Tbl *Ste=0;
    struct Site site;
    Pf *pf;
    int wantgain=0, dasnum;


    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " option ?arg arg ...?\"", (char *) NULL);
	return TCL_ERROR;
    }
    orbname = argv[0];
    command = argv[1];
    argv += 2;
    argc -= 2;

    if (!strcmp(command, "clear")) {
    	 if (argc < 2) {
    		Tcl_AppendResult(interp, "wrong # args: should be \"",
    			argv[0], " clear cmdname\"", (char *) NULL);
		return TCL_ERROR;
	 }
         str = (char *) *argv;
         delarr(DPOrbs, str );
	 free(tclorb);
	 if( !Tcl_DeleteCommand( interp, str )) return TCL_OK;
	 else return TCL_ERROR;

    }  else if (!strcmp(command, "setid")) {
    
    	 if (argc < 2) {
    		Tcl_AppendResult(interp, "wrong # args: should be \"",
    			argv[0], " setid dasname dasid\"", (char *) NULL);
		return TCL_ERROR;
	 }
         str = (char *) *argv;
         argv++;
         strcpy( srcname, str );
         val = atoi(*argv);
         val--;
         if( val < 0 || val >= 16 )  {
    		Tcl_AppendResult(interp, "wrong dasid # ", (char *) NULL);
		return TCL_ERROR;
	 }
         argv++;
         if( Staname == 0 ) Staname = newarr(0);
         newname = strdup( srcname);
         sprintf( srcname, "%d\0", val );

         setarr( Staname, srcname, newname );

         return TCL_OK;   
    } else  if (!strcmp(command, "orbsel")) {
    
    	 if (argc < 1) {
    		Tcl_AppendResult(interp, "wrong # args: should be \"",
    			argv[0], " orbsel srcmatch\"", (char *) NULL);
		return TCL_ERROR;
	 }
         str = (char *) *argv;
         argv++;
         strcpy( srcname, str );
         if( strlen(srcname) <= 0 ) return TCL_OK;


         if( orbselect (tclorb->orbid, srcname ) < 0) {
             elog_clear_register(1);
             Tcl_AppendResult(interp, argv[0], ": orbselect() error", (char *) NULL);
             return TCL_ERROR;
         }
         return TCL_OK;   
          
    } else  if (!strcmp(command, "select")) {
    
    	 if (argc < 2) {
    		Tcl_AppendResult(interp, "wrong # args: should be \"",
    			argv[0], " select dcsel dassel\"", (char *) NULL);
		return TCL_ERROR;
	 }
         str = (char *) *argv;
         argv++;
         strcpy( dcpkt, str );
    	 if( strlen( dcpkt ) <= 0 ) sprintf( dcpkt, ".*\0");
         
         str = (char *) *argv;
         argv++;
         strcpy( daspkt, str );
    	 if( strlen( daspkt ) <= 0 ) sprintf( daspkt, ".*\0");

         if ( regcomp(&dasmatch, &daspkt[0], REG_EXTENDED|REG_NOSUB) != 0)   {
	    Tcl_AppendResult(interp, "regcomp error for CALS\n", (char *) NULL); 
	    return TCL_ERROR;
         }
									            
	 if (regcomp(&dcmatch, &dcpkt[0], REG_EXTENDED|REG_NOSUB) != 0)  {
	    Tcl_AppendResult(interp, "regcomp error for ASP\n", (char *) NULL); 
	    return TCL_ERROR;
        }
 
        return TCL_OK;                     
    } else if (!strcmp(command, "getdaspar")) {
   
    	if (argc < 2) {
    	      Tcl_AppendResult(interp, "wrong # args: should be \"",
                               argv[0], " getdaspar pfname wantgain \"", (char *) NULL);
	      return TCL_ERROR;
	 }
         str = (char *) *argv;
	 argv++;
	 wantgain = atoi(*argv);
       
         Das_ID_toName = newarr(0);
        
         if(pfread( str, &pf) != 0)  {
    	      Tcl_AppendResult(interp, "can't read parameter file ", str, (char *) NULL);
	      return TCL_ERROR;
	 }
                            
         Ste = pfget_tbl( pf, "Site" );
         dasnum = maxtbl( Ste );
         if( dasnum <= 0 )  {
    	      Tcl_AppendResult(interp, "can't get Site table ", (char *) NULL);
	      return TCL_ERROR;
	 }
         for( i = 0; i < dasnum; i++)  {
              str = (char *) gettbl(Ste, i);
              sscanf(str, STE_SCS,  STE_RVL(&site));
              stanam = strdup( site.name );
              sprintf( srcname, "%d\0", site.sid );
              setarr( Das_ID_toName, srcname, stanam );
        }
	if( wantgain )  {
    	    GainStop = 0;
            gaintcl = tclorb;
    	}  else  {
	    DasStop = 0;
            dastcl = tclorb;
        }
        if( !Started ) GetPar();
        return TCL_OK;                     

    }  else if (!strcmp(command, "getdcpar")) {
    	DcStop = 0;
         if( orbselect (tclorb->orbid, ".*BSP" ) < 0) {
             elog_clear_register(1);
             Tcl_AppendResult(interp, argv[0], ": orbselect() error", (char *) NULL);
             return TCL_ERROR;
         }
        dctcl = tclorb;
        if( !Started ) GetPar();
        return TCL_OK;                     
    }  else if (!strcmp(command, "getanzapar")) {
    	AnzaStop = 0;
        anzatcl = tclorb;
        if( !Started ) GetPar();
        return TCL_OK;                     

    } else if (!strcmp(command, "stop")) {

        strcpy( string, tclorb->cmdname );

        if( strncmp( string, "anza", 4 ) == 0 ) { AnzaStoped = 0; AnzaStop = 1; }
        if( strncmp( string, "das", 3 ) == 0 ) { DasStoped = 0;  DasStop = 1; }
        if( strncmp( string, "dc", 2 ) == 0 ) { DcStoped = 0; DcStop = 1; }
        if( strncmp( string, "gain", 4 ) == 0 ) { GainStoped = 0; GainStop = 1; }
        return TCL_OK;                     
    } else if (!strcmp(command, "beep")) {
    	if (argc < 1) {
    		Tcl_AppendResult(interp, "wrong # args: should be \"",
    			argv[0], " beep 1|2\"", (char *) NULL);
		return TCL_ERROR;
	}
        val = atoi(*argv);
        val = val != 0 ? 0:1; 

	StopBeep = val;
        return TCL_OK;                     
    } else if (!strcmp(command, "after")) {
    	if (argc < 1) {
    		Tcl_AppendResult(interp, "wrong # args: should be \"",
    			argv[0], " after tim_string\"", (char *) NULL);
		return TCL_ERROR;
	}
	str = (char *) strdup (*argv);
        after = str2epoch (str);
        if( ( tclorb->pktid = orbafter (tclorb->orbid, after-0.001 )) < 0) {
   	   elog_clear_register(1);
	   Tcl_AppendResult(interp, argv[0], ": orbafter() ", str, (char *) NULL);
	   return TCL_ERROR;
        }
        return TCL_OK;                     
                
    } else if (!strcmp(command, "alarm")) {
    	if (argc < 1) {
    		Tcl_AppendResult(interp, "wrong # args: should be \"",
    			argv[0], " alarm function\"", (char *) NULL);
		return TCL_ERROR;
	}
	tclorb->alarmcall = (char *) strdup (*argv);
        return TCL_OK;                     
    } else if (!strcmp(command, "balarm")) {
    	if (argc < 1) {
    		Tcl_AppendResult(interp, "wrong # args: should be \"",
    			argv[0], " alarm function\"", (char *) NULL);
		return TCL_ERROR;
	}
	tclorb->balarmcall = (char *) strdup (*argv);
        return TCL_OK;                     
    } else if (!strcmp(command, "parcallback")) {
    	if (argc < 1) {
    		Tcl_AppendResult(interp, "wrong # args: should be \"",
    			argv[0], " parcallback function\"", (char *) NULL);
		return TCL_ERROR;
	}
	tclorb->parcallback = (char *) strdup (*argv);
        return TCL_OK;                     
    } else if (!strcmp(command, "verbose")) {
    	tclorb->verbose = 1;
        return TCL_OK;                     
    } else if (!strcmp(command, "add")) {
    	 if (argc < 1) {
    		Tcl_AppendResult(interp, "wrong # args: should be \"",
    			argv[0], " add dpar_name\"", (char *) NULL);
		return TCL_ERROR;
	 }
         newname = strdup( *argv);
         argv++;
 
         if ( DP == 0 ) {
              DP = newarr(0);
              if ( DP == 0 ) { 
                   Tcl_AppendResult(interp, "newarr() error", (char *) NULL);
                   return TCL_ERROR;
              }
         }
         str =  getarr ( DP, newname );

         if ( str != 0 && *str != 0 ) {
              Tcl_AppendResult(interp, "add ", newname , " already in use.", (char *) NULL);
              return TCL_ERROR;
         }
         if ( tclorb->dpars == 0) {
              tclorb->dpars = newarr (0);
              if ( tclorb->dpars == NULL) {
                    Tcl_AppendResult(interp, "newarr() error", (char *) NULL);
                    return TCL_ERROR;
              }
         }
 
         dp = ( DPars *) ckalloc( sizeof( DPars) );  
         strcpy(dp->dpar, newname);
         dp->ptime = 0.0;
         dp->last_lock = 0.0;
         dp->crnt_val = 0.0;
         dp->alert = 0;
         dp->min_val = 0.0;
         dp->max_val = -99999.0;
         dp->widget[0] = '\0'; 
         dp->minwidget[0] = '\0'; 
         dp->maxwidget[0] = '\0'; 
         setarr ( tclorb->dpars, newname, dp);
         setarr ( DP, newname, newname);
          
           return TCL_OK;                     
 
     } else if (!strcmp(command, "lcmd")) {
    	 if (argc < 2) {
    		Tcl_AppendResult(interp, "wrong # args: should be \"",
    			argv[0], " lcmd callback widget\"", (char *) NULL);
		return TCL_ERROR;
	 }
	 tclorb->lcmdcall = (char *) strdup (*argv++);
         
         str = (char *) *argv++;
         strcpy( srcname, str);
         strcpy( tclorb->lcmdwidget, srcname );
         
        
         return TCL_OK;                     
         
     } else if (!strcmp(command, "timwidget")) {
    	 if (argc < 1) {
    		Tcl_AppendResult(interp, "wrong # args: should be \"",
    			argv[0], " timwidget widget\"", (char *) NULL);
		return TCL_ERROR;
	 }
         str = (char *) *argv;
         strcpy( srcname, str);
         strcpy( tclorb->timwidget, srcname );
        
         return TCL_OK;                     
         
     } else if (!strcmp(command, "widget")) {
    	 if (argc < 4) {
    		Tcl_AppendResult(interp, "wrong # args: should be \"",
    			argv[0], " widget dpar minwidget widget maxwidget\"", (char *) NULL);
		return TCL_ERROR;
	 }
         str = (char *) *argv;
         argv++;
         strcpy( srcname, str );
          
         if ( tclorb->dpars == 0 || DP == 0 ) {
    		Tcl_AppendResult(interp, "wrong # args: should be \"",
    			argv[0], " tclorb or/and DP are empty \"", (char *) NULL);
                return TCL_ERROR;
         }
         dp = (DPars * ) getarr ( tclorb->dpars, srcname );
 
         if ( dp == 0 ) {
    		Tcl_AppendResult(interp, srcname, " doesn't exist in DP \"", (char *) NULL);
                return TCL_ERROR;
         }
         str = (char *) *argv;
         argv++;
         strcpy( dp->minwidget, str);
         str = (char *) *argv;
         argv++;
         strcpy( dp->widget, str);
         str = (char *) *argv;
         argv++;
         strcpy( dp->maxwidget, str);
         setarr ( tclorb->dpars, srcname, dp);
         return TCL_OK;                     
         
    } else {
	Tcl_AppendResult(interp, "Unrecognized orb command \"", 
		command, "\"", (char *) NULL);
	return TCL_ERROR;
    }

}

int GetPar( )

{
    double apkttime, ppkttime, dpkttime;
    int i;
    int anbytes, abufsize;
    int pnbytes, pbufsize;
    int dnbytes, dbufsize;
    int dc, das;
    char akey[64], pkey[64], dkey[64];
    char cmdstr1[256]; 
    char *apacket, *ppacket, *dpacket, *tim;
    int done = 0;
    ushort_t pkttype;

    Started = 1;


        apacket = ppacket = dpacket = 0; abufsize = pbufsize = dbufsize = 0;
    while ( 1 )  {


        if ( !AnzaStop )  {

            dc = das = 1;
            if ( !orbreap (anzatcl->orbid, &anzatcl->pktid, akey, &apkttime, 
                 &apacket, &anbytes, &abufsize) )  {
                 dc = regexec( &dcmatch, akey, (size_t) 0, NULL, 0 ) ;  
                 das = regexec( &dasmatch, akey, (size_t) 0, NULL, 0 ) ;
	         while ( Tk_DoOneEvent (TK_DONT_WAIT) )  
	             ;  
                 if( !dc || !das ) { 
        
                     if( anzatcl->parcallback != NULL && !dc )  {
                         sprintf ( &cmdstr1[0], "%s {%s} {%s} {none} \0", 
                         anzatcl->parcallback, anzatcl->timwidget, tim=strtime(apkttime) );
                         free(tim);
                         if (anzatcl->verbose > 0) {
	                      printf ("%s\n", cmdstr1 );
	                      fflush (stdout);
                         }
                         Tcl_Eval (anzatcl->interp, cmdstr1 );
                  }
        
                  pkttype = *((ushort_t *) ( apacket + 6));
                  pkttype = (ushort_t ) ntohs( pkttype );
                  switch ( pkttype )  {

                       case CAHS:
                       case CALS:
                           if( !AnzaDas( anzatcl , akey, apkttime, apacket)) return 0;
                           break;
                       case ASP:
                           if( !AnzaDc( anzatcl, akey, apkttime, apacket )) return 0;
                           break;
                       default: break; 

                   }
                }
             } 
         }
                      
         if ( !GainStop )  {
            if ( !orbreap ( gaintcl->orbid, &gaintcl->pktid, pkey, &ppkttime, 
                 &ppacket, &pnbytes, &pbufsize) )  {

	         while ( Tk_DoOneEvent (TK_DONT_WAIT) )  
	              ;  
        
                 if( gaintcl->parcallback != NULL )  {
                     sprintf ( &cmdstr1[0], "%s {%s} {%s} {none} \0", 
                     gaintcl->parcallback, gaintcl->timwidget, tim=strtime(ppkttime) );
                     free(tim);
                     if (gaintcl->verbose > 0) {
                        printf ("%s\n", cmdstr1 );
                        fflush (stdout);
                     }
                     Tcl_Eval (gaintcl->interp, cmdstr1 );
                 }
        
                 pkttype = *((ushort_t *) ( ppacket + 6));
                 pkttype = (ushort_t ) ntohs( pkttype );
                 switch ( pkttype )  {
                     case CBBHS:
                     case CBBLS:
                     case CBB1S:
                        if( !DAS_gain( gaintcl, pkey, ppkttime, ppacket )) return 0;
                              break;
       
                     case BSP:
                     default:  break;
                 }
             } 
         }
                      
         if ( !DasStop )  {
            dc = das = 1;
            if ( !orbreap ( dastcl->orbid, &dastcl->pktid, pkey, &ppkttime, 
                 &ppacket, &pnbytes, &pbufsize) )  {
                 dc = regexec( &dcmatch, pkey, (size_t) 0, NULL, 0 ) ;  
                 das = regexec( &dasmatch, pkey, (size_t) 0, NULL, 0 ) ;
	         while ( Tk_DoOneEvent (TK_DONT_WAIT) )  
	              ;  
                 if( !dc || !das )  { 
        
                     if( dastcl->parcallback != NULL && !dc )  {
                          sprintf ( &cmdstr1[0], "%s {%s} {%s} {none} \0", 
                          dastcl->parcallback, dastcl->timwidget, tim=strtime(ppkttime) );
                          free(tim);
                          if (dastcl->verbose > 0) {
	                      printf ("%s\n", cmdstr1 );
	                      fflush (stdout);
                          }
                          Tcl_Eval (dastcl->interp, cmdstr1 );
                      }
        
                      pkttype = *((ushort_t *) ( ppacket + 6));
                      pkttype = (ushort_t ) ntohs( pkttype );
                      switch ( pkttype )  {
                           case CBBHS:
                           case CBBLS:
                           case CBB1S:
                              if( !BBA_DasPar( dastcl, pkey, ppkttime, ppacket )) return 0;
                              break;
       
                           case BSP:
                              if( !BBA_DcDas( dastcl, pkey, ppkttime, ppacket )) return 0;
                              if( !BBA_DcDc( dastcl, pkey, ppkttime, ppacket )) return 0;
                              break;
                           default:  break;
                      }
                 }
             } 
         }
                      
         if ( !DcStop )  {

             dc = das = 1;
             if ( !orbreap (dctcl->orbid, &dctcl->pktid, dkey, &dpkttime, 
                  &dpacket, &dnbytes, &dbufsize) )  {
              dc = regexec( &dcmatch, dkey, (size_t) 0, NULL, 0 ) ;  
                  das = regexec( &dasmatch, dkey, (size_t) 0, NULL, 0 ) ;
	          while ( Tk_DoOneEvent (TK_DONT_WAIT) )  
	               ;  
                  if( !dc || !das ) { 
                       if( dctcl->parcallback != NULL && !dc )  {
                           sprintf ( &cmdstr1[0], "%s {%s} {%s} {none} \0", 
                           dctcl->parcallback, dctcl->timwidget, tim=strtime(dpkttime) );
                           free(tim);
                           if (dctcl->verbose > 0) {
	                        printf ("%s\n", cmdstr1 );
	                        fflush (stdout);
                           }
                           Tcl_Eval (dctcl->interp, cmdstr1 );
                       }
        
                       pkttype = *((ushort_t *) ( dpacket + 6));
                       pkttype = (ushort_t ) ntohs( pkttype );
                       switch ( pkttype )  {
                          case BSP:
                             if( !BBA_Dc( dctcl, dkey, dpkttime, dpacket )) return 0;
                             if( !BBA_DcPar( dctcl , dkey, dpkttime, dpacket)) return 0;
                             break;
 
                          default: break;
                       }
                   }
              } 
          }
                 
          if( DcStop && !DcStoped ) { 
              stop_clean( dctcl ); 
              DcStoped = 1;
          }
          if( AnzaStop && !AnzaStoped ) { 
              stop_clean( anzatcl ); 
              AnzaStoped = 1; 
          }
          if( DasStop && !DasStoped ) { 
              stop_clean( dastcl ); 
              DasStoped = 1 ;
          }
          if( GainStop && !GainStoped ) { 
              stop_clean( gaintcl ); 
              GainStoped = 1 ;
          }
                      
          if( AnzaStop && DasStop && DcStop && GainStop ) {
             Started = 0;
             return 1; 
          }
          fflush(stdout);
     }
 
}

int stop_clean( clientData )
ClientData clientData;
 
{

   TclOrb *tclorb  = (TclOrb *) clientData;
   Tbl *list;
   DPars *dp;
   char *keys, *name;
   int num, i;
   Arr *tmp;
 
  /* printf( "start cleaning \n");   */
 
   if( tclorb->orbid > 0 )  {
        orbclose( tclorb->orbid );
        tclorb->orbid = -1;
   } 
   tmp = tclorb->dpars; 
   list = keysarr( tmp );
   num = maxtbl( list );
   for( i = 0; i < num ; i++ )   {
     keys = gettbl( list, i );
     dp = getarr( tmp, keys );
     ckfree(dp);
     dp = 0;
     delarr( tmp, keys );
     delarr(DP, keys );
  /* printf( "clean tclorb->dpars& DP for %s\n", keys);  */
          fflush(stdout);
   }
   
   freearr( tmp, 0 );
   tclorb->dpars = 0;
   freetbl(list, 0); free(keys);
 
   
   delarr(DPOrbs, tclorb->cmdname );
 /* printf( "clean DPOrbs for %s\n", tclorb->cmdname ); */ 
          fflush(stdout);
   

   return 1;
 
}
 

