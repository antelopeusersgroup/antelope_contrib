/*
 *  getutil.c  functions for looking up shared memory rings,
 *             installations, modules, and message types in
 *             Earthworm tables.  Given a character-string name,
 *             these functions return a numerical value.
 *
 *             The installation table is set up in the include-file
 *             earthworm.h; it is global to all Earthworm installations.
 *             Changes to this table require a recompilation of the
 *             whole Earthworm tree.
 *  
 *             The shared memory ring, module, and message type tables
 *             are set up in the ascii table file (whose name is stored 
 *             in the global variable, TableFile) which resides in the 
 *             EW_PARAMS directory.  Changes to this file do not require
 *             recompilation.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "earthworm.h"
#include "kom.h"

/* Table of shared memory ring names & their keys
 ************************************************/
#define MAXRING 20
static struct {
  long   key;
  char   name[28];
} EW_Ring[MAXRING];
static int Max_Ring = 0;      /* # ring/keys currently in table */

/* Table of module names and their module_ids
 ********************************************/
#define MAXMODID 256
static struct {
  unsigned char id;
  char          name[31];
} EW_Module[MAXMODID];
static int Max_ModuleId = 0;  /* # modules currently in table */

/* Table of message names and their type-values
 **********************************************/
#define MAXMSGTYPE 256 
static struct {
  unsigned char type;
  char          name[31];
} EW_Message[MAXMSGTYPE];
static int Max_MessageType = 0;  /* # msg types currently in table */

#define MAXLEN 255
static char  FullTablePath[MAXLEN+1];
static char *TableFile  = "earthworm.d"; 
static int   LoadTables = 1;    /* Do tables need to be loaded from */
                                /* the TableFile?    1=yes 0=no     */
 
         /*******************************************************
          *                      GetKey                         *
          *                                                     *
          *  Convert ring name to key number using table        *
          *  in TableFile                                       *
          *  Return <key number> on success;                    *
          *          -1 if the specified ring name is unknown.  *
          *******************************************************/
long GetKey( char *ringName )
{
   int i;

   if( LoadTables ) {
       GetUtil_LoadTable();
       LoadTables = 0;
   }

/* Find transport ring name in earthworm.d table
 ***********************************************/
   for ( i = 0; i < Max_Ring; i++ )
      if ( strcmp( EW_Ring[i].name, ringName ) == 0 )
         break;

/* Didn't find ring name in table
 ********************************/
   if ( i == Max_Ring )
   {
      fprintf( stderr, "GetKey: Invalid ring name <%s>\n", ringName );
      return( -1 );
   }

/* Found it!
 ***********/
   return( EW_Ring[i].key );
}


         /*******************************************************
          *                   GetLocalInst                      *
          *                                                     *
          *  Convert the local installation name, set by the    *
          *  environment variable EW_INSTALLATION, to a number  *
          *  using the table in earthworm.h                     *
          *  Return  0 on success;                              *
          *         -1 if the environment variable is not       *
          *            defined;                                 *
          *         -2 if the env. variable has no value        *
          *         -3 if the specified installation name is    *
          *            not listed in earthworm.h                *
          *******************************************************/
int GetLocalInst( unsigned char *localId )
{
   char *envInst;

   *localId = 0;

/* Get the installation name from environment variable EW_INSTALLATION
   *******************************************************************/
   envInst = getenv( "EW_INSTALLATION" );

   if ( envInst == (char *) NULL )
   {
      fprintf( stderr,
              "GetLocalInst: Environment variable EW_INSTALLATION not defined.\n" );
      return( -1 );
   }

   if ( *envInst == '\0' )
   {
      fprintf( stderr, "GetLocalInst: Environment variable EW_INSTALLATION" );
      fprintf( stderr, " defined, but has no value.\n" );
      return( -2 );
   }

/* Find the id-value in the table
 ********************************/
   if ( GetInst( envInst, localId ) < 0 )
   {
      fprintf( stderr,
              "GetLocalInst: Environment variable EW_INSTALLATION" );
      fprintf( stderr, " has invalid value <%s>\n", envInst );
      return( -3 );
   }

   return( 0 );
}


         /*******************************************************
          *                      GetInst                        *
          *                                                     *
          *  Convert installation name to number using table    *
          *  in earthworm.h                                     *
          *  Return  0 on success;                              *
          *         -1 if the specified installation name is    *
          *            unknown.                                 *
          *******************************************************/
int GetInst( char *instName, unsigned char *instId )
{
   int i;

/* Find installation name in earthworm.h table
 *********************************************/
   for ( i = 0; i < Max_Installation; i++ )
      if ( strcmp( EW_Installation[i].name, instName ) == 0 )
         break;

/* Didn't find installation name in table
 ****************************************/
   if ( i == Max_Installation )
   {
      fprintf( stderr,
              "GetInst: Invalid installation name <%s>\n", instName );
     *instId = 0;
      return( -1 );
   }

/* Found it!
 ***********/
  *instId = EW_Installation[i].id;
   return( 0 );
}

         /*******************************************************
          *                      GetModId                       *
          *                                                     *
          *  Convert module name to modid number using table    *
          *  defined in TableFile                               *
          *  Return  0 on success;                              *
          *         -1 if the specified module name is unknown. *
          *******************************************************/
int GetModId( char *modName, unsigned char *modId )
{
   int i;

   if( LoadTables ) {
       GetUtil_LoadTable();
       LoadTables = 0;
   }

/* Find module name in earthworm.h table
 ***************************************/
   for ( i = 0; i < Max_ModuleId; i++ )
      if ( strcmp( EW_Module[i].name, modName ) == 0 )
         break;

/* Didn't find module name in table
 **********************************/
   if ( i == Max_ModuleId )
   {
      fprintf( stderr, "GetModId: Invalid module name <%s>\n", modName );
     *modId = 0;
      return( -1 );
   }

/* Found it!
 ***********/
  *modId = EW_Module[i].id;
   return( 0 );
}

         /*******************************************************
          *                      GetType                        *
          *                                                     *
          * Convert message-type name to number using table     *
          * defined in TableFile                                *
          * Return  0 on success;                               *
          *        -1 if specified message-type name is unknown *
          *******************************************************/
int GetType( char *msgName, unsigned char *msgType )
{
   int i;

   if( LoadTables ) {
       GetUtil_LoadTable();
       LoadTables = 0;
   }

/* Find message-type name in earthworm.h table
 *********************************************/
   for ( i = 0; i < Max_MessageType; i++ )
      if ( strcmp( EW_Message[i].name, msgName ) == 0 )
         break;

/* Didn't find message-type name in table
 ****************************************/
   if ( i == Max_MessageType )
   {
      fprintf( stderr,
              "GetType: Invalid message-type name <%s>\n", msgName );
     *msgType = 0;
      return( -1 );
   }

/* Found it!
 ***********/
  *msgType = EW_Message[i].type;
   return( 0 );
}

  /********************************************************************
   * GetUtil_LoadTable  loads the ring, module, and message tables    *
   *            from an ascii file (TableFile) using kom.c functions. *
   *            Exits if any errors are encountered.                  *
   ********************************************************************/
void GetUtil_LoadTable( void )
{
   int      ncommand;     /* # of required commands you expect to process   */
   char     init[10];     /* init flags, one byte for each required command */
   int      nmiss;        /* number of required commands that were missed   */
   char    *paramdir;    /* points to environment variable, EW_PARAMS      */
   char    *com;
   char    *str;
   int      nfiles, nopen;
   int      success;
   long     tmpkey;
   int      tmp;
   int      conflict;
   int      i;
   size_t   len;

/* Set to zero one init flag for each required command
 *****************************************************/
   ncommand = 3;
   for( i=0; i<ncommand; i++ )  init[i] = 0;

/* Get the environment variable, EW_PARAMS
 *****************************************/
   paramdir = getenv( "EW_PARAMS" ); 

   if ( paramdir == (char *)NULL )   
   {
      printf( "GetUtil_LoadTable: Environment variable EW_PARAMS not defined!" );
      printf( " exitting!\n" );
      exit(-1);
   }
   if ( *paramdir == '\0' )
   {
      printf( "GetUtil_LoadTable: Environment variable EW_PARAMS " );
      printf( "defined, but has no value; exitting!\n" );
      exit( -1 );
   }

/* Build full path to table file
 *******************************/
   if( strlen(paramdir)+strlen(TableFile)+1 > (size_t)MAXLEN )
   {
      printf("GetUtil_LoadTable: length of EW_PARAMS+TableFile " );
      printf("exceeds FullTablePath, MAXLEN=%d; exitting!\n", MAXLEN );
      exit( -1 );
   }
   strcpy( FullTablePath, paramdir  );
   len = strlen( FullTablePath );
#ifdef _SOLARIS
   if( FullTablePath[len-1] != '/' )   strcat( FullTablePath, "/" );
#else  /* OS/2 or NT */
   if( FullTablePath[len-1] != '\\' )  strcat( FullTablePath, "\\" );
#endif
   strcat( FullTablePath, TableFile );
   /*printf( "path to modid/msgtype table: <%s>\n", FullTablePath );*//*DEBUG*/

/* Open the main table file
 **************************/
   nfiles = k_open( FullTablePath );
   if( nfiles == 0 ) {
        fprintf( stderr,
                "GetUtil_LoadTable: Error opening file <%s>; exitting!\n",
                 FullTablePath );
        exit( -1 );
   }
   nopen = nfiles-1;  /* keep track of # open files before TableFile */

/* Process all command files
 ***************************/
   while( nfiles > nopen ) /* While there are getutil-files open */
   {
        while(k_rd())        /* Read next line from active file  */
        {
            com = k_str();         /* Get the first token from line */

        /* Ignore blank lines & comments
         *******************************/
            if( !com )           continue;
            if( com[0] == '#' )  continue;

        /* Open a nested configuration file
         **********************************/
            if( com[0] == '@' ) {
               success = nfiles+1;
               nfiles  = k_open(&com[1]);
               if ( nfiles != success ) {
                  fprintf( stderr,
                          "GetUtil_LoadTable: Error opening file <%s>; exitting!\n",
                           &com[1] );
                  exit( -1 );
               }
               continue;
            }

         /* Process anything else as a command:
          *************************************/

         /* Load shared memory ring name/key table
          ****************************************/
  /*0*/     if( k_its("Ring") ) 
            {
             /* see if there's more room in the table */
                if ( Max_Ring+1 >= MAXRING ) {
                    fprintf( stderr,
                            "GetUtil_LoadTable: Too many <Ring> lines in <%s>",
                             FullTablePath );
                    fprintf( stderr, "; max=%d; exitting!\n", (int) MAXRING );
                    exit( -1 );
                }
                str    = k_str();    /* get ring name from line */
                tmpkey = k_long();   /* get ring key from line  */

             /* look thru current table for duplicate key or name */
                conflict = 0;
                for( i=0; i<Max_Ring; i++ ) {
                     if( tmpkey == EW_Ring[i].key ) {
			if( strcmp( EW_Ring[i].name, str ) != 0 ) conflict=1;
                        break;
                     }
                     if( strcmp( EW_Ring[i].name, str ) == 0 ) {
                        if( tmpkey != EW_Ring[i].key ) conflict=1;
                        break;
                     }
                }

             /* complain if there was a conflict with a previous setting */
                if( conflict ) {
	             fprintf( stderr, 
		             "GetUtil_LoadTable: conflict in <%s>, new setting ignored\n", 
                              FullTablePath );
	             fprintf( stderr, 
                             "                   original: <Ring %s %ld>\n", 
                              EW_Ring[i].name, EW_Ring[i].key );
	             fprintf( stderr, 
                             "                        new: <Ring %s %ld>\n", 
                              str, tmpkey);
                }

             /* add new entry to table */
                if( i==Max_Ring ) {
                   strcpy( EW_Ring[Max_Ring].name, str );
                   EW_Ring[Max_Ring].key = tmpkey;
                   Max_Ring++;
                }
                init[0] = 1;
            }

         /* Enter module name/id table
          ****************************/
  /*1*/     else if( k_its("Module") ) 
            {
             /* see if there's more room in the table */
                if ( Max_ModuleId+1 >= MAXMODID ) {
                    fprintf( stderr,
                            "GetUtil_LoadTable: Too many <Module> lines in <%s>",
                             FullTablePath );
                    fprintf( stderr, "; max=%d; exitting!\n", (int) MAXMODID );
                    exit( -1 );
                }
                str = k_str();    /* get module name from line */
                tmp = k_int();    /* get module id from line   */

             /* check validity of module id */
                if( tmp<0 || tmp>255 ) {
                     fprintf( stderr,
			     "GetUtil_LoadTable: Invalid module id <%d> in <%s>",
                              tmp, FullTablePath );
		     fprintf( stderr, " (0-255 are valid); exitting!\n" );
		     exit( -1 );
	        }     

             /* look thru current table for duplicate key or name */
                conflict = 0;
                for( i=0; i<Max_ModuleId; i++ ) {
                     if( tmp == (int)EW_Module[i].id ) {
			if( strcmp( EW_Module[i].name, str ) != 0 ) conflict=1;
                        break;
                     }
                     if( strcmp( EW_Module[i].name, str ) == 0 ) {
                        if( tmp != (int)EW_Module[i].id ) conflict=1;
                        break;
                     }
                }

             /* complain if there was a conflict with a previous setting */
                if( conflict ) {
	             fprintf( stderr, 
		             "GetUtil_LoadTable: conflict in <%s>, new setting ignored\n", 
                              FullTablePath );
	             fprintf( stderr, 
                             "                   original: <Module %s %d>\n", 
                              EW_Module[i].name, (int) EW_Module[i].id );
	             fprintf( stderr, 
                             "                        new: <Module %s %d>\n", 
                              str, tmp );
                }

             /* add new entry to table */
                if( i==Max_ModuleId ) {
                    strcpy( EW_Module[Max_ModuleId].name, str );
                    EW_Module[Max_ModuleId].id = (unsigned char) tmp;
                    Max_ModuleId++;
                }
                init[1] = 1;
            }

         /* Enter message name/type table
          *******************************/
  /*2*/     else if( k_its("Message") ) 
            {
             /* see if there's more room in the table */
                if ( Max_MessageType+1 >= MAXMSGTYPE ) {
                    fprintf( stderr,
                            "GetUtil_LoadTable: Too many <Message> lines in <%s>",
                             FullTablePath );
                    fprintf( stderr, "; max=%d; exitting!\n", (int) MAXMSGTYPE );
                    exit( -1 );
                }
                str = k_str();    /* get message name from line */
                tmp = k_int();    /* get message type from line */

             /* check validity of module id */
                if( tmp<0 || tmp>255 ) {
                     fprintf( stderr,
			     "GetUtil_LoadTable: Invalid message type <%d> in <%s>",
                              tmp, FullTablePath );
		     fprintf( stderr, " (0-255 are valid); exitting!\n" );
		     exit( -1 );
	        }     
                
             /* look thru current table for duplicate type or name */
                conflict = 0;
                for( i=0; i<Max_MessageType; i++ ) {
                     if( tmp == (int)EW_Message[i].type ) {
			if( strcmp( EW_Message[i].name, str ) != 0 ) conflict=1;
                        break;
                     }
                     if( strcmp( EW_Message[i].name, str ) == 0 ) {
                        if( tmp != (int)EW_Message[i].type ) conflict=1;
                        break;
                     }
                }

             /* complain if there was a conflict with a previous setting */
                if( conflict ) {
	             fprintf( stderr, 
		             "GetUtil_LoadTable: conflict in <%s>, new setting ignored\n", 
                              FullTablePath );
	             fprintf( stderr, 
                             "                   original: <Message %s %d>\n", 
                              EW_Message[i].name, (int) EW_Message[i].type );
	             fprintf( stderr, 
                             "                        new: <Message %s %d>\n", 
                              str, tmp );
                }

             /* add new entry to table */
                if( i==Max_MessageType ) {
                    strcpy( EW_Message[Max_MessageType].name, str );
                    EW_Message[Max_MessageType].type = (unsigned char) tmp;
                    Max_MessageType++;
                }
                init[2] = 1;
            }

         /* Otherwise, it's unknown
          *************************/
            else  
            {
                fprintf( stderr, "GetUtil_LoadTable: <%s> Unknown command in <%s>.\n",
                         com, FullTablePath );
                continue;
            }

        /* See if there were any errors processing the command
         *****************************************************/
            if( k_err() ) 
            {
               fprintf( stderr,
                       "GetUtil_LoadTable: Bad <%s> line in <%s>; exitting!\n",
                        com, FullTablePath );
               exit( -1 );
            }
        }
        nfiles = k_close();
   }

/* After all files are closed, check init flags for missed commands
 ******************************************************************/
   nmiss = 0;
   for ( i=0; i<ncommand; i++ )  if( !init[i] ) nmiss++;
   if ( nmiss ) {
       fprintf( stderr, "GetUtil_LoadTable: ERROR, no " );
       if ( !init[0] )  fprintf( stderr, "<Ring> "      );
       if ( !init[1] )  fprintf( stderr, "<Module> "    );
       if ( !init[2] )  fprintf( stderr, "<Message> "   );
       fprintf( stderr, "line(s) in <%s>; exitting!\n", FullTablePath );
       exit( -1 );
   }

   return;
}

         /*******************************************************
          *                      GetModName                       *
          *                                                     *
          *  Convert modid number to module name using table    *
          *  defined in TableFile                               *
          *  Return  0 on success;                              *
          *         -1 if the specified module name is unknown. *
          *******************************************************/
int GetModName( unsigned char *modId, char *modName )
{
   int i;

   if( LoadTables ) {
       GetUtil_LoadTable();
       LoadTables = 0;
   }

/* Find module name in earthworm.h table
 ***************************************/
   for ( i = 0; i < Max_ModuleId; i++ )
      if ( EW_Module[i].id == *modId ) break;

/* Didn't find module name in table
 **********************************/
   if ( i == Max_ModuleId )
   {
      fprintf( stderr, "GetModName: Invalid module type <%d>\n", *modId );
     sprintf( modName, "<Module %03d>", (int) *modId );
      return( -1 );
   }

/* Found it!
 ***********/
  strcpy( modName, EW_Module[i].name );
   return( 0 );
}

         /*******************************************************
          *                      GetTypeName                    *
          *                                                     *
          *  Convert message type number to type name using table    *
          *  defined in TableFile                               *
          *  Return  0 on success;                              *
          *         -1 if the specified module name is unknown. *
          *******************************************************/
int GetTypeName( unsigned char *typeId, char *typeName )
{
   int i;

   if( LoadTables ) {
       GetUtil_LoadTable();
       LoadTables = 0;
   }

/* Find message-type name in earthworm.d table
 ***************************************/
   for ( i = 0; i < Max_MessageType; i++ )
      if ( EW_Message[i].type == *typeId ) break;

/* Didn't find message-type name in table
 **********************************/
   if ( i == Max_MessageType )
   {
      fprintf( stderr, "GetTypeName: Invalid message type <%d>\n", *typeId );
     sprintf( typeName, "<Type %03d>", (int) *typeId );
      return( -1 );
   }

/* Found it!
 ***********/
  strcpy( typeName, EW_Message[i].name );
   return( 0 );
}
