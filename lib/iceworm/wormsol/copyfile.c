/* 
 * copyfile.c for Solaris                              960112:LDD
 *
 * Copies a file from local machine to a remote machine using rcp
 *
 */

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include <wait.h>

/*****************************************************************************
 *  copyfile( )  copies a local file to a remote Solaris or SunOS machine    *
 *****************************************************************************/   
int copyfile( char *fname,    /* name of file to copy                 */
              char *tname,    /* temporary remote file name           */
              char *host,     /* remote machine to copy file to       */
              char *dir,      /* directory on remote machine          */
              char *userid,   /* use this user name on remote machine */
              char *passwd,   /* userid's password on remote machine  */
              char *errtxt )  /* string to return error message in    */

/* For this function to work, make sure that the following files are set   *
 * up properly on the remote machine:				           *
 *   /etc/hosts         must contain address and localhostname		   *
 *   /etc/hosts.equiv   must contain local_hostname			   *
 *   .rhosts            in <userid>'s home directory must contain a line:  *
 *                      local_hostname local_username			   *
 *                      describing who is running this program. 	   *
 *									   *
 *  Also make sure that entries for the remote host are in the		   *
 *  local machine's /etc/hosts and /etc/hosts.equiv files.		   */
 
{
   char   rcppath[175];          /* path to initially copy file to */
   char   tmpname[100];          /* temporary file name (remote)   */
   char   finalname[100];        /* final name for copied file     */
   pid_t  pid;
   int    status, exitstat;
   int    n;

/* Build temporary path & final path to write file to
 ****************************************************/
   n = strlen(dir);
   if( dir[n-1] == '/' ) 
   {
      sprintf( rcppath,  "%s@%s:%s%s",  userid, host, dir, tname );
      sprintf( tmpname,  "%s%s",        dir, tname );
      sprintf( finalname, "%s%s",       dir, fname );
   }
   else
   {
      sprintf( rcppath,  "%s@%s:%s/%s", userid, host, dir, tname );
      sprintf( tmpname,  "%s/%s",       dir, tname );
      sprintf( finalname, "%s/%s",      dir, fname );
   }


/* Start new process to copy file
 ********************************/
   pid = fork1();
   switch( pid ) 
   {
      case -1: /* fork failed */
               sprintf( errtxt, "copyfile <%s>: fork failed", fname );
               perror( errtxt );
               return(1);

      case  0: /* in new child process */
               execl( "/usr/bin/rcp", 
                      "rcp", "-p", fname, rcppath,
                      (char *) 0  );
               perror( "copyfile: execl" );
               exit(1);
      
      default: /* in parent */
                break;
   }

   if ( waitpid( pid, &status, 0 ) == -1 )
   {
      sprintf( errtxt, "copyfile <%s>: waitpid error", fname );
      return( 2 );
   }

/* See if the child (in this case, rcp) terminated abnormally
 ************************************************************/
   if ( WIFSIGNALED(status) )
   {
      sprintf( errtxt, "copyfile <%s>: rcp terminated by signal %d", 
               fname, WTERMSIG(status) );      
      return( 3 );
   }
   else if ( WIFSTOPPED(status) )
   {
      sprintf( errtxt, "copyfile <%s>: rcp stopped by signal %d", 
               fname, WSTOPSIG(status) );      
      return( 3 );
   }
   else if ( WIFEXITED(status) )
   {
      exitstat = WEXITSTATUS(status);
      if( exitstat != 0 )
      {
        sprintf( errtxt, "copyfile <%s>: rcp exitted with status %d", 
                 fname, exitstat );      
        return( 3 );
      }
   }

/* Start new process to rename the remote file
 *********************************************/
   pid = fork1();
   switch( pid ) 
   {
      case -1: /* fork failed */
               sprintf( errtxt, "copyfile <%s>: fork failed", fname );
               perror( errtxt );
               return(1);

      case  0: /* in new child process */
               execl( "/bin/rsh", 
                      "rsh", "-l", userid, host,
                      "/usr/bin/mv", tmpname, finalname,
                      (char *) 0  ); 
               perror( "copyfile: execl" ); 
               exit(1);
      
      default: /* in parent */
                break;
   }

   if ( waitpid( pid, &status, 0 ) == -1 )
   {
      sprintf( errtxt, "copyfile <%s>: waitpid error", fname );
      return( 2 );
   } 

/* See if the child (in this case, rsh mv) terminated abnormally
 ***************************************************************/
   if ( WIFSIGNALED(status) )
   {
      sprintf( errtxt, "copyfile <%s>: rsh mv terminated by signal %d", 
               fname, WTERMSIG(status) );      
      return( 3 );
   }
   else if ( WIFSTOPPED(status) )
   {
      sprintf( errtxt, "copyfile <%s>: rsh mv stopped by signal %d", 
               fname, WSTOPSIG(status) );      
      return( 3 );
   }
   else if ( WIFEXITED(status) )
   {
      exitstat = WEXITSTATUS(status);
      if( exitstat != 0 )
      {
        sprintf( errtxt, "copyfile <%s>: rsh mv exitted with status %d", 
                 fname, exitstat );      
        return( 3 );
      }
   }

/* Everything went smoothly
 **************************/
   return( 0 );
}
