/***********************************************************************//**
 * @file logging.c
 *
 * Log handling routines for libdali.
 *
 * These logging routines are used throughout the library and allow
 * all log, diagnostic and error message output to be redirected or
 * otherwise fine tuned.
 *
 * The most important routines for general library use are
 * dl_loginit() and dl_log().
 *
 * @author Chad Trabant, IRIS Data Management Center
 *
 * modified: 2008.193
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "libdali.h"

void dl_loginit_main (DLLog *logp, int verbosity,
		      void (*log_print)(char*), const char *logprefix,
		      void (*diag_print)(char*), const char *errprefix);

int dl_log_main (DLLog *logp, int level, int verb, va_list *varlist);

/** Initial global logging parameters */
DLLog gDLLog = {NULL, NULL, NULL, NULL, 0};


/***********************************************************************//**
 * @brief Initialize global logging system parameters
 *
 * Initialize the global logging parameters.
 *
 * See dl_loginit_main() description for usage.
 ***************************************************************************/
void
dl_loginit (int verbosity,
	    void (*log_print)(char*), const char *logprefix,
	    void (*diag_print)(char*), const char *errprefix)
{
  dl_loginit_main (&gDLLog, verbosity, log_print, logprefix, diag_print, errprefix);
}  /* End of dl_loginit() */


/***********************************************************************//**
 * @brief Initialize logging parameters specific to a DLCP
 *
 * Initialize DLCP specific logging parameters.  If the logging parameters
 * have not been initialized (dlconn->log == NULL) new parameter space will
 * be allocated.
 *
 * See dl_loginit_main() description for usage.
 ***************************************************************************/
void
dl_loginit_r (DLCP *dlconn, int verbosity,
	      void (*log_print)(char*), const char *logprefix,
	      void (*diag_print)(char*), const char *errprefix)
{
  if ( ! dlconn )
    return;

  if ( dlconn->log == NULL )
    {
      dlconn->log = (DLLog *) malloc (sizeof(DLLog));

      dlconn->log->log_print = NULL;
      dlconn->log->logprefix = NULL;
      dlconn->log->diag_print = NULL;
      dlconn->log->errprefix = NULL;
      dlconn->log->verbosity = 0;
    }

  dl_loginit_main(dlconn->log, verbosity, log_print, logprefix, diag_print, errprefix);
}  /* End of dl_loginit_r() */


/***********************************************************************//**
 * @brief Initialize logging parameters for a specific DLLog
 *
 * Initialize DLLog specific logging parameters.  If the logging parameters
 * have not been initialized (log == NULL) new parameter space will
 * be allocated.
 *
 * See dl_loginit_main() description for usage.
 *
 * @return A pointer to the created/re-initialized DLLog struct.
 ***************************************************************************/
DLLog *
dl_loginit_rl (DLLog *log, int verbosity,
	       void (*log_print)(char*), const char *logprefix,
	       void (*diag_print)(char*), const char *errprefix)
{
  DLLog *logp;

  if ( log == NULL )
    {
      logp = (DLLog *) malloc (sizeof(DLLog));

      logp->log_print = NULL;
      logp->logprefix = NULL;
      logp->diag_print = NULL;
      logp->errprefix = NULL;
      logp->verbosity = 0;
    }
  else
    {
      logp = log;
    }

  dl_loginit_main (logp, verbosity, log_print, logprefix, diag_print, errprefix);

  return logp;
}  /* End of dl_loginit_rl() */


/***********************************************************************//**
 * @brief Initialize the logging system
 *
 * Initialize the logging subsystem.  The logging paramters determine
 * how dl_log() and dl_log_r() emit messages.
 *
 * This function modifies the logging parameters in the passed DLLog.
 *
 * Any log/error printing functions indicated must accept a single
 * argument, namely a string (char *).  The dl_log() and dl_log_r()
 * functions format each message and then pass the result on to the
 * log/error printing functions.
 *
 * If the log/error prefixes have been set they will be pre-pended to the
 * message.
 *
 * Use NULL for the function pointers or the prefixes if they should not
 * be changed from previously set or default values.  The default behavior
 * of the logging subsystem is given in the example below.
 *
 * Example: dl_loginit_main (0, (void*)&printf, NULL, (void*)&printf, "error: ");
 *
 * @param logp The DLLog parameters to change
 * @param verbosity The verbosity level
 * @param log_print Pointer to a log message printing function
 * @param logprefix Prefix to add to each log & diganostic message
 * @param diag_print Pointer to a diagnostic & error message printing function
 * @param errprefix Prefix to add to each error message
 ***************************************************************************/
void
dl_loginit_main (DLLog *logp, int verbosity,
		 void (*log_print)(char*), const char *logprefix,
		 void (*diag_print)(char*), const char *errprefix)
{
  if ( ! logp )
    return;

  logp->verbosity = verbosity;

  if ( log_print )
    logp->log_print = log_print;

  if ( logprefix )
    {
      if ( strlen(logprefix) >= MAX_LOG_MSG_LENGTH )
	{
	  dl_log_rl (logp, 2, 0, "log message prefix is too large\n");
	}
      else
	{
	  logp->logprefix = logprefix;
	}
    }

  if ( diag_print )
    logp->diag_print = diag_print;

  if ( errprefix )
    {
      if ( strlen(errprefix) >= MAX_LOG_MSG_LENGTH )
	{
	  dl_log_rl (logp, 2, 0, "error message prefix is too large\n");
	}
      else
	{
	  logp->errprefix = errprefix;
	}
    }

  return;
}  /* End of dl_loginit_main() */


/***********************************************************************//**
 * @brief Log a message using the global logging parameters
 *
 * A wrapper to dl_log_main() that uses the global logging parameters.
 *
 * @param level Level at which to log the message (1, 2 or 3)
 * @param verb Verbosity threshold at which to log the message
 * @param ... Message format and optional arguments in printf style
 *
 * @return See dl_log_main() description for return values.
 ***************************************************************************/
int
dl_log (int level, int verb, ...)
{
  int retval;
  va_list varlist;
  
  va_start (varlist, verb);

  retval = dl_log_main (&gDLLog, level, verb, &varlist);

  va_end (varlist);

  return retval;
}  /* End of dl_log() */


/***********************************************************************//**
 * @brief Log a message using the log parameters from a DLCP
 *
 * A wrapper to dl_log_main() that uses the logging parameters in a
 * supplied DLCP. If the supplied pointer is NULL the global logging
 * parameters will be used.
 *
 * @param dlconn DataLink Connection Parameters with associated logging paramters
 * @param level Level at which to log the message (1, 2 or 3)
 * @param verb Verbosity threshold at which to log the message
 * @param ... Message format and optional arguments in printf style
 *
 * @return See dl_log_main() description for return values.
 ***************************************************************************/
int
dl_log_r (const DLCP *dlconn, int level, int verb, ...)
{
  int retval;
  va_list varlist;
  DLLog *logp;

  if ( ! dlconn )
    logp = &gDLLog;
  else if ( ! dlconn->log )
    logp = &gDLLog;
  else
    logp = dlconn->log;
  
  va_start (varlist, verb);
  
  retval = dl_log_main (logp, level, verb, &varlist);

  va_end (varlist);

  return retval;
}  /* End of dl_log_r() */


/***********************************************************************//**
 * @brief Log a message using the log parameters from a DLCP
 *
 * A wrapper to dl_log_main() that uses the logging parameters in a
 * supplied DLLog.  If the supplied pointer is NULL the global logging
 * parameters will be used.
 *
 * @param log DLLog logging paramters
 * @param level Level at which to log the message (1, 2 or 3)
 * @param verb Verbosity threshold at which to log the message
 * @param ... Message format and optional arguments in printf style
 *
 * @return See dl_log_main() description for return values.
 ***************************************************************************/
int
dl_log_rl (DLLog *log, int level, int verb, ...)
{
  int retval;
  va_list varlist;
  DLLog *logp;

  if ( ! log )
    logp = &gDLLog;
  else
    logp = log;
  
  va_start (varlist, verb);
  
  retval = dl_log_main (logp, level, verb, &varlist);

  va_end (varlist);

  return retval;
}  /* End of dl_log_rl() */


/***********************************************************************//**
 * @brief Primary log message processing routine
 *
 * Prinmary logging/printing routine.
 *
 * This routine acts as a central message facility for the all of the
 * libdali functions.
 *
 * The function uses logging parameters specified in the supplied
 * DLLog.
 * 
 * This function expects 3+ arguments, message level, verbosity level,
 * fprintf format, and fprintf arguments.  If the verbosity level is
 * less than or equal to the set verbosity (see dl_loginit_main()),
 * the fprintf format and arguments will be printed at the appropriate
 * level.
 *
 * Three levels are recognized:
 * 0  : Normal log messages, printed using log_print with logprefix
 * 1  : Diagnostic messages, printed using diag_print with logprefix
 * 2+ : Error messagess, printed using diag_print with errprefix
 *
 * This function builds the log/error message and passes to it as a
 * string (char *) to the functions defined with dl_loginit() or
 * dl_loginit_r().  If the log/error printing functions have not been
 * defined messages will be printed with fprintf, log messages to
 * stdout and error messages to stderr.
 *
 * If the log/error prefix's have been set with dl_loginit() or
 * dl_loginit_r() they will be pre-pended to the message.
 *
 * All messages will be truncated to the MAX_LOG_MSG_LENGTH, this includes
 * any set prefix.
 *
 * @param logp DLLog logging paramters
 * @param level Level at which to log the message (1, 2 or 3)
 * @param verb Verbosity threshold at which to log the message
 * @param varlist Message format and optional arguments in printf style
 *
 * @return The number of characters formatted on success, and a
 * a negative value on error.
 ***************************************************************************/
int
dl_log_main (DLLog *logp, int level, int verb, va_list *varlist)
{
  static char message[MAX_LOG_MSG_LENGTH];
  int retvalue = 0;
  
  message[0] = '\0';
  
  if (verb <= logp->verbosity)
    {
      int presize;
      const char *format;

      format = va_arg (*varlist, const char *);

      if ( level >= 2 )  /* Error message */
	{
	  if ( logp->errprefix != NULL )
	    {
	      strncpy (message, logp->errprefix, MAX_LOG_MSG_LENGTH);
	    }
	  else
	    {
	      strncpy (message, "error: ", MAX_LOG_MSG_LENGTH);
	    }

	  presize = strlen(message);
	  retvalue = vsnprintf (&message[presize],
				MAX_LOG_MSG_LENGTH - presize,
				format, *varlist);

	  message[MAX_LOG_MSG_LENGTH - 1] = '\0';

	  if ( logp->diag_print != NULL )
	    {
	      logp->diag_print (message);
	    }
	  else
	    {
	      fprintf(stderr, "%s", message);
	    }
	}
      else if ( level == 1 )  /* Diagnostic message */
	{
	  if ( logp->logprefix != NULL )
	    {
	      strncpy (message, logp->logprefix, MAX_LOG_MSG_LENGTH);
	    }

	  presize = strlen(message);
	  retvalue = vsnprintf (&message[presize],
				MAX_LOG_MSG_LENGTH - presize,
				format, *varlist);

	  message[MAX_LOG_MSG_LENGTH - 1] = '\0';

	  if ( logp->diag_print != NULL )
	    {
	      logp->diag_print (message);
	    }
	  else
	    {
	      fprintf(stderr, "%s", message);
	    }
	}
      else if ( level == 0 )  /* Normal log message */
	{
	  if ( logp->logprefix != NULL )
	    {
	      strncpy (message, logp->logprefix, MAX_LOG_MSG_LENGTH);
	    }

	  presize = strlen(message);
	  retvalue = vsnprintf (&message[presize],
				MAX_LOG_MSG_LENGTH - presize,
				format, *varlist);

	  message[MAX_LOG_MSG_LENGTH - 1] = '\0';

	  if ( logp->log_print != NULL )
	    {
	      logp->log_print (message);
	    }
	  else
	    {
	      fprintf(stdout, "%s", message);
	    }
	}
    }

  return retvalue;
}  /* End of dl_log_main() */
