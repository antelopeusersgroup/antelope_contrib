  /************************************************************
   *                       parse_trig.c                       *
   *                                                          *
   * Mon Nov  2 11:02:23 MST 1998 lucky                       *
   *  Y2K compliance:                                         *
   *   SNIPPET struct has been modified to include 8 digit    *
   *   date (YYYYMMDD), and corresponding changes have been   *
   *   made in parse_Snippet().                               *
   *                                                          *
   *   t_atodbl() changed to reflect YYYYMMDD. Introduced     *
   *   defines that hold string lengths in case these formats *
   *   ever need to be changed in the future. Call to         *
   *   epocchsec15() replaced by epochsec17().                *
   *                                                          *
   ************************************************************/  

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifndef EARTHWORM_H
#include "earthworm.h"
#endif
#include "parse_trig.h"


#define LINE_LEN         500
#define BAD_TIME_VALUE   500000000

/* Function declarations */
extern int epochsec17 (double *, char *);
int getNextLine (char**, char*);
int t_atodbl (char*, char*, double*); 

/*
Story:
	parseSnippet() parses a trigger message. Inspired by, and uses,
strtok. Therefore,   IT IS NOT MULTI-THREAD SAFE. It must be mutex
protected against concurrent calls to strtok.

Arguments:
	msg:	pointer to the trigger message.
	pSnp:	pointer to a SNIPPET structure (see parse_trig.h)
	nxtLine:running pointer to current line to be parsed.

Usage:
For the first call, set nxtLine to msg. This tells parseSnippet that
we're starting a new message. It will search for, and parse  the event
id. It will also parse the first line, and stuff the elements of the
SNIPPET structure.  It will then advance nxtLine to point to the next
line to be parsed and return.  Subsequent calls, with the adjusted
value of nxtLine, will result in the next line of the message being
parsed.  When the last line has been parsed, a EW_FAILURE will be
returned.  
*/


/***********************************************************************
 *  parseSnippet() parses snippet parameters from next line of trigger *
 *                 message. Returns EW_FAILURE when nothing left.      *
 *		   Does its own error logging via logit.                       *
 ***********************************************************************/
int parseSnippet( char* msg, SNIPPET* pSnp, char** nxtLine)
{
   static char terminators[] = " \t"; /* we accept space and tab as terminators */
   char* nxttok;
   char line[LINE_LEN];
   static int ourEventId=-1;

   /* if pointer to next line points to start of message, start clean 
    *****************************************************************/
   if (*nxtLine == msg) ourEventId  = -1;
   else goto GetNextStation;

   /* if event id is negative, search for new EVENT ID
    **************************************************/


   GetNextEvent:    /* we jump to here when we run off the end of an event (below) */

   /* we're looking for a line with the form shown below:
 EVENT DETECTED     19970729 03:01:13.22 UTC  EVENT ID: 123456 AUTHOR: Harry
	 *
	 *************************  Y2K  ************************/

   if ( ourEventId == -1)
	{
	/* get next non-zero length line from trigger message */
   	while ( getNextLine(nxtLine, line) >= 0 ) /* it returns number of chars in line */
	   {
	   /* skip this line if it doesn't start with EVENT */
	   /*** MULTI-THREAD WARNING: strtok is not safe (but it is ubiquitus) ***/
	   if ( strcmp( strtok( line, terminators), "EVENT" ) != 0) continue;

	   /* on this line, find "ID:" which is followed by the event id integer */
	   while ( (nxttok = strtok( (char*)NULL, terminators)) != NULL ) /* over tokens on this line */
		{
	   	if ( strcmp( nxttok, "ID:" ) != 0) continue;
		nxttok = strtok( (char*)NULL, terminators); /* this had better be the event id */
		if (nxttok == NULL) /* oops - there was nothing after ID: */
		   {
		   logit("et", "parse_trig: Bad syntax in trigger message."
			       " Cant find event id in:\n.%s.\n", line);
		   break;
		   }
		ourEventId = atol( nxttok ); /* put away this token as the event id */
		if ( ourEventId <= 0) /* oops - it wasn't an integer */
		   {
		   ourEventId = -1; /* to indicate no deal */
		   logit("et", "parse_trig: Bad syntax in trigger message."
			       " Bad event id value in:\n.%s.\n", line);
		   break;
		   }
		/* If we're here, we're happy: we've got an event id */
		pSnp->eventId = ourEventId;
		/* Now search for author id */
		nxttok = strtok( (char*)NULL, terminators); /* this had better be "AUTHOR:" */
	   	if ( strcmp( nxttok, "AUTHOR:" ) != 0) 
		   {
		   logit("et", "parse_trig: Bad syntax in trigger message."
			       " Cant find AUTHOR:\n.%s.\n", line);
		   break;
		   }
		nxttok = strtok( (char*)NULL, terminators); /* this had better be author's name */
		strncpy(pSnp->author, nxttok, AUTHOR_FIELD_SIZE);
                pSnp->author[AUTHOR_FIELD_SIZE-1] = '\0';           /* Ensure null termination */

		goto GotNextEvent;
 		}	/* end of loop over tokens in this line */
	   }		/* end of loop over lines in message */
	   return(EW_FAILURE); 	/* no more lines */
	} 		/* end of search for event id */



   GotNextEvent:

   /* So we have an event id. Now parse and return snippet parameters until a blank line
   *************************************************************************************/ 
   getNextLine(nxtLine, line); /* step over the blank line */
   getNextLine(nxtLine, line); /* step over the column titles line */
   getNextLine(nxtLine, line); /* step over the silly dashes line */

   /* Read the next station line
   *****************************/
   GetNextStation:
   pSnp->eventId = ourEventId;
   if( getNextLine(nxtLine, line) <= 0 ) /* this should be a station trigger line */
	{
	return(EW_FAILURE); 	/* no more lines */
	}
   if ( ( nxttok=strtok(line, terminators) ) == NULL ) /* first token should be station name */
	{
	logit("et","parse_trig: Bad syntax in trigger message:"
 		   "Strange station line - no tokens in: \n.&s.\n",line);
	goto GetNextEvent;
	}

   /* Find SCN names
    ****************/
   if (nxttok ==NULL) /* oops - should have been the station name */
   	{
   	logit("et", "parse_trig: Bad syntax in trigger message."
	            " Cant find statio name in:\n.%s.\n", line);
   	goto GetNextStation;
   	}
   strncpy( pSnp->sta, nxttok, 7);      /* Put away the station name */
   pSnp->sta[6] = '\0';                 /* Ensure null termination */

   nxttok = strtok( (char*)NULL, terminators); /* should be the component */
   if (nxttok ==NULL) /* oops - there was nothing after station name */
   	{
   	logit("et", "parse_trig: Bad syntax in trigger message."
	            " Cant find comp name in:\n.%s.\n", line);
   	goto GetNextStation;
   	}
   strncpy( pSnp->chan, nxttok, 9 );    /* Put away the component */
   pSnp->chan[8] = '\0';                /* Ensure null termination */

   nxttok = strtok( (char*)NULL, terminators); /* should be the net */
   if (nxttok ==NULL) /* oops - there was nothing after component name */
   	{
   	logit("et", "parse_trig: Bad syntax in trigger message."
	            " Cant find net name in:\n.%s.\n", line);
   	goto GetNextStation;
   	}
   strncpy( pSnp->net, nxttok, 9 );     /* put away the net */
   pSnp->net[8] = '\0';                 /* Ensure null termination */

   /* And now, find "save:"  
   ***********************/
   while ( (nxttok=strtok((char*)NULL, terminators) ) != NULL ) 	/* run down the tokens */
	{
   	if ( strcmp( nxttok, "save:" ) == 0 ) goto FoundSave; /* looking for 'save:' */
	}
   logit("et","parse_trig: Bad syntax in trigger message:"
 	   "Bad station line - no save token found: \n.%s.\n",line);
   goto GetNextStation;      
   FoundSave:
		
   /* now find time strings
    ***********************/
   nxttok = strtok( (char*)NULL, terminators); /* should be the save start date */
   if (nxttok ==NULL) /* oops - there was nothing after save: */
   	{
   	logit("et", "parse_trig: Bad syntax in trigger message."
	            " Cant find save date in:\n.%s.\n", line);
   	goto GetNextStation;
   	}
   strcpy( pSnp->startYYYYMMDD, nxttok ); 	/* put away the date string */

   nxttok = strtok( (char*)NULL, terminators); /* sould be the save start time-of-day */
   if (nxttok ==NULL) /* oops - there was nothing after save: */
   	{
	logit("et", "parse_trig: Bad syntax in trigger message."
                    " Cant find save time of day in:\n.%s.\n", line);
	goto GetNextStation;
	}
   strcpy( pSnp->startHHMMSS, nxttok ); 	/* put away the time-of-day string */

   /* Convert start time to double 
    ******************************/
   if( t_atodbl(pSnp->startYYYYMMDD, pSnp->startHHMMSS, &(pSnp->starttime) ) < 0)
  	{
  	logit("et", "parse_trig: Bad syntax in trigger message."
	            " Dont understand start-time in:\n.%s.\n", line);
	goto GetNextStation;
	}
   if ( pSnp->starttime < BAD_TIME_VALUE ) /* unreasonable time value */
   	{
	logit("et", "parse_trig: Bad syntax in trigger message."
	            " Bad start time value in:\n.%s.\n", line);
	goto GetNextStation;
	}

   /* find duration to save
    ***********************/
   nxttok = strtok( (char*)NULL, terminators); /* should be the duration */
   if (nxttok ==NULL) /* oops - there was nothing after save: */
   	{
   	logit("et", "parse_trig: Bad syntax in trigger message."
	            " Cant find duration in:\n.%s.\n", line);
   	goto GetNextStation;
   	}
   pSnp->duration = atol( nxttok );
   if ( pSnp->duration <= 0 )
   	{
   	logit("et", "parse_trig: Bad syntax in trigger message."
	            " Bad duration value in:\n.%s.\n", line);
   	goto GetNextStation;
   	}

   return(EW_SUCCESS);
}   
/* ----------------------------------- end of parseSnippet() -------------------------------------- */

/**************************************************************************
 *    getNextLine(msg, line) moves the next line from 'msg' into 'line    *
 *                           returns the number of characters in the line *
 *			     Returns negative if error.			  *
 **************************************************************************/
int getNextLine ( char** pNxtLine, char* line)
{
   int i;
   char* nxtLine;

   nxtLine=*pNxtLine;

   for (i =0; i< LINE_LEN; i++)
	{
	line[i] = *nxtLine++;
	if ( (int)line[i] == 0 )
		{
		return(-1); /*  Not good */
		}
	if (line[i] == '\n') goto normal;
	}
   logit("","getNextLine error: line too long \n");
   return(-1);

   normal:
   line[i+1]=0;
   *pNxtLine = nxtLine;
   return(i);
   
}
/* ----------------------------------- end of getNextLine() -------------------------------------- */


/**************************************************************************
 *    t_atodbl() takes date and time strings, and converts to double      *
 *               seconds since 1970. Note  that the time of day string    *
 *               is of the form "hh:mm:ss.ss"                             *
 *	         Returns negative if error.			          *
 **************************************************************************/

/** Lengths of fields in the time string **/
#define TIMESTR_LEN		17	/* whole string */
#define YMD_LEN			8	/* date part: YYYYMMDD */
#define HOUR_LEN		2	/* hours from HH:MM:SS.SS */
#define MIN_LEN			2	/* minutes from HH:MM:SS.SS */
#define SEC_LEN			5	/* seconds from HH:MM:SS.SS */

int t_atodbl(char* YYYYMMDD, char* HHMMSS, double* starttime) 
{
   char timestr[TIMESTR_LEN+1]; /* need space for the null-terminator */
   int ret;
   int tgtind, srcind;	/* indices for copying fields into timestr */

	/*************************  Y2K  ************************
	 *
	 * All instances of YYMMDD have been changed to YYYYMMDD
	 *
	 *************************  Y2K  ************************/

   /* we want a string of  the form yyyymmddhhmmss.ss */
   /*				    01234567890123456             */

   tgtind = 0;
   srcind = 0;

   /* Copy in the date */
   strcpy(timestr,YYYYMMDD);

   /* Append the hour */
   tgtind = tgtind + YMD_LEN;
   strncpy(&timestr[tgtind], HHMMSS, HOUR_LEN);

   /* Append the minute */
   tgtind = tgtind + HOUR_LEN;
   srcind = srcind + HOUR_LEN + 1; /** skip over the ':' in time field */
   strncpy(&timestr[tgtind] ,&HHMMSS[srcind], MIN_LEN); 

   /* Append the seconds */
   tgtind = tgtind + MIN_LEN;
   srcind = srcind + MIN_LEN + 1; /** skip over the ':' in time field */
   strncpy(&timestr[tgtind], &HHMMSS[srcind], SEC_LEN); 

   /* Null terminate the string */
   tgtind = tgtind + SEC_LEN;
   timestr[tgtind] = '\0';


   /* convert to double seconds */
   ret = epochsec17(starttime, timestr);
   if (ret < 0)
	{
	logit("t","bad return from epochsec17\n");
	return(-1);
	} 
   return(1);
}
/* ----------------------------------- end of t_atodbl() ----------------------------------------- */
