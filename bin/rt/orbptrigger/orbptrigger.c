/***************************************************************************
 * orbptrigger.c:
 * An module to execute a system call when specified packets are read from
 * an ORB.  Will also pass the contents of parameters/fields in either
 * parameter file or database table packets by subsituting for the
 * parameters denoted on the command line (see the man page).
 *
 * Substitutions using '%' delimiters (i.e. %evid%) are for fields and
 * parameters directly in the packet read from the orb.
 * Substitutions using '@' delimiters (i.e. @origin.event@ ) are for fields
 * inside database rows inside a pf packet.  For example an 'evid' field in
 * an origin db row inside a /pf/orbmag parameter file packet.
 * (see the man page)
 *
 * Written by Chad Trabant, ORFEUS/EC-Project MEREDIAN
 *
 ***************************************************************************/

#include <signal.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include "tr.h"
#include "orb.h"
#include "db.h"
#include "Pkt.h"
#include "coords.h"
#include "bury.h"

#include "strutils.h"

#define  MAXCMDSIZE 400		/* Maximum size of final system call */
#define  MAXSUBS 10		/* Maximum number of variables to substitute */

static void usage (void);
int isalldig (char *check);
void dummy_sighandler (int sig);
void term_sighandler (int sig);

short int stopsig = 0;
char *version = "1.4a (2015.022)";
char *statefile = 0;
int orb = 0;

typedef struct subst
{				/* To facilitate variable substitution */
  int beg;
  int end;
  int type;			/* type: 1:parameter/field, 2:db row in a pf */
  char param[50];
  char substitution[MAXCMDSIZE];
}
subst;

int main (int argc, char **argv)
{
  long polltime = 50000;	/* loop usleep interval (us) */
  char *select = 0;		/* Selected packet */
  char *orbaddr = 0;		/* Location of the ORB, in host:port format */
  char *startat = 0;		/* Where to start in the the ORB */
  char scpre[200];		/* The preliminary system call */
  char scfinal[MAXCMDSIZE];	/* The final system call (after substitution) */

  int background = 0;		/* Start system call in the background? */
  int pktcount = -1;		/* How many packets to process */
  int argoff = 0;
  int orbret;
  int i, j, k, l;		/* Ugly names, used for substitution processing */
  int beg, end, psize;
  int length = 0;
  int subreq = 0;
  int optind;
  double tepoch;
  struct subst *subarr[MAXSUBS];	/* Up to 10 substitutions */

  /* For use with the statefile */
  int last_pktid;
  double last_pkttime;

  /* Parameters filled/used by orbreap() */
  int pktid;
  char *srcname;
  double time;
  char *packet = NULL;
  int nbytes = 0;
  int bufsize = 0;

  /* Signal handling, use POSIX calls with standardized semantics */
  struct sigaction sa;

  sa.sa_handler = dummy_sighandler;
  sa.sa_flags = SA_RESTART;
  sigemptyset (&sa.sa_mask);
  sigaction (SIGALRM, &sa, NULL);

  sa.sa_handler = term_sighandler;
  sigaction (SIGINT, &sa, NULL);
  sigaction (SIGQUIT, &sa, NULL);
  sigaction (SIGTERM, &sa, NULL);

  sa.sa_handler = SIG_IGN;
  sigaction (SIGHUP, &sa, NULL);
  sigaction (SIGPIPE, &sa, NULL);

  /* Initialization */
  Program_Name = argv[0];
  elog_init (argc, argv);
  *scpre = '\0';
  *scfinal = '\0';

  /* Process command line arguments */
  for (optind = 1; optind < argc; optind++)
    {

      if (*(argv[optind]) != '-')
	break;

      if (!strcmp (argv[optind], "-select"))
	{
	  select = argv[++optind];
	  argoff += 2;
	  continue;
	}

      if (!strcmp (argv[optind], "-background"))
	{
	  background++;
	  argoff++;
	  continue;
	}

      if (!strcmp (argv[optind], "-start"))
	{
	  startat = argv[++optind];
	  argoff += 2;
	  continue;
	}

      if (!strcmp (argv[optind], "-state"))
	{
	  statefile = argv[++optind];
	  argoff += 2;
	  continue;
	}

      if (!strcmp (argv[optind], "-number"))
	{
	  pktcount = atoi (argv[++optind]);
	  argoff += 2;
	  continue;
	}
    }

  if ((argc - optind) < 2)
    usage ();

  elog_notify (0, "%s version %s\n", Program_Name, version);

  /* Fill in a default packet srcname */
  if (!select)
    select = strdup ("/db/origin");

  orbaddr = argv[argoff + 1];

  /* Build the preliminary system call string */
  for (i = (argoff + 2); i < argc; i++)
    {
      local_strlcat (scpre, argv[i], sizeof (scpre));
      local_strlcat (scpre, " ", sizeof (scpre));
    }

  /* NULL the substitution array pointers */
  for (i = 0; i < MAXSUBS; i++)
    subarr[i] = NULL;

  /* Just in case I ever have to think about it again...
     i = the index in the scpre string (original system call)
     j = number of '%' delimeters encountered
     k = number of '@' delimeters encountered
     l = total number of substitutions encountered
   */

  length = strlen (scpre);
  /* Check for variable substitution and build the replacement structs */
  for (i = 0, j = 0, k = 0, l = 0; i < length; i++)
    {

      if (scpre[i] == '%' || scpre[i] == '@')
	{
	  if (l >= MAXSUBS)
	    elog_die (0, "no more than %d substitutions", MAXSUBS);

	  if (scpre[i] == '%')
	    {			/* A '%%' substitution */
	      if ((k % 2) != 0)
		elog_die (0, "Cannot mix substituition types!");

	      j++;
	      if ((j % 2) != 0)
		{		/* Beginning of substitution */
		  subarr[l] = malloc (sizeof (struct subst));
		  subarr[l]->beg = i;
		  subarr[l]->type = 1;
		  *subarr[l]->substitution = '\0';
		  beg = i;
		}
	      else
		{		/* End of substitution */
		  subarr[l]->end = i;
		  end = i;
		  psize = ((end - beg) - 1 < 50) ? ((end - beg) - 1) : 50;
		  if (psize == 0)
		    {
		      elog_die (0, "Empty substitution detected");
		    }

		  if (psize >= sizeof (subarr[l]->param))
		    {
		      elog_die (0, "Substitution parameter too large\n");
		    }

		  local_strlcpy (subarr[l]->param,
				 &scpre[beg + 1], psize + 1);
		  l++;
		}
	    }			/* End of '%%' substitution */

	  else
	    {			/* A '@@' substitution */
	      if ((j % 2) != 0)
		elog_die (0, "Cannot mix substituition types!");

	      k++;
	      if ((k % 2) != 0)
		{		/* Beginning of substitution */
		  subarr[l] = malloc (sizeof (struct subst));
		  subarr[l]->beg = i;
		  subarr[l]->type = 2;
		  *subarr[l]->substitution = '\0';
		  beg = i;
		}
	      else
		{		/* End of substitution */
		  subarr[l]->end = i;
		  end = i;
		  psize = ((end - beg) - 1 < 50) ? ((end - beg) - 1) : 50;
		  if (psize == 0)
		    {
		      elog_die (0, "Empty substitution detected");
		    }

		  if (psize >= sizeof (subarr[l]->param))
		    {
		      elog_die (0, "Substitution parameter too large\n");
		    }

		  local_strlcpy (subarr[l]->param,
				 &scpre[beg + 1], psize + 1);

		  /* Make sure there is a '.' separating table and field */
		  if (strchr (subarr[l]->param, '.') == NULL)
		    {
		      elog_die (0,
				"Malformed substitution: '%s', must be 'table.field'\n",
				subarr[l]->param);
		    }
		  l++;
		}
	    }			/* End of '@@' substitution */
	}			/* END if a substitution character */
    }				/* End for each character in the system call string */

  if ((j && (j % 2) != 0) || (k && (k % 2) != 0))
    {
      elog_die (0, "Unmatched substitution delimiter, '%%' or '@'");
    }
  else if (j || k)
    subreq = 1;			/* Substitution will be performed */

  /* DEBUG - Print out the substitution array */
  /*
     for ( l=0; subarr[l] != NULL; l++ ) {
     printf("SubArr %d\n", l);
     printf("  beg: %d -> end: %d\n", subarr[l]->beg, subarr[l]->end);
     printf("  type: %d\n", subarr[l]->type);
     printf("  param: \'%s\' -> \'%s\'\n", subarr[l]->param,
     subarr[l]->substitution);
     }
   */

  /* ORB setup */
  orb = orbopen (orbaddr, "r&");
  if (orb < 0)
    {
      elog_die (0, "%s: orbopen() error for %s\n", Program_Name, orbaddr);
    }

  /* Read/setup the state file if supplied */
  if (statefile)
    {
      if ((exhume (statefile, 0, 0, 0)) < 0)
	{
	  elog_complain (0, "exhume() of '%s' failed\n", statefile);
	}
      if ((orbresurrect (orb, &last_pktid, &last_pkttime)) >= 0)
	{
	  elog_notify (0, "ORB position resurrected\n");
	}
      else
	{
	  elog_notify (0, "ORB position resurrection unsuccessful\n");
	}
    }

  /* Init the ORB position if '-start' is given */
  if (startat != 0 && isalldig (startat))
    {				/* if all digits assume pktid */
      if (orbseek (orb, atoi (startat)) == -1)
	{
	  elog_notify (0, "orbseek() error");
	}
    }
  else if (startat != 0)
    {
      if (is_epoch_string (startat, &tepoch))
	{
	  orbposition (orb, startat);
	}
    }
  orbselect (orb, select);
  srcname = (char *) malloc (100);

  /* Start the primary loop  */
  while (stopsig == 0 && pktcount != 0)
    {

      orbret = orbreap (orb, &pktid, srcname, &time, &packet,
			&nbytes, &bufsize);
      if (orbret < 0)
	{
	  elog_complain (0, "orbreap() error\n");
	  stopsig = 1;
	}

      else
	{			/* Process the packet */

	  last_pktid = pktid;
	  last_pkttime = time;
	  if (pktcount > 0)
	    pktcount--;

	  if (subreq)
	    {			/* If substitution should be applied */
	      struct Packet *Pkt = NULL;
		  int my_pkttype=unstuffPkt (srcname, time, packet, nbytes, &Pkt);

	      /* Process a database table packet */
	      if ( my_pkttype == Pkt_db )
		{		/* If a DB table packet */
		  /* Find packet values to substitute */
		  for (l = 0; subarr[l] != NULL; l++)
		    {
		      Dbptr tdb;
		      char tstr[MAXCMDSIZE];
		      int nspc;

		      if (subarr[l]->type == 2)
			{
			  elog_complain (0,
					 "cannot use @@ substitution for a db packet!");
			  continue;
			}

		      /* Find the pointer for the needed field */
		      tdb = dblookup (Pkt->db, 0, 0, subarr[l]->param, 0);
		      if (dbget (tdb, tstr) == dbINVALID)
			{
			  elog_complain (0,
					 "dbget(): field '%s' not found in '%s' packet",
					 subarr[l]->param, srcname);
			  *subarr[l]->substitution = '\0';
			}
		      else
			{
			  /* Remove spaces at the beginning */
			  nspc = strspn (tstr, " ");
			  local_strlcpy (subarr[l]->substitution, &tstr[nspc],
					 MAXCMDSIZE);
			}
		    }
		}

	      /* Process parameter file packet */
	      else if (my_pkttype == Pkt_pf) 
		{		/* If a PF packet */

		  /* Find packet values to substitute */
		  for (l = 0; subarr[l] != NULL; l++)
		    {
		      Dbptr tdb;
		      char tstr[MAXCMDSIZE];
		      char *pfstring;
		      char *record;
		      char table[50];
		      char field[50];
		      int period;

		      if (subarr[l]->type == 2)
			{	/* A '@@' substitution */
			  period = strcspn (subarr[l]->param, ".");
			  local_strlcpy (table, subarr[l]->param, period + 1);
			  strcpy (field, subarr[l]->param + (period + 1));

			  /* Extract the DB record */
			  if ((record = pfget_string (Pkt->pf, table)) == 0)
			    {
			      elog_complain (0,
					     "db table '%s' not found in '%s' packet",
					     table, srcname);
			      continue;
			    }

			  /* Assume rt1.0 schema and generate a scratch DB */
			  tdb = dbtmp ("css3.0");
			  tdb = dblookup (tdb, 0, table, 0, 0);
			  tdb.record = dbSCRATCH;

			  /* Insert the record into a scratch DB */
			  if (dbput (tdb, record) == dbINVALID)
			    {
			      elog_complain (0, "dbput error for table '%s'",
					     table);
                  dbclose(tdb);
			      continue;
			    }

			  /* Find the pointer for the needed field */
			  tdb = dblookup (tdb, 0, 0, field, 0);

			  if (dbget (tdb, tstr) == dbINVALID)
			    {
			      elog_complain (0,
					     "dbget(): field '%s' not found in '%s' packet",
					     field, srcname);
			      *subarr[l]->substitution = '\0';
                  dbclose(tdb);
			      continue;
			    }
			  else
			    {
			      /* Remove spaces at the beginning */
			      period = strspn (tstr, " ");
			      local_strlcpy (subarr[l]->substitution,
					     &tstr[period], MAXCMDSIZE);
                  dbclose(tdb);
			    }
			}

		      else
			{	/* A '%%' substitution */
			  pfstring = pfget_string (Pkt->pf, subarr[l]->param);
			  if (pfstring == 0)
			    {
			      elog_notify (0,
					   "parameter '%s' not found in '%s' packet",
					   subarr[l]->param, srcname);
			      *subarr[l]->substitution = '\0';
			    }
			  else
			    local_strlcpy (subarr[l]->substitution, pfstring,
					   MAXCMDSIZE);
			}
		    }
		}		/* End of processing a pf packet */

	      else
		{
		  elog_notify (0, "Cannot substitute, not a DB or PF packet");
		}

	      /* Build the sys. call string, i.e. do the actual substitution */
	      *scfinal = '\0';
	      for (i = 0, l = 0; subarr[l] != NULL; l++)
		{

		  /* Bounds checking */
		  if (((subarr[l]->beg - i) + strlen (scfinal)) > MAXCMDSIZE)
		    {
		      break;
		    }
		  strncat (scfinal, &scpre[i], subarr[l]->beg - i);
		  i = subarr[l]->end + 1;

		  local_strlcat (scfinal, subarr[l]->substitution,
				 MAXCMDSIZE);
		}

	      /* Stick on any stuff trailing the last substitution */
	      if (i < strlen (scpre))
		{
		  local_strlcat (scfinal, &scpre[i], MAXCMDSIZE);
		}

	    }			/* End of substitution processing */

	  else
	    {			/* No substitution */
	      local_strlcpy (scfinal, scpre, MAXCMDSIZE);
	    }

	  /* Add an '&' so the system call is in the background if requested */
	  if (background)
	    {
	      if (strlen (scfinal) >= MAXCMDSIZE)
		scfinal[MAXCMDSIZE - 1] = '&';
	      else
		local_strlcat (scfinal, "&", MAXCMDSIZE);
	    }

	  /* Make the actual system call */
	  elog_notify (0, "pktid %d (%s), executing: %s", pktid, srcname,
		       scfinal);
	  if (system (scfinal) == -1)
	    elog_complain (0, "system() error: %s", strerror (errno));

	}			/* End of processing packet */

      /* ZZzzzz... */
      usleep (polltime);

    }				/* End of primary while loop */

  /* Getting ready to exit */

  if (statefile)
    {
      if ((bury ()) < 0)
	elog_complain (0, "error saving ORB position to state file\n");
      else
	elog_notify (0, "ORB position saved to state file\n");
    }

  orbclose (orb);

  return 0;
}				/* End of main() */


/* Simple check for a string with all digits */
int
isalldig (char *check)
{
  int i;
  for (i = 0; i < strlen (check); i++)
    {
      if (strchr ("0123456789", check[i]) == NULL)
	return 0;
    }
  return 1;
}


/* Signal handler routines */
void
term_sighandler (int sig)
{
  /* Bury the state information */
  if (statefile)
    {
      if ((bury ()) < 0)
	elog_complain (0, "error saving ORB position to state file\n");
      else
	elog_notify (0, "ORB position saved to state file\n");
    }

  /* Close the ORB connection */
  orbclose (orb);

  exit (0);
}

void
dummy_sighandler (int sig)
{
}


static void
usage (void)
{
  printf("%s version %s\n", Program_Name, version);
  printf("Usage: orbptrigger [-select packet] [-start {pktid|time}] [-number number]\n"
         "                   [-state file] [-background] ORB command ...\n"
         "\n"
         "Antelope Contributed Software\n"
         "\n"
         "Chad Trabant\n"
         "ORFEUS/EC-Project MEREDIAN\n"
         "\n"
         "Please report problems to trabant@knmi.nl\n"
         "\n");
  
  exit (1);
}
