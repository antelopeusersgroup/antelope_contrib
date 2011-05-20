
/****
  db2sac

  This program creates a CSS database with waveform files in SAC format.

  Author: Eric Winkelman  ewink@lemond.Colorado.edu  6/18/92
  Drastically modified: Daniel Quinlan danq@brtt.com 9/8/93
  Modified again: Danny Harvey danny@brtt.com 5/2/94
  ****/

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>

#include "elog.h"
#include "scv2.h"
#include "dbl2.h"
#include "ahsac.h"

#define	MAXSTR        (1024)
#define CSS_28        (1)
#define CSS_30        (2)


#define	STREQ(a, b) \
  (strcmp((a), (b)) == 0)

static option_t option_list[] =
{
    's', "sc", NORMAL_ARG, 
    'b', "ts", NORMAL_ARG,
    'e', "te", NORMAL_ARG,
    'g', "gap", NORMAL_ARG,
    'w', "wfdir", NORMAL_ARG,
    'c', "counts", NO_ARGUMENT,
    'v', "version", NO_ARGUMENT,
    'h', "help", NO_ARGUMENT,
    'i', "intel", NO_ARGUMENT,
};

#define NUM_OPTIONS (sizeof(option_list) / sizeof(option_t))

void
usage() 
{
        fprintf(stderr, 
	        "\nUsage: %s [-i] [-sc stachan ] [-ts start-time] [-te end-time] [-w wfdir ]\n",
		Program_Name); 
        fprintf(stderr, 
	        "             [-gap {none|zero|interp}] [-counts] dbin dbout\n");
	banner(Program_Name, 0L) ;
        exit(1); 
} 

extern char    *Program_Name;	       /* Required for error messages...  */

int             Debug_Level	       /* Adjust verbosity by this amount  */
  = 0;


int
main(int argc, char **argv)
{
    char            option	       /* Terse option specifier  */
      = NULL;
    char           *input_name	       /* Name of the input file  */
      = "";
    char           *output_name	       /* Name of the output file  */
      = "";
    char           *wfdir_name	       /* Name of the wf directory  */

#ifdef SAC
      = "sac";
#else
      = "ah";
#endif

    static char fixgaps[64] = "zero";
    int counts = 0;
    double epoch, tstrt=-9e35, tnd=9e35 ; 
    char tstart[25], tend[25] ; 
    int	intel = 0 ; /* flag to write intel order files */

    char    *Stachan_sift= "", 
	    *Tstart_sift= "", 
	    *Tend_sift= "" ; 

    Dbptr	  dbin, dbout ;
    SCV           **scvs;
    int             nscvs;
    int             scv_index;

    Program_Name = argv[0];

    while ((option = get_option(&argc, argv, option_list, NUM_OPTIONS))
	   != NULL)
      {
	switch (option)
	  {

	case 'b' : 
	    epoch = str2epoch ( get_argument() ) ; 
	    tstrt = epoch ;
	    sprintf ( tstart, "%.5f", epoch ) ;
	    Tstart_sift = tstart ; 
	    break ;

	case 'e' : 
	    epoch = str2epoch ( get_argument() ) ; 
	    tnd = epoch ;
	    sprintf ( tend, "%.5f", epoch ) ;
	    Tend_sift = tend ; 
	    break ;

	case 'i':
	    intel = 1 ; 
	    break ; 

	case 's' : 
	    Stachan_sift = get_argument() ; 
	    break ;

	case ('w'):
	    wfdir_name = get_argument() ; 
	    break;
	case ('g'):
            strcpy (fixgaps, get_argument());
	    break;
	case ('c'):
	    counts = 1 ;
	    break;
	case ('h'):
	    elog_clear_register(1);
	    usage();
	case ('V'):
	case ('v'):
	    banner(Program_Name, 0L) ;
	    exit(0);
	default:
	    usage();
	  }
      }

    input_name = get_argument () ; 
    output_name = get_argument () ; 

    if ( input_name == 0 || output_name == 0 || get_argument() != 0 ) 
      {
	usage();
      }

    dbopen ( input_name, "r", &dbin ) ; 
    dbopen ( output_name, "r+", &dbout ) ; 

    scvs = SCV_create("css3.0", 1, &input_name, Stachan_sift, 
    	Tstart_sift, Tend_sift, 1, &nscvs);
    if ((scvs == NULL) || (nscvs == 0))
      {
	elog_clear_register(1);
	elog_die(1, "No corresponding waveform data found");
      }

    for (scv_index = 0; scv_index < nscvs; scv_index++)
      {
	/****  Move the waveform files, and fix up file links.  ****/
#ifdef SAC
	write_sac(scvs[scv_index], dbin, dbout, tstrt, tnd, intel, fixgaps, counts, wfdir_name );
#else
	write_ah(scvs[scv_index], dbin, dbout, tstrt, tnd, fixgaps, counts, wfdir_name );
#endif
	SCV_free(scvs[scv_index]);
      }

    return 0;
}
