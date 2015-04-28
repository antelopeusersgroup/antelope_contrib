/* Open and close routines. */

#include <stdio.h>
#include "devpar.h"

char output_file[L_tmpnam];

extern int speed;
extern int auto_lpr;

opendev()
{
/* Initialize */
	if (auto_lpr) {
		if (tmpnam(output_file) == NULL) {
			fprintf (stderr, "Unable to generate tmp filename.\n");
			exit(1);
		}
		if (freopen (output_file, "w", stdout) == NULL) {
			fprintf (stderr, "Unable to open zeta output %s.\n",
				output_file);
			exit(1);
		}
	}
	fputs (PLOTTER_ENABLE, stdout);
	setcolor(0);
	setspeed(speed);
}
	

closedev() 
{
	char lpr_cmd[80];
	int status;

	/* Output the current plot.  Output the PLOTTER_DISABLE string.	*/
	fputs (PEN_DOWN_CMD, stdout);
	fputs (END_OF_PLOT_CMD, stdout);
	fputs (PLOTTER_DISABLE, stdout);
	if (auto_lpr) {
		fclose (stdout);
		sprintf (lpr_cmd, "lpr -P%s -r %s", ZETA_SPOOLER, output_file);
		if ((status = system (lpr_cmd)) != 0) 
			fprintf (stderr, "error %d while spooling output.\n",
				status);
	}
}
