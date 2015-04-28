#include <stdio.h>
#include "devpar.h"

setspeed(ispeed)
int ispeed;
{
	int i;
	char str[3];

	if (ispeed == 0) return;		/* speed 0 -> default	*/
	if (ispeed >= 1 && ispeed <= 20) {
		fputs (SPEED_CMD, stdout);
		i = utocrb (str, ispeed);
		strcpy (&str[1], "000");
		fputs (str, stdout);
	}
}
