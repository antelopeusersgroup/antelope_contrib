/* from Kernighan/ Pike */
#include	<stdio.h>

char progname[] = "ttystuff";

ttyin()
{
	char buf[10];
	FILE *efopen();
	static FILE *tty = NULL;

	if (tty == NULL)
		tty = efopen("/dev/tty","r");
	if (fgets(buf, 10, tty) == NULL || buf[0] == 'q')
		exit(0);
	else
		return(buf[0]);
}

FILE *efopen(file,mode)
char *file, *mode;
{
	FILE *fp, *fopen();

	if ((fp = fopen(file,mode)) != NULL)
		return fp;
	fprintf(stderr,"%s can't open file %s mode %s\n",progname , file ,mode);
	exit(-1);
}



