#include	<stdio.h>
#include	<signal.h>
#include	"../../h/igl.h"
#include	"../com/global.h"

int pausetime;
int do_pause;

main(ac,av)
int ac; char **av;
   {
	int more, ok,plotnum;
	extern char *devname;
	extern int closedev();
	extern int labeldev;
	extern int do_label;

	devname= *av;	/* for error processing */
	signal(SIGTERM,closedev);

	setpar(ac,av);
	get_std_par();	/* get standard parameters */
	get_spc_par();	/* get special parameters */
	endpar();

	/*setbuf(stdin,BUFSIZ);*/
	opendev();
	plotnum = 0;
	do
	   {
		plotnum++;
		newplot();
		initcolortable();
			/*init first 8 values to values in globalvar.c*/ 
		more= readcom(stdin,1);
	 	if (labeldev & do_label) labelplot();   
		ok= sendplot(plotnum,more);
	   }	while( more && ok );

	closedev();
	return(0);
   }
