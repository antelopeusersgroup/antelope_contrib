/* dummy versions of open, close, and send routines */

opendev() { }

closedev() { }

sendplot(more)
int more;
   {
	int ok;
	ok= 1;
	return(ok);
   }
