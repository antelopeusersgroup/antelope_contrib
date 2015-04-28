boxfill () { }

linefill(x,y1,y2)
int x, y1, y2;
   {
	/* need to fill according to current pattern */
	vector(x,y1,x,y2);
   }

xlinefill(y,x1,x2)
int y, x1, x2;
   {
	/* need to fill according to current pattern */
	vector(x1,y,x2,y);
   }
