sendplot(plotnum,more)
int plotnum,more;
   {
	int ok;
	int ix, iy;
	/* here we add the grid lines if requested */
	extern int grid, xmax, ymax;
	if(grid)
	   {
		for(ix=0; ix < xmax; ix += grid)
			vector(ix,0,ix,ymax);
		for(iy=0; iy < ymax; iy += grid)
			vector(0,iy,xmax,iy);
	   }
	ok= 1;
	return(ok);
   }
