void cascade_grid_locate(Tbl *attbl, Tbl *utbl,
        Gridloc_options o,
        Tbl **convergence_history,
        Tbl **restbl)
{
	int i;
	double r1,r2;  /* grid range that is adjusted dynamically*/
	Point *p;  /* Grid of points searched.  */

	p = (Point *) calloc((o.nr1)*(o.nr2)*(o.nz), sizeof(Point));
	if(p == NULL)
		die(1,"cascade_grid_locate cannot alloc array of gridpoints\n");

	for(i=0,r1=o.r1,r2=o.r2;i<ncyles;++i,r1*=o.multiplier,r2*=o.multiplier)
	{
		switch(o.gridtype)
		{
		case(POLAR):
		case(LAT_LON_GRID):
		case(CARTESIAN_GRID):

/* $Id$ */
