// Writes dx test file to output stream out
#include <iostream>
#include "gclgrid.h"

void dx_output(GCLscalarfield3d& g, ostream& out)
{
	int i,j,k;
	int ncoords=g.n1*g.n2*g.n3;
	out << "object 1 class array type float rank 1 shape 3 items "
		<< ncoords << " data follows" <<endl;
	for(i=0;i<g.n1;++i)
		for(j=0;j<g.n2;++j)
			for(k=0;k<g.n3;++k)
				out << g.x1[i][j][k] << " "
					<< g.x2[i][j][k] << " "
					<< g.x3[i][j][k] << endl;
	out << "object 2 class gridconnections counts "
		<< g.n1 << " "
		<< g.n2 << " "
		<< g.n3 << endl;
	out << "object 3 class array type float rank 0 items "
		<< ncoords << " data follows" << endl;
	for(i=0;i<g.n1;++i)
		for(j=0;j<g.n2;++j)
			for(k=0;k<g.n3;++k)
				out << g.val[i][j][k] << endl;
	out << "attribute \"dep\" string \"positions\" " << endl;
	out << "object \"irreg positions regular connections\" class field" << endl;
	out << "component \"positions\" value 1" << endl;
	out << "component \"connections\" value 2" << endl;
	out << "component \"data\" value 3" << endl;
	out << "end" << endl;
}
