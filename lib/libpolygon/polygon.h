#include "db.h"
#define polyINT 1 
/** actual data are multiplied by 1e6 to save precision...*/
#define polyINTELINT 2
#define polyGSHHS 3
#define polyFLOAT 4
#define polyINTELFLOAT 5

#define default_dir "pdata"
#define default_dfile "polygons"
typedef struct Point {
	double lat;/*west - east*/
	double lon;/*south - noth*/
} Point;

int polycode (char *type);
/** returns storage type for polygons */

char *polytype(int code);
/** returns strintype for polygon storage type */

long readPolygon(Dbptr db, Point **Poly);
/** read vertices from database into array *Point */
long writePolygonData(Dbptr db, Point *poly, long npoints, char *pname, int closed, int level, char *ptype, char *auth, char *dir, char *dfile, int pcode);
	/** writes polygon data into datafile
		fills database record accordingly
		returns record number or dbINVALID
	*/


Dbptr inWhichPolygons(Dbptr db,Point P);

char *windrose(double azimuth);

int isGeographicallyInside(Point P, Point *polygon, long n);

double distance_from_polygon( Point P, Point *polygon, long n);
double distanceToPolygon( Dbptr db, Point P);
