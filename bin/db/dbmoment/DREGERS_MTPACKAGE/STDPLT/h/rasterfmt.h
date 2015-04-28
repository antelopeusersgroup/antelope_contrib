/* definition of header for raster plot data */

struct rasterheader
   {
	int rh_magic0;
	int rh_format;
	int rh_magic1;
	int rh_param1;
	int rh_magic2;
	int rh_param2;
	int rh_magic3;
	int rh_param3;
   };

#define RAST_MAGIC0	0x01234567
#define RAST_MAGIC1	0x89abcdef
#define RAST_MAGIC2	0x76543210
#define RAST_MAGIC3	0xfedcba98

#define RAST_MAGIC0SB	0x10325476
#define RAST_MAGIC1SB	0x98badcfe
#define RAST_MAGIC2SB	0x67452301
#define RAST_MAGIC3SB	0xefcdab89
