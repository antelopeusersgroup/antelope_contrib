/* constants for err processing */
#define FATAL	1
#define WARN	2
#define CONT	3

struct intpolygon {
	short ixv;
	short iyv;
};

struct pentypes
   {
	unsigned char pen_bwflags;
	unsigned char pen_cflags;
	unsigned char pen_color;
	unsigned char pen_fat;
	unsigned char pen_cfat;
	unsigned char pen_idash;
	unsigned char pen_cdash;
	unsigned char pen_mode;
   };

struct brushtypes
   {
	unsigned char brush_bwflags;
	unsigned char brush_cflags;
	unsigned char brush_color;
	unsigned char brush_ipat;
	unsigned char brush_mode;
   };


