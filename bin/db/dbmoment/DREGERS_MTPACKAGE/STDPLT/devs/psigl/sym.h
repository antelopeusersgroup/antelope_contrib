#define SYMDIR "/u1/clay/subs/stdplt/symboldata/"
#define SYMFILE "symfile"
#define NAMEFILE "namefile"

struct symnames {
	char *name; 
	int index;
};


struct symlist {
	int length;
	int centerx;
	int centery;
	struct intpolygon *vertices;
};

