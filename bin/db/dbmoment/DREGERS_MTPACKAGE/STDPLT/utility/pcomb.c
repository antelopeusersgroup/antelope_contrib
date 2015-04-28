#include	<stdio.h>

#define MOVE		 1
#define DRAW		 2
#define LINE		 3
#define TEXT		 4
#define TEXTSIZE	 5
#define TEXTCENTER	 6
#define TEXTFONT	 7
#define TEXTANGLE	 8
#define INCLUDE		 9
#define ORIGIN		10
#define RASTER		11
#define BOX		12

struct comtable
   {
	char cname[16];
	int chash;
	int ckey;
   }	com[] = {
	"move",		0,	MOVE,
	"draw",		0,	DRAW,
	"line",		0,	LINE,
	"text",		0,	TEXT,
	"textsize",	0,	TEXTSIZE,
	"textcenter",	0,	TEXTCENTER,
	"textfont",	0,	TEXTFONT,
	"textangle",	0,	TEXTANGLE,
	"include",	0,	INCLUDE,
	"origin",	0,	ORIGIN,
	"raster",	0,	RASTER,
	"box",		0,	BOX,
	"",		0,	0 };

main(ac,av)
int ac; char **av;
   {
	ncom= buildcomtable();

	while( (n=getline(line)) != EOF)
	   {
		if(n == 0) continue;
		narg= parse(line,args);
