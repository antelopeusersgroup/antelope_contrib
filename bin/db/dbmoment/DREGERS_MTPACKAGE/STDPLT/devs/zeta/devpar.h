/* Device parameters for zeta.				*/
/* NOTE: Many of these are hardware switchable.		*/

#define PIXINCH		400.0
#define INCHPIX		0.0025
#define XMAX		691200
#define YMAX		4400
#define XPAGELEN	3400
#define YPAGELEN	4400
#define COLORDEV	1
#define LABELDEV	0
#define DEVID		'z'
#define ZETA_SPOOLER	"zeta"

/*********	Initial values that can be 		*/
/*		overridden by command line options.	*/
#define DEFAULT_DRAW_FAT	0

/********* 	Device specific information.	*********/

/* Define a pagesize in pixels.				*/
#define PAGELEN		3400

/* Define parameters that are based on GML level.	*/
/* Release 5.3Z and belowo have 16 bit vector range.	*/
/* Release 5.4 and above have 20 bit vector range.	*/
#define BITRANGESIXTEEN
#ifdef BITRANGESIXTEEN
#define MAXDX		32767
#define MINDX		-32768
#else
#define MAXDX		524287
#define MINDX		-524288
#endif

/* Define color info.					*/
#define NCOLORS		8
#define PEN_UP		0
#define PEN_DOWN	1

/* Define plotter control strings.			*/
#define PLOTTER_ENABLE		"ZZZZZZZZZZ"
#define PLOTTER_DISABLE		"Z"
#define PEN_UP_CMD		"1"
#define PEN_DOWN_CMD		"2"
#define PEN_SELECT_CMD		"6"
#define END_OF_PLOT_CMD		"70"
#define SPEED_CMD		"75"
