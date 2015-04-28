/* New data types. Used in every file.
 */


typedef long PatRow;	/* Brush pattern row (32 bit integer) */

struct CmdEntry {	/* For building command table */
  int	code;		/*    Binary code (0 is the list terminator) */
  char	*name;		/*    Command name (for error messages) */
  int	format;		/*    Parse format (0..15)		*/
};

struct Cmd {		/* For code-keyed command table */
  char  *name;
  int	format;
};

struct PenType {	/* For the pen table. */
  int	fat;		/*    Virtual pixels: special case: 0 = 1 */
  int	dash;		/*    Dash index */
  int	mode;		/*    PENBLACK, PENXOR or PENWHITE */
};

struct BrushType {	/* For the brush table */
  int	pat;		/*    Index to pattern table */
  int	mode;		/*    BRUSHOR, BRUSHXOR, BRUSHEQU, BRUSHWHITE */
};

struct vert {		/* Polygon vertex */
  float x, y;
};
