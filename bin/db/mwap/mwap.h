/* This use of these global variables is ugly, but a necessary evil
for now.  They are used in a tcl/tk gui as substitutes for
stdin/stdout.
This could have been avoided with perl which allows a script to be
connected to stdin and stdout of a program but tcl does not have this
capability.  These are file pointers for the input and output streams
from
mwap.  Note they are the reverse ina gui.*/
extern FILE *MWpin,*MWpout;

/* function prototypes */
int  MWcompute_array_particle_motion(MWgather **, int, MWstack *,
	double, double *, double *, Arr **, Arr **, Arr **, Arr **);
MWstack *MWcompute_arrival_times(MWgather **,int , double,
	double *, Spherical_Coordinate, Time_Window,
	Arr *, char *,
	Arr **, Arr **, double *, double *, int *);
void mwap_process(Dbptr, char *, Pf *);
void copy_arrival_array(Arr *,Arr **);
