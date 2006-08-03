#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>

#include "coords.h"
#include "db.h"
#include "stock.h"
#include "pf.h"
#include "tr.h"

#include "cgeom.h"

extern int Verbose;
extern int Force;

extern void free_views( Dbptr db, Tbl *views );
extern int delegate_to_matlab( Dbptr, Pf * );
extern int delegate_to_perl( Dbptr, Pf * );
extern int sp_bssa87( Dbptr, Pf * );
extern int trinetsm_es99( Dbptr, Pf * );
extern int trinetsm_es99_mmi( Dbptr, Pf * );

