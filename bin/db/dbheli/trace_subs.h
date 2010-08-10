
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "tr.h"
#include "db.h"
#include "arrays.h"
#include "scv2.h"

extern int write_trace (Dbptr db, char *sta, char *chan, char *dir, char *dfile, Trace *trace, double tstart, double tend, int overwrite);
extern Trace *convert_trace (Trace *trace, char *format);
extern Trace *copy_trace (Trace *trace, int copydata);
extern int decimate_trace (Trace *trace, Tbl *ncoefs, Tbl *coefs, Tbl *dec_fac, double tref);
extern int convsym (float *datain, int nsin, float *coefs, int ncoefs, int decfac, int ioff, int *nsout, float *dataout);
extern int add_trace (Dbptr db, double tstart, double tend, Trace *trace, Trace **traceo);
extern Trace *off_read_trace (Dbptr db, double tstart, double tend);
extern int read_file (char *fname, long foff, char *datatype, long *nsamps, void **buf);
extern int zaccess (char *path, int mode);
extern int wf_read_idacompress (char *file, long foff, int nbytes, int sampoff, long nsamps, int size_samp, void *buf);
extern Trace *read_trace (Dbptr db, double tstart, double tend);
