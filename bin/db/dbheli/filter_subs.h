
#include "trace_subs.h"

typedef struct filter_ {
	int type;
	double param1;
	double param2;
	double param3;
	double param4;
	int iparam1;
	int iparam2;
	int iparam3;
	int iparam4;
} Filter;

extern Trace *filter_trace (Trace *trace, char *filter, int save_oldtrace);
extern int parse_filter (char *filstr, Filter **filter);
extern int filter_tr (Trace *intrace, Trace *outtrace, Filter *filter);
