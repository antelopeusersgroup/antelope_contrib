#ifndef PFXML
#define PFXML
#include "pf.h"

/* SCAFFOLD */
#define PFXML_DEFAULTHEADER 1
#define PFXML_NEWLINES 2
#define PFXML_EXPLICIT_NAMESSPACE 4
#define PFXML_STANDALONE 8
#define PFXML_STRONG 16
#define PFXML_PRESERVE_PFFILE 32

#ifdef	__cplusplus
extern "C" {
#endif

extern char *pf2xml( Pf *pf, char *name, char *prolog, int flags );

#ifdef	__cplusplus
}
#endif

#endif
