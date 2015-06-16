#pragma ident "$Id$"
#ifndef rtp2orb_h_included
#define rtp2orb_h_included

#include "rtp.h"
#include "reftek.h"
#include "util.h"

extern char *ReleaseIdent;

/* Module idents for meaningful exit codes */

#define RTP2ORB_MOD_MAIN  ((UINT32) 1000)
#define RTP2ORB_MOD_DB    ((UINT32) 4000)
#define RTP2ORB_MOD_ORB   ((UINT32) 4000)
#define RTP2ORB_MOD_SRATE ((UINT32) 6000)

#define SNAMELEN 7
#define CNAMELEN 7
#define NNAMELEN 7
#define LNAMELEN 7
#define SEGLEN   3

typedef struct {
    char sta[SNAMELEN+1];
    char cha[CNAMELEN+1];
    char net[NNAMELEN+1];
    char loc[LNAMELEN+1];
    char seg[SEGLEN+1];
} IDENTIFIER;

/* Run time parameters */

typedef struct {
    int fd;
    char host[MAXPATHLEN];
    int port;
    char ident[MAXPATHLEN+20];
} SERVER;

typedef struct {
    SERVER rtpd;
    SERVER orb;
    CHAR dbname[MAXPATHLEN];
    struct rtp_attr attr;
    BOOL UseLoc;
} PARAM;

/* Function prototypes */

/* db.c */
BOOL InitDB(char *dbname);
void LookupDB(struct reftek_dt *dt, IDENTIFIER *ident, double *calib, double *calper);

/* main.c */
void GracefulExit(int status);

/* orb.c */
void InitPkt();
void WriteToOrb(SERVER *orb, UINT8 *pkt, int verbose, BOOL UseLoc);

/* par.c */
BOOL ReadPar(char *prog, char *pfpath, PARAM *par);
void LogPar(char *prog, char *pfpath, PARAM *par);

/* srate.c */
BOOL DeriveSampleRate(struct reftek_dt *dt, double *output);

#endif  /* rtp2orb_h_included */

/* Revision History
 *
 * $Log$
 * Revision 1.1  2004/03/09 18:28:23  vernon
 * Initial revision
 *
 */
