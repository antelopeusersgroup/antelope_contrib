#pragma ident "$Id$"
/*======================================================================
 *
 * Handle mapping from digitizer packet header to sta/chan/net, etc.
 *
 *====================================================================*/
#include "rtp2orb.h"

#define DELIMITERS " \t"
#define MAX_TOKEN    10
#define BUFFER_LEN   512

#define DEFAULT_CALIB   1.0;
#define DEFAULT_CALPER -1.0;

static int nentry = 0;
static struct lookup_table {
    struct {
        UINT16 unit;
        UINT16 stream;
        UINT16 chan;
    } dt;
    IDENTIFIER ident;
    double calib;
    double calper;
    double samprate;
} *entry;

BOOL InitDB(char *dbname)
{
FILE *fp;
INT32 i, status, lineno, ntok, len=BUFFER_LEN;
char buffer[BUFFER_LEN];
char *token[MAX_TOKEN];
static char *fid = "InitDB";

    if (strlen(dbname) == 0) return TRUE;

/* Open data file */

    if ((fp = fopen(dbname, "r")) == (FILE *) NULL) {
        rtp_log(RTP_ERR, "%s: fopen: %s: %s", fid, dbname, strerror(errno));
        return FALSE;
    }

/* Read once to count how many entries */

    lineno = 0;
    nentry = 0;
    while ((status = util_getline(fp, buffer, len, '#', &lineno)) == 0) {
        if (strncmp(buffer, "//", 2) == 0) continue;
        ntok = util_parse(buffer, token, DELIMITERS, MAX_TOKEN, 0);
        if (ntok != MAX_TOKEN) {
            rtp_log(RTP_ERR, "%s: bad entry in %s, line %d",
                fid, dbname, lineno
            );
            return FALSE;
        }
        ++nentry;
    }

    if (nentry == 0) {
        rtp_log(RTP_INFO, "%s: WARNING: %s: no valid entries", fid, dbname);
        fclose(fp);
        return TRUE;
    }

/* Allocate space for everything and read it in */

    entry = (struct lookup_table *)
            malloc(nentry*sizeof(struct lookup_table));
    if (entry == (struct lookup_table *) NULL) {
        rtp_log(RTP_ERR, "%s: malloc: %s", fid, strerror(errno));
        fclose(fp);
        return FALSE;
    }

    i = 0;
    rewind(fp);
    lineno = 0;
    while ((status = util_getline(fp, buffer, len, '#', &lineno)) == 0) {
        if (strncmp(buffer, "//", 2) == 0) continue;
        ntok = util_parse(buffer, token, DELIMITERS, MAX_TOKEN, 0);
        if (ntok != MAX_TOKEN) continue;

        if (i == nentry) {
            rtp_log(RTP_ERR, "%s: UNEXPECTED ERROR #1", fid);
            free(entry);
            fclose(fp);
            return FALSE;
        }

        entry[i].dt.unit   =        (UINT16) strtoul(token[0],(char **)NULL,16);
        entry[i].dt.stream =                    atoi(token[1]) - 1;
        entry[i].dt.chan   =                    atoi(token[2]) - 1;
        memcpy((void *) entry[i].ident.sta, (void *) token[3], SNAMELEN);
        entry[i].ident.sta[SNAMELEN] = 0;
        memcpy((void *) entry[i].ident.cha, (void *) token[4], CNAMELEN);
        entry[i].ident.cha[CNAMELEN] = 0;
        memcpy((void *) entry[i].ident.net, (void *) token[5], NNAMELEN);
        entry[i].ident.net[NNAMELEN] = 0;
        memcpy((void *) entry[i].ident.loc, (void *) token[6], NNAMELEN);
        entry[i].ident.loc[LNAMELEN] = 0;
        memcpy((void *) entry[i].ident.seg, (void *) token[7], SEGLEN);
        entry[i].ident.seg[SEGLEN] = 0;
        entry[i].calib  =                       atof(token[8]);
        entry[i].calper =                       atof(token[9]);

        ++i;
    }

    if (i != nentry) {
        rtp_log(RTP_ERR, "%s: UNEXPECTED ERROR #1", fid);
        free(entry);
        fclose(fp);
        return FALSE;
    }

    fclose(fp);
    return TRUE;
}

void LookupDB(struct reftek_dt *dt, IDENTIFIER *ident, double *calib, double *calper)
{
int i;

/* Take information from the table, if we've got it */

    for (i = 0; i < nentry; i++) {
        if (
            entry[i].dt.unit   == dt->unit   &&
            entry[i].dt.stream == dt->stream &&
            entry[i].dt.chan   == dt->chan
        ) {
            *ident  = entry[i].ident;
            *calib  = entry[i].calib;
            *calper = entry[i].calper;
            return;
        }
    }

/* Otherwise derive entries from the packet header */

    sprintf(ident->sta, "%04x", dt->unit);
    sprintf(ident->cha, "%02d:%02d", dt->stream+1, dt->chan);
    sprintf(ident->net, "N?");
    sprintf(ident->loc, "L?");
    sprintf(ident->seg, "S?");
    *calib  = DEFAULT_CALIB;
    *calper = DEFAULT_CALPER;
}

/* Revision History
 *
 * $Log$
 * Revision 1.1  2004/03/09 18:28:23  vernon
 * Initial revision
 *
 */
