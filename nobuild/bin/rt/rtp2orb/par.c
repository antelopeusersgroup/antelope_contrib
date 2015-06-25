#pragma ident "$Id$"
/*======================================================================
 *
 *  Read import_rtp.d configuration file
 *
 *====================================================================*/
#include "rtp2orb.h"

#define DELIMITERS " \t"
#define MAX_TOKEN    32

static BOOL fail(char *prog, FILE *fp, char *pfpath, char *token, INT32 lineno)
{
    fprintf(stderr, "%s: FATAL ERROR: error at line ", prog);
    fprintf(stderr, "%ld, file `%s', token `%s'\n", lineno, pfpath, token);
    fclose(fp);
    return FALSE;
}

#define BUFFER_LEN (MAXPATHLEN*2)

BOOL ReadPar(char *prog, char *pfpath, PARAM *par)
{
INT32 len = BUFFER_LEN;
FILE *fp;
UINT16 status, ntok;
INT32 lineno;
char *token[MAX_TOKEN];
static char buffer[BUFFER_LEN];

/* Initialize parameter structure */

    par->dbname[0]        = 0;
    par->rtpd.port        = -1;
    par->orb.port         = -1;
    par->attr             = RTP_DEFAULT_ATTR;
    par->attr.at_block    = TRUE;
    par->UseLoc           = TRUE;

/* Open configuration file */

    if ((fp = fopen(pfpath, "r")) == (FILE *) NULL) {
        fprintf(stderr, "%s: FATAL ERROR: fopen: ", prog);
        perror(pfpath);
        return FALSE;
    }

/* Read configuration file */

    lineno = 0;
    while ((status = util_getline(fp, buffer, len, '#', &lineno)) == 0) {

        ntok = util_parse(buffer, token, DELIMITERS, MAX_TOKEN, 0);

        if (strcasecmp(token[0], "ORB") == 0) {
            if (ntok != 3) return fail(prog,fp,pfpath,token[0],lineno);
            memcpy(par->orb.host, token[1], MAXPATHLEN),
            par->orb.host[MAXPATHLEN] = 0;
            if ((par->orb.port = atoi(token[2])) <= 0) return fail(prog,fp,pfpath,token[2],lineno);
            sprintf(par->orb.ident, "%s:%d", par->orb.host, par->orb.port);

        } else if (strcasecmp(token[0], "RTPD") == 0) {
            if (ntok != 3) return fail(prog,fp,pfpath,token[0],lineno);
            memcpy(par->rtpd.host, token[1], MAXPATHLEN),
            par->rtpd.host[MAXPATHLEN] = 0;
            if ((par->rtpd.port = atoi(token[2])) <= 0) return fail(prog,fp,pfpath,token[2],lineno);
            sprintf(par->rtpd.ident, "%s:%d", par->rtpd.host, par->rtpd.port);

        } else if (strcasecmp(token[0], "Database") == 0) {
            memcpy(par->dbname, token[1], MAXPATHLEN),
            par->dbname[MAXPATHLEN] = 0;

        } else if (strcasecmp(token[0], "UseLoc") == 0) {
            if (strcasecmp(token[1], "yes") == 0) {
                par->UseLoc = TRUE;
            } else if (strcasecmp(token[1], "no") == 0) {
                par->UseLoc = FALSE;
            }

        } else if (strcasecmp(token[0], "DASid") == 0) {
            if (ntok != 2) return fail(prog,fp,pfpath,token[0],lineno);
            par->attr.at_dasid = (UINT32) atoi(token[1]);

        } else if (strcasecmp(token[0], "StrMask") == 0) {
            if (!rtp_encode_smask(&token[1], (WORD) (ntok - 1), &par->attr.at_smask)) {
                fprintf(stderr, "%s: FATAL ERROR: %s: illegal ", prog, pfpath);
                fprintf(stderr, "StrMask specification\n");
                fclose(fp);
                return FALSE;
            }

        } else {
            fprintf(stderr, "%s: FATAL ERROR: %s: unrecognized ", prog, pfpath);
            fprintf(stderr, "token `%s'\n", token[0]);
            fclose(fp);
            return FALSE;
        }
    }

    fclose(fp);

    if (status != 1) {
        fprintf(stderr, "%s: FATAL ERROR: %s: ", prog, pfpath);
        perror("read");
        return FALSE;
    }

    if (par->orb.port < 0) {
        fprintf(stderr, "%s: FATAL ERROR: %s: ", prog, pfpath);
        fprintf(stderr, "missing ORB specification\n");
        return FALSE;
    }

    if (par->rtpd.port < 0) {
        fprintf(stderr, "%s: FATAL ERROR: %s: ", prog, pfpath);
        fprintf(stderr, "missing RTPD specification\n");
        return FALSE;
    }

    return TRUE;
}

void LogPar(char *prog, char *pfpath, PARAM *par)
{
char sbuf[RTP_MINSMASKBUFLEN];

    rtp_log(RTP_INFO, "%s version %s", prog, ReleaseIdent);
    rtp_log(RTP_INFO, "Parameter file %s loaded", pfpath);
    rtp_log(RTP_INFO, "ORB               %s",    par->orb.ident);
    rtp_log(RTP_INFO, "RTPD              %s",    par->rtpd.ident);
if (strlen(par->dbname) > 0) {
    rtp_log(RTP_INFO, "Database          %s",    par->dbname);
    rtp_log(RTP_INFO, "UseLoc            %s",    par->UseLoc ? "Yes" : "No");
}
    rtp_log(RTP_INFO, "DASid             %ld",   par->attr.at_dasid);
    rtp_log(RTP_INFO, "StrMask           %s",
        rtp_decode_smask(par->attr.at_smask, sbuf)
    );
}

/* Revision History
 *
 * $Log$
 * Revision 1.1  2004/03/09 18:28:23  vernon
 * Initial revision
 *
 */
