
#ifndef MSEED2ORBPKT_H
#define MSEED2ORBPKT_H 1

#ifdef __cplusplus
extern "C" {
#endif

extern int mseed2orbpkt (char payloadformat, const char *msrec, uint32_t mssize,
                         char *calibdb, char *mappingdb,
                         int remap, char *srcname, double *time, char **packet,
                         int *nbytes, int *packetsz, int verbose);

#ifdef __cplusplus
}
#endif

#endif /* MSEED2ORBPKT_H */
