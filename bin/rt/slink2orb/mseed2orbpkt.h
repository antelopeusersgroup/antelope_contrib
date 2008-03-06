
#ifndef MSEED2ORBPKT_H
#define MSEED2ORBPKT_H 1

#ifdef __cplusplus
extern "C" {
#endif

extern int mseed2orbpkt ( char *msrec, int mssize, char *calibdb, char *mappingdb,
                          int remap, char *srcname, double *time, char **packet,
                          int *nbytes, int *bufsize );

#ifdef __cplusplus
}
#endif

#endif /* MSEED2ORBPKT_H */
