#ifndef _stock_
#define _stock_

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <pthread.h>

#include "deviants_include.h"

#include <sys/stat.h>
#include H_STATVFS 

#include <netdb.h>
#include <sys/types.h>
#include <inttypes.h>
#include <regex.h>

#include "deviants.h"
#include "stocktime.h"

#define MAX_STA_SIZE	32
#define MAX_CHAN_SIZE	32

#define MAX_SIGNED_INT   (0x7fffffff)
#define MAX_UNSIGNED_INT (0xffffffffU)

#define strtod(A,B) fix_strtod(A,B)
 
#include "arrays.h"
#include "cabinet.h"
#include "idarr.h"
#include "rtl.h"

#define BITS_PER_ELEMENT (8*sizeof(char *))
#define bitnew() inittbl(0L,1L,0,0L,(int)BITS_PER_ELEMENT/8)

#define bitfree(B) freetbl(B,0)
#define bitempty(B) clrtbl(B,0) 

#define HOOK_MAGIC 814615491

typedef struct Hook {
    int magic ; 
    void (*free)(void *) ;
    int type ;
    void *p ;
    pthread_mutex_t lock ;
} Hook ; 

typedef struct Xlat {
        char *name ;
        int num ;
} Xlat ;

typedef struct Submap { 
    char *pattern ; 
    regex_t prog ; 
    char *substitution ; 
} Submap ;

typedef Tbl Morphtbl ;

#define MORPH_PARTIAL 1
#define MORPH_ALL 2

typedef struct InfoPath {
    unsigned long 	device ;		/* device id */
    char	host[MAXHOSTNAMELEN] ;  /* host: null if localhost */
    char        fs[FILENAME_MAX] ;	/* filesystem mount point */
    double 	avail ; 		/* # bytes available */
    double	total ;			/* # bytes total */
    double	avail_inodes ; 		/* # inodes available */
    double	total_inodes ;		/* # inodes total */
    char	fstype[32] ;		/* filesystem type */
    int		max_filename ;		/* maximum path length ? */
    int		flags ;			/* bit 0 (==1) is set when mounted read-only */
    char	fstr[32] ; 
} InfoPath ; 

#define ORB_MAX_DATA_BYTES 4000000

#define STASH "\377"
#define STASHC 0xff
#define STASH_LENGTH 1
#define is_stash(SRCNAME,BUFFER,NBYTES) (((NBYTES)>0)&&(BUFFER[0]&0xff) == STASHC)

#define SG_PORT 56789
#define SG_MAX_SIGNATURE 512
#define SG_SIGNER_SIZE 64

#define FS_DEPTH_FIRST 4
#define FS_ONLY_DIR 2
#define FS_ONLY_FILE 1

typedef struct Mapdata {
    char           *mmap_data;
    size_t          mmap_size;
    int             counter ;
    int		    ref ;
}               Mapdata;

#define RB_IDSZ 64
#define RB_MAGIC  "newringb"
typedef struct RbData { 
    double t ; 
    double v ; 
} RbData ; 

typedef struct RbInfo { 
    char magic[8] ;
    char id[RB_IDSZ] ; 
    int p, cnt, max, flags ; 
    double tgap, vmissing ; 
} RbInfo ; 

typedef struct RingBuffer { 
    RbInfo *info ; 
    RbData *data ;
    size_t size ;
    int readonly ;
    char *filename ;
} RingBuffer ;

typedef struct Btpoint { 
    double x, y ; 
} Btpoint ; 

typedef struct Btipoint { 
    int x, y ; 
} Btipoint ; 

typedef struct Btrange { 
    double x0, x1, y0, y1 ; 
} Btrange ;

typedef struct Btxform { 
    double uslope, u0 ; 
    double vslope, v0 ; 
} Btxform ; 

typedef struct Btlpt { 
    int x, y0, y1 ; 
} Btlpt ; 

#define BTNULL -99e-35


typedef struct AllocBuffer { 
    unsigned char *data ; 		/* the actual buffer */
    unsigned long   ndata ; 		/* number of meaningful bytes in the buffer */
    unsigned long   allocated_size ; 	/* allocated size (in bytes) of the buffer */
} AllocBuffer ; 

typedef struct Hls { 
    double hue, lightness, saturation ; 
} Hls ; 

typedef struct Hlsrange { 
    Hls hls0, hls1 ; 
} Hlsrange ; 

#define gets(ALINE) getaline(stdin, ALINE, sizeof(ALINE))

#include "elog.h"
#include "pf.h"
#include "bury.h"

extern long     Orb_max_data_bytes ;
extern Xlat Elog_xlat[] ;
extern int Elog_xlatn ;

#ifdef  __cplusplus
extern "C" {
#endif


extern double fix_strtod ( const char *in, char **end ) ;

extern char * align_int(char * i) ;
extern char * align_long(char * i) ;
extern char * align_double(char * i) ;

extern Hook *new_hook ( void (*hookfree)(void *p) );
extern void free_hook ( Hook **hookp );

extern char *ask ( char *fmt, ... );
extern char *asknoecho ( char *fmt, ... );
extern int askyn ( char *fmt, ... );
extern int askynaq ( char *fmt, ... );

extern int tty_noecho ( int fd );
extern int tty_echo ( int fd );
extern int tty_immediate ( int fd );
extern int tty_byline ( int fd );
extern int tty_reset ( int fd );
extern char * cmdresult ( char *cmd ) ;

extern char * getantelope ( void );
extern int newdata ( char *returnpath, char *envname, char *dirname, char *filename, char *suffix );

extern Tbl * datasearch ( char *path, char *dirname, char *filename, int flags );
extern char * datapath ( char *envname, char *dirname, char *filename, char *suffix );
extern char * datafile ( char *envname, char *filename );

extern char *newcs ( char *f_s, int n_s ) ;
extern char * expand_env ( char *s );

extern int nstrcmp(char *a, char *b);
extern Tbl *split ( char *s, int c );
extern FILE * zopen ( char *filename );
extern int gz_openfd ( char *filename );
extern FILE *gz_open ( char *filename );
extern FILE *gz_cat ( char *filename );
extern int blank ( char *s );
extern void dirbase ( char *path, char *dir, char *base );
extern int fixup_env(char **envsetup, int nenv, int force );
extern int envfile ( char *filename );
extern int gethdir ( char *user, char *home );
extern void isort ( char *a, long n, int size, int (*compare) (char *, char*, void *), void *pvt );

extern char * homedir ( char *user );
extern int cleanpath ( char *, int, char *);
extern char *concatpaths ( char *s0, ... );
extern int abspath ( char *relp, char *absp );
extern int relpath ( char *from, char *to, char *relp );
extern void parsepath ( char *path, char *dir, char *base, char *suffix );
extern int fspath ( char *path, char *host, char *fs );
extern InfoPath *infopath ( char *path );
extern int samepath(char *path1, char *path2) ;

extern int freespace ( char *path, double mbavail );

extern int make_pathdirs ( char *filename );
extern int makedir ( char *dir );
extern void quicksort ( void *pbase, long total_elems, int size, int (*cmp)(char *, char *, void*), void *);
extern void shellsort ( char *a, long n, int size, int (*compare)(char *, char *, void*), void *);
extern long ssearch ( char *key, int keysize, char *index, long nkeys, int unique, 
			int (*compare)(char *, char *, void *), void *, long *ns, long *ne );

extern int strmatches ( char *s, char *pattern, Hook **hookp );
extern int strcontains ( char *s, char *pattern, Hook **hookp, long *start, long *nchars );
extern char *strdbl ( double x, char *buf );

extern int blank ( char *s );
extern int whitespace ( char *s );
extern Tbl *split ( char *s, int c );
extern Tbl *splitws ( char *s );
extern void sncopy ( char *dest, char *source, long n );
extern void szcopy ( char *dest, char *source, long n );
extern void copystrip ( char *dest, char *source, long n );
extern void * memdup ( void *a, long n );
extern void str2upper ( char *s );
extern void str2lower ( char *s );
extern char * strconcat ( char *s, ... );
extern char * strjoin ( int c, char *s, ... );
extern void strtrim ( char *s );
extern void strsub ( char *oldstr, char *pattern, char *replacement, char *newstr );
extern long pushstr ( void **vstack, char *s );
extern long pushstrc ( void **vstack, int c ) ;
extern long pushstrn ( void **vstack, char *s, long l );
extern char *popstr ( void **vstack, int vfree );
extern void strtr ( unsigned char *s, unsigned const char *from, unsigned const char *to );
extern char *getaline( FILE *file, char *aline, long n );
extern char *intactline ( FILE *file, char **aline, long *size );
extern void xargs ( char *cmd, Tbl *args, char *tail, int verbose );
extern void chomp (char *s) ;
extern double sz2dbl(char *size) ;

extern int bitset ( Bitvector *bvec, long index );
extern int bitclr ( Bitvector *bvec, long index );
extern int bittst ( Bitvector *bvec, long index );
extern long bitmax ( Bitvector *b );
extern int bitcmp ( Bitvector *b1, Bitvector *b2 );
extern Bitvector *bitfile(char *filename, long size);

#ifndef	__cplusplus
extern Bitvector * bitand ( Bitvector *b1, Bitvector *b2 );
extern Bitvector * bitor ( Bitvector *b1, Bitvector *b2 );
#endif

extern Bitvector * bitnot ( Bitvector *b );
extern Bitvector * bitxor ( Bitvector *b1, Bitvector *b2 );

extern void setabit ( int *flag, int abit );
extern void clrabit ( int *flag, int abit );

extern unsigned long abitset(unsigned long flag, int which) ;
extern unsigned long abitclr(unsigned long flag, int which) ;
extern int abittst(unsigned long flag, int which) ;

extern int fdkey ( int fd );
extern int fdwait ( int fd, int msec );

extern long fdtell ( int fd );
extern int fdgets ( int fd, char *buf, int maxbytes );
extern int fdread ( int fd, char *buf, int nbytes );
extern int fdgetc ( int fd );
extern off_t fdseek ( int fd, off_t offset, int whence );
extern int fdclose ( int fd );
extern int fdwrite ( int fd, char *buf, int nbytes );
extern int fdflush ( int fd );

extern off_t fdseek ( int fd, off_t offset, int whence );
extern void fdhexdump ( int fd, void *memory, int l );
extern char * asciistring ( char *memory, int l );
extern void fdasciidump ( int fd, char *memory, int l );
extern int fdread_asciidump ( int fd, char *memory, int l );
extern int fdread_hexdump ( int fd, char *memory, int l );

extern char * aprintchar ( int c );
extern void aprintstr ( FILE *file, char *s, int n );
extern char * hexdump_string ( long base, void *memory, int l );
extern void Hexdump ( FILE *file, long base, void *memory, int l );
extern void hexdump ( FILE *file, void *memory, int l );
extern void printable_string ( char *buf, int n, char *memory, int l );
extern void asciidump ( FILE *file, char *memory, int l );
extern int hex2int ( char c );
extern int read_asciidump ( FILE *file, char *memory, int l );
extern int read_hexdump ( FILE *file, char *memory, int l );
extern long hex2mem ( char *hex, void *mem );
extern void mem2hex ( void *mem, int n, char *s );
extern int printable ( char *s, int n );
extern char * printable_char(int c, char *buf) ;
extern void Xchardump ( FILE *file, char *label, char *desc, char *bytes, int nbytes );
extern void showdouble ( FILE *file, double x );
extern void showfloat ( FILE *file, float x );
extern void prthexcmp ( FILE *file, char *msg, char *a, char *b, int size );

extern void banner ( char *program, char *version );
extern void cbanner ( char *version, char *usage, char *author, char *location, char *email );
extern void announce ( char *program, char *version );
extern char * announce_str ( char *program, char *version );

extern int gencompress ( unsigned char **out, int *nout, int *size, int *in, int nsamp, int length );
extern int genuncompress ( int **out, int *nout, int *size, unsigned char *in, int nbytes );

extern int uucsd ( int *data, char **bufp, int bufcnt, int nsamp );
extern int cucsd ( int *data, int nsamp, char **buf, int *nbytes, int *bufsiz );

extern int ccanada ( int *data, int nsamp, char **compressedp, int *nbytes, int *compressedsz );
extern int ucanada ( char *compressed, int nbytes, int **datap, int *nsamp, int *datasz );

extern void gse_checksum ( int *signal, long nsamp, int *checksum );
extern int cm6 ( int *data, long nsamp, char **compressedp, long *nbytes, long *compressedsz );
extern long um6 ( char *compressed, long nbytes, int **datap, long *nsamp, long *datasz );

extern char * xlatnum ( int def, Xlat *xlat, int nxlat );
extern char * xlatnum_str ( int num, char *name, int size, Xlat *xlat, int nxlat ) ;
extern int xlatname ( char *name, Xlat *xlat, int nxlat );

extern int runcmd ( char *argv[], char **result );
extern int verbose_exists (void) ;

extern int set_random ( int intcount );
extern void fill_random ( unsigned char *packet, int nchar, int *pp );
extern int check_random ( unsigned char *packet, int nchar );

extern void ignoreSIGPIPE ( void );
extern void * usermethod ( char *methodname );
extern int set_fl ( int fd, int flags );
extern int clr_fl ( int fd, int flags );
extern int yesno ( char *s );

extern Mapdata * vmapfile ( char *filename, Arr ** maparr );
extern long mapfile ( char *filename, Arr ** maparr, char **mmap_datap );
extern int unmapfile ( char *filename, Arr ** maparr );
extern int unmaparr ( Arr ** maparr );
extern long wmapfile ( char *filename, Arr ** maparr, char **mmap_datap );
extern long lmapfile ( char *filename, char **mmap_datap, long foff, long maxbytes );

extern int check_cmd ( char *command, char *pathenv );
extern int my_system ( char *command, int share, int *coreflag, int *termsig );
extern char * parse_on_1char ( char *string, char parse_char1 );

extern void qtcrc ( int *crcp, unsigned char *buffer, int bufsize );
extern void qtchecksum ( short int *checksump, unsigned char *buffer, int bufsize );
extern int userauth ( char *user );
extern int xuserauth ( char *user );

extern int mkfile ( char *name, unsigned long nbytes, char *init, int initsize );
extern void regsubstitute ( char *, char *, regmatch_t *, char *);
extern int patsub ( char *, regex_t *, char *, char *);

extern void freemorphtbl ( Morphtbl *morphmap );
extern int newmorphtbl ( Tbl *list, Morphtbl **morphmap );
extern int morphtbl ( char *old, Morphtbl *morphmap, int flags, char *result );

extern double an_infinity ( void );
extern double a_signaling_nan ( void );
extern double a_quiet_nan ( void );
extern double a_nan ( void );
extern int is_infinity ( double x );
extern int is_inf ( double x ) ;
extern int is_negative_infinity ( double x );
extern int is_denormalized ( double x ) ;
//#define is_nan(X)    isnan((X))

extern double Gbytes(double bytes) ;
extern double Mbytes(double bytes) ;
extern double kbytes(double bytes) ;
extern int is_file ( char *path );
extern int is_executable_file (char *path);
extern int is_filename (char *path);
extern int is_dir ( char *path );
extern int is_present ( char *path );
extern int is_empty ( char *path ) ;
extern int is_writable_file (char *path) ;
extern int is_changed ( char *filename );
extern long cmpfiles (char *filenamea, char *filenameb) ;
extern int cpfile ( char *srcname, char *dstname );
extern int defragment_file ( char *srcname );
extern unsigned long filesize ( char *path );
extern unsigned long filetime ( char *path );
extern unsigned int filemode ( char *path );
extern Tbl * dirfiles ( char *path, int flag );
extern int fstraverse ( char *path, int flags, int (*user)(char *, struct stat *, void *), void *pvt );

extern int is_tty ( int fd );
extern int tty_noecho ( int fd );
extern int tty_echo ( int fd );
extern int tty_immediate ( int fd );
extern int tty_byline ( int fd );
extern int tty_reset ( int fd );

extern void spfit ( double *x, float *y, int n, double dy0, double dyn, float *dy );
extern void spinterp ( double *x, float *y, float *dy, int n, double X, float *Y );

extern int newring ( char *filename, char *id, int max, int flags );
extern RingBuffer * openring ( char *filename, char *id, int max, int flags );
extern void closering ( RingBuffer *rb );
extern int rmring (RingBuffer *rb) ;
extern int pushring ( RingBuffer *rb, double time, double value );
extern int cramring(RingBuffer *rb, double time, double value, char *method )  ;
extern int countring ( RingBuffer *rb );
extern int getring ( RingBuffer *rb, int i, double *time, double *value );
extern int valring ( RingBuffer *rb, double *time, double *value ) ;
extern int findring ( RingBuffer *rb, double value, double deltat );
extern double maxring ( RingBuffer *rb, double deltat, int *index );
extern int nearbymaxring(RingBuffer *rb, double nearby, double *time, double *value ) ;
extern double minring ( RingBuffer *rb, double deltat, int *index );
extern double avgring ( RingBuffer *rb, double deltat );
extern int changedring ( RingBuffer *rb, double deltat );
extern int meanring(RingBuffer *rb, double deltat, int *npts, double *mean, double *stddev ) ;
extern int linearfitring(RingBuffer *rb, double deltat, int *npts, double *slope, double *y0, double *correlation) ;
extern const char * idring (RingBuffer *rb) ;
extern const char * filenamering (RingBuffer *rb) ;
extern int tallyring(RingBuffer *rb, double deltat) ;
extern int setring(RingBuffer *rb, double tgap, double vmissing ) ;
extern int ptsring(RingBuffer *rb, double t0, double dt, Tbl **pts ) ;
extern int ptsrange ( Tbl *pts, Btrange *range ) ;
extern void scalepts ( Tbl *inpts, Btxform m, Tbl **outpts ) ;
extern void iscalepts ( Tbl *inpts, Btxform m, Tbl **outpts ) ;
extern void decimatepts ( Tbl *inpts, int xpixels, int ypixels, Tbl **outpts ) ;
extern void debug_btipoint_tbl ( char *msg, Tbl *points ) ;
extern void debug_btpoint_tbl ( char *msg, Tbl *points ) ;
extern void debug_btlpt_tbl ( char *msg, Tbl *points ) ;

extern void closeringlet ( void *rb );
extern int newringlet ( char *filename, int max, int entrysize );
extern void * openringlet ( char *filename, int max, int entrysize );
extern int pushringlet ( void *rb, char *entry );
extern int countringlet ( void *rb );
extern int getringlet ( void *rb, int i, char **entry );

extern double disordered ( void );
extern double r_disordered (Hook **);

extern void showenv ( FILE *file );

extern char *machine_info(int flag) ; 

extern int legit_ip_pf ( Pf *pf, unsigned int ip, char **rstrict );
extern int legit_ip ( char *pfname, unsigned int ip, char **rstrict );
extern int legit_ipv6 ( char *pfname, Pf *pf, struct sockaddr *sockaddr, char **rstrict );
extern int legit_fd ( char *pfname, Pf *pf, int fd, char **rstrict );

extern int bt_inet_family ( char *ip_str );
extern char * bt_inet_ntop ( const struct sockaddr * sa, char *s, size_t maxlen );
extern int bt_inet_pton ( char *ip_str, struct sockaddr *sa );
extern int bt_getpeername (int fd, char ipstring[INET6_ADDRSTRLEN]) ;
extern int bt_portnumber (char *name, char *signer) ;
extern Tbl * bt_portnames () ;
extern Tbl * bt_portbynumber (int myport) ;
extern int bt_parse_servername ( char *name, char *defaultport, char *emulator, char *server, int *port, char *signer ) ;
extern int bt_open_socket ( char *name, char *pfname, char *signer );
extern int bt_open_socket2 (char *name, char *defaultport, char *emulator, char *server, int *port, char *signer ) ;

extern int bt_spawn ( char **argv );
extern int bt_exec_subprogram ( char *program, char *name );

extern int lfit ( double *x, double *y, int npts, double *slope, double *intercept );
extern void lchi ( double *x, double *y, int npts, double slope, double intercept, double *ymaxdev, double *chisqr );

extern double dround ( double x );
extern double d4round ( double x );
extern double d6round ( double x );

extern int units_convert ( double in, char *in_units, char *want, double *out, char *out_units );
extern Tbl * units_match ( char *in_units );
extern void show_all_units () ;

extern double findmin ( double a, double b, double (*f) (double, void *), double tol, void *p );
extern double findmax ( double a, double b, double (*f) (double, void *), double tol, void *p );
extern double findzero ( double ax, double bx, double (*f) (double, void *), double tol, void *p );

extern int restat ( const char *path, struct stat *buf );
extern off_t relseek ( int fd, off_t offset, int whence );
extern int relstat ( const char *path, struct stat *buf );
extern int refstat ( int fd, struct stat *buf );
extern ssize_t reread(int fd, void *buf, size_t nbytes) ;
extern ssize_t rewrite(int fd, void *buf, size_t nbytes) ;
extern int reclose ( int fd ) ;
extern int reopen(const char *path, int oflag, ...) ;
extern int relockf(int fd, int function, off_t size ) ;
extern int reaccept ( int s, struct sockaddr *addr, socklen_t *addrlen );
extern int reconnect ( int s, struct sockaddr *name, socklen_t *namelen );

#ifdef TRIPDEBUG
extern int tripdebug ( char *name );
extern int tripset ( char *filename, char *appname, char *name );
extern int tripclr ( char *filename, char *appname, char *name );
extern int triptst ( char *filename, char *appname, char *name );
extern char *tripappname(char *filename);
#endif

extern void debugtbl ( FILE *out, char *msg, Tbl *tbl ) ;
extern void debugarr ( FILE *out, char *msg, Arr *arr ) ;
extern void debugchar ( FILE *out, char *msg, char *s, long n) ;
extern long ascii_cnt ( char *s, long n ) ;
extern char *debugcharstr ( char *s, long n );
extern char *spaces(long n) ;
extern char *okstr(char *s) ;
extern char * strhead (char *input, int nlines, char *prefix);

extern int mapcenter ( long npts, double *lat, double *lon, double *clat, double *clon, double *radius ) ;

extern void deprecate ( char *alternates );

extern int match_string (char *spec, char *string) ;
extern int match_stachans (char *stachans, char *sta, char *chan) ;
extern int match_stachans_ (char *stachans_, char *sta_, char *chan_, int lenstachans, int lensta, int lenchan) ;

extern void hls2rgb ( double gamma, double h, double l, double s, double *r, double *g, double *b );
extern void rgb2hls ( double r, double g, double b, double *h, double *l, double *s );
extern void hls2rgbstring(double h, double l, double s, char *rgb) ;
extern Hls hlsinterpolate ( double x, double x0, double x1, Hlsrange *hlsrange ) ;

extern char *cmdline ( int argc, char **argv, int abspath );
extern void echo_cmdline ( int argc, char **argv );

extern char *crypt(const char *key, const char *salt);

extern int float_interval (float a, float b) ;
extern long double_interval ( double a, double b) ;

/*PRINTFLIKE1*/
extern void measure ( char *format, ... )__attribute__ ((format (printf, 1, 2))) ;

extern long new_thread_id () ;
extern int periodically (char *name, double period, int (*task) (void *), void *arg, int *quit ) ;
extern int pkill (int td) ;

extern int levenshtein_distance (char *s, char *t) ;
extern char * btunique_filename(char *id) ;
extern int basicreadfile(char *filename, char **buffer, long *bufsize) ;
extern int basicgetline(char *buffer, long bufsize, long *index, char *line, long linesz) ;

extern int brack_(int *n, float *x, float *x0, int *ileft) ;
extern void spline (float *x, float *y, int n, double yp1, double ypn, float *y2) ;
extern void splint (float *xa, float *ya, float *y2a, int n, double x, float *y) ;

extern int btcompress ( AllocBuffer *in, AllocBuffer *out )  ;
extern int btexpand ( AllocBuffer *in, AllocBuffer *out ) ;

#ifdef	__cplusplus
}
#endif

/* When a macro generates multiple output statements, there is a danger
 * that these statements might lead to odd results in a context where a
 * single statement is expected, eg. after an if -- STMT forces the macro
 * output to be a single statement, and the following stuff with ZERO is
 * to make lint work properly. */

#ifdef lint
extern int      ZERO;
#else
#define ZERO 0
#endif

#define STMT(stuff) do { stuff } while (ZERO)

/* The following macros simplify memory allocation and testing
 *
 * allot and reallot are convenient interfaces to malloc and realloc which
 * perform testing on the result without cluttering the code with
 * branches and messages which are seldom if ever used.
 *
 * ALLOTERROR specifies what happens when a malloc or realloc fails -- it
 * may be overridden with a special macro within the file */

#ifdef __STDC__
#define ALLOTDIE(ptr,size)	die(1,(char*)"Out of memory in %s at line %d of '%s'\n\tCan't malloc %lu bytes for " #ptr "\n", __func__, __LINE__, __FILE__, (long)size)
#else
#define ALLOTDIE(ptr,size)	die(1,(char*)"Out of memory in %s at line %d of '%s'\n\tCan't malloc %lu bytes for ptr\n", __func__, __LINE__, __FILE__, (long)size)
#endif

#ifndef BADALLOT
#define BADALLOT -1
#endif

#ifdef lint
extern int ReTuRn(int) ;
#define BADRETURN ReTuRn(BADALLOT)
#else
#define BADRETURN return BADALLOT
#endif

#ifdef __STDC__
#define ALLOTREGISTER_ERROR(ptr,size) \
    STMT(elog_log(1,(char*)"Out of memory in %s at line %d of '%s'\n\tCan't malloc %lu bytes for " #ptr "\n", __func__, __LINE__, __FILE__, (long)size); BADRETURN ;)
#else
#define ALLOTREGISTER_ERROR(ptr,size) \
    STMT(elog_log(1,(char*)"Out of memory in %s at line %d of '%s'\n\tCan't malloc %lu bytes for ptr\n", __func__, __LINE__, __FILE__, (long)size); BADRETURN ;)
#endif

#ifdef __STDC__
#define ALLOTCOMPLAIN(ptr,size) \
    STMT({elog_complain(1,(char*)"Out of memory in %s at line %d of '%s'\n\tCan't malloc %lu bytes for " #ptr "\n", __func__, __LINE__, __FILE__, (long)size); BADRETURN ;})
#else
#define ALLOTCOMPLAIN(ptr,size) \
    STMT({elog_complain(1,(char*)"Out of memory in %s at line %d of '%s'\n\tCan't malloc %lu bytes for ptr\n", __func__, __LINE__, __FILE__, (long)size); BADRETURN ;})
#endif

#ifndef ALLOTERROR
#define ALLOTERROR ALLOTDIE
#endif

#define SIZE_BUFFER(TYPE,BUFFER,BUFSZ,NEEDED) \
    if ((NEEDED)>(BUFSZ)) { \
	if ((BUFSZ) != 0) { \
	    free(BUFFER) ; \
	} \
	callot(TYPE,BUFFER,NEEDED) ; \
	BUFSZ = NEEDED ; \
    } else { ; }

#define CSIZE_BUFFER(TYPE,BUFFER,BUFSZ,NEEDED) \
    if ((NEEDED)>(BUFSZ)) { \
	if ((BUFSZ) != 0) { \
	    free(BUFFER) ; \
	} \
	callot(TYPE,BUFFER,NEEDED) ; \
	BUFSZ = NEEDED ; \
    } else { ; }

#define RESIZE_BUFFER(TYPE,BUFFER,BUFSZ,NEEDED) \
    if ((NEEDED)>(BUFSZ)) { \
	if ((BUFSZ) != 0) { \
	    reallot(TYPE,BUFFER,NEEDED) ; \
	} else { \
	    callot(TYPE,BUFFER,NEEDED) ; \
	} \
	BUFSZ = NEEDED ; \
    } else { ; }


/* allotted can be used in an if statement when the action in allot is
 * inadequate */
#define allotted(type,ptr,size) \
  ( (ptr=(type) malloc((size_t)((size)*sizeof(*(ptr))))) != NULL )
#define callotted(type,ptr,size) \
  ( (ptr=(type) calloc((size_t)(size),sizeof(*(ptr))) ) != NULL )

/* allot checks the results of malloc and generates an error message in
 * the case of failures */
#define allot(type,ptr,size)    \
  STMT( if (!callotted(type,ptr,size)) ALLOTERROR(ptr,size);)
#define callot(type,ptr,size)    \
  STMT( if (!callotted(type,ptr,size)) ALLOTERROR(ptr,size);)

/* reallot and reallotted correspond to allot and allotted and are
 * interfaces to realloc. */
#define reallotted(type,ptr,size) \
  ( (ptr=(type) realloc((char *)ptr,(size_t)((size)*sizeof(*ptr)))) != NULL )
#define reallot(type,ptr,size)    \
  STMT( if (!reallotted(type,ptr,size)) ALLOTERROR(ptr,size);)


/* insist provides a concise method for consistency checks which are
 * never expect to fail, avoiding cluttering the code with branches which
 * are seldom if ever taken.
 * 
 * INSISTFAIL specifies what happens when an insist fails -- it may be
 * overridden with a special macro within a file */

#ifndef INSISTFAIL
#define INSISTFAIL die
#endif

#define insist(ex)	STMT( if (!(ex)){ INSISTFAIL(1, (char*) "*stack*Unexpected failure: %s in file %s, line %d\n", __func__, __FILE__, __LINE__ ) ;} )
#define btwarn(ex)	STMT( if (!(ex)){ elog_debug(1, (char*) "*stack*Unexpected failure: %s in file %s, line %d\n", __func__, __FILE__, __LINE__ ) ;} )

#define insistmsg(ex,msg)	STMT( if (!(ex)){ INSISTFAIL(1, (char*) "%s: %s in file %s, line %d\n", msg, __func__, __FILE__, __LINE__ ) ;} )
#define btwarnmsg(ex,msg)	STMT( if (!(ex)){ elog_debug(1, (char*) "%s: file %s, %s in line %d\n", msg, __func__, __FILE__, __LINE__ ) ;} )

/* The meaning of the following macros is fairly obvious; however, they
 * are all dangerous in the sense that their arguments will be evaluated
 * multiple times, which can lead to erroneous results. for example,
 * sqr(a++) will give an undefined result which is probably not what you
 * wanted and may differ among compilers. */

#define sqr(d) 		((d)*(d))

#ifndef SQR
#define SQR(d) 		((d)*(d))
#endif

#ifndef ABS
#define ABS(d) 		((d)< 0  ? -(d) : (d))
#endif

#ifndef MAX
#define MAX(a,b)        ((a)>(b) ? (a) : (b))
#endif

#ifndef MIN
#define MIN(a,b)        ((a)<(b) ? (a) : (b))
#endif

#ifndef SIGN
#define SIGN(d) 	(((d)>0)? 1.0 : ((d)<0) ? -1.0 : 0.0)
#endif

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

/* The following size is judged to be large enough to avoid problems with
 * overflowing string buffers, so that the various string routines which
 * do no checking can be used without much fear. */
#define STRSZ 1024

#endif


