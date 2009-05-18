#include <stdlib.h>
#include <string.h>
#include <rpc/types.h>  
#include <stdio.h> 
#include<rpc/xdr.h>

#include "db.h"
#include "scv2.h"
#include "tmpl.h"
#include "ahhead.h"
#include "swapbytes.h"

extern int gethead ( ahhed *head, FILE *file_pt );
extern int puthead ( ahhed *head, FILE *file_pt );
extern int size ( ahhed *head );
extern int tohead ( int n, FILE *file_pt );
extern int getdata ( ahhed *head, char *array, FILE *file_pt );
extern int putdata ( ahhed *head, char *array, FILE *file_pt );
extern int putrecord ( ahhed *head, char *array, FILE *file_pt );
extern int getrecord ( ahhed *head, char *array, FILE *file_pt );
extern int getrecord2 ( ahhed *head, char **array, FILE *file_pt );
extern int gogethead ( int n, ahhed *head, FILE *file_pt );
extern int gogetrecord ( int n, ahhed *head, char *array, FILE *file_pt );
extern int logger ( char *char_pt, ahhed *head_pt );
extern int out_is_tty ( void );
extern int in_is_tty ( void );
extern char *mkdatspace ( ahhed *head );
extern void get_null_head ( ahhed *hed );
extern void acpy ( char *from, char *to, unsigned nbytes );
extern void ah_error ( char *s1, char *s2, int status );
extern int maxamp ( ahhed *head, char *data );
extern int xdr_gethead ( ahhed *head, XDR *xdrs );
extern int xdr_puthead ( ahhed *head, XDR *xdrs );
extern int xdr_tohead ( int n, XDR *xdrs );
extern int xdr_getdata ( ahhed *head, char *array, XDR *xdrs );
extern int xdr_putdata ( ahhed *head, char *array, XDR *xdrs );
extern int xdr_putrecord ( ahhed *head, char *array, XDR *xdrs );
extern int xdr_getrecord ( ahhed *head, char *array, XDR *xdrs );
extern int xdr_getrecord2 ( ahhed *head, char **array, XDR *xdrs );
extern int xdr_gogethead ( int n, ahhed *head, XDR *xdrs );
extern int xdr_gogetrecord ( int n, ahhed *head, char *array, XDR *xdrs );
extern int xdr_ahhead ( XDR *xdrsp, ahhed *ahheadp );
extern char get_option ( int *argc, char **argv, option_t option_list[], int num_options );
extern char * get_argument ( void );
extern char * get_directory ( void );
extern char * get_input_file ( FILE **input_file );
extern char * get_output_file ( FILE **output_file );
extern char * last_argument ( void );
extern char * last_input_file ( FILE **input_file );
extern char * last_output_file ( FILE **output_file );
extern int readable_file ( char *file_name );
extern int writable_file ( char *file_name );
extern void parse_path ( char *path, char *dir, char *file );
extern void concat_paths ( char *base, char *next, char *net );
extern void clean_path ( char *path );
extern void add_slash ( char *directory );
extern void del_slash ( char *directory );
extern void write_ah ( SCV *scv, Dbptr dbin, Dbptr dbout, double tstrt, double tend, char *fixgaps, int counts, char *wfdir );
extern void write_sac ( SCV *scv, Dbptr dbin, Dbptr dbout, double tstrt, double tend, int intel, char *fixgaps, int counts, char *wfdir );
