/*
 *   kom.h:  include file for kom.c
 */

#ifndef KOM_H
#define KOM_H

/* Prototypes for functions in kom.c
 ***********************************/
int    k_open( char * );        /* open new file for k-system input     */
int    k_close( void );         /* close current file                   */
char  *k_get( void );           /* return pointer to current command    */
void   k_dump( void );          /* print last card read from file       */
int    k_err( void );           /* return last error code and clear     */
int    k_put( char * );         /* insert command line to be parsed     */
int    k_rd( void );            /* read a line from file into buffer    */
int    k_its( char * );         /* compare string of last token to      */
                                /* given string; 1=match 0=no match     */
char  *k_com( void );           /* returns last line read from file     */
char  *k_str( void );           /* return next token as pntr to string  */
double k_val( void );           /* return next token as a double real   */
int    k_int( void );           /* return next token as an integer      */
long   k_long( void );          /* return next token as a long integer  */

#endif
