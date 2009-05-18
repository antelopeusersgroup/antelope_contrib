/****  tmpl.h  ****/


#ifndef __TMPL__
#define __TMPL__


/****  Type definitions / Enumerations  ****/
typedef struct
{
    char            terse;
    char           *verbose;
    int             arg;
}               option_t;

#define END_OPTIONS    ('\0')

#define NO_ARGUMENT    (0)
#define NORMAL_ARG     (1)
#define NORMAL_ARGS    (2)
#define SPECIAL_ARG    (3)
#define SPECIAL_ARGS   (4)


/****  Function Declarations  ****/
char            get_option();
char           *get_argument();
char           *get_directory();
char           *get_input_file();
char           *get_output_file();
char           *last_argument();
char           *last_input_file();
char           *last_output_file();
int             readable_file();
int             writable_file();
void            parse_path();
void            concat_paths();
void            clean_path();
void            add_slash();
void            del_slash();

#endif

/* $Id$ */
