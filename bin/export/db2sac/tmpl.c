#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <dirent.h>

#include "ahsac.h"

#define OPTION_ONLY          (0)
#define ONE_ARGUMENT         (1)
#define FIRST_ARGUMENT       (2)
#define MORE_ARGUMENTS       (3)
#define ONE_SPECIAL          (4)
#define FIRST_SPECIAL        (5)
#define MORE_SPECIALS        (6)
#define ALL_ARGUMENTS        (7)
#define ERROR_RECOVERY       (8)



/****  Function macro definitions  ****/
#define	STREQ(a, b) \
  (strcmp((a), (b)) == 0)



/****  Type definitions / Enumerations  ****/



/****  Global variables  ****/
static int      Parse_State	       /* FSM parsing control variable  */
= OPTION_ONLY;

static char   **Argv_Ptr	       /* Pointer to the argument list  */
= NULL;

static int      Arg_Count	       /* Total number of arguments  */
= 0;



/****  Forward declarations  ****/
static FILE    *open_file ();
static int      dash_seen ();
static int      shift_arg ();



/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *
   get_option - get the next switch from the command list.
 * -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

char
get_option (argc, argv, option_list, num_options)
int            *argc;		       /* Number of arguments in argv */
char          **argv;		       /* Argument vector */
option_t        option_list[];	       /* Option vector */
int             num_options;	       /* Number of options in option_list */

{
    static char    *comp_opt = NULL;   /* Pointer to compound option  */
    static char   **follow_ptr = NULL; /* Location of 1st empty  */
    static int      follow_count = 0;  /* Count of followers  */

    int             index;

    /****  Initialize the global / static variables  ****/
    if (follow_ptr == NULL) {
	follow_ptr = argv + 1;
	Argv_Ptr = argv + 1;
	Arg_Count = *argc - 1;
    }
    /****  Was a required argument retrieved?  ****/
    if ((Parse_State == ONE_ARGUMENT) || (Parse_State == FIRST_ARGUMENT)
	|| (Parse_State == ONE_SPECIAL) || (Parse_State == FIRST_SPECIAL)) {
	elog_log(0, "get_option: Option argument not retrieved\n");
	Parse_State = ERROR_RECOVERY;
	return '-';
    }
    /****  Are we to the end of the arguments?  ****/
    if (Parse_State == ALL_ARGUMENTS) {
	elog_log(0, "get_option: All options were retrieved\n");
	return '-';
    }
    /****  Get the next option switch  ****/
    if ((NULL == comp_opt) || (NULL == *(comp_opt + 1))) {
	if (*Argv_Ptr == NULL) {
	    /****  End of the option switches  ****/
	    Argv_Ptr = argv + 1;
	    Arg_Count = follow_count;
	    *(Argv_Ptr + Arg_Count) = NULL;
	    *argc = follow_count + 1;

	    Parse_State = ALL_ARGUMENTS;
	    return END_OPTIONS;
	} else if (STREQ (*Argv_Ptr, "-")) {
	    /****  The user is only allowed one stdin diversion  ****/
	    if (dash_seen ()) {
		elog_log(0, "get_option: More than one '-' seen\n");
		return '-';
	    }
	    (void) shift_arg (&follow_ptr, &follow_count);

	    Parse_State = OPTION_ONLY;
	    return get_option (argc, argv, option_list, num_options);
	} else if (STREQ (*Argv_Ptr, "--")) {
	    /****  Force the end of option switches  ****/
	    Argv_Ptr++;
	    Arg_Count--;
	    while (shift_arg (&follow_ptr, &follow_count));

	    Parse_State = OPTION_ONLY;
	    return get_option (argc, argv, option_list, num_options);
	} else if (**Argv_Ptr == '-') {
	    for (index = 0; index < num_options; index++) {
		if (((*(*Argv_Ptr + 2) == '\0')
		     && (*(*Argv_Ptr + 1) == option_list[index].terse))
		   || STREQ ((*Argv_Ptr) + 1, option_list[index].verbose)) {
		    Argv_Ptr++;
		    Arg_Count--;
		    if (NO_ARGUMENT == option_list[index].arg)
			Parse_State = OPTION_ONLY;
		    else if (NORMAL_ARG == option_list[index].arg)
			Parse_State = ONE_ARGUMENT;
		    else if (NORMAL_ARGS == option_list[index].arg)
			Parse_State = FIRST_ARGUMENT;
		    else if (SPECIAL_ARG == option_list[index].arg)
			Parse_State = ONE_SPECIAL;
		    else if (SPECIAL_ARGS == option_list[index].arg)
			Parse_State = FIRST_SPECIAL;
		    else {
			elog_log(0, "get_option: unknown option type\n");
			return '-';
		    }

		    return option_list[index].terse;
		}
	    }
	    comp_opt = *Argv_Ptr;
	    Argv_Ptr++;
	    Arg_Count--;
	} else {
	    (void) shift_arg (&follow_ptr, &follow_count);
	    return get_option (argc, argv, option_list, num_options);
	}
    }
    /****  Compound argument  ****/
    comp_opt++;
    for (index = 0; index < num_options; index++) {
	if (*comp_opt == option_list[index].terse) {
	    if (NO_ARGUMENT != option_list[index].arg) {
		elog_log(0, "get_option: illegal compound option\n");
		return '-';
	    } else {
		return *comp_opt;
	    }
	}
    }

    elog_log(0, "get_option: unknown option...\n");
    return '-';
}



/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *
   shift_arg - shift the argument to the unassociated argument list.
 * -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

static int
shift_arg (follow_ptr, follow_count)
char         ***follow_ptr;	       /* End of unassociated arguments */
int            *follow_count;	       /* Number of unassociated arguments */

{
    if (NULL != *Argv_Ptr) {
	if (Argv_Ptr != *follow_ptr) {
	    **follow_ptr = *Argv_Ptr;
	}
	(*follow_ptr)++;
	Argv_Ptr++;
	Arg_Count--;
	(*follow_count)++;

	return 1;
    }
    return 0;
}



/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *
   dash_seen - was a "-" seen previously?
 * -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

static int
dash_seen ()
{
    static int      dash_flag = 0;     /* Have you seen a "-"?  */

    if (0 != dash_flag) {
	return dash_flag;
    } else {
	dash_flag = 1;
	return 0;
    }
}



/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *
   get_argument - get the next argument from the argument list.
 * -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

char           *
get_argument ()
{
    char           *return_value;

    /****  Make sure we should be here  ****/
    if (Parse_State == OPTION_ONLY) {
	elog_log(0, "get_argument: Argument not expected now\n");
	return NULL;
    }
    /****  Are we to the end of the argument list?  ****/
    if (NULL == *Argv_Ptr)
	return NULL;

    /****  Make a state transition  ****/
    switch (Parse_State) {
      case (ONE_ARGUMENT):
	if (STREQ ("--", *Argv_Ptr)) {
	    Argv_Ptr++;
	    Arg_Count--;
	    return_value = *Argv_Ptr;
	    Argv_Ptr++;
	    Arg_Count--;
	    Parse_State = OPTION_ONLY;
	    return return_value;
	} else if ('-' != **Argv_Ptr) {
	    return_value = *Argv_Ptr;
	    Argv_Ptr++;
	    Arg_Count--;
	    Parse_State = OPTION_ONLY;
	    return return_value;
	} else {
	    elog_log(0, "get_argument: special argument not expected\n");
	    Parse_State = ERROR_RECOVERY;
	    return NULL;
	}

      case (FIRST_ARGUMENT):
      case (MORE_ARGUMENTS):
	if (STREQ ("--", *Argv_Ptr)) {
	    Argv_Ptr++;
	    Arg_Count--;
	    return_value = *Argv_Ptr;
	    Argv_Ptr++;
	    Arg_Count--;
	    Parse_State = MORE_ARGUMENTS;
	    return return_value;
	} else if ('-' != **Argv_Ptr) {
	    return_value = *Argv_Ptr;
	    Argv_Ptr++;
	    Arg_Count--;
	    Parse_State = MORE_ARGUMENTS;
	    return return_value;
	} else {
	    Parse_State = OPTION_ONLY;
	    return NULL;
	}

      case (ONE_SPECIAL):
	if (STREQ ("--", *Argv_Ptr)) {
	    Argv_Ptr++;
	    Arg_Count--;
	    return_value = *Argv_Ptr;
	    Argv_Ptr++;
	    Arg_Count--;
	    Parse_State = OPTION_ONLY;
	    return return_value;
	} else {
	    return_value = *Argv_Ptr;
	    Argv_Ptr++;
	    Arg_Count--;
	    Parse_State = OPTION_ONLY;
	    return return_value;
	}

      case (FIRST_SPECIAL):
      case (MORE_SPECIALS):
	if (STREQ ("--", *Argv_Ptr)) {
	    Argv_Ptr++;
	    Arg_Count--;
	    return_value = *Argv_Ptr;
	    Argv_Ptr++;
	    Arg_Count--;
	    Parse_State = MORE_SPECIALS;
	    return return_value;
	} else {
	    return_value = *Argv_Ptr;
	    Argv_Ptr++;
	    Arg_Count--;
	    Parse_State = MORE_SPECIALS;
	    return return_value;
	}

      case (ALL_ARGUMENTS):
	return_value = *Argv_Ptr;
	Argv_Ptr++;
	Arg_Count--;
	Parse_State = ALL_ARGUMENTS;
	return return_value;

      case (ERROR_RECOVERY):
	return_value = *Argv_Ptr;
	Argv_Ptr++;
	Arg_Count--;
	Parse_State = ERROR_RECOVERY;
	return return_value;

      default:
	elog_log(0, "get_argument: internal error\n");
    }

    return NULL;
}



/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *
   get_directory - get the next input and ensure that it's a directory.
 * -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

char           *
get_directory ()
{
    DIR            *testDir;
    char           *dir_name;

    if ((dir_name = get_argument ()) == NULL) {
	elog_log(0, "get_directory: empty argument list\n");
	return NULL;
    }
    if ((testDir = opendir (dir_name)) == NULL) {
	return NULL;
    } else {
	closedir (testDir);
	return dir_name;
    }
}


/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *
   get_input_file - switch to the next file.  Note that the old file is
   closed unless it is stdin, or stdout.
 * -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

char           *
get_input_file (input_file)
FILE          **input_file;	       /* FILE assiciated with  */

{
    char           *input_name;

    if ((input_name = get_argument ()) == NULL) {
	elog_log(0, "get_input_file: empty argument list\n");
	return NULL;
    }
    if ((*input_file = open_file (*input_file, input_name, "r")) == NULL) {
	elog_log(0, "get_input_file: can't open input file '%s'\n",
			input_name);
	return NULL;
    }
    return input_name;
}



/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *
   get_output_file - switch to the next file.  Note that the old file is
   closed unless it is stdin, or stdout.
 * -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

char           *
get_output_file (output_file)
FILE          **output_file;

{
    char           *output_name;

    if ((output_name = get_argument ()) == NULL) {
	elog_log(0, "get_output_file: empty argument list\n");
	return NULL;
    }
    if ((*output_file = open_file (*output_file, output_name, "w")) == NULL) {
	elog_log(0, "get_output_file: can't open output file '%s'\n",
			output_name);
	return NULL;
    }
    return output_name;
}



/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *
   last_argument - get last argument from the command list
 * -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

char           *
last_argument ()
{
    char           *arg_name;

    if (Arg_Count <= 0) {
	elog_log(0, "last_argument: empty argument list\n");
	return (NULL);
    }
    arg_name = Argv_Ptr[Arg_Count - 1];
    Argv_Ptr[Arg_Count - 1] = NULL;
    Arg_Count--;

    return arg_name;
}



/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *
   last_input_file - get last file specified for input.
 * -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

char           *
last_input_file (input_file)
FILE          **input_file;

{
    char           *input_name;

    if (Arg_Count <= 0) {
	elog_log(0, "last_input_file: empty argument list\n");
	return (NULL);
    }
    input_name = Argv_Ptr[Arg_Count - 1];
    Argv_Ptr[Arg_Count - 1] = NULL;
    Arg_Count--;

    if ((*input_file = open_file (*input_file, input_name, "r")) == NULL) {
	elog_log(0, "last_input_file: can't open input file '%s'\n",
			input_name);
	return NULL;
    }
    return input_name;
}



/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *
   last_output_file - get last file specified for output.
 * -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

char           *
last_output_file (output_file)
FILE          **output_file;

{
    char           *output_name;

    if (Arg_Count <= 0) {
	elog_log(0, "last_output_file: empty argument list\n");
	return (NULL);
    }
    output_name = Argv_Ptr[Arg_Count - 1];
    Argv_Ptr[Arg_Count - 1] = NULL;
    Arg_Count--;

    if ((*output_file = open_file (*output_file, output_name, "w")) == NULL) {
	elog_log(0, "last_output_file: can't open output file '%s'\n",
			output_name);
	return NULL;
    }
    return output_name;
}



/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
   open_file - switch to the next file.  Note that the old file is closed
   unless it is stdin, or stdout.
 * -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

static FILE    *
open_file (old_file, new_name, access)
FILE           *old_file;
char           *new_name;
char           *access;

{
    if ((old_file != NULL) && (old_file != stdin) && (old_file != stdout)) {
	if (fclose (old_file) != 0) {
	    elog_log(1, "open_file: unable to close the old file\n");
	}
    }
    /****  Advance to the next file  ****/
    if (new_name == NULL) {
	elog_log(0, "open_file: NULL file name given\n");
	return NULL;
    }
    /****  Open the file, if possible  ****/
    if ((STREQ (new_name, "-")) && (STREQ (access, "r"))) {
	old_file = stdin;
    } else if ((STREQ (new_name, "-")) && (STREQ (access, "w"))) {
	old_file = stdout;
    } else if (((STREQ (access, "r")) && readable_file (new_name))
	       || ((STREQ (access, "w")) && writable_file (new_name))) {
	old_file = fopen (new_name, access);
	if (old_file == NULL) {
	    elog_log(1, "open_file: can't open file '%s'\n", new_name);
	    return NULL;
	}
    } else {
	elog_log(1, "open_file: can't open file '%s' for '%s'\n",
			new_name, access);
	return NULL;
    }

    /****  Everything worked like a charm, return the file name  ****/
    return old_file;
}



/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *
   readable_file - test if the file is readable.
 * -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

int
readable_file (file_name)
char           *file_name;	       /* Name of the file to test */

{
    struct stat     statbuf;
    FILE           *test_file;

    if (file_name == NULL) {
	elog_log(0, "readable_file: called with null file_name\n");
	return 0;
    }
    if (stat (file_name, &statbuf) != 0) {
	elog_log(1, "readable_file: can't stat file '%s'\n", file_name);
	return 0;
    }
    if (S_ISREG (statbuf.st_mode)) {
	if ((test_file = fopen (file_name, "r")) != NULL) {
	    fclose (test_file);
	    return 1;
	}
	return 0;
    }
    elog_log(0, "readable_file: file '%s' is not regular\n", file_name);
    return 0;
}



/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *
   writable_file - test if the file is writable.
 * -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

int
writable_file (file_name)
char           *file_name;	       /* Name of the file to test */

{
    struct stat     statbuf;
    FILE           *test_file;

    if (file_name == NULL) {
	elog_log(0, "writable_file: called with null file_name\n");
	return 0;
    }
    if (stat (file_name, &statbuf) != 0) {
	if (errno == ENOENT) {
	    return 1;
	}
	elog_log(1, "writable_file: can't stat file '%s'\n", file_name);
	return 0;
    }
    if (S_ISREG (statbuf.st_mode)) {
	if ((test_file = fopen (file_name, "a")) != NULL) {
	    fclose (test_file);
	    return 1;
	}
	return 0;
    }
    elog_log(0, "writable_file: file '%s' is not regular\n", file_name);
    return 0;
}



/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *
   parse_path - parse the path into directory and file name
 * -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

void
parse_path (path, dir, file)
char           *path;
char           *dir;
char           *file;

{
    char           *next = dir;
    char           *last = NULL;

    /****  Find the divider between directory and file  ****/
    strcpy (dir, path);
    while (*next != NULL) {
	if (*next == '/')
	    last = next;

	next++;
    }

    /****  Copy the file name to file ****/
    if (last == NULL) {
	strcpy (file, path);
    } else {
	strcpy (file, last + 1);
    }

    /****  Trim the file name off dir  ****/
    if (last == NULL) {
	*dir = '\0';
    } else {
	*last = '\0';
    }
}


/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *
   concat_paths - concatinate the two paths
 * -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

void
concat_paths (base, next, net)
char           *base;		       /* Base section of path */
char           *next;		       /* Next section of path */
char           *net;		       /* Composite path */

{
    int             name_len;
    int             name_index;
    char            temp[160];

    /****  Trivial cases  ****/
    if ((base == NULL) || (base[0] == '\0')) {
	strcpy (net, next);
	return;
    } else if ((next == NULL) || (next[0] == '\0')) {
	strcpy (net, base);
	return;
    } else if (next[0] == '/') {
	strcpy (net, next);
	return;
    } else if (base[0] == '.' && base[1] == 0
	       && next[0] == '.' && next[1] == '/') {
	strcpy (net, next);
	return;
    }
    /****  Copy the base  ****/
    strcpy (temp, base);
    add_slash (temp);

    /****  Copy the next segment to the end of the path  ****/
    name_len = strlen (temp) + strlen (next);
    for (name_index = strlen (temp); name_index < name_len; name_index++) {
	temp[name_index] = *next;
	next++;
    }
    temp[name_index] = '\0';

    /****  Remove backups / stalls  ****/
    /* clean_path(temp); This code doesn't work !! */

    strcpy (net, temp);
}



/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *
   clean_path - cleanup any `//', `/../' or `/./' segments

   Finite State Machine:
	
         	  save 	  |<---     |<-------------------------------
	       	  section |   ^     |				    ^
       	           [^/]	  |   |     |     			    |
		   	  v   |     v				    |
		       	+-------------+	 `\0'			    |
		------->| START_STATE |---------->|		    |
		^       +-------------+		  |		    |
		|         |     		  |		    |
		|         |      `/'    	  |		    |
		|      `/'|    |<----   	  | combine         |
		|         |    |    ^   	  | sections        |
		|         v    v    |   	  |	            |
		|  [^./] +-----------+	`\0'	  |	            |
		|<-------| ONE_SLASH |----------->|	            |
		|        +-----------+		  |	            |
		|          |       ^    	  v	            |
       combine  |          |       |   	    +-----------+           |
       sections |       `.'|       |`/'     | END_STATE |           | drop
		|          |       |        +-----------+           | section
		|          v       |   	       	  ^	            |
		|  [^./]  +---------+	`\0'      |	            |
		|<--------| ONE_DOT |------------>|	            |
		|         +---------+		  |	            |
		|              |		  |	            |
		|              |		  | drop       	    |
		|              | `.'		  | section         |
		|              |		  |		    |
		|              v		  |		    |
		|  [^/]   +----------+	`\0'      |		    |
		|<--------| TWO_DOTS |----------->|		    |
			  +----------+				    |
			       |				    |
			       v	`/'     		    |
			       ------------------------------------>|
			
 * -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

#define  START_STATE  (0)
#define  ONE_SLASH    (1)
#define  ONE_DOT      (2)
#define  TWO_DOTS     (3)
#define  END_STATE    (4)

void
clean_path (path)
char           *path;

{
    char            result[80];
    char            buffer[80];
    char           *temp;
    int             state;
    char           *next;

    result[0] = '\0';
    buffer[0] = '\0';
    temp = buffer;
    state = START_STATE;
    next = path;
    do {
	switch (state) {
	  case (START_STATE):
	    if (*next == '\0') {
		*temp = '\0';
		strcat (result, buffer);
		state = END_STATE;
	    } else if (*next == '/') {
		*temp = '\0';
		state = ONE_SLASH;
	    } else {
		*temp = *next;
		temp++;
		state = START_STATE;
	    }
	    break;
	  case (ONE_SLASH):
	    if (*next == '\0') {
		*temp = '\0';
		strcat (result, buffer);
		state = END_STATE;
	    } else if (*next == '/') {
		state = ONE_SLASH;
	    } else if (*next == '.') {
		state = ONE_DOT;
	    } else {
		strcat (result, buffer);
		strcat (result, "/");
		buffer[0] = *next;
		temp = buffer + 1;
		state = START_STATE;
	    }
	    break;
	  case (ONE_DOT):
	    if (*next == '\0') {
		*temp = '\0';
		strcat (result, buffer);
		state = END_STATE;
	    } else if (*next == '/') {
		state = ONE_SLASH;
	    } else if (*next == '.') {
		state = TWO_DOTS;
	    } else {
		strcat (result, buffer);
		strcat (result, "/");
		buffer[0] = '.';
		buffer[1] = *next;
		temp = buffer + 2;
		state = START_STATE;
	    }
	    break;
	  case (TWO_DOTS):
	    if (*next == '\0') {
		state = END_STATE;
	    } else if (*next == '/') {
		temp = buffer;
		state = START_STATE;
	    } else {
		strcat (result, buffer);
		strcat (result, "/");
		buffer[0] = '.';
		buffer[1] = '.';
		buffer[2] = *next;
		temp = buffer + 3;
		state = START_STATE;
	    }
	    break;
	  case (END_STATE):
	    del_slash (result);
	    strcpy (path, result);
	    return;
	  default:
	    fprintf (stderr, "ERROR(clean_path): unknown state %i\n", state);
	    exit (1);
	}
	next++;
    }
    while (1);

    fprintf (stderr, "ERROR(clean_path): unknown error\n");
    exit (1);
}


/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *
   add_slash - Add a trailing '/' to the end of the directory name if
   required.
 * -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

void
add_slash (directory)
char           *directory;	       /* Name of the directory path */

{
    /****  Don't add a slash if the directory is null  ****/
    if (*directory == NULL)
	return;

    /****  Scan to the end of the directory name.  ****/
    while (*directory != NULL)
	directory++;

    /****  Add a trailing slash if required.  ****/
    if (*(directory - 1) != '/') {
	*directory = '/';
	*(directory + 1) = NULL;
    }
}



/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *
  del_slash -  delete a trailing '/' from the end of the directory name if
  required.
 * -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

void
del_slash (directory)
char           *directory;	       /* Name of the directory path */

{
    /****  Scan to the end of the directory name.  ****/
    while (*directory != NULL)
	directory++;

    /****  Add a trailing slash if required.  ****/
    if (*(directory - 1) == '/') {
	*(directory - 1) = NULL;
    }
}


#ifdef DEBUG_TMPL
static option_t option_list[] =
{
    'a', "a_argument", NO_ARGUMENT,
    'b', "b_argument", NO_ARGUMENT,
    'n', "normal_arg", NORMAL_ARG,
    'N', "normal_args", NORMAL_ARGS,
    's', "special_arg", SPECIAL_ARG,
    'S', "special_args", SPECIAL_ARGS,
};

#define NUM_OPTIONS (sizeof(option_list) / sizeof(option_t))

#define TEST_NO(num) \
  printf("Performing test number: %i\n", num)

#define GET(option) \
    if ((option = get_option(&argc, argv, option_list, NUM_OPTIONS)) \
      == END_OPTIONS) \
      printf("ERROR - Couldn't get the option"); \
    else

#define NO_GET(option) \
    if ((option = get_option(&argc, argv, option_list, NUM_OPTIONS)) \
      != '-') \
      printf("ERROR - Got an option before the argument"); \
    else

#define TEST_GET(option, truth) \
    if (option != truth) \
      printf("ERROR - Wrong value returned %c not %c\n", option, truth); \
    else

#define TEST_NO_ARG() \
  if (get_argument() != NULL) \
    printf("ERROR - argument returned from get_argument()\n"); \
  else

#define TEST_ARG(arg, truth) \
  if (((arg = get_argument()) == NULL) \
      || !STREQ(arg, truth)) \
    printf("ERROR - bad argument returned from get_argument %s not %s\n", \
	   arg, truth); \
  else


main ()
{
    char            option;
    char           *arg;

    static char    *argv[] =
    {
	"tmpl",

	/****  Test simple options  (1)  ****/
	"-a",
	"-a_argument",
	"-b",
	"-b_argument",

	/****  Test compound options  (5)  ****/
	"-ba",
	"-abba",

	/****  Test simple argument options  (11)  ****/
	"-n", "normal",
	"-normal_arg", "normal",

	/****  Test option arguments with premature GET  (13)  ****/
	"-n", "normal",
	"-normal_arg", "normal",

	/****  Test option with illegal value  (15) ****/
	"-n", "-illegal",
	"-normal_arg", "-normal_arg",

	/****  Test option with missing argument  (17)  ****/
	"-n", "-a_argument",
	"-normal_arg", "-normal_arg", "normal",

	/****  Test options with multiple arguments  (19)  ****/
	"-N", "normal",
	"-normal_args", "normal",
	"-N", "normal", "normal", "normal",
	"-normal_args", "normal", "normal", "normal",

	/****  Test options with premature GET  (23) ****/
	"-N", "normal",
	"-normal_args", "normal", "normal",

	/****  Test options with illegal values  (25) ****/
	"-N", "-illegal", "-illegal",
	"-normal_args", "-normal_args", "-normal_args",

	/****  Test options with missing arguments  (27)  ****/
	"-N", "-a_argument",
	"-normal_args", "-normal_args", "normal", "normal",

	/****  Test special argument options  (29)  ****/
	"-s", "-special",
	"-special_arg", "-special",

	/****  Test special options with multiple arguments  (31)  ****/
	"-S", "-special",
	"-special_args", "-special",
	"-S", "-special", "-special", "-special",
	"-special_args", "-special", "-special", "-special",

	/****  Test the `--' argument protector  (35)  ****/
	"-n", "--", "-normal",
	"-n", "--", "--",
	"-N", "--", "-normal", "--", "--",
	"-s", "--", "-special",
	"-s", "--", "--",
	"-S", "--", "-special", "--", "--",

	/****  Test the argument shuffling  (41)  ****/
	"shift_1",
	"-a",
	"shift_2",
	"-n", "normal",
	"shift_3",
	"-N", "normal", "normal",
	"shift_4",
	"-s", "-special",
	"shift_5",
	"-S", "-special", "-special",
	"shift_6",

	/****  Test the option `--'  (47)  ****/
	"--", "-a", "-n", "-N", "-s", "-S", "--",

	NULL
    };

    int             argc = sizeof (argv) / sizeof (char *);

    /****  Test simple options  ****/
    TEST_NO (1);
    GET (option);
    TEST_GET (option, 'a');

    TEST_NO (2);
    GET (option);
    TEST_GET (option, 'a');

    TEST_NO (3);
    GET (option);
    TEST_GET (option, 'b');

    TEST_NO (4);
    GET (option);
    TEST_GET (option, 'b');

    /****  Test compound options  ****/
    TEST_NO (5);
    GET (option);
    TEST_GET (option, 'b');

    TEST_NO (6);
    GET (option);
    TEST_GET (option, 'a');

    TEST_NO (7);
    GET (option);
    TEST_GET (option, 'a');
    TEST_NO_ARG ();

    TEST_NO (8);
    GET (option);
    TEST_GET (option, 'b');
    TEST_NO_ARG ();

    TEST_NO (9);
    GET (option);
    TEST_GET (option, 'b');
    TEST_NO_ARG ();

    TEST_NO (10);
    GET (option);
    TEST_GET (option, 'a');
    TEST_NO_ARG ();

    /****  Test simple argument options  ****/
    TEST_NO (11);
    GET (option);
    TEST_GET (option, 'n');
    TEST_ARG (arg, "normal");

    TEST_NO (12);
    GET (option);
    TEST_GET (option, 'n');
    TEST_ARG (arg, "normal");

    /****  Test option arguments with premature GET  ****/
    TEST_NO (13);
    GET (option);
    TEST_GET (option, 'n');
    NO_GET (option);
    TEST_ARG (arg, "normal");

    TEST_NO (14);
    GET (option);
    TEST_GET (option, 'n');
    NO_GET (option);
    TEST_ARG (arg, "normal");

    /****  Test option with illegal value  ****/
    TEST_NO (15);
    GET (option);
    TEST_GET (option, 'n');
    NO_GET (option);
    TEST_ARG (arg, "-illegal");

    TEST_NO (16);
    GET (option);
    TEST_GET (option, 'n');
    NO_GET (option);
    TEST_ARG (arg, "-normal_arg");

    /****  Test option with missing argument  ****/
    TEST_NO (17);
    GET (option);
    TEST_GET (option, 'n');
    NO_GET (option);
    GET (option);
    TEST_GET (option, 'a');

    TEST_NO (18);
    GET (option);
    TEST_GET (option, 'n');
    NO_GET (option);
    GET (option);
    TEST_ARG (arg, "normal");

    /****  Test options with multiple arguments  ****/
    TEST_NO (19);
    GET (option);
    TEST_GET (option, 'N');
    NO_GET (option);
    TEST_ARG (arg, "normal");

    TEST_NO (20);
    GET (option);
    TEST_GET (option, 'N');
    TEST_ARG (arg, "normal");

    TEST_NO (21);
    GET (option);
    TEST_GET (option, 'N');
    TEST_ARG (arg, "normal");
    TEST_ARG (arg, "normal");
    TEST_ARG (arg, "normal");

    TEST_NO (22);
    GET (option);
    TEST_GET (option, 'N');
    TEST_ARG (arg, "normal");
    TEST_ARG (arg, "normal");
    TEST_ARG (arg, "normal");

    /****  Test options with premature GET  ****/
    TEST_NO (23);
    GET (option);
    TEST_GET (option, 'N');
    NO_GET (option);
    TEST_ARG (arg, "normal");

    TEST_NO (24);
    GET (option);
    TEST_GET (option, 'N');
    NO_GET (option);
    TEST_ARG (arg, "normal");
    TEST_ARG (arg, "normal");

    /****  Test options with illegal values  ****/
    TEST_NO (25);
    GET (option);
    TEST_GET (option, 'N');
    NO_GET (option);
    TEST_ARG (arg, "-illegal");
    TEST_ARG (arg, "-illegal");

    TEST_NO (26);
    GET (option);
    TEST_GET (option, 'N');
    NO_GET (option);
    TEST_ARG (arg, "-normal_args");
    TEST_ARG (arg, "-normal_args");

    /****  Test options with missing arguments   ****/
    TEST_NO (27);
    GET (option);
    TEST_GET (option, 'N');
    NO_GET (option);
    GET (option);
    TEST_GET (option, 'a');

    TEST_NO (28);
    GET (option);
    TEST_GET (option, 'N');
    NO_GET (option);
    GET (option);
    TEST_ARG (arg, "normal");
    TEST_ARG (arg, "normal");

    /****  Test special argument options  ****/
    TEST_NO (29);
    GET (option);
    TEST_GET (option, 's');
    TEST_ARG (arg, "-special");

    TEST_NO (30);
    GET (option);
    TEST_GET (option, 's');
    TEST_ARG (arg, "-special");

    /****  Test special options with multiple arguments  ****/
    TEST_NO (31);
    GET (option);
    TEST_GET (option, 'S');
    NO_GET (option);
    TEST_ARG (arg, "-special");

    TEST_NO (32);
    GET (option);
    TEST_GET (option, 'S');
    TEST_ARG (arg, "-special");

    TEST_NO (33);
    GET (option);
    TEST_GET (option, 'S');
    TEST_ARG (arg, "-special");
    TEST_ARG (arg, "-special");
    TEST_ARG (arg, "-special");

    TEST_NO (34);
    GET (option);
    TEST_GET (option, 'S');
    TEST_ARG (arg, "-special");
    TEST_ARG (arg, "-special");
    TEST_ARG (arg, "-special");

    /****  Test the `--' argument protector  (35)  ****/
    TEST_NO (35);
    GET (option);
    TEST_GET (option, 'n');
    TEST_ARG (arg, "-normal");

    TEST_NO (36);
    GET (option);
    TEST_GET (option, 'n');
    TEST_ARG (arg, "--");

    TEST_NO (37);
    GET (option);
    TEST_GET (option, 'N');
    TEST_ARG (arg, "-normal");
    TEST_ARG (arg, "--");

    TEST_NO (38);
    GET (option);
    TEST_GET (option, 's');
    TEST_ARG (arg, "-special");

    TEST_NO (39);
    GET (option);
    TEST_GET (option, 's');
    TEST_ARG (arg, "--");

    TEST_NO (40);
    GET (option);
    TEST_GET (option, 'S');
    TEST_ARG (arg, "-special");
    TEST_ARG (arg, "--");

    /****  Test the argument shuffling  (41)  ****/
    TEST_NO (41);
    GET (option);
    TEST_GET (option, 'a');

    TEST_NO (42);
    GET (option);
    TEST_GET (option, 'n');
    TEST_ARG (arg, "normal");

    TEST_NO (43);
    GET (option);
    TEST_GET (option, 'N');
    TEST_ARG (arg, "normal");
    TEST_ARG (arg, "normal");

    TEST_NO (44);
    GET (option);
    TEST_GET (option, 's');
    TEST_ARG (arg, "-special");

    TEST_NO (45);
    GET (option);
    TEST_GET (option, 'S');
    TEST_ARG (arg, "-special");
    TEST_ARG (arg, "-special");

    TEST_NO (46);
    if ((option = get_option (&argc, argv, option_list, NUM_OPTIONS))
	    != END_OPTIONS) {
	fprintf (stderr, "ERROR - Still getting options");
    }
    /****  Test the option `--'  ****/
    TEST_NO (45);
    TEST_ARG (arg, "shift_1");

    TEST_NO (46);
    TEST_ARG (arg, "shift_2");

    TEST_NO (47);
    TEST_ARG (arg, "shift_3");

    TEST_NO (48);
    TEST_ARG (arg, "shift_4");

    TEST_NO (49);
    TEST_ARG (arg, "shift_5");

    TEST_NO (50);
    TEST_ARG (arg, "shift_6");

    TEST_NO (51);
    TEST_ARG (arg, "-a");

    TEST_NO (52);
    TEST_ARG (arg, "-n");

    TEST_NO (53);
    TEST_ARG (arg, "-N");

    TEST_NO (54);
    TEST_ARG (arg, "-s");

    TEST_NO (55);
    TEST_ARG (arg, "-S");

    TEST_NO (56);
    TEST_ARG (arg, "--");

    exit (0);
}

#endif


/* $Id$ */
