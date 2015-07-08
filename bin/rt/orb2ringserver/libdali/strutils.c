/***********************************************************************//**
 * @file strutils.c
 *
 * Generic routines for string manipulation
 *
 * @author Chad Trabant, IRIS Data Management Center
 *
 * modified: 2008.193
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "libdali.h"


/***********************************************************************//**
 * @brief Parse/split a string on a specified delimiter
 *
 * Splits a 'string' on 'delim' and puts each part into a linked list
 * pointed to by 'list' (a pointer to a pointer).  The last entry has
 * it's 'next' set to 0.  All elements are NULL terminated strings.
 *
 * It is up to the caller to free the memory associated with the
 * returned list.  To facilitate freeing this special string list
 * dl_strparse() can be called with both 'string' and 'delim' set to
 * NULL and then the linked list is traversed and the memory used is
 * free'd and the list pointer is set to NULL.
 *
 * @param string String to parse/split
 * @param delim Delimiter to split string on
 * @param list Returned list of sub-strings.
 *
 * @return The number of elements added to the list, or 0 when freeing
 * the linked list.
 ***************************************************************************/
int
dl_strparse (const char *string, const char *delim, DLstrlist **list)
{
  const char *beg;			/* beginning of element */
  const char *del;			/* delimiter */
  int stop = 0;
  int count = 0;
  int total;

  DLstrlist *curlist = 0;
  DLstrlist *tmplist = 0;

  if (string != NULL && delim != NULL)
    {
      total = strlen (string);
      beg = string;

      while (!stop)
	{

	  /* Find delimiter */
	  del = strstr (beg, delim);

	  /* Delimiter not found or empty */
	  if (del == NULL || strlen (delim) == 0)
	    {
	      del = string + strlen (string);
	      stop = 1;
	    }

	  tmplist = (DLstrlist *) malloc (sizeof (DLstrlist));
	  tmplist->next = 0;

	  tmplist->element = (char *) malloc (del - beg + 1);
	  strncpy (tmplist->element, beg, (del - beg));
	  tmplist->element[(del - beg)] = '\0';

	  /* Add this to the list */
	  if (count++ == 0)
	    {
	      curlist = tmplist;
	      *list = curlist;
	    }
	  else
	    {
	      curlist->next = tmplist;
	      curlist = curlist->next;
	    }

	  /* Update 'beg' */
	  beg = (del + strlen (delim));
	  if ((beg - string) > total)
	    break;
	}

      return count;
    }
  else
    {
      curlist = *list;
      while (curlist != NULL)
	{
	  tmplist = curlist->next;
	  free (curlist->element);
	  free (curlist);
	  curlist = tmplist;
	}
      *list = NULL;

      return 0;
    }
}  /* End of dl_strparse() */


/***********************************************************************//**
 * @brief Copy a string while removing space charaters
 *
 * Copy @a length characters from @a source to @a dest while removing
 * all spaces.  The result is left justified and always null
 * terminated.  The source string must have at least @a length
 * characters and the destination string must have enough room needed
 * for the non-space characters within @a length and the null
 * terminator.
 * 
 * @param dest Destination string
 * @param source String to copy
 * @param length Copy up to a maximum of this many characters to @a dest
 *
 * @return The number of characters (not including the null
 * terminator) in the destination string.
 ***************************************************************************/
int
dl_strncpclean (char *dest, const char *source, int length)
{
  int sidx, didx;

  for ( sidx=0, didx=0; sidx < length ; sidx++ )
    {
      if ( *(source+sidx) != ' ' )
	{
	  *(dest+didx) = *(source+sidx);
	  didx++;
	}
    }

  *(dest+didx) = '\0';

  return didx;
}  /* End of dl_strncpclean() */


/***********************************************************************//**
 * @brief Concatinate one string to another growing the destination as needed
 *
 * Concatinate one string to another with a delimiter in-between
 * growing the destination string as needed up to a maximum length.
 * 
 * @param string Destination string to be added to
 * @param add String to add to @a string
 * @param delim Optional delimiter between added strings (cannot be NULL, but can be an empty string)
 * @param maxlen Maximum number of bytes to grow @a string
 *
 * @return 0 on success, -1 on memory allocation error and -2 when
 * string would grow beyond maximum length.
 ***************************************************************************/
int
dl_addtostring (char **string, char *add, char *delim, int maxlen)
{
  int length;
  char *ptr;
  
  if ( ! string || ! add )
    return -1;
  
  /* If string is empty, allocate space and copy the addition */
  if ( ! *string )
    {
      length = strlen (add) + 1;
      
      if ( length > maxlen )
        return -2;
      
      if ( (*string = (char *) malloc (length)) == NULL )
        return -1;
      
      strcpy (*string, add);
    }
  /* Otherwise tack on the addition with a delimiter */
  else
    {
      length = strlen (*string) + strlen (delim) + strlen(add) + 1;
      
      if ( length > maxlen )
        return -2;
      
      if ( (ptr = (char *) realloc (*string, length)) == NULL )
        return -1;
      
      *string = ptr;
      
      strcat (*string, delim);
      strcat (*string, add);
    }
  
  return 0;
}  /* End of dl_addtostring() */
