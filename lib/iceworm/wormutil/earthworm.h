
/*
 *   THIS FILE IS UNDER RCS - DO NOT MODIFY UNLESS YOU HAVE
 *   CHECKED IT OUT USING THE COMMAND CHECKOUT.
 *
 *    $Id$
 *
 *    Revision history:
 *     $Log$
 *     Revision 1.3  2003/06/01 08:25:39  lindquis
 *     Upgrade Iceworm libraries to Earthworm6.2. Add some rudimentary man
 *     pages. Preparation for the rewritten ew2orb.
 *
 *     Revision 1.9  2001/04/06 21:03:30  davidk
 *     moved the guts of earthworm.h into three separate files, which
 *     are now #include'd in earthworm.h.
 *
 *     Revision 1.8  2001/04/05 18:31:04  cjbryan
 *     added prototype for RecursiveCreateDir
 *     also added MAX_DIR_LEN for max path lenghts
 *
 *     Revision 1.7  2001/03/21 23:12:31  cjbryan
 *     *** empty log message ***
 *
 *     Revision 1.6  2000/09/28 22:10:08  dietz
 *     *** empty log message ***
 *
 *     Revision 1.5  2000/08/09 16:47:55  lucky
 *     Added prototype for html_logit (to quiet down lint)
 *
 *     Revision 1.4  2000/07/27 16:15:56  lucky
 *     Added constants to set the max number of phases, and related max
 *     sizes of events in the database, which are different than the
 *     pre-existing limits on the events processed by hypoinverse
 *
 *     Revision 1.3  2000/07/24 20:49:17  lucky
 *     Added MAX_MOD_STR, MAX_INST_STR, MAX_RING_STR, and MAX_TYPE_STR in
 *     order to implement global limits to module, installation, ring,
 *     and message type strings.
 *
 *     Revision 1.2  2000/07/20 17:45:32  lucky
 *     Increased MAX_PHS_PER_EQ to accomodate large events, especially of Dewey type.
 *
 *     Revision 1.1  2000/02/14 20:05:54  lucky
 *     Initial revision
 *
 *
 */


          /***************************************************
           *                  earthworm.h                    *
           *                                                 *
           *         Earthworm administrative setup:         *
           *        global info for all installations        *
           *                                                 *
           ***************************************************/

#ifndef EARTHWORM_H
#define EARTHWORM_H

/* include simple definitions */
#include "earthworm_defs.h"

/* include simple functions whose prototypes do
   not require any convoluted STUFF from platform.h */
#include "earthworm_simple_funcs.h"

/* include the really ugly stuff that won't event compile 
   without platform.h or other include files */
#include "earthworm_complex_funcs.h"

#endif
