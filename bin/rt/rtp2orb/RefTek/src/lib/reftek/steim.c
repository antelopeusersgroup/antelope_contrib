#pragma ident "$Id$"
/* Steim 1 compression and decompression routines for RefTek PASSCAL packets.
 *
 * These routines are based on the work of many others, the compression
 * algorithm implementation is based on the work of Guy Stewart at IRIS.
 *
 * Developed using Microsoft C/C++ 7.0 in DOS by:
 *
 *    Robert Banfill (banfill@gldvxa.cr.usgs.gov)
 *    Small Systems Support
 *    2 Boston Harbor Place
 *    Big Water,  UT  84741-0205
 *    (801) 675-5827 Voice, (801) 675-3780 Fax
 *
 * Revision history:
 *
 *    First effort, 13-May-1993, RLB
 *    Modified for RefTek PASSCAL packets, 01-Nov-1993, RLB
 *    Modified for for use in RTP library, 18-Jul-1198, DEC
 */

/* If REVERSE_BYTE_ORDER is defined, the encode_steim( ) and decode_steim( )
 * functions will swap the byte order of all INT16 and INT32 integers
 * during processing.  This constant should be defined if this code
 * will run on an 80x86 or VAX.  If this code will run on 68000
 * hardware, this constant should not be defined as no swapping is
 * required.
 * 
 * THIS CONSTANT IS DEFINED AT COMPILE TIME IN "private.h", THERE IS NO
 * NEED TO DEFINE OR UNDEF IT HERE.
 * 
 */

/* Debug messages */
#undef DEBUG

/* Includes ----------------------------------------------------------- */
#include <stdio.h>         /* Standard C runtime */
#include <stdlib.h>        /* . */

#include "private.h"

/* Byte order swaps for 16 and 32 bit integers */
INT16 swap_w( INT16 inword );
INT32 swap_l( INT32 inlong );

/* Local helper functions, these should not be called externally */
static BOOL _save_chunk( STEIM *rec, DATA_STATE *ds, CHUNK chunk, INT32 c_type );
static VOID _finish_rec( VOID *ptr, DATA_STATE *ds );
static INT8 _chunk_type( UINT32 flags, INT8 c_ndx );

/* Globals ------------------------------------------------------------ */

/* DFA transition table */
TRANSITION transition[] = {
   { _D1,            0,  0 },
   { _D2,            0,  0 },
   { _D4_f,          0,  0 },
   { _D1_D1,         0,  1 },
   { _D2_D2_f,       0,  1 },
   { _D4_f,          1, -1 },
   { _D2_D2_f,       0,  1 },
   { _D2_D2_f,       0,  1 },
   { _D4_f,          1, -1 },
   { _D1_D1_D1,      0,  2 },
   { _D2_D2_f,       1, -1 },
   { _D2_D2_f,       1, -1 },
   { _D1_D1_D1_D1_f, 0,  3 },
   { _D2_D2_f,       2, -1 },
   { _D2_D2_f,       2, -1 }
};

/* Final state indicators */
BOOL final[] = {
   FALSE,
   FALSE,
   FALSE,
   FALSE,
   FALSE,
   TRUE,
   TRUE,
   TRUE
};

/*---------------------------------------------------------------------
 * Encode INT32 samples using Steim 1 compression.
 *
 *    INT16 encode_steim( INT32 *samples, INT16 n_rawsamp, VOID *ptr );
 *
 * Arguments:
 *    samples   = INT32 array of uncompressed samples
 *    n_rawsamp = Number of samples in samples array
 *    ptr       = VOID pointer to a SEED 2.0 data record
 *
 * Returns the actual number of samples Steim 1 encoded in the PASSCAL
 * data record.
 *
 * ptr should point to a 960 byte block of memory that will contain the
 * PASSCAL output record.  samples should be a INT32 array with at least 892
 * elements and n_rawsamp should specify the actual number of samples
 * contained in samples starting at samples[0].
 *
 * The algorithm:
 *
 * Each Steim compressed PASSCAL record is composed of a 64 byte fixed data
 * header followed by 15 frames of 64 bytes, each beginning with a 4 byte
 * control header and followed by 15 4 bytes "chunks".  Each of these chunks
 * can contain 4 byte values, 2 word values, or 1 long value, depending on
 * the 2 bit flags contained in the control header.  Generally all binary
 * data in PASSCAL records are in big endian (68000) byte order.
 *
 * The compressor is implemented as a Deterministic Finite Automaton (DFA).
 * The transition table for the DFA is:
 *
 * note: _f signifies a final state.
 * ----------------------------------------------------------
 *                | # of    |                | # of  |
 *                | bytes   |                | DIFS  |
 *                | DIF     |                | to    | DIF
 * Current state  | fits in | New State      | unget | index
 * ---------------+---------+----------------+-------+-------
 * _START_STATE   |  1      | _D1            | 0     |  0
 * _START_STATE   |  2      | _D2            | 0     |  0
 * _START_STATE   |  4      | _D4_f          | 0     |  0
 *          _D1   |  1      | _D1_D1         | 0     |  1
 *          _D1   |  2      | _D2_D2_f       | 0     |  1
 *          _D1   |  4      | _D4_f          | 1     | -1
 *          _D2   |  1      | _D2_D2_f       | 0     |  1
 *          _D2   |  2      | _D2_D2_f       | 0     |  1
 *          _D2   |  4      | _D4_f          | 1     | -1
 *       _D1_D1   |  1      | _D1_D1_D1      | 0     |  2
 *       _D1_D1   |  2      | _D2_D2_f       | 1     | -1
 *       _D1_D1   |  4      | _D2_D2_f       | 1     | -1
 *    _D1_D1_D1   |  1      | _D1_D1_D1_D1_f | 0     |  3
 *    _D1_D1_D1   |  2      | _D2_D2_f       | 2     | -1
 *    _D1_D1_D1   |  4      | _D2_D2_f       | 2     | -1
 * ----------------------------------------------------------
 */

INT16 encode_steim( INT32 *samples, INT16 n_rawsamp, VOID *ptr ) {
   INT8  state, token, t_ndx;
   UINT16 n_samp;
   INT32  i, dif, difs[4], c_type;
   CHUNK chunk;
   DATA_STATE  ds;
   STEIM *rec;

   /* Initialize the data state */
   ds.x0    = samples[0];     /* Save the forward integrating constant */
   ds.xn    = 0;              /* First dif will = x-0                  */
   ds.flags = 0x00000000;     /* Clear the frame flags                 */
   ds.f_ndx = 0;              /* Start at first frame,                 */
   ds.c_ndx = 2;              /* 3rd chunk, the 1st and 2nd will store */
                              /* the integrating constants             */

   /* Initialize the sample counter and record pointer */
   n_samp = 0;
   rec = (STEIM *)((INT8 *)ptr+64);

   /* Initialize the DFA state */
   state = _START_STATE;

   /* Start of compression loop, compress until out of samples or the */
   /* record is full (_save_chunk() returns FALSE). */
   for( i=0; i<n_rawsamp; i++ ) {

      /* Compute the difference */
      dif = samples[i] - ds.xn;
      ds.xn = samples[i];

      /* Examine the difference */
      if( dif <= 127 && dif >= -128 )
         token = 0;
      else if( dif <= 32767 && dif >= -32768 )
         token = 1;
      else
         token = 2;

#ifdef DEBUG
      printf( "state:     %d, i: %ld, sam: %ld, dif: %ld, token: %d\n",
         state, i, samples[i], dif, token );
#endif

      /* Make the transition */
      t_ndx = (state * 3) + token;

      /* Unget samples as needed */
      if( transition[t_ndx].unget ) {
         i -= transition[t_ndx].unget;
         ds.xn = samples[i];
      }

      /* Save dif (if needed) in the proper slot in the buffer */
      if( transition[t_ndx].d_ndx >= 0 )
         difs[transition[t_ndx].d_ndx] = dif;

      /* Get new state */
      state = transition[t_ndx].new_state;

#ifdef DEBUG
      printf( "new state: %d, i: %ld, buf: %ld %ld %ld %ld\n",
         state, i, difs[0], difs[1], difs[2], difs[3] );
#endif

      /* Deal with final states */
      if( final[state] ) {
         switch( state ) {

            /* 1 INT32 difference */
            case _D4_f:
               chunk.l = difs[0];
               c_type = CHUNK_LONG;
               n_samp += 1;
               break;

            /* 2 INT16 differences */
            case _D2_D2_f:
               chunk.w[0] = (UINT16)difs[0];
               chunk.w[1] = (UINT16)difs[1];
               c_type = CHUNK_WORDS;
               n_samp += 2;
               break;

            /* 4 BYTE differences */
            case _D1_D1_D1_D1_f:
               chunk.b[0] = (INT8)difs[0];
               chunk.b[1] = (INT8)difs[1];
               chunk.b[2] = (INT8)difs[2];
               chunk.b[3] = (INT8)difs[3];
               c_type = CHUNK_BYTES;
               n_samp += 4;
               break;
         }

         /* Save this chunk and see if we're done */
         if( ! _save_chunk( rec, &ds, chunk, c_type ) )
            break;

         /* Reset the state */
         state = _START_STATE;
      }
   /* End of compression loop */
   }

   /* If we ran out of data before running out of room, resolve any */
   /* non-final state */
   if( ! final[state] ) {
      switch( state ) {

         /* 1 BYTE or INT16 difference, save as a INT32 */
         case _D1:
         case _D2:
            chunk.l = difs[0];
            c_type = CHUNK_LONG;
            n_samp += 1;
            _save_chunk( rec, &ds, chunk, c_type );
            break;

         /* 2 BYTE differences, save as INT16's */
         case _D1_D1:
            chunk.w[0] = (UINT16)difs[0];
            chunk.w[1] = (UINT16)difs[1];
            c_type = CHUNK_WORDS;
            n_samp += 2;
            _save_chunk( rec, &ds, chunk, c_type );
            break;

         /* 3 BYTE differences, save first 2 as INT16S's, 3rd as a INT32 */
         case _D1_D1_D1:
            chunk.w[0] = (UINT16)difs[0];
            chunk.w[1] = (UINT16)difs[1];
            c_type = CHUNK_WORDS;
            n_samp += 2;
            if( ! _save_chunk( rec, &ds, chunk, c_type ) )
               break;
            chunk.l = difs[2];
            c_type = CHUNK_LONG;
            n_samp += 1;
            _save_chunk( rec, &ds, chunk, c_type );
            break;
      }
   }

   /* Finish out the record */
   if( n_samp > 0 )
      _finish_rec( ptr, &ds );

   /* Return the number of samples actually encoded */
   return( (UINT16)n_samp );
}

/*---------------------------------------------------------------------
 * Save the current chunk, called by encode_steim().
 * Returns TRUE if there is room for another chunk, FALSE if not.
 */

BOOL _save_chunk( STEIM *rec, DATA_STATE *ds, CHUNK chunk, INT32 c_type ) {
   INT8 tmp;

   /* Update frame flags */
   ds->flags |= c_type << (2 * (15 - (ds->c_ndx + 1)));

#ifdef REVERSE_BYTE_ORDER
   /* Perform byte swapping as needed */
   switch( c_type ) {
      case CHUNK_WORDS:
         tmp = chunk.b[0];
         chunk.b[0] = chunk.b[1];
         chunk.b[1] = tmp;
         tmp = chunk.b[2];
         chunk.b[2] = chunk.b[3];
         chunk.b[3] = tmp;
         break;
      case CHUNK_LONG:
         tmp = chunk.b[0];
         chunk.b[0] = chunk.b[3];
         chunk.b[3] = tmp;
         tmp = chunk.b[1];
         chunk.b[1] = chunk.b[2];
         chunk.b[2] = tmp;
         break;
   }
#endif

   /* Save the chunk */
   rec->frame[ds->f_ndx].chunk[ds->c_ndx].l = chunk.l;

#ifdef DEBUG
   printf( "final state, saved type %ld chunk in frame %d, chunk %d\n",
      c_type, ds->f_ndx, ds->c_ndx );
#endif

   /* Increment the chunk index */
   ds->c_ndx++;

   /* See if we need a new frame */
   if( ds->c_ndx >= 15 ) {

      /* Save and reset the flags */
      rec->frame[ds->f_ndx].flags = swap_l( ds->flags );
      ds->flags = 0x00000000;

      /* Reset the chunk index */
      ds->c_ndx = 0;

      /* Increment the frame index */
      ds->f_ndx++;

      /* See if we hit the end of the record */
      if( ds->f_ndx >= 15 )
         return( FALSE );
   }

   return( TRUE );
}

/*---------------------------------------------------------------------
 * Finish off a SEED record, called by encode_steim()
 */

VOID _finish_rec( VOID *ptr, DATA_STATE *ds ) {
   STEIM *rec;

   /* Save integrating constants and flags for last (possibly incomplete) frame */
   rec = (STEIM *)((INT8 *)ptr+64);
   rec->frame[ds->f_ndx].flags = swap_l( ds->flags );
   rec->frame[0].chunk[0].l = swap_l( ds->x0 );
   rec->frame[0].chunk[1].l = swap_l( ds->xn );

   return;
}

/*---------------------------------------------------------------------
 * Decode INT32 Steim 1 compressed samples in a PASSCAL data record.
 *
 *    INT16 decode_steim( VOID *ptr, INT16 *n, INT32 *samples );
 *
 * Arguments:
 *    ptr     = VOID pointer to PASSCAL data record block (1024 bytes)
 *    samples = INT32 array for uncompressed samples
 *
 * Returns TRUE if successful, FALSE otherwise
 *
 * samples should be a INT32 array containing at least 892 elements.
 */

BOOL decode_steim( VOID *ptr, INT16 *n, INT32 *samples ) {
   BOOL  skip_ird;
   INT8 f_ndx, c_ndx, n_difs;
   INT16 i, n_samp;
   INT32 sum, difs[4], fi_con, ri_con;
   UINT32 flags;
   STEIM *rec;

   n_samp = *n;

   /* Get the integration constants */
   rec = (STEIM *)((INT8 *)ptr+64);
   fi_con = swap_l( rec->frame[0].chunk[0].l );
   ri_con = swap_l( rec->frame[0].chunk[1].l );

   /* Init the running sum and save the first sample */
   sum = fi_con;
   n_samp--;
   *samples++ = sum;

   /* Skip the inter-record delta, i.e., the difference between records */
   skip_ird = TRUE;

   /* Process up to 15 frames */
   for( f_ndx=0; f_ndx<15; f_ndx++ ) {

      /* Get the frame flags */
      flags = swap_l( rec->frame[f_ndx].flags );

      /* Process 15 chunks per frame */
      for( c_ndx=0; c_ndx<15; c_ndx++ ) {

         /* Count down samples to 0 */
         if( n_samp <= 0 )
            break;

         /* Process a chunk based on type */
         switch( _chunk_type( flags, c_ndx ) ) {

            case CHUNK_NULL:
               continue;

            case CHUNK_BYTES:
               for( i=0; i<4; i++ )
                  difs[i] = (INT32)rec->frame[f_ndx].chunk[c_ndx].b[i];
               n_difs = 4;
               break;

            case CHUNK_WORDS:
               for( i=0; i<2; i++ )
                  difs[i] = (INT32)swap_w( rec->frame[f_ndx].chunk[c_ndx].w[i] );
               n_difs = 2;
               break;

            case CHUNK_LONG:
               difs[0] = swap_l( rec->frame[f_ndx].chunk[c_ndx].l );
               n_difs = 1;
               break;
         }

         /* Save the decoded samples */
         for( i=0; i<n_difs; i++ ) {
            if( skip_ird )
               skip_ird = FALSE;
            else {
               sum += difs[i];
               *samples++ = sum;
               n_samp--;
               if( n_samp <= 0 )
                  break;
            }
         }
      }
   }

   if( n_samp > 0 ) {
#ifdef DEBUG
      printf( "WARNING: Partial decompression of sample data, n_samp = %d\n", n_samp );
#endif
      rtp_log(RTP_WARN, "WARNING: Partial decompression of sample data, n_samp = %d\n", n_samp );
      *n = n_samp;
      return( FALSE );
   }

   if( sum != ri_con ) {
#ifdef DEBUG
      printf( "WARNING: Unreconciled reverse integrating constant: %ld, sum: %ld\n",
         ri_con, sum );
#endif
      rtp_log(RTP_WARN, "WARNING: Unreconciled reverse integrating constant: %ld, sum: %ld\n",
         ri_con, sum );
      return( FALSE );
   }

   return( TRUE );
}

/*---------------------------------------------------------------------
 * Returns the chunk type, called by decode_steim( ).
 */

INT8 _chunk_type( UINT32 flags, INT8 c_ndx ) {

   flags >>= (2 * (15 - (c_ndx+1)));

   return( (INT8)(flags & 0x03) );
}

/*---------------------------------------------------------------------
 * Swap byte order in a word integer
 */

INT16 swap_w( INT16 inword ) {

#ifdef REVERSE_BYTE_ORDER
   INT8 tmp;

   union {
      INT8 bytes[2];
      INT16 outword;
   } parts;

   parts.outword = inword;

   tmp = parts.bytes[0];
   parts.bytes[0] = parts.bytes[1];
   parts.bytes[1] = tmp;

   return( parts.outword );
#else
   return( inword );
#endif
}

/*---------------------------------------------------------------------
 * Swap byte order in a long integer
 */

INT32 swap_l( INT32 inlong ) {

#ifdef REVERSE_BYTE_ORDER
   INT8 tmp;

   union {
      INT8 bytes[4];
      INT32 outlong;
   } parts;

   parts.outlong = inlong;

   tmp = parts.bytes[0];
   parts.bytes[0] = parts.bytes[3];
   parts.bytes[3] = tmp;
   tmp = parts.bytes[1];
   parts.bytes[1] = parts.bytes[2];
   parts.bytes[2] = tmp;

   return( parts.outlong );
#else
   return( inlong );
#endif
}

/* Revision History
 *
 * $Log$
 * Revision 1.1  2004/03/09 18:28:23  vernon
 * Initial revision
 *
 * Revision 1.2  2002/01/18 17:55:57  nobody
 * replaced WORD, BYTE, LONG, etc macros with size specific equivalents
 * changed interpretation of unit ID from BCD to binary
 *
 * Revision 1.1.1.1  2000/06/22 19:13:09  nobody
 * Import existing sources into CVS
 *
 */
