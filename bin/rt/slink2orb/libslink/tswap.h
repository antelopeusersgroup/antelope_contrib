/***************************************************************************
 * tswap.h:
 *
 * Interface declarations for the triggered swap routines in tswap.c
 *
 * Written by Chad Trabant, ORFEUS/EC-Project MEREDIAN
 *   Design elements taken from various code bases.
 *
 * Version: 2003.168
 ***************************************************************************/


#ifndef TSWAP_H
#define TSWAP_H 1

#ifdef __cplusplus
extern "C" {
#endif

extern void tswap2 ( uint16_t *data, int swapflag );
extern void tswap3 ( uint8_t *data, int swapflag );
extern void tswap4 ( uint32_t *data, int swapflag );
extern void tswap8 ( uint64_t *data, int swapflag );

#ifdef __cplusplus
}
#endif
  
#endif /* TSWAP_H */
