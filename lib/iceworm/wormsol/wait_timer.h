
              /**********************************************
               *                wait_timer.h                *
               *                                            *
               *  Include file for wait_timer functions.    *
               **********************************************/

#include <earthworm.h>

int init_wait_timer( timer_t *, DWORD * );
int start_wait_timer( timer_t, LONG, DWORD * );
int wait_timer( timer_t, DWORD * );
