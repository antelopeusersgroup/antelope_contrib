/*
 * seedutils.h
 *
 * Declarations for SEED structures and the seedutils.c routines
 */

#ifndef SEEDUTILS_H
#define SEEDUTILS_H

/* Some SEED structures */
typedef struct s_btime {      /* RAW TIME IN FSDH */
  unsigned short year;
  unsigned short day;
  unsigned char hour;
  unsigned char minute;
  unsigned char secs;
  unsigned char unused;
  unsigned short fracts;
} t_btime;

typedef struct s_fsdh {       /* FIXED-SECTION-DATA-HEADER DATA */
  char seqnum[6];
  char headind;
  char reserved;
  char station_code[5];
  char location_id[2];
  char channel_id[3];
  char network_code[2];  
  struct s_btime start_time;
  unsigned short num_samples;
  short sample_rate;
  short multiplier;
  unsigned char activity_flags;
  unsigned char io_flags; 
  unsigned char dq_flags;
  unsigned char num_blockettes;
  int time_correct;
  unsigned short int begin_data;
  unsigned short int begin_blockette;
} t_fsdh;

typedef struct s_blk_head {  /* Generic struct for head of blockettes */
  unsigned short blk_type;
  unsigned short next_blk;
} t_blk_head;

typedef struct s_blk_1000 {  /* BLOCKETTE 1000 */
  unsigned short blk_type;
  unsigned short next_blk;
  unsigned char encoding;
  unsigned char word_swap;
  unsigned char rec_len;
  unsigned char reserved;
} t_blk_1000;


/* Function declarations for routines in seedutil.c */

int parse_record ( char *lptr, int rec_size,
                   t_fsdh *fsdh, t_blk_1000 *blk_1000);
double calcsamprate ( t_fsdh *fsdh );
void swap_2bytes ( unsigned short *a );
void swap_4bytes ( unsigned int *a );
char *get_encoding ( char enc );

#endif /* SEEDUTILS_H */
