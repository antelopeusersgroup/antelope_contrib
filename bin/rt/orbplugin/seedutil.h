/*
 * seedutil.h
 * Declarations for the seedutil.c routines
 *
 */

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

typedef struct s_dev_id {     /* DEVICE ID IN FSDH */
  char station_code[5];
  char location_id[2];
  char channel_id[3];
  char network_code[2];
} t_dev_id;

typedef struct s_fsdh_data {  /* FIXED-SECTION-DATA-HEADER DATA */
  struct s_btime start_time;
  unsigned short num_samples;
  short sample_rate;
  short multiplier;
  unsigned char activity_flags;
  unsigned char io_flags;
  unsigned char dq_flags;
  unsigned char num_blockettes;
  long time_correct;
  unsigned short int begin_data;
  unsigned short int begin_blockette;
} t_fsdh_data;

typedef struct s_blk_head {  /* generic struct for head of blockettes */
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

int find_reclen ( const char *msrecord, int maxheaderlen );
int insert_blkt ( unsigned char *msrecord, int maxheaderlen,
                  const char *blkt, int blktlen );
int get_blktlen (int blkttype, const unsigned char *blktdata);
void swap_2bytes ( unsigned short *a, char f );
