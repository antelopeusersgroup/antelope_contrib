#include "liss2orb.h"

#define LIKE_A_SUN 0x76543210   /* MC680x0 word order    */
#define LIKE_A_VAX 0x10325476   /* VAX, 80x86 word order */
#define LIKE_A_PDP 0x54761032   /* PDP word order        */

int Big_Endian=0;
int Little_Endian=0;
int PDP_order = 0;
 
void set_byte_order()
{
    union {
        uchar_t character[4];
        ulong integer;
    } loc_val;

/* Construct a 4-byte word of known contents - 0x76543210 */
 
    loc_val.character[0] = 0x76;
    loc_val.character[1] = 0x54;
    loc_val.character[2] = 0x32;
    loc_val.character[3] = 0x10;
		 

/* determine the 4-byte word order of this machine */
 
    if (loc_val.integer == LIKE_A_SUN) {
	        Big_Endian = 1;    
    } else if (loc_val.integer == LIKE_A_VAX) {
	        Little_Endian = 1;
    } else if (loc_val.integer == LIKE_A_PDP) {
	        PDP_order = 1;
    } else 
        die( 0, "can't get local word_order \n");

}

void short_swap( short *input, int num )
{
     char byte[2];
     char temp;
     int i;
 
     for (i = 0; i < num; i++) {
        memcpy(byte, input + i, 2);
        temp = byte[0];
        byte[0] = byte[1];
        byte[1] = temp;
        memcpy(input + i, byte, 2);
    }

}


long_swap ( long *input, int num )
{

    short s_temp[2];
    short temp;
    int i;
     
    for (i = 0; i < num; i++) {
        memcpy(s_temp, input + i, 4);
        temp      = s_temp[0];
        s_temp[0] = s_temp[1];
        s_temp[1] = temp;
        memcpy(input + i, s_temp, 4);
    }
}


int lbyte_order( int val, int nbytes ) 

{
    long *lval;
    short *sval;

    if( Big_Endian ) return val;
   
    if( Little_Endian )  { 
       switch ( nbytes )  {

	   case 2:
               sval =  ( short *) &val;
	       short_swap( sval, 1 );
	       return (int) *sval;
	       break;
           case 4:
               lval =  ( long *) &val;
               short_swap( (short *) lval, 2 );
	       long_swap ( lval, 1 );
	       return *lval;
	       break;

	   default:
	       complain(0, "non-valid value size - %d.\n", nbytes );
	       return 0;
	       break;
       }
    } die( 0, "unknown local word_order!\n");

}
/* convert string to LOWER case  */
 
char *lcase( char *string)
{
      int i;
	   
      for (i = 0; i < strlen(string); i++)
          string[i] = tolower(string[i]);
          return string;
}
			        

