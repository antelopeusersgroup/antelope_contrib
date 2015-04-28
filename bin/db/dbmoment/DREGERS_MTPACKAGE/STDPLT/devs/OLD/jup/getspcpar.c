int do_reset	=0;
/* int do_erase	=0; */

get_spc_par()
   {
	getpar("reset","b",&do_reset);
	/* getpar("erase","b",&do_erase); */
	/* note getpar "erase" has been moved to ../com/getstdpar.c */
	/* so that it can be used as an option in readcom.c         */
   }
