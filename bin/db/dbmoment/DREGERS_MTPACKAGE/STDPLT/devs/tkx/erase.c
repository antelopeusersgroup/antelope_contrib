erase()
   {
	/*
	putc(ESC,pltout);
	putc('[',pltout);
	putc('2',pltout);
	putc('J',pltout);
	*/
	putc(ESC,pltout);
	putc(ERASE,pltout);
	fflush(pltout);
	sleep(1);
	gsreqd = 1;
   }
