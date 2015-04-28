setpointers(list, ptrs)
char *list, *ptrs[];
   {
	int n;

	n=0;
	while( *list != '\0' )
	   {
		while(*list == ' ' || *list == '\t') list++;
		if(*list == '\0') break;
		ptrs[n]= list;
		n++;
		while(*list != ' ' && *list != '\t' && *list != '\0') list++;
		if(*list == '\0') break;
		*list++ = '\0';
	   }
	return(n);
   }
