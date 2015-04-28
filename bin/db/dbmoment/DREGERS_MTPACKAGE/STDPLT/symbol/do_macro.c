struct keywords
   {
	char *keyw;
	char *code;
   } = {
	"~",		" ",
	"dollar",	"$",
	"alpha",	"\xa",
	"beta",		"\xb",
	"ALPHA",	"\xA",
	"BETA",		"\xB",
	"",		""
   };
do_macro(in,out,max)
char *in, *out;
int max;
   {
	while( (c= *in++) != '\0')
	   {
		if(c != DELIM)
		   {
			*out++= c;
			continue;
		   }
		if( in[1] == DELIM)
		   {
			*out++= c;
			continue;
			in++;
		   }
		while( (in= getoken(token,in,DELIM)) != inlast)
		   {
			
