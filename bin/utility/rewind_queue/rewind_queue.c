/* Special purpose program to reset a file created by a
ProcessingQueue object to allow processing orphans left behind
in a crash.   This version is not an editor, but simply resets
the position counter for the queue file.  Ultimately this program
should allow a method edit the contents of the queue to mark
specific records for reprocessing. That would be a much more
elaborate algorithm as it would require something like a matching
criteria on the rows of the database view linked to the queue.  

usage:  rewind_queue qfile
*/
#include <stdio.h>
void usage()
{
	fprintf(stderr,"Usage:\nrewind_queue qfile\n");
	exit(-1);
}
int main(int argc, char **argv)
{
	if(argc!=2) usage();
	FILE *fp=fopen(argv[1],"r+");
	if(fp==NULL)
	{
		fprintf(stderr,"rewind_queue:  open failed on file %s\n",
			argv[1]);
		usage();
	}
	int position;
	position=0;
	fseek(fp,2*sizeof(int),SEEK_SET);
	fwrite((void *)(&position),sizeof(int),1,fp);
}

