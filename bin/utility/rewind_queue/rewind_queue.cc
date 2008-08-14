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
#include "ProcessingQueue.h"
using namespace std;
using namespace SEISPP;
void usage()
{
	fprintf(stderr,"Usage:\nrewind_queue qfile\n");
	exit(-1);
}
int main(int argc, char **argv)
{
	if(argc!=2) usage();
	ios::sync_with_stdio();
	FILE *fp=fopen(argv[1],"r+");
	cout << "rewind_queue:  resetting queue file="<<argv[1]<<endl;
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
	rewind(fp);
	fseek(fp,3*sizeof(int),SEEK_SET);
	ProcessingStatus status_this_record;
	int number_cleared(0);
	/* Above read the 3 word header at the start. Now we can
	just read in the results one at a time and switch when 
	necessary.  We buffer the queue and write it all at once
	to simplify the algorithm. */
	vector<ProcessingStatus> newqueue;
	int irec=0;
	while(fread((void *)&status_this_record,sizeof(ProcessingStatus),1,fp)
		== 1)
	{
		
		if(status_this_record==PROCESSING) 
		{
			status_this_record=TODO;
			++number_cleared;
			cout << "Clearing BUSY status for record="<<irec<<endl;
		}
		newqueue.push_back(status_this_record);
		++irec;
	}
	if(number_cleared==0)
	{
		cout << "No records were marked as BUSY.  Did nothing"
			<<endl;
	}
	else
	{
		cout << "Reset "<<number_cleared<<" records previously "
			<< "set as BUSY."<<endl;
		cout << "Size of this queue="<<newqueue.size()<<" records."<<endl;
		rewind(fp);
		fseek(fp,3*sizeof(int),SEEK_SET);
		fwrite((void *)(&(newqueue[0])),sizeof(ProcessingStatus),
			newqueue.size(),fp);
	}
}

