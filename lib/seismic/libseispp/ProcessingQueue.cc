#include <unistd.h>
#include <sstream>
#include "seispp.h"
#include "dbpp.h"
#include "ProcessingQueue.h"
using namespace std;
using namespace SEISPP;
namespace SEISPP
{

void DatascopeProcessingQueue::save()
{
	rewind(fp);
	long buf[3];
	buf[0]=records_in_this_view;
	buf[1]=view_table;
	buf[2]=current_record;
	long count;
	count=fwrite((void *)buf,sizeof(long),3,fp);
	if(count!=3) throw SeisppError(string("DatascopeProcessingQueue:: fwrite error"));
	count=fwrite((void *)(&(status[0])),
		sizeof(ProcessingStatus),records_in_this_view,fp);
	if(count!=records_in_this_view) 
		throw SeisppError(string("DatascopeProcessingQueue:: fwrite error"));
	fflush(fp);
}
void DatascopeProcessingQueue::load()
{
	rewind(fp);
	long buf[3];
	long count;
	count=fread((void *)buf,sizeof(long),3,fp);
	if(count!=3)
		throw SeisppError(string("DatascopeProcessingQueue:: fread error"));
	records_in_this_view=buf[0];
	view_table=buf[1];
	current_record=buf[2];
	count=fread((void *)(&(status[0])),sizeof(ProcessingStatus),
		records_in_this_view,fp);
	if(count!=records_in_this_view) 
		throw SeisppError(string("DatascopeProcessingQueue:: fread error"));
}

ostream& operator<<(ostream& os, DatascopeProcessingQueue& q)
{
	os << "Processing queue for table number "<<q.view_table<<endl
		<< " Current record is "<< q.current_record 
		<< " of " << q.records_in_this_view << " total."<<endl
		<<"Processing status of each member of queue:"<<endl;
	long i;
	for(i=0;i<q.status.size();++i)
	{
		os << i <<" ";
		switch (q.status[i])
		{
		case TODO:
			os << "Incomplete"<<endl;
			break;
		case FINISHED:
			os << "Finished"<<endl;
			break;
		case SKIPPED:
			os << "Skipped"<<endl;
			break;
		case PROCESSING:
			os << "Processing"<<endl;
			break;
		default:
			os << "Unrecognized status value."<<endl;
		}
	}
	return(os);
}

	

DatascopeProcessingQueue::DatascopeProcessingQueue(DatascopeHandle& dbh, string fname)
{
	struct stat sbuffer;
	const string base_error("DatascopeProcessingQueue constructor:  ");
	if(stat(fname.c_str(),&sbuffer))
		fp=fopen(fname.c_str(),"w");
	else
		fp=fopen(fname.c_str(),"r+b");
	if(fp==NULL)
		throw SeisppError(base_error
		 + string("Cannot open queue file=") + fname);
	int fd=fileno(fp);
	if(lockf(fd,F_LOCK,(off_t) 0 ) ) 
		throw SeisppError(base_error
			+ "Could not lock queue file.  Cannot proceed");
	fseek(fp,0L,SEEK_END);
	long foff=ftell(fp);
	try {
	    records_in_this_view=dbh.number_tuples();
	    if(records_in_this_view<=0) 
		throw SeisppError(base_error
		 + "Database view is empty. Run dbverify and/or fix your code");
	    if(foff==0)
	    {
		if(SEISPP_verbose)
		{
			cerr << "DatascopeProcessingQueue constructor:  queue file = "
				<< fname 
				<< " is being created"<<endl
				<< "Buildng queue with "<<records_in_this_view<<" entries"<<endl;
		}
		status.reserve(records_in_this_view);
		for(long i=0;i<records_in_this_view;++i) status.push_back(TODO);
		current_record=0;
		view_table=dbh.db.table;
		status[0]=PROCESSING;
		save();
		rewind(fp);  // necessary to be sure we unlock file
		lockf(fd,F_ULOCK,(off_t)0);
		freopen(fname.c_str(),"r+b",fp);
		AtEOF = false;
	    }
	    else
	    {
		long rtest=records_in_this_view;
		/* We have to do a partial load to get number of records so we 
		can initialize the status vector.  Once that is done we call
		the load method. Note we don't need to mess with locking because
		the size variable is never changed and it is the first entry in the file.*/
		rewind(fp);
		long count=fread((void *)&records_in_this_view,sizeof(long),1,fp);
		if(count!=1) throw SeisppError("DatascopeProcessingQueue constructor:  "
			+ string("fread error on first read of existing queue file") );
		if(records_in_this_view != rtest)
		{
		   char buf[256];
		   stringstream serr(buf);
		   serr << "DatascopeProcessingQueue constructor:  "
			<< "queue size does not match size of view passed through database handle"<<endl
			<< "Queue size = records_in_this_view"<<endl
			<< "DatascopeHandle passed has "<<rtest<<" rows"<<endl;
		   rewind(fp);  // necessary to be sure we unlock file
		   lockf(fd,F_ULOCK,(off_t)0);
		   throw SeisppError(serr.str());
		}
		for(count=0;count<records_in_this_view;++count) status.push_back(TODO);
		/* Now we can load the current state. */
		load();
		if(current_record>=records_in_this_view)
			throw SeisppError("DatascopeProcessingQueue constructor:  "
			 + string("Processing queue indicates all data have been processed"));
		while((status[current_record]!=TODO) 
			&& (current_record<records_in_this_view) )
		{
			++current_record;
		}
		if(current_record>=records_in_this_view)
			AtEOF=true;
		else
			AtEOF=false;
	    }
	    //if(flock(fd,LOCK_UN))
	    rewind(fp);
	    if(lockf(fd,F_ULOCK,(off_t)0))
		throw SeisppError(string("DatascopeProcessingQueue::mark:  ")
		  + "flock failed when attempting to release lock on queue file");
	} catch (SeisppError& serr){throw serr;};
}
DatascopeProcessingQueue::~DatascopeProcessingQueue()
{	
	if(SEISPP_verbose)
	{
		cerr << "Closing DatascopeProcessinqQueue.  Final contents on closure."<<endl;
		cerr << *this;
	}
	fclose(fp);
}
/* This is a private method used to search forward for next record marked
as TODO.  Returns the index.  Returns -1 if called on the last record. */
long DatascopeProcessingQueue::position_to_next(long rec)
{
	current_record=rec;
	do {
		++current_record;
	}
	while( (status[current_record]!=TODO) 
		&& (current_record<records_in_this_view) );
	if(current_record>=records_in_this_view)
	{
		AtEOF=true;
		return(-1);
	}
	else
		return(current_record);
}
bool DatascopeProcessingQueue::has_data()
{
	if(AtEOF) return(false);
	if(current_record<(records_in_this_view-1) ) 
		return(true);
	else if( (current_record==(records_in_this_view - 1) )
	   && (status[current_record]!=TODO) )
		return(true);
	else
		return(false);
}
/* This is more complicated than necessary to allow it to be adapted in the 
near future for use on a cluster sharing a common file space.  For this
reason we lock the file, read the current contents in case another process has
modified it 
*/
void DatascopeProcessingQueue::mark(ProcessingStatus pstat)
{
	int fd=fileno(fp);
	rewind(fp);
	if(lockf(fd,F_LOCK,(off_t)0))
	//if(flock(fd,LOCK_EX) ) 
		throw SeisppError(string("DatascopeProcessingQueue::mark:  ")
			+ "Could not lock queue file.  Cannot proceed");
	long my_current_record=current_record;
	load();

	status[my_current_record]=pstat;
	my_current_record=position_to_next(my_current_record);
	if(my_current_record>=0) status[my_current_record]=PROCESSING;
	save();

	rewind(fp);
	if(lockf(fd,F_ULOCK,(off_t)0))
		throw SeisppError(string("DatascopeProcessingQueue::mark:  ")
		  + "flock failed when attempting to release lock on queue file");
}
void DatascopeProcessingQueue::set_to_current(DatascopeHandle& dbh)
{
	if(dbh.db.table!=view_table)
	{
		char buf[256];
		stringstream serr(buf);
		serr << "DatascopeProcessingQueue::set_to_current:  "
		  << "db table mismatch.  "<<endl;
		serr << "Queue is defined for table number " << view_table
			<< " but handle passed points to table "
			<< dbh.db.table<<endl;
		throw SeisppError(serr.str());
	}
	dbh.db.record=current_record;
}

}  // end SEISPP Namespace encapsulation

