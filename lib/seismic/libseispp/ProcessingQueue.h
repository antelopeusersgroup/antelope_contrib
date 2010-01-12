#ifndef _PROCESSINGQUEUE_H_
#define _PROCESSINGQUEUE_H_

#include <stdio.h>
#include <string>
#include <vector>
#include "dbpp.h"
namespace SEISPP
{
/*! Used in DatascopeProcessingQueue to mark state of each record to be processed.

The DatascopeProcessingQueue can mark a record as one of these items.
As the names sugested, TODO means processing has not yet been completed,
FINISHED means processing is completed and was successful, SKIPPED means
the records was skipped for some reason, and PROCESSING is used to 
mark records locked and being processed by this or another process.
*/
enum ProcessingStatus {TODO, FINISHED, SKIPPED, PROCESSING};
/*! \brief Processing queue for database driven procesing using Datascope.

Many numerical algorithms are data driven and can be abstracted as driven by 
a table.  This object is used to define a queue of data to be processed and 
handles the negotiation between multiple processes to maintain this queue.
That is, multiple processes in an environment like a computer cluster can
be running the same algorithm on a common database and this object takes
care of negotiating which record a given process should next handle.  It
can also be used for a single program to provide a mechanism to restart 
a long processing sequence where you left of.  This is mechanism is used,
for example, in dbxcor to process a large database of gathers.  It allows
the analyst to stop work for the day, come back the next day, and pick up 
where he/she left off.  

The algorithm used here is not that sophisticated and makes two very very
important assumptions.  First, it assumes the database view it is supposed
to handle is static.  It is guaranteed to exit with an exception, for example,
if you run a program, stop, and try later when the database has changed 
(e.g. on the output database of a real time system).  The entire purpose
of this is NOT real time operations but large scale processing of assembled
data sets that are no longer changing.  The second major assumption of this 
algorithm is that the processing algorithm is driven by a group table 
in which each row of the group defininition is one data set that is to 
be processed.  For example, in dbxcor it is assumed dbprocess has been used
to assemble data into abstract gathers in which each row of the group view
is one ensemble.  

In addition to the above fundamental assumptions as this version works only 
for an Antelope (Datascope) database handle.  The dependency on Antelope/Datascope,
however, is very very light and this object and and likely will be adapted in
the future to alternatives.  

The interface is pretty general and was designed to work in a more general
setting.  This implementation, however, further requires all processors to 
see a common disk pool.  The reason is that the algorithm used is fairly simple
and make use of stock unix file locking to allow concurrent processes to all
hit the same queue.  This also means if the input view is very large there is 
a potentially large overhead in updating the file used to build the queue, 
but the authors expectation is this is likely to be a minor concern because
the database behind the view would likely eat up all memory long before this
became a concern.  At least that is my theoretical view at this point in time.
*/
class DatascopeProcessingQueue
{
public:
	/*! Primary constructor for this object.

	The processing queue for this object is build from a handle to 
	a Datascope database and a file name to hold the data that defines
	the queue.  There must be one and only one copy of this object for
	a single process (a limitation that could be lifted, but not present
	in this implementation).  Hence you will note there is no operator =
	or copy constructor for this object.  
	\param dbh is the handle to the database used to define the queue.  
	The assumption is the user wants to mark and process this database
	view one row at a time.  Thus, in almost every situation of any
	conceivable use this would be a view whose final stage was a call to 
	dbjoin (alternatively the join method in the DatascopeHandle).
	\param fname is the file name that will hold the queue data.  
	The process must have read and write permission to this file.
	It must either be a previously created queue file, empty, or 
	simply nonexistent.  
	\exception SeisppError objects are thrown in one of several conditions
	that an unambiguously fatal.  Never ever try to continue a program 
	if this constructor throws an exception.  
	*/
	DatascopeProcessingQueue(DatascopeHandle& dbh, string fname);
	/*! Standard destructor.  

	About all this does is close the queue file.  This means, of course,
	that this object keeps a handle to this file open until the 
	object is destroyed.
	*/
	~DatascopeProcessingQueue();
	
	/*! Mark the state of the current record.

	This is the workhorse method of this object.  It does two basic things.
	First, it takes the current record, marks it with the value passed, 
	and then updates the queue file.  The later processes is done properly 
	to work in a multiprocessor/multithreaded environment by locking the file
	until the update completes.  Before releasing the lock it does it's second
	task.  That is, it searches forward to find the next record marked TODO.
	It sets the record pointer for the queue to that value and marks it 
	PROCESSING.  At that point the actual update to the file is done and 
	the file lock is relased.  
	
	\param ps ProcessingStatus value to be assigned to the record currently
	being handled by this process.  Although this isn't tested the expectation
	is this would always be either FINISHED or SKIPPED.
	*/
	void mark(ProcessingStatus ps);
	/*! Set the database handle to the current record.

	This object maintains a list of integer record numbers that define the 
	queue.  This method sets the record field of the handle it is passed
	to the current record.  This is a core method for this object as the
	standard algorithm to use it is to call the mark method, test that
	the queue isn't empty, and then call this method. 
	
	\param dbh Datascope (Antelope) database handle that is to be updated. 
	\exception throws a SeisppError object if the handle properties are not
		consistent with this queue.  
	*/
	void set_to_current(DatascopeHandle& dbh);
	/*! Used to test when the queue is empty.   When using this object
	the main processing loop should call this method immediately after a
	mark call and exit it's loop on this condition.*/
	bool has_data();
	/*! Prints current queue contents. */
	friend ostream& operator<<(ostream&, DatascopeProcessingQueue& q);
private:
	vector<ProcessingStatus> status;
	long records_in_this_view;
	long view_table;
	long current_record;
	bool AtEOF;
	FILE *fp; 
	/*! Saves the current queue in this instance.  Locking stuff is handled
	outside of this method. */
	void save();
	/* Loads data from the queue file. */
	void load();
	/*! Position the queue pointer to the next record marked for processing. 

	Positioning is simple in a single threaded enviroment, but if multiple processors
	are hitting a database the next record is rarely the one next for processing.
	Hence this method searches forward from the position passed as startrec.  
	Returns the current_record.  When there is no data remaining to 
	be processed returns -1 and sets AtEOF true.
	*/
	long position_to_next(long startrec);
};
}
#endif
