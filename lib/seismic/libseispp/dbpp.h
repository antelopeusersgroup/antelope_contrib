#ifndef _SEISPP_DATABASE_H_
#define _SEISPP_DATABASE_H_
#include <string>
#include <list>
#include <vector>
#include <set>
#include "stock.h"
#include "db.h"
#include "pf.h"
#include "AttributeMap.h"
#include "Metadata.h"
#include "databasehandle.h"

namespace SEISPP
{
using namespace std;
using namespace SEISPP;
/*! \brief Defines an encapsulation of the Antelope group pointer concept.
*
* Antelope an awkward method to define the output of a group by (dbgroup procedure)
* result.  They manipulate the 4 element integer Dbptr struct to tell their
* system a particular Dbptr is a "group" pointer.  This simple class encapsulates
* that concept into a single C++ object.  It's purpose is to define a single 
* "bundle".  This can be used, for example, to group data into three-component
* groups or some general ensemble definition.
*/
class DBBundle
{
public:
/*! First record of bundle this object defines.*/
	long start_record;
/*! Final record of the bundle this object defines.*/
	long end_record;
/*! Dbptr for view that dbgroup was called on.  We always need to know
* this to be able to extract more than the keys from the working view. */
	Dbptr parent;
};
/*! \brief Handle for manipulating a datascope (Antelope) database.
*
* This is a concrete implementation of the abstract base class DatabaseHandle
* for an Antelope dbms.  It implements the virtual methods defined in the
* base class and a few additional ones that are more more specific to the
* Antelope datascope API.  
* \par
* This handle attempts as best possible to manage memory used by 
* an Antelope database automatically.  It does this through a mechanism
* similar to the way a shared_ptr works, but modified for peculiarities
* of the Datascope interface.  The aim of the memory management is 
* to avoid leaking memory connected with what Datascope calls a "view".
* This means the results of sorts, subsets, joins, and pretty much
* any relational db concept.  To do this it keeps track of the count
* of each table that is a view in an internal container.  A pointer
* to this single container is passed between copies of a particular
* handle.  The CRITICAL corollary of this for application is that
* memory management will ONLY work if all copies of a handle are 
* derived from a single, original handle.  i.e. a program should
* use only operator = and the copy constructor to create any
* secondary copies of a handle.  Multiple constructors or multiple
* calls to the C function dbopen are likely to cause trouble. 
*/
class DatascopeHandle : public DatabaseHandle
{
public:
	/*! Default constructor.  Does nothing but set the Dbptr 
	to have all dbINVALID values.  If called the result is not useable.*/
	DatascopeHandle();
	/*! Simple creation of a specific named database.  This method
	can best be thought of as a simplified call to dbopen.
	\param dbname name of Antelope db to open.
	\param readonly if true the db is openned read only.
	\param retain_all_views is a switch that controls the way
		memory associated with views is managed.  When false
		(the default) memory management releases all view 
		pointers (see man dbfree) whenever the copy count for
		that view drops to zero.  When true any operation
		that creates a view (e.g. join or subset) will never
		release the memory of any parent that is itself a view.
		Setting this variable true is not advised as it will
		nearly always lead to bad memory leaks.  
	*/
	DatascopeHandle(string dbname,bool readonly,
		bool retain_all_views=false);
	/*! Construct a full view from a parameter file recipe.
	This constructor uses an Antelope parameter file 
	to construct a complete view using dbprocess.  It differs from
	a similar constructor have a Pf * argument by accessing the 
	file directly instead of the Pf object.  The parameter file
	passed as an argument is openned and then closed by this constructor.
	This is thus most useful if the parameter file is not being used
	for anything else.

	\param dbname Datascope (Antelope) database name.
	\param pfname file name of parameter file.  Note the usual Antelope
		convention that this string must not include the trailing ".pf". 
	\param tag keyword marking the Tbl&{} block of commands passed to dbprocess.
		The parameter file is parsed for a Tbl block with this tag
		(calls gettbl(...)  see man gettbl(3)) and the resulting Tbl pointer
		is passed directly to dbprocess.
	\param readonly if set true the database will be openned in read only mode.
	\param retain_all_views is a switch that controls the way
		memory associated with views is managed.  When false
		(the default) memory management releases all view 
		pointers (see man dbfree) whenever the copy count for
		that view drops to zero.  When true any operation
		that creates a view (e.g. join or subset) will never
		release the memory of any parent that is itself a view.
		Setting this variable true is not advised as it will
		nearly always lead to bad memory leaks.  

	\exception SeisppDberror is thrown if there are any problems.
	*/
	DatascopeHandle(string dbname, string pfname, 
			string tag,bool readonly,
				bool retain_all_views=false);
	/*! Construct a full view from an Antelope Pf * object recipe.
	This constructor uses an Antelope parameter file object
	to construct a complete view using dbprocess.  It differs from a similar
	constructor that has a file name for a parameter file as an argument 
	only in the sense that it does not need to open and close the parameter 
	file.  This constructor is thus preferred if the Pf is, as is the usual
	case, used to contain other parameters required by the program.

	\param dbh Datascope handle object  
	\param pf Antelope Pf * object to be parsed for dbprocess commands.
	\param tag keyword marking the Tbl&{} block of commands passed to dbprocess.
		The parameter file is parsed for a Tbl block with this tag
		(calls gettbl(...)  see man gettbl(3)) and the resulting Tbl pointer
		is passed directly to dbprocess.
	\param manage_memory can be used to switch off the automatic
		memory management of the handle created.  This is rarely a good
		idea, but is a necessary option because the constructor is
		passed the raw pointer instead of another DatascopeHandle
		object.  The default has memory management turned on.
	\param retain_all_views is a switch that controls the way
		memory associated with views is managed.  When false
		(the default) memory management releases all view 
		pointers (see man dbfree) whenever the copy count for
		that view drops to zero.  When true any operation
		that creates a view (e.g. join or subset) will never
		release the memory of any parent that is itself a view.
		Setting this variable true is not advised as it will
		nearly always lead to bad memory leaks.  

	\exception SeisppDberror is thrown if there are any problems.
	*/
	DatascopeHandle(DatascopeHandle& dbh, Pf *pf, string tag,
		bool manage_memory=true,
			bool retain_all_views=false);
	/*! Standard copy constructor.  
	The copy constructor here is far from "standard" even though I've labeled it 
	as such.  The result is indeed an exact copy of the parent, but the purpose
	most common purpose should be noted carefully.  That is, if one needs a series
	of views that are related (e.g. wfdisc->sitechan than later a grouping of this view
	by sitechan) the normal procedure would be to call this constructor to get a clone
	of the parent, then call the method to build the new view (group of the above 
	example) on the copy.  The copy would then relate to the new view.  Fairly 
	standard object copy operation, but potentially confusing here because the handle
	is mutable.
	*/
	DatascopeHandle(const DatascopeHandle& dh);
	/*! \brief Special copy constructor to create a new handle from only a Dbptr definition.
	An odd specialized copy constructor for Datascope db.
	Handle is constructed using an existing db pointer, dbi,
	and setting the "parent" for this handle using dbip.
	This was used by the author in some earlier code before
	realizing some special features of the Datascope API that
	made it unnecessary.  It has been retained for my personal
	convenience, but any other potential users should avoid this
	constructor.  The biggest problem with this constructor is
	any view created by this constructor is guaranteed to cause
	a memor leak.

	\param dbi Dbptr to be copied.
	\param dbip parent table to dbi.  As noted above this is needed 
		to build a complete handle from only a Dbptr descriptor.  
		The need for this is largely an unresolvable weakness in 
		the exceptionally terse Dbptr descriptor. 
	*/
	DatascopeHandle(Dbptr dbi,Dbptr dbip);
	/*! \brief Destructor.
	This is far from a standard destructor.  Data are cleared but the 
	database is NOT closed unless the close_on_destruction private variable
	in this handle is set.  This can only be done through a call to the close
	method BEFORE the handle is explicitly destroyed or is implicitly 
	destroyed when the handle goes out of scope. */
	~DatascopeHandle();
	/*! \brief Get an attribute from current tuple returning a double.
	This method returns the value 
	of an real-valued attribute for the current tuple.  The result it always
	returned as a double independent of the actual internal precision for
	simplicity.  This follows the datascope convention of returning all real 
	numbers as doubles.
	\param attribute_name name of the desired attribute in the schema associated with this handle.
	\exception SeisppDberror is thrown if dbgetv fails.
	*/
	double get_double(string attribute_name);
	/*! \brief Get a attribute from current tuple returning a long (64bit)  integer.

	This method returns the value 
	of an integer-valued attribute for the current tuple.  The result is 
	returned as a long int.  A short int can be handled with a cast. 
	A standard int can be cast or use the get_int method directly.
	\param attribute_name name of the desired attribute in the schema associated with this handle.
	\exception SeisppDberror is thrown if dbgetv fails.
	*/
	long get_long(string attribute_name);
	/*! \brief Get an attribute from current tuple returning an integer.

	This method returns the value 
	of an integer-valued attribute for the current tuple.  The result is 
	returned as a 32bit int.  A short int can be handled with a cast while
	long should use the alternative interface get_long. 
	\param attribute_name name of the desired attribute in the schema associated with this handle.
	\exception SeisppDberror is thrown if dbgetv fails.
	*/
	int get_int(string attribute_name);
	/*! \brief Get an attribute from current tuple returning a string.
	This method returns the value 
	of a string attribute for the current tuple.  The result is always
	returned as an std string for simplicity.  
	\param attribute_name name of the desired attribute in the schema associated with this handle.
	\exception SeisppDberror is thrown if dbgetv fails.
	*/
	string get_string(string attribute_name);
	/*! \brief Create a filename from attributes stored in a database. 
	For this implementation this method is unix specific concatenating the dir and dfile
	attributes found in the current tuple.  Note this method is deficient and won't handle
	a view properly that has more than one dir and dfile in the view.  It will always 
	extract the attribute tagged as plain dir and dfile.  Order of joins is thus very important. 
	\exception SeisppDberror is thrown if dbgetv fails for dir or dfile attributes.
	*/
	string get_filename();
	/*! Put a real variable to a database at the current record position.
	* This method will put one attribute to a database table at the current 
	* record position. 
	* \param name attribute name of quantity to be saved to database.
	* \param value value of this attribute to save to current tuple. 
	*
	* \exception SeisppDberror is thrown if dbputv fails to put the requested variable.
	*/
	void put(string name, double value);
	/*! Put a real variable to a database at the current record position.
	* This method will put one attribute to a database table at the current 
	* record position.  
	* \param name attribute name of quantity to be saved to database.
	* \param value value of this attribute to save to current tuple. 
	*
	* \exception SeisppDberror is thrown if dbputv fails to put the requested variable.
	*/
	void put(string name, float value);
	/*! Put a long integer variable to a database at the current record position.
	* This method will put one attribute to a database table at the current 
	* record position. 
	* \param name attribute name of quantity to be saved to database.
	* \param value value of this attribute to save to current tuple. 
	*
	* \exception SeisppDberror is thrown if dbputv fails to put the requested variable.
	*/
	void put(string name,long value);
	/*! Put an integer variable to a database at the current record position.
	* This method will put one attribute to a database table at the current 
	* record position. 
	* \param name attribute name of quantity to be saved to database.
	* \param value value of this attribute to save to current tuple. 
	*
	* \exception SeisppDberror is thrown if dbputv fails to put the requested variable.
	*/
	void put(string name,int value);
	/*! Put a string variable to a database at the current record position.
	* This method will put one attribute to a database table at the current 
	* record position.  For this version the value is stored as an std string.
	* \param name attribute name of quantity to be saved to database.
	* \param s string variable containg value to save to current tuple. 
	*
	* \exception SeisppDberror is thrown if dbputv fails to put the requested variable.
	*/
	void put(string name,string s);
	/*! Put a string variable to a database at the current record position.
	* This method will put one attribute to a database table at the current 
	* record position.  For this version the value is stored as a raw C char *.
	* \param name attribute name of quantity to be saved to database.
	* \param s char * variable containing strng to store in current tuple. 
	*
	* \exception SeisppDberror is thrown if dbputv fails to put the requested variable.
	*/
	void put(string name,char *s);
	/*! Put a string variable to a database at the current record position.
	* This method will put one attribute to a database table at the current 
	* record position. This method assumes the string is stored as a raw
	* plain C char *.  
	* \param name attribute name of quantity to be saved to database.
	* \param s constant char * variable containing string to store to current tuple.
	*
	* \exception SeisppDberror is thrown if dbputv fails to put the requested variable.
	*/
	void put(string name, const char *s);
	/*! \brief Add a new tuple.  
	* A basic database operation is to add new data to a table.  This method uses a 
	* concept in Antelope's API that one can add a new row with no data and then later
	* push actual information to that tuple with one or more calls to a put procedure
	* (dbputv in Antelope).  In any 
	* case the current record holds a special significance here.  After this method
	* is called the current record pointer will point at the tuple created through
	* this method.    
	* \return a number defining the row index of this tuple.  This is the same value
	*    to which db.record of the handle's Dbptr is set.
	* \exception SeisppDberror is thrown if dbaddnull fails.  
	*/
	long append();
	/*! Return the current record number. */
	long current_record(){return(db.record);};
	/*! Explicitly reposition the record counter to a specified value.
	*  This is an Antelope specific method that depends on Antelope's use of
	* simple integers to index rows and columns of a view.  
	* \param rec value to which the record pointer should be set.  This is not
	*   checked for validity.
	*/
	void set_record(long rec){db.record=rec;};
	/*! Make the handle point at a specific table.
	* This is essentially a front end to dblookup.  If sucessful the pointer will
	* be valid for accessing the raw table in the same sense as the result of 
	* calling dblookup. 
	* \param tablename name of table you want to access.
	*
	* \exception SeisppDberror is thrown if the lookup operation fails.
	*/ 
	void lookup(string tablename);
	/*! \brief Increment the record pointer by one.  
	This is a function form alternative to operator ++ for those who think that
	is esthetically clearer than ++.
	*/
	void next_record(){++db.record;};
	/*! Query to ask for the number of rows (tuples) in the view to which this handle points.
	\return number of rows in the current view.
	\exception SeisppDberror is thrown if the query fails.
	*/
	long number_tuples();
	/*! Query to ask the number of columns (attributes) in the view to which this handle points.
	\return number of columns in the current view.
	\exception SeisppDberror is thrown if the query fails.
	*/
	int number_attributes();
	/*! Reset the record pointer to 0 (top of the table) */
	void rewind(){db.record=0;};
	/*! Sort the current view with a set of specified keys. 
	This method is essentially a front end to dbsort.  The input is an STL list which 
	is translated immediately into an Antelope Tbl passed to dbsort.  The order of the 
	list should then be the same as dbsort.  That is the first element of the list has
	the highest precedence and the last member has the lowest precedence.  
	\param keys list of attribute names to define the sort order.
	\exception throws a SeisppDberror if dbsort fails.  
	*/
	void sort(list<string>keys);
	/*! Join two tables in a relational database.
	*  A database join is the fundamental operation of a relational database.
	*  This method implements this operation for a datascope (Antelope) database.
	*  The method is essentially a front end to dbjoin.
	*
	* \param t1 left table of join operation.
	* \param t2 right table of join operation.
	* \param keys1 list of keys to define join operation of left table.
	* \param keys2 list of keys to define the join operation for right table.
	*
	* \exception throws a SeisppDberror if dbjoin fails.  
	*/
	void join(string t1, string t2, 
		list<string> keys1,list<string>keys2);
	/*! Right join of a table to current view.

	This is similar to the two table join method but the left table
	is assumed to be the current view.  That is table t is right
	joined to the current view using the keys defind in keys1 and keys2.

	\param t right table of join operation.
	\param keys1 list of keys to define join operation of left table.
	\param keys2 list of keys to define the join operation for right table.
	
	\exception throws a SeisppDberror if dbjoin fails.  
	*/
	void join(string t,list<string> keys1,list<string>keys2);
	/*! Right join of a table to current view specified by Dbptr.

	This similar to the one table join method, but the table is
	defined by a raw Dbptr instead of using a name. 

	\param db Dbptr of table/view to join to this handle
	\param keys1 list of keys to define join operation of left table.
	\param keys2 list of keys to define the join operation for right table.
	
	\exception throws a SeisppDberror if dbjoin fails.  
	*/
	void join(Dbptr db ,list<string> keys1,list<string>keys2);
	/*! Right join of a table to current view specified by DatascopeHandle.

	This similar to the one table join method, but the table is
	defined by another DatascopeHandle object instead of the name.
	This is a convenient way to join complex tables each individually
	build from on or more relational operators.

	\param dbh DatascopeHandle to join on the right with this one
	\param keys1 list of keys to define join operation of left table.
	\param keys2 list of keys to define the join operation for right table.
	
	\exception throws a SeisppDberror if dbjoin fails.  
	*/
	void join(DatascopeHandle& dbh ,list<string> keys1,list<string>keys2);
	/*! Join table on the left. 

	A left join means the parent view is considered the t2 (right)
	table in the join.  This method left joins table t to the
	existing view using keys specified in the keys1 and keys2 lists.
	
	\param t table to join on the left of existing view.
	\param keys1 list of keys to define join operation of left table.
	\param keys2 list of keys to define the join operation for right table.
	
	\exception throws a SeisppDberror if dbjoin fails.  
	*/
	void leftjoin(string t,list<string> keys1,list<string>keys2);
	/*! Simplified join operator using natural join feature of Antelope. 
	* The datascope (Antelope) dbms takes advantage of what they call a 
	* "natural join".  A natural join is one defined by the primary keys
	* for the two tables to be joined.  As such there is an implied set of
	* keys that can sometimes be exploited.  The css3.0 schema is well 
	* designed as the "natural join" is the most commonly used operation
	* for most tables.  (e.g. wfdisc->sitechan in this manner).  
	* \par
	* This version of the natural_join method opens one named table
	* and does a right join of a second named table to the first.  
	*
	* \param t1 left table of join.
	* \param t2 right table of join.
	* 
	* \exception SeisppDberror is thrown if dbjoin fails.
	*/
	void natural_join(string t1, string t2);
	/*! Simplified join operator using natural join feature of Antelope. 
	* The datascope (Antelope) dbms takes advantage of what they call a 
	* "natural join".  A natural join is one defined by the primary keys
	* for the two tables to be joined.  As such there is an implied set of
	* keys that can sometimes be exploited.  The css3.0 schema is well 
	* designed as the "natural join" is the most commonly used operation
	* for most tables.  (e.g. wfdisc->sitechan in this manner).  
	* \par
	* This version of the natural_join method assumes the handle currently
	* points to a table (view) that will form the left table of the join.
	*
	* \param t2 right table of join.
	* 
	* \exception SeisppDberror is thrown if dbjoin fails.
	*/
	void natural_join(string t2);
        /*! Apply nojoin operation of named table with default keys.

          Datascope has a nojoin operator that returns a view with rows that
          have no matching entries in a table against which it references.
          This version of of this method uses default join keys for t2
          with the current view.

          \param t2 is the name of the second table to be tested.
          */
        void nojoin(string t2);
        /*! Apply nojoin operation of named table with specified keys.

          Datascope has a nojoin operator that returns a view with rows that
          have no matching entries in a table against which it references.
          This version of this method uses keys specified by two input 
          lists.

          \param t2 is the name of the second table to be tested.
          \param key1 is the set of keys to match in the current view
             this handle defines.
          \param key2 is the set of keys to match against key1 list
             for the right table to which the nojoin operation is applied.
             */
        void nojoin(string t2, list<string> key1, list<string> key2);
	/*! Subset the current view by a specific sort clause.
	*  Subsetting a view by a specific criteria is a basic dbms operation.
	*  This  method is a front end to the datascope (Antelope) procedure 
	*  called dbsubset.  The string argument is passed directly to dbsubset
	*  so the rules of dbsubset (3) define the syntax for the subset definition.
	*
	* \param sstr subset string to pass to dbsubset.  Note this is an std::string variable.
	*
	* \exception SeisppDberror is thrown if the subset operation fails.
	*/
	void subset(string sstr);
	/*! Group a table (view) by key attributes.
	* This method is a front end to dbgroup. As man dbgroup(3) states dbgroup does
	* the equivalent of the "group by" clause in SQL.  They keys passed as arguments serve
	* as the "by" attribute definition.  After this method is called the handle will
	* define "bundle" pointer in Antelope jargon.  The handle itself will change state
	* and be marked as such which helps avoid certain common errors with bundle pointers.
	* Bundles are most commonly used for defining groupings like three-component channel
	* groupings (wfdisc->sitechan grouped by sta:chan, 
	* various forms of ensembles (e.g. by event), 
	* or event grouping of catalog
	* data (event->origin->assoc->arrival join grouped by evid). 
	* \par
	* It is VERY IMPORTANT to recognize that the table should ALWAYS be sorted first 
	* by the grouping keys before group is called.  Otherwise random results are nearly
	* guaranteed.
	*
	* \param group_keys list of keywords used to define the grouping.  This STL list is
	*	converted to an Antelope Tbl internally so the order of the members must be
	*	congruent with the Tbl convention used in dbgroup (same as dbsort).  That is,
	*	the first element of the list has the highest precedence.
	*/
	void group(list<string>group_keys);
	/*! Get the range the parent table that defines a bundle.
	* Datascope's (Antelope) dbgroup procedure produces a "bundle" pointer.
	* A bundle is defined as a range of rows in a parent table.  This method
	* returns that range and related info in a DBBundle object.  The concept
	* here is that table-driven processing in which the table is logically 
	* grouped (e.g. three-component seismogram groupings) would commonly use
	* this method after calling the group method.  The main loop would be over
	* the handle produced by group.  Call this method on each row of the group
	* table to get the information that defines that "bundle".  My (glp) hope is
	* this is less confusing than the way this is done in the datascope procedural
	* methods.  
	*/
	DBBundle get_range();
	/*! \brief Standard assignment operator. 
	* Assignment is sometimes a different thing than a copy, but in this case there
	* is virtually no difference. 
	*/
	DatascopeHandle& operator=(const DatascopeHandle&);
	/*! Increment the record (row) index by one.
	*A key concept in this object is that a table has a row index that can be manipulated.
	* Incrementing the handle object with this operator means that the row index is incremented
	* by one.  
	*/
	void operator ++();
	/*! Mark this handle to close the database on destruction.
	* As noted in the overview this object is a handle with a somewhat unusual property
	* with respect to the idea of closure and destruction.  Because the handle is mutable 
	* and we expect multiple copies and multiple views to be associated with the same
	* database the idea of closing the database becomes problematic.  We cannot, for 
	* example, call dbclose on one handle pointing at wfdisc and expect a different handle
	* located elsewhere in the program that points at, say origin, to be valid any longer.
	* The approach used here follows from an implicit (i.e. not well documented) feature
	* of datascope that assumes a database is open for most of the life of a program.  i.e.
	* the normal model is open once, do what you do, and close only on exit.  
	* \par
	* If the close method is called on this object when it is destroyed (with delete) or 
	* when the object goes out of scope (the more normal process) and the destructor is
	* called, the database will be closed with dbclose.  Normal behaviour for this object
	* if close is not called is to do nothing except destroy the internal data.  
	*/
	void close();  
	/*! \brief Datascope pointer for the working view.
	* The Dbptr data structure is the focal point of the entire API.  It consists
	* of four integers, which highly restricts the flexibility of the interface while
	* making it amenable to a FORTRAN interface.  Many books would recommend the 
	* interface should hide this entity from the user, but I concluded this was 
	* not a good idea for this application because the Dbptr is the core of the 
	* entire datascope API.  To me it seemed me it would be a version of painting
	* yourself into a corner to make this quantity private.  Hence, it is public 
	* here to allow the user to directly manipulate.  This is used repeated, in fact,
	* in the existing library both for efficiency and because it is sometime just simpler.
	*/
	Dbptr db;
	/*! Mark this handle has defining a bundle pointer.
	* The datascope pointer has no way to explicitly define a Dbptr as being a bundle
	* pointer.  The user has to keep track of this themselves.  We use this variable 
	* to allow explicit tests to allow the state of the database to be derived from
	* the handle rather than require a history context.  It was made a public 
	* variable to avoid the unnecessary burden of a is_bundle() method that would
	* accomlish the same thing at a much higher cost.
	*/
	bool is_bundle;
protected:
	/*! This is the variable set by close() to mark handle to be closed ond destruction.*/
	bool close_on_destruction;
	/*! Parent table or view.  In datascope views cascade in a sequence with each child 
	* derived from a parent. 
	*/
	Dbptr parent_table;
	/*! Holds one entry for every created view.  

	Memory needs to be managed with views in Datascope or large
	bad memory leaks can be created.  This handles this problem in
	a fairly elegant way using the stl multimap which is perfectly
	suited to this problem.  Whenever a view is created it's table
	index is pushed to this multimap.  When a view is copied 
	an additional entry for that table is posted.  The destructor
	uses this to manage memory by a simple rule.  When the count of
	entries for a given view drops to zero the dbfree is called to
	release the array of pointers held by that view.  Note this
	is made a pointer so a single copy is passed around to 
	all copies of the original handle.*/
	multiset<int> *views;
	/*! procedure to manage memory of Datascope views. 

	This is an internal method used or memory management of views
	that is a companion to the multiset views.  Datascope uses
	arrays of pointers to construct relational db views that have
	to be managed manually.  This method must only be called when
	the retain_parent booloeans used in the relational methods
	is set false.  In his situation this procedure does two things.
	First, it checks to see if the parent is a view.  If it is a plain
	table it does nothing.  Otherwise it calls dbfree on the parent.
	Second, if the view is destroyed it sets the parent_table Dbptr
	to all dbINVALID. */
	void manage_parent();
	/*! Escape switch for retaining parent tables of a view.

	Automatic memory management used in this handle will normally 
	automatically release memory associated with a view when a new
	view is created as a child of another view.  When retain_parent is
	true dbfree is not called on the parent after a new view is created.
	This is desirable only if the user wants to access the view 
	again as a raw db pointer.  The proper solution with this interface
	is to make a copy of the parent handle before calling the 
	method that creates a new view.  Then the copy will cause the 
	view count to be larger than 1 and dbfree will not be used.  
	In short, I've retained this features as a hidden escape valve
	to provide a hack fix for some programs, but setting this
	boolean true is to be avoided. */
	bool retain_parent; 
};
/*! \brief Simplified interface to dbmatches.

The datascope (Antelope) db interface has a useful, but extremely difficult to use
procedure called dbmatches.  The purpose of dbmatches is to provide a fast alternative
to subsetting a database.  That is dbmatches aims to produce a list of record numbers
that match exactly a set of key attributes.  Once built a match handle can provide
much faster access to a data subset than what could be achieved through repeated 
calls to dbsubset.  The cost is, from my experience, the most messy procedure in 
the whole datascope library.  The intention of this object is to simplify that 
process by providing an alternative front end.  The basic model behind this interface
is that one calls the base constructor to build the dbmatches array of pointers
that define that process.  That obscurity of the datascope interface is hidden 
behind the constructor which, I hope, is simpler than dbmatches.  The process of 
finding the matching components is done through the find method.  Thus the model
when this object is most appropriate is many calls to the find method after the
original handle is constructed.  
*/
class DatascopeMatchHandle : public DatascopeHandle
{
public:
	/*! \brief Default constructor.
	*
	* Default constructor is required in some contexts.  The object
	* it creates should never be used as it will always be invalid.
	* This is done, as in a DatascopeHandle, by setting all the
	* Dbptr fields to dbINVALID. */
	DatascopeMatchHandle();
	/*! \brief The only valid constructor for this object.
	* 
	* The default constructor is defined, but is always invalid.  This
	* should be considered the only valid constructor.
	* It is driven by a set of key attributes passed through 
	* an STL list. 
	*
	* \param parent is a handle to the database view which repeated matches
	*	are to be requested.
	* \param table is the name of the table to which dbmatches should be applied.
	*	If this string is zero length (string("")) the Dbptr in the parent
	*	handle will be used directly.  Otherwise dblookup is called using
	*	this table name.
	* \param matchkeys is a list of key attributes to be matched for this view.
	*	dbmatches allows a more general definition of matching than is possible
	*	through through this handle.  Here this is a list of attributes that 
	*	will be tested for a match conditions for all requests to this handle.
	* \param am defines the mapping operator between external and internal name
	*	conventions for the database schema being used.  This object also 
	*	provides some capability in detecting mismatches in data type between
	*	the find method and the internal contents of the handle.  
	* \exception SeisppDberror is thrown if there are database related errors.
	* \exception SeisppError is thrown for other possible errors.
	*/
	DatascopeMatchHandle(DatascopeHandle& parent,
			string table, 
			list<string> matchkeys,
			AttributeMap& am);
	/*! Standard copy constructor. */
	DatascopeMatchHandle(const DatascopeMatchHandle& parent);
	/*! Standard assignment operator. */
	DatascopeMatchHandle& operator = (const DatascopeMatchHandle& parent);
	/*! Standard destructor. 
	The destructor here will release the memory held by the hook that stores the
	matching index, but will not normally close the database unless the parent
	db had called the close() method. 
	*/
	~DatascopeMatchHandle();
	/*! Find a particular record using the match index.
	This is the primary working method for this object.  The idea is to post
	a set of specific values to a Metadata object using an internal name
	set and find the associated records that match that condition.  Internal
	rather than external names are used for a very good reason.  In SEISPP
	several data objects (TimeSeries, ThreeComponentSeismograms, various
	ensemble objects, and others) inherit Metadata objects.  This means we
	can do things like ask for a matching record to a particular seismogram
	by casting something like a TimeSeries into a Metadata object.  This 
	can provide a very clean way to match a data object to it's parent database
	row.  It is more troublesome with views, but the concept is the same.
	It does require much more care to get it right though.  
	\par
	The normal behaviour of this method will be be take input metadata
	parameters and map them to fully qualified names used by Datascope
	defined by the AttributeMap object cached in the private area of
	this object.  (Fully qualified means names like wfdisc.sta as
	opposed to the attribute name, in this case sta, alone.)  This
	behaviour can be overridden by setting boolean use_fullname
	argument false using the 2 argument call to this method.  
	An approach where this is known to be required is in matching
	against a dbgroup view.  Apparently dbgroup uses the literal
	attribute string passed to dbgroup.  This means, for example,
	if you call dbgroup with evid, attempts to match with event.evid
	will fail.  The use_fullname=false mechanism is a workaround
	for this problem but seemed a sensible extension of the interface
	anyway.
	\par
	Note this routine will spew some output to stderr in the event of certain 
	nonfatal errors.  There probably should be a verbose flag flag to turn these
	on and off as desired, but for now they are there.  These errors don't deserve
	an exception error because they are not fatal, but the user should be warned
	they occur.  Probably is a better way to handle this, but this seemed a 
	sensible compromise for now.

	\param md the contents of the matchkey attributes (defined by the one
		and only valid constructor for this object) are extracted from
		this object and matched against the table held inside this handle.
	\param use_fullname is a boolean controlling alias name expansion.
		To make sure a key is unambiguous the default expands 
		keys to full qualified names (The format is table.attribute.  
		For example, wfdisc.sta).  If this parameter is set false, only
		the base attribute name will be used to define the match
		criteria.  (See above for more on this feature.)
	\return list of integer record numbers that match the contents of md.
		List will be zero length if there are no matches.
	\exception SeisppError is thrown if the contents of md do not contain the 
		match keys held by the handle.
	\exception SeisppDberror is thrown if the internal call to dbmatches fails.

	*/
	list<long> find(Metadata& md, bool use_fullname=true);
private:
	Dbptr dbscratch_record;
	Dbptr dbt;
	// These are always the same at present, but we'll store both
	// partly to clarify the interface.  storage cost is small
	Tbl *kpattern;
	Tbl *tpattern;
	Hook *hook;
	// This stores the stl equivalents of the kpattern and tpattern
	// antelope tbls.  i.e. matchkeys[i] is an expansion of
	// gettbl(kpattern,i)
	vector<AttributeProperties> matchkeys;
	/* This is a parallel vector with matchkeys that links Metadata
	attribute names that may be aliases unambiguously to the matchkeys
	vector.  At one point this interface allowed unqualified names 
	(e.g. time as shorthand for wfdisc.time) through the AttributeMap.
	March 2008 this as altered to provide a more flexible alias mechanism.*/
	vector<string> MDnamekeys;
	/*! This is a copy of the attribute map used by this object hidden
	behind the interface as the user should only know they need to 
	define the schema. */
	AttributeMap amap;
};

/*! \brief Find first valid table in a view for an alias attribute. 

Because some attributes are conveniently specified by shorthand aliases that attribute
can exist in more than one table in a view.  Bad things happen if one tries to ask for
an attribute by a fully qualified name (i.e. one that includes the table) if that 
table is not the current database handle view.  This procedure returns the table name 
of the first match it finds in a list of possible tables.  

\param db Datascope db pointer of view that is to be scanned.  
\param tables_to_test is a list of table names to try using specified alias

\return Normal return is the first valid table name.  Returns the string "BAD" 
if no match is found in the input view.  The caller must arrange to trap this
as an error.  Intentional design decision to avoid unnecessary overhead of 
an exception.  Assumption is this is a low level return totally appropriate
as a more standard procedure.  The frozen string is not very elegant, but 
preferable to dealing with a constant in an include file  The frozen string is not very elegant, but 
preferable to dealing with a constant in an include file.
*/
string FindFirstValidTable(Dbptr db,list<string> tables_to_test);

}  // end namespace seispp
#endif
