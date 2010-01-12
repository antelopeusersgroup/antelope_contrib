#ifndef _DATABASEHANDLE_H_
#define _DATABASEHANDLE_H_

#include <string>
#include <list>

namespace SEISPP
{
using namespace std;
using namespace SEISPP;
/*! \brief Abstract, base class defining a generic database handle.

Databases are ubiquitous is science today, but there is no standard 
programming interface.  That is although things like Structured Query
Language (SQL) standardize command used to make a database query, each
vendor tends to supply a different API to interact with a database at
the application level.  As a result code that needs to directly access
a database tends to be filled with API calls that are tightly connected
to a specific database.  Just look at typical programs that interact with
Oracle or Antelope and you quickly understand this problem.  The 
SEISPP library was written with a prejudice that all seismology data
could and should always be contained in a relational database.  
Thus, many constructors are passed this generic database handle assuming
the API is capable of sorting out what the handle really means.  This
is an excellent example of the use of an abstract base class to make
an interface as flexible as possible.  Constructors passed a reference
to a DatabaseHandle can and should sort out what it really is through
RTTI mechanisms or (as the present library does) simply assume something
and throw and exception if that assumption is invalid.  
\par
This object is purely virtual.  It's design is heavily impacted by 
the author's experience with the Antelope datascope API.  As a result
it may be missing some concepts found in some databases, but that should
be largely irrelevant.  That is, it contains method definitions that 
span what I believe are the primary concepts behind any relational
database.  If anything is missing here, it can be defined in a derived 
class and as a new method.  A more likely problem in the design with
this background is that the argument list for some of the method is
not appropriate.
\par
A key concept this interface assumes is that a database view can be 
conceptualized as a table.  The current order of the tuples is assumed
to be a static property of this handle.  (i.e. every view has an 
different handle. )  Thus there is the concept of a row pointer that 
can be used too work through the table row (tuple) by row.  This is
a central concept in the Antelope API and I know it exists at least
for postgres.  This pointer is manipulated by the ++ operator and
rewind method.  Because the pointer is considered abstract there is 
no way to ask for a value of the pointer.  
\par
A final concept worth noting in this interface is that a handle is
considered mutable.  For example, methods like subset will replace
the current handle with one pointing only at the new view created
by the subset.  The understanding is that if one wants to retain the
previous view a copy constructor would be called to hold a copy of 
the review before that operation.  It is not clear if all dbms 
engines allow this functionality, but it is a central concept in
the Antelope API that I considered worth making a part of this interface.
*/

class DatabaseHandle
{
public:
	/*! \brief Get an attribute from current tuple returning a double.
	All relational dbms systems have a string tag associated with the attribute defining
	the "name" of that attribute within the dbms.  Real numbers are always
	a standard type supported by any dbms.  This method returns the value 
	of an real-valued attribute for the current tuple.  The result it always
	returned as a double independent of the actual internal precision for
	simplicity.  
	\param attribute_name name of the desired attribute in the schema associated with this handle.
	*/
	virtual double get_double(string attribute_name)=0;
	/*! \brief Get an attribute from current tuple returning an integer.
	All relational dbms systems have a string tag associated with the attribute defining
	the "name" of that attribute within the dbms.  Integers are always
	a standard type supported by any dbms.  This method returns the value 
	of an integer-valued attribute for the current tuple.  The base
        class defines only a long int as a required method.  Concrete 
        implementations may want to add automated conversion routines for
        short or standard int output.
	\param attribute_name name of the desired attribute in the schema associated with this handle.
	*/	
	virtual long get_long(string attribute_name)=0;
	/*! \brief Get an attribute from current tuple returning a string.
	All relational dbms systems have a string tag associated with the attribute defining
	the "name" of that attribute within the dbms.   Strings are
	a standard type supported by any dbms.  This method returns the value 
	of a string attribute for the current tuple.  The result is always
	returned as an std string for simplicity.  
	\param attribute_name name of the desired attribute in the schema associated with this handle.
	*/
	virtual string get_string(string attribute_name)=0;
	/*! \brief Create a filename from attributes stored in a database. 
	* In seismology we often store data in files with the dbms used to index data within these
	* files.  When implemented this method should return a valid filename associated with the
	* current tuple. */
	virtual string get_filename()=0;
	/*! \brief Add a new tuple.  
	* A basic database operation is to add new data to a table.  This method uses a 
	* concept in Antelope's API that one can add a new row with no data and then later
	* push actual information to that tuple with one or more calls to a put procedure
	* (dbputv in Antelope).  This may not be workable for all dbms APIs.  In any 
	* case the current record holds a special significance here.  After this method
	* is called the current record pointer should point at the tuple created through
	* this method.  
	* \return a number defining the row index of this tuple.  
	*/
	virtual long append()=0;
	/*! Put a real variable to a database at the current record position.
	* This method will put one attribute to a database table at the current 
	* record position. 
	* \param name attribute name of quantity to be saved to database.
	* \param value value of this attribute to save to current tuple. 
	*/
	virtual void put(string name,double value)=0;
	/*! Put a real variable to a database at the current record position.
	* This method will put one attribute to a database table at the current 
	* record position.  
	* \param name attribute name of quantity to be saved to database.
	* \param value value of this attribute to save to current tuple. 
	*/
	virtual void put(string name,float value)=0;
	/*! Put an integer variable to a database at the current record position.
	* This method will put one attribute to a database table at the current 
	* record position. 
	* \param name attribute name of quantity to be saved to database.
	* \param value value of this attribute to save to current tuple. 
	*/
	virtual void put(string name,long value)=0;
	/*! Put a string variable to a database at the current record position.
	* This method will put one attribute to a database table at the current 
	* record position.  For this version the value is stored as an std string.
	* \param name attribute name of quantity to be saved to database.
	* \param s string variable containg value to save to current tuple. 
	*/
	virtual void put(string name,string s)=0;
	/*! Put a string variable to a database at the current record position.
	* This method will put one attribute to a database table at the current 
	* record position.  For this version the value is stored as a raw C char *.
	* \param name attribute name of quantity to be saved to database.
	* \param s char * variable containing strng to store in current tuple. 
	*/
	virtual void put(string name,char *s)=0;
	/*! Put a string variable to a database at the current record position.
	* This method will put one attribute to a database table at the current 
	* record position. This method assumes the string is stored as a raw
	* plain C char *.  
	* \param name attribute name of quantity to be saved to database.
	* \param s constant char * variable containing string to store to current tuple.
	*/
	virtual void put(string name,const char *s)=0;
	/*! Return number of tuples in current view (table). */
	virtual long number_tuples()=0;
	/*! Return the number of attributes in the current view. */
	virtual int number_attributes()=0;
	/*! \brief Set the table pointer to the top of the table.
	* The assumption of this interface is that one will normally want to build
	* a database view and work through the table from the top down.  This positions
	* the pointer to the top of the table. This abstract base does not contain a 
	* find function which might be useful (i.e. position the pointer to match some
	* condition.)  
	*/
	virtual void rewind()=0;  
	/*! Increment the current record pointer by one.  */
	virtual void operator ++()=0;
	/*! Sort the current view (table) using a set of key attributes.
	* All dbms systems allow a sort on one or more keys.  The assumption of this
	* interface is that the result of the source is a view with a logical order
	* defined by this sort operation.  It also assumes the sort alters the handle
	* and the result after calling this method is a different database view.
	*
	* \param keys list of attribute names to use to define the sort order.  The keys
	*   are assumed in increasing precedence order (i.e. first element is the highest 
	*   precedence, second is next, etc.).  
	*/
	virtual void sort(list<string>keys)=0;
	/*! Subset the current view by a specific sort clause.
	*  Subsetting a view by a specific criteria is a basic dbms operation.
	*  The syntax to define a subset condition is highly variable, but as
	*  far as I can judge all systems use some type of command that can be 
	*  passed as a string.  SQL is a specific example, but Antelope also 
	*  does using unix regular expressions.  Either fit this interface.
	*/
	virtual void subset(string)=0;
	/*! Join two tables in a relational database.
	*  A database join is the fundamental operation of a relational database.
	*  This method implements this operation on a particular dbms.
	*
	* \param t1 left table of join operation.
	* \param t2 right table of join operation.
	* \param keys1 list of keys to define join operation of left table.
	* \param keys2 list of keys to define the join operation for right table.
	*/
	virtual void join(string t1, string t2, 
		list<string> keys1,list<string>keys2)=0;
	/*! Group a database view by a set of key attributes.
	* This method should implement the group by clause of SQL which is 
	* also known as dbgroup in the Antelope API.  
	*
	* \param group_keys list of attributes to define the grouping.  Precedence order
	*   is left to right in the list.  i.e the first element of the list has the highest
	*   precendence and the rightmost element has the lowest precedence.
	*/
	virtual void group(list<string>group_keys)=0;
};
}  // end namespace seispp
#endif
