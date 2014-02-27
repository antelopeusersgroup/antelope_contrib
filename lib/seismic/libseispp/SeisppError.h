#ifndef _SEISPPERROR_H_
#define _SEISPPERROR_H_
#include <iostream>
#include "db.h"
namespace SEISPP {
using namespace std;
/*! \brief Base class for error object thrown by seispp library routines.

 This is the generic error object thrown by the seispp library. 
 it is similar in concept to basic error objects described in various
 books by Stroustrup.  The base object contains only a simple 
 generic message and a virtual log_error method common to all 
 seispp error objects that are it's descendents.
\author Gary L. Pavlis
**/
class SeisppError 
{
public:
/*!
 Holds error message that can be printed with log_error method.
**/
	string message;
/*!
 Default constructor built inline.
**/
	SeisppError(){message="seispp library error\n";};
/*! Construct from a std::string
**/
	SeisppError(const string mess){message=mess;};
/*! Construct from a char *
**/
	SeisppError(const char *mess){message=string(mess);};
/*!
 Sends error message thrown by seispp library functions to standard error.
**/
	void log_error(){
            cerr << message << endl;
        };
/*! This overrides the method in std::exception to load our string.
  This allows handlers to use the what method and get the error string
  from seisp.  Idea copied from:  
  http://www.cplusplus.com/doc/tutorial/exceptions/
  */
        const char * what() const throw()
        {
            return message.c_str();
        }
};

#ifndef NO_ANTELOPE
/*!
 Defines severity code for errors thrown by database routines.
**/
enum ErrorSeverity {fault, fatal, complain, notify, log, unknown};
/*! \brief Error object thrown by routines accessing an Antelope database.  

 This object normally leads to errors written by Antelope elog functions
 and extra messages attached to the error object.
\author Gary L. Pavlis
**/
class SeisppDberror : public SeisppError
{
public:
/*!
 Antelope (Datascope) database pointer when error occurred.
**/
        Dbptr db;
/*!
 Error severity level code.  
**/
	ErrorSeverity error_type;
/*!
 Standard constructor for this object.
**/
        SeisppDberror(const string mess,Dbptr dbi);
/*!
 Copy constructor.
**/
	SeisppDberror(const string mess, 
		Dbptr dbi, ErrorSeverity et);
};
#endif
/*! \brief Special error object thrown by SAC file reader.

\author Gary L. Pavlis
**/

class SACdataError : public SeisppError
{
public:
/*!
 Standard constructor for this object.
**/
	SACdataError(const string mess){message=mess;};
/*!
 Sends error message thrown by seispp library functions to standard error.
**/
	void log_error()
	{
		cerr<<"Error processing SAC format time series"<<endl;
		cerr<<"Error message = "<<message;
	}
};


}  // End SEISPP namespace declaration
#endif
