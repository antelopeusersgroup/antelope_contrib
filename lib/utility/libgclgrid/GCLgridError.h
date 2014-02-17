#ifndef _GCLGRIDERROR_H_
#define _GCLGRIDERROR_H_
#include <stdlib.h>
#include <string>
using namespace std;
/* \brief Error class thrown for errors in this package.

Simple child of stdlib abstract exception class.  Handler should catch a
generic exception and call the what() method to print the message.
*/
class GCLgridError : public exception
{
public:
    GCLgridError(string mess){message="GCLgridError:  "+mess;};
    GCLgridError(const char *s){message="GCLgridError:  "+string(s);};
    virtual const char* what() const throw()
   {
    return message.c_str();
   };
   /* This is necessary baggage to mesh with std::exception declaration
      for this virtual method.  Not required on all compilers */
   ~GCLgridError() throw ()
   {};
protected:
    string message;
};
#endif
