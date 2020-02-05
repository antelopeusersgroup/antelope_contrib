#include "Metadata.h"
using namespace SEISPP;
class HdrmathArg
{
public:
  HdrmathArg();
  /*!~ Primary constructor for this application.  Parse command line 
    argument that assumes syntax name:type or a number */
  HdrmathArg(char *arg);
  HdrmathArg(long val);
  HdrmathArg(int val);
  HdrmathArg(float val);
  HdrmathArg(double val);
  /*! Initialize from metdata.

    This constructor is used for the left hand side argument of 
    a sequence of operations.  e.g. chan += 2 */
  HdrmathArg(Metadata& md, string key, MDtype mt);
  /*! Copy constructor. */
  HdrmathArg(const HdrmathArg& parent);
  /*! Boolean defines if this entity is a metadata attribute.

  If set true the contents are expected to be available by fetching
  with get methods defined in Metadata.h.   If false, the value is 
  assumed to be a constant. 
  */
  bool IsMetadata;
  string key;   
  MDtype mdt;
  long get_long();
  double get_double();
  /* In every other class definition I've (glp) ever done these operator
     definitions have a const qualifier for the argument.   For reasons
     I don't get this created a conflict here so they are by necessity
     NOT const. */
  //HdrmathArg& operator=(HdrmathArg&& parent);
  HdrmathArg& operator=(const HdrmathArg& parent);
  /* All of the operators will require that if the type of the
     left hand side will always take precedence.  That will assure 
     that the ultimate target of a string of operators will match
     the required type of the result that is to be set. */
  HdrmathArg& operator+=(HdrmathArg& other);
  HdrmathArg& operator-=(HdrmathArg& other);
  HdrmathArg& operator*=(HdrmathArg& other);
  HdrmathArg& operator/=(HdrmathArg& other);
  HdrmathArg& operator%=(HdrmathArg& other);
  HdrmathArg operator+(HdrmathArg& other);
  HdrmathArg operator-(HdrmathArg& other);
  HdrmathArg operator*(HdrmathArg& other);
  HdrmathArg operator/(HdrmathArg& other);
  HdrmathArg operator%(HdrmathArg& other);
  /*! Set the Metadata component with new data.  

    The way this object is used is expected to be this.  The arg list
    is parsed to make a skeleton for each HdrmathArg object in which the
    metadata itself is empty.   When a new block of data is read we load
    each HdrmathArg object with the metadata (header) from the parent data.
    This method is used to set that data. */
  void SetMetadata(Metadata& dnew);
private:
  /* One of these to variables are set when arg is constant 
     (IsMetadata is false). */
  long ival;
  double rval;
  Metadata d;  // a copy of the header of the object being processed
};
