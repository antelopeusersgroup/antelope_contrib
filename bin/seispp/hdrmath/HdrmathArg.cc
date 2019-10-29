#include "HdrmathArg.h"
#include "boost/lexical_cast.hpp"
bool IsNumeric(const std::string& input) {
    return std::all_of(input.begin(), input.end(), ::isdigit);
}
bool IsInteger(const std::string& input)
{
    int i;
    for(i=0;i<input.length();++i)
    {
        if(!isdigit(input[i])) return false;
    }
    return true;
}
/* Modified from:  https://stackoverflow.com/questions/447206/c-isfloat-function*/

bool IsReal(const std::string &someString)
{
      using boost::lexical_cast;
      using boost::bad_lexical_cast; 

      try
      {
        boost::lexical_cast<float>(someString);
      }
      catch (bad_lexical_cast &)
      {
        return false;
      }
     return true;
}
pair<string,MDtype> parse_md_pair(string arg)
{
  const string seperator(":");
  size_t pos;
  pos=arg.find(seperator);
  if(pos==string::npos)
  {
    throw SeisppError("parse_md_pair:  syntax error in arg="
        +arg+"\nMust have format name:type\n");
  }
  MDtype mdt;
  string key=arg.substr(0,pos);
  string s=arg.substr(pos+1);
  if((s=="int") || (s=="INT") || (s=="integer") || (s=="INTEGER") )
    mdt=MDint;
  else if((s=="real") || (s=="REAL") || (s=="double") || (s=="DOUBLE") 
      || (s=="float") || (s=="FLOAT") )
    mdt=MDreal;
  else
    throw SeisppError("parse_md_pair:  syntax error in arg="
        +arg+"\nMust have format name:type\n");
  return pair<string,MDtype>(key,mdt);
}

HdrmathArg::HdrmathArg(char *arg) : d()
{
  string sarg(arg);
  char *endptr;   //used by strtod
  if(IsInteger(sarg))
  {
    IsMetadata=false;
    ival=strtol(arg,&endptr,10);
    rval=0.0;
    key=string("");
    mdt=MDint;
  }
  else if(IsReal(sarg))
  {
    IsMetadata=false;
    mdt=MDreal;
    rval=strtod(arg,&endptr);
    ival=0;
    key=string("");
  }
  else
  {
    try{
      pair<string,MDtype> argpair;
      argpair=parse_md_pair(sarg);
      key=argpair.first;
      mdt=argpair.second;
      rval=0.0;
      ival=0;
      IsMetadata=true;
    }catch(SeisppError& serr){throw serr;};
  }
}
HdrmathArg::HdrmathArg() : d()
{
  IsMetadata=false;
  ival=0;
  rval=0.0;
  mdt=MDinvalid;
  key=string("");
}
HdrmathArg::HdrmathArg(long val) : d()
{
  IsMetadata=false;
  ival=val;
  rval=0.0;
  mdt=MDint;
  key=string("");
}
HdrmathArg::HdrmathArg(int val) : d()
{
  IsMetadata=false;
  ival=(long)val;
  rval=0.0;
  mdt=MDint;
  key=string("");
}
HdrmathArg::HdrmathArg(double val) : d()
{
  IsMetadata=false;
  rval=val;
  ival=0;
  mdt=MDreal;
  key=string("");
}
HdrmathArg::HdrmathArg(float val) : d()
{
  IsMetadata=false;
  rval=(double)val;
  ival=0;
  mdt=MDreal;
  key=string("");
}
HdrmathArg::HdrmathArg(Metadata& md, string k, MDtype mt) : d(md)
{
  key=k;
  mdt=mt;
  IsMetadata=true;
  try{
    switch(mdt)
    {
      case MDint:
        ival=d.get<long>(key);
        rval=0.0;
        break;
      case MDreal:
        rval=d.get<double>(key);
        ival=0;
        break;
      default:
        throw SeisppError("HdrmathArg Metadata constructor:  illegal type argument");
    };
  }catch(...){throw;};
}
HdrmathArg::HdrmathArg(const HdrmathArg& parent) : d(parent.d)
{
  IsMetadata=parent.IsMetadata;
  mdt=parent.mdt;
  rval=parent.rval;
  ival=parent.ival;
  key=parent.key;
}
long HdrmathArg::get_long()
{
  if((this->mdt)!=MDint)
    throw SeisppError(string("HdrmathArg::get_long():  data in this object is not defined as integer"));
  if(IsMetadata)
  {
    try{
      ival=d.get<long>(this->key);
    }catch(MetadataGetError& mde){throw mde;};
  }
  return ival;
}
double HdrmathArg::get_double()
{
  if((this->mdt)!=MDreal)
    throw SeisppError(string("HdrmathArg::get_long():  data in this object is not defined a real number"));
  if(IsMetadata)
  {
    try{
      rval=d.get<double>(this->key);
    }catch(MetadataGetError& mde){throw mde;};
  }
  return rval;
}
/* This operator= has to have a very odd behavior to allow
   the constructs like x = 2.0.   The reason is that the left 
   hand side of a string of operators to command line arguments
   for this code MUST be metadata with a key used to load the
   final result.   Hence, operator= must check for parent
   being a constant (IsMetadata false) and behave differently
   if that is the case.  Further, for assignments like
   x=y the key needs to not be altered but left the same
   as x.   This is very very different than normal assignment
   so beware if this code is altered. 
 
   That oddity required this weird C11 extension.   The &&
   tells the compiler to use a copy of parent in the method.
   In this and all other operator= variants we always need to 
   post the computed value to metadata to allow accumulation in
   a chain.*/
HdrmathArg& HdrmathArg::operator=(const HdrmathArg& parent)
{
  const string base_error("HdrmathArg::operator= error: ");
  /* As always do nothing if parent and this are identical.  */
  if(this!=&parent)
  {
    if(parent.IsMetadata)
    {
      /* We seem to need to copy the parent.  Some oddity of lvalue and rvalue
       * I don't quite grok */
      HdrmathArg p2(parent);
      if(key.length()<=0)
        throw SeisppError(base_error+"lhs key is undefined");
      MDtype tmdt=this->mdt;
      /* In the assignment like the rest of the operators the left hand
         side determines the type of the result. Not the normal 
         behavior at all for operator=*/
      if(tmdt==MDreal)
      {
        double rtmp;
        switch(p2.mdt)
        {
          case MDreal:
            rtmp=p2.get_double();
            this->rval=rtmp;
            break;
          case MDint:
            this->rval=static_cast<double>(p2.get_long());
            break;
          default:
            throw SeisppError(base_error + "rhs has illegal type");
        }
        this->d.put(this->key,this->rval);
      }
      else if(tmdt==MDint)
      {
        switch(p2.mdt)
        {
          case MDreal:
            this->ival=static_cast<long>(p2.get_double());
            break;
          case MDint:
            this->ival=p2.get_long();
            break;
          default:
            throw SeisppError(base_error + "rhs has illegal type");
        };
        this->d.put(this->key,this->ival);
      }
      else
        throw SeisppError(base_error + "lhs argument has undefined or illegal type");
    }
    else
    {
        if((this->mdt)==MDint)
        {
            if(parent.mdt==MDint) 
                this->ival=parent.ival;
            else if(parent.mdt==MDreal)
                this->rval=static_cast<long>(parent.rval);
            else
                throw SeisppError(base_error + "rhs argument has illegal type");
            this->d.put(this->key,this->ival);
        }
        else if((this->mdt)==MDreal)
        {
            if(parent.mdt==MDint) 
                this->ival=static_cast<double>(parent.ival);
            else if(parent.mdt==MDreal)
                this->rval=parent.rval;
            else
                throw SeisppError(base_error + "rhs argument has illegal type");
            this->d.put(this->key,this->rval);
        }
        else
            throw SeisppError(base_error + "lhs argument has undefined or illegal type");
    }
  }
  return(*this);
}
HdrmathArg& HdrmathArg::operator+=(HdrmathArg& other)
{
  try{
    const string base_error("HdrmathArg operator+= error:  ");
    /* temporaries */
    long tmpint;
    double tmpreal;
    MDtype lhsmdt;
    lhsmdt=this->mdt;
    if(lhsmdt==MDreal)
    {
      switch (other.mdt)
      {
        case MDreal:
          this->rval+=other.get_double();
          break;
        case MDint:
          tmpint=other.get_long();
          this->rval+=static_cast<double>(tmpint);
          break;
        default:
          throw SeisppError(base_error + "Right hand side has illegal type");
      };
      this->d.put(this->key,this->rval);
    }
    else if(lhsmdt==MDint)
    {
      switch(other.mdt)
      {
        case MDreal:
          tmpreal=other.get_double();
          this->ival+=static_cast<long>(tmpreal);
          break;
        case MDint:
          this->ival+=other.get_long();
          break;
        default:
          throw SeisppError(base_error + "Right hand side has illegal type");
      };
      this->d.put(this->key,this->ival);
    }
    else
    {
      throw SeisppError(base_error  + "Target for assignment has illegal type");
    }
    return(*this);
  }catch(...){throw;};  
}
HdrmathArg& HdrmathArg::operator-=(HdrmathArg& other)
{
  try{
    const string base_error("HdrmathArg operator-= error:  ");
    /* temporaries */
    long tmpint;
    double tmpreal;
    MDtype lhsmdt;
    lhsmdt=this->mdt;
    if(lhsmdt==MDreal)
    {
      switch (other.mdt)
      {
        case MDreal:
          this->rval-=other.get_double();
          break;
        case MDint:
          tmpint=other.get_long();
          this->rval-=static_cast<double>(tmpint);
          break;
        default:
          throw SeisppError(base_error + "Right hand side has illegal type");
      };
      this->d.put(this->key,this->rval);
    }
    else if(lhsmdt==MDint)
    {
      switch(other.mdt)
      {
        case MDreal:
          tmpreal=other.get_double();
          this->ival-=static_cast<long>(tmpreal);
          break;
        case MDint:
          this->ival-=other.get_long();
          break;
        default:
          throw SeisppError(base_error + "Right hand side has illegal type");
      };
      this->d.put(this->key,this->ival);
    }
    else
    {
      throw SeisppError(base_error  + "Target for assignment has illegal type");
    }
    return(*this);
  }catch(...){throw;};  
}
HdrmathArg& HdrmathArg::operator*=(HdrmathArg& other)
{
  try{
    const string base_error("HdrmathArg operator*= error:  ");
    /* temporaries */
    long tmpint;
    double tmpreal;
    MDtype lhsmdt;
    lhsmdt=this->mdt;
    if(lhsmdt==MDreal)
    {
      switch (other.mdt)
      {
        case MDreal:
          this->rval*=other.get_double();
          break;
        case MDint:
          tmpint=other.get_long();
          this->rval*=static_cast<double>(tmpint);
          break;
        default:
          throw SeisppError(base_error + "Right hand side has illegal type");
      };
      this->d.put(this->key,this->rval);
    }
    else if(lhsmdt==MDint)
    {
      switch(other.mdt)
      {
        case MDreal:
          tmpreal=other.get_double();
          this->ival*=static_cast<long>(tmpreal);
          break;
        case MDint:
          this->ival*=other.get_long();
          break;
        default:
          throw SeisppError(base_error + "Right hand side has illegal type");
      };
      this->d.put(this->key,this->ival);
    }
    else
    {
      throw SeisppError(base_error  + "Target for assignment has illegal type");
    }
    return(*this);
  }catch(...){throw;};  
}
HdrmathArg& HdrmathArg::operator/=(HdrmathArg& other)
{
  try{
    const string base_error("HdrmathArg operator/= error:  ");
    /* temporaries */
    long tmpint;
    double tmpreal;
    MDtype lhsmdt;
    lhsmdt=this->mdt;
    if(lhsmdt==MDreal)
    {
      switch (other.mdt)
      {
        case MDreal:
          this->rval/=other.get_double();
          break;
        case MDint:
          tmpint=other.get_long();
          this->rval/=static_cast<double>(tmpint);
          break;
        default:
          throw SeisppError(base_error + "Right hand side has illegal type");
      };
      this->d.put(this->key,this->rval);
    }
    else if(lhsmdt==MDint)
    {
      switch(other.mdt)
      {
        case MDreal:
          tmpreal=other.get_double();
          this->ival/=static_cast<long>(tmpreal);
          break;
        case MDint:
          this->ival/=other.get_long();
          break;
        default:
          throw SeisppError(base_error + "Right hand side has illegal type");
      };
      this->d.put(this->key,this->ival);
    }
    else
    {
      throw SeisppError(base_error  + "Target for assignment has illegal type");
    }
    return(*this);
  }catch(...){throw;};  
}
/* This operator is a bit different because mod arithmetic only make
   sense on integers.  To cope we force conversions back and forth from real to int
   if required */
HdrmathArg& HdrmathArg::operator%=(HdrmathArg& other)
{
  try{
    const string base_error("HdrmathArg operator%= error:  ");
    /* temporaries */
    long tmpint,t2;
    double tmpreal;
    MDtype lhsmdt;
    lhsmdt=this->mdt;
    if(lhsmdt==MDreal)
    {
      switch (other.mdt)
      {
        case MDreal:
          tmpreal=other.get_double();
          tmpint=(static_cast<long>(this->rval))%(static_cast<long>(tmpreal));
          this->rval=static_cast<double>(tmpint);
          break;
        case MDint:
          tmpint=other.get_long();
          t2=static_cast<long>(this->rval);
          t2 %= tmpint;
          this->rval=static_cast<double>(t2);
          break;
        default:
          throw SeisppError(base_error + "Right hand side has illegal type");
      };
      this->d.put(this->key,this->rval);
    }
    else if(lhsmdt==MDint)
    {
      switch(other.mdt)
      {
        case MDreal:
          tmpreal=other.get_double();
          this->ival%=static_cast<long>(tmpreal);
          break;
        case MDint:
          this->ival%=other.get_long();
          break;
        default:
          throw SeisppError(base_error + "Right hand side has illegal type");
      };
      this->d.put(this->key,this->ival);
    }
    else
    {
      throw SeisppError(base_error  + "Target for assignment has illegal type");
    }
    return(*this);
  }catch(...){throw;};  
}
HdrmathArg HdrmathArg::operator+(HdrmathArg& other)
{
  try{
    return ((*this)+=other);
  }catch(...){throw;};
}
HdrmathArg HdrmathArg::operator-(HdrmathArg& other)
{
  try{
    return ((*this)-=other);
  }catch(...){throw;};
}
HdrmathArg HdrmathArg::operator*(HdrmathArg& other)
{
  try{
    return ((*this)*=other);
  }catch(...){throw;};
}
HdrmathArg HdrmathArg::operator/(HdrmathArg& other)
{
  try{
    return ((*this)/=other);
  }catch(...){throw;};
}
HdrmathArg HdrmathArg::operator%(HdrmathArg& other)
{
  try{
    return ((*this)%=other);
  }catch(...){throw;};
}
void HdrmathArg::SetMetadata(Metadata& din)
{
 try{
  d=din;
  if(mdt==MDint)
      ival=d.get<long>(this->key);
  else if(mdt==MDreal)
      rval=d.get<double>(this->key);
  else
      throw SeisppError("HdrmathArg::SetMetadata: coding error.  MDtype is not valid");
 }catch(...){throw;};
}