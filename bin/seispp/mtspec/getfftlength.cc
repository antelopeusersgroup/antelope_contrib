#include "SeisppError.h"
using namespace SEISPP;
int getfftlength(int n)
{
  if(n<=0) 
    throw SeisppError(string("getfftlength - illegal argument.  Input must be greater than zero."));
  unsigned int N(n);
  int nshift;
  nshift=0;
  while(N>0)
  {
    N=(N>>1);
    // only allow 32 bit shifting - ints may be 64 but safer
    ++nshift;
    if(nshift>=32) break;
  }
  unsigned int nfft(1);
  nfft=(nfft<<nshift);
  return ((int)nfft);
}
