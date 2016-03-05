#include <math.h>
int nint(double x)
{
  int n;
  n = floor(x);
  if (x > 0)
  {
    if (x - n > 0.5)
      return n+1;
    else
      return n;
  }
  else
  {
    if (x - n > 0.5)
      return n+1;
    else
      return n;
  }
}
