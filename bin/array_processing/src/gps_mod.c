/*Here is a suggested way to handle GPS data with extremely large slowness
  values.  It requires that a variable "scale" be added to the program.   
  Convert to s/mm for slowness if scale flag is 1.  This is actually 10^(-6)
  times the value that should be used in the table (s/km) in order to fit
  slownesses for GPS data into CSS3.0 schema definition.*/
  if (scale == 0)
    factor = 1.0;
  else
    factor = 1000000.0;
  semin = kmin/111.19/factor;
  semax = kmax/111.19/factor;
  snmin = kmin/111.19/factor;
  snmax = kmax/111.19/factor;

