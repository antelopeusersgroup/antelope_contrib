      function xnorm(sigma) 
c   xnorm  - function which generates successive random samples from a  
c             normal population at each call
c        xnorm  uses random number generator ranf and a transformation
c             algorithm to generate the random samples
c) the seed for xnorm can be changed by calling ranset(x) 
c)   with x as the new desired seed (can use either a 
c)   call statement or dummy function statement 
c        the maximum value of xnorm  is limited in the program to 
c             approximately three times sigma 
c        usage
c           x = xnorm(sigma)
c             x is the variable into which the next sample is stored
c             sigma is standard deviation of desired sequence 
      data c0,c1,c2,d1,d2,d3/2.515517,.802853,.010328,1.432788,.189269,.
     $001308/ 
 100  u = ranf()-0.5
      if (abs(u).lt.1.0e-03) go to 100
      sgn = u/abs(u)  
      u = 1./(u*u)
      t = alog(u) 
      t = sqrt(t) 
      a = ((c2*t)+c1)*t+c0
      b = (((d3*t)+d2)*t+d1)*t+1. 
      xnorm = (t-(a/b))*sgn*sigma 
      return
      end 

c $Id$ 
