      logical function ftest(x1,nu1,x2,nu2) 
c------------------------------------------------------------------ 
c      this routine applies an f test to two input chi-squared
c  random variables.  a value of .true. is returned if x1/nu1 is
c  significantly greater than x2/nu2 at the 95% confidence level. 
c  also returns a value of .true. if data input errors are detected.
c  otherwise the routine returns a value of .false..  the routine 
c  uses tables interpolated using the reciprocals of nu1 and nu2 as the 
c  interpolation variable for which this table is approximatly linear.  
c  tables are from abramowitz and stegun (1964) 
c  "handbook of mathematical functions", dover pub., new york.
c 
c  arguments- 
c    x1, nu1  - x1 is expected to be a sample from a chi squared
c               distribution with nu1 degrees of freedom. 
c    x2, nu2  - x2 is expected to be a sample from a chi squared
c               distribution with nu2 degrees of freedom. 
c 
c  author   gary l. pavlis
c           geophysics program ak-50
c           university of washington
c           seattle, wa  98195  
c 
c  written   march 3, 1983
c------------------------------------------------------------------ 
      real x1,x2
      real dq1,da2
      integer nu1,nu2 
c--nu1tab and nu2tab are the degrees of freedom corresponding to the
c--table of critical f values stored in fcrit 
      parameter(num1=13,num2=34)
      integer nu1tab(num1),nu2tab(num2) 
      real fcrit(num1,num2) 
c--work is used to pass table values in correct order to interpolation
c--routine "shape". 
      real work(4)
c--i1 and i2 point to table degrees of freedom for nearest
c--tabulated degrees of freedom greater than nu1 and nu2 respectively 
      integer i1,i2 
c--these variables are reciprocal degrees of freedom from table nearest 
c--neighbors to nu1 and nu2 
      real t1low,t2low,t1high,t2high
c--holds interpolated table value 
      real test 
      data nu1tab/1,2,3,4,5,6,8,12,15,20,30,60,10000/ 
      data nu2tab/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20, 
     $              21,22,23,24,25,26,27,28,29,30,40,60,120,10000/
      data ((fcrit(i,j),i=1,num1),j=1,10) 
     1          /161.4,199.5,215.7,224.6,230.2,234.,238.9,243.9,
     $          245.9,248.,250.1,252.2,254.3, 
     2          18.51,19.,19.16,19.25,19.3,19.33,19.37,19.41,19.43, 
     $          19.45,19.46,19.48,19.5, 
     3          10.13,9.55,9.28,9.12,9.01,8.94,8.85,8.74,8.7, 
     $          8.66,8.62,8.57,8.53,
     4          7.71,6.94,6.59,6.39,6.26,6.16,6.04,5.91,5.86, 
     $          5.8,5.75,5.69,5.63, 
     5 6.61,5.79,5.41,5.19,5.05,4.95,4.82,4.68,4.62,4.56,4.5,4.43,4.36, 
     6 5.99,5.14,4.76,4.53,4.39,4.28,4.15,4.,3.94,3.87,3.81,3.74,3.67,
     7 5.59,4.74,4.35,4.12,3.97,3.87,3.73,3.57,3.51,3.44,3.38,3.3,3.23, 
     8 5.32,4.46,4.07,3.84,3.69,3.58,3.44,3.28,3.22,3.15,3.08,3.01,2.93,
     9 5.12,4.26,3.86,3.63,3.48,3.37,3.23,3.07,3.01,2.94,2.86,2.79,2.71,
     e 4.96,4.1,3.71,3.48,3.33,3.22,3.07,2.91,2.85,2.77,2.7,2.62,2.54/
      data ((fcrit(i,j),i=1,num1),j=11,20)
     1/4.84,3.98,3.59,3.36,3.2,3.09,2.95,2.79,2.72,2.65,2.57,2.49,2.4,
     2 4.75,3.89,3.49,3.26,3.11,3.,2.85,2.69,2.62,2.54,2.47,2.38,2.3, 
     3 4.67,3.81,3.41,3.18,3.03,2.92,2.77,2.6,2.53,2.46,2.38,2.3,2.21,
     4 4.6,3.74,3.34,3.11,2.96,2.85,2.7,2.53,2.46,2.39,2.31,2.22,2.13,
     5 4.54,3.68,3.29,3.06,2.9,2.79,2.64,2.48,2.4,2.33,2.25,2.16,2.07,
     6 4.49,3.63,3.24,3.01,2.85,2.74,2.59,2.42,2.35,2.28,2.19,2.11,2.01,
     7 4.45,3.59,3.2,2.96,2.81,2.7,2.55,2.38,2.31,2.23,2.15,2.06,1.96,
     8 4.41,3.55,3.16,2.93,2.77,2.66,2.51,2.34,2.27,2.19,2.11,2.02,1.92,
     9 4.38,3.52,3.13,2.9,2.74,2.63,2.48,2.31,2.23,2.16,2.07,1.98,1.88, 
     e 4.35,3.49,3.1,2.87,2.71,2.6,2.45,2.28,2.2,2.12,2.04,1.95,1.84/ 
      data ((fcrit(i,j),i=1,num1),j=21,30)
     1/4.32,3.47,3.07,2.84,2.68,2.57,2.42,2.25,2.18,2.1,2.01,1.92,1.81, 
     2 4.3,3.44,3.05,2.82,2.66,2.55,2.4,2.23,2.15,2.07,1.98,1.89,1.78,
     3 4.28,3.42,3.03,2.8,2.64,2.53,2.37,2.2,2.13,2.05,1.96,1.86,1.76,
     4 4.26,3.4,3.01,2.78,2.62,2.51,2.36,2.18,2.11,2.03,1.94,1.84,1.73, 
     5 4.24,3.39,2.99,2.76,2.6,2.49,2.34,2.16,2.09,2.01,1.92,1.82,1.71, 
     6 4.23,3.37,2.98,2.74,2.59,2.47,2.32,2.15,2.07,1.99,1.9,1.8,1.69,
     7 4.21,3.35,2.96,2.73,2.57,2.46,2.31,2.13,2.06,1.97,1.88,1.79,1.67,
     8 4.2,3.34,2.95,2.71,2.56,2.45,2.29,2.12,2.04,1.96,1.87,1.77,1.65, 
     9 4.18,3.33,2.93,2.7,2.55,2.43,2.28,2.1,2.03,1.94,1.85,1.75,1.64,
     e 4.17,3.32,2.92,2.69,2.53,2.42,2.27,2.09,2.01,1.93,1.84,1.74,1.62/
      data ((fcrit(i,j),i=1,num1),j=31,34)
     1/4.08,3.23,2.84,2.61,2.45,2.34,2.18,2.,1.92,1.84,1.74,1.64,1.51,
     2 4.,3.15,2.76,2.53,2.37,2.25,2.1,1.92,1.84,1.75,1.65,1.53,1.39, 
     3 3.92,3.07,2.68,2.45,2.29,2.17,2.02,1.83,1.75,1.66,1.55,1.43,1.25,
     4 3.84,3.,2.6,2.37,2.21,2.1,1.94,1.75,1.67,1.57,1.46,1.32,1.0/ 
      if((nu1.le.0).or.(nu2.le.0).or.(x2.le.0.0)) then
c--exit for errors
           ftest = .true. 
           return 
      endif 
      if((x1/nu1).le.(x2/nu2)) then 
c--return immediately in this case to minimize computations 
           ftest = .false.
           return 
      endif 
c--the two loops that follow find the nearest neighbors in the
c--table to nu1 and nu2.  some added complexity is required 
c--because the num1 and num2 column and row of the table
c--correspond to infinite degrees of freedom.  this is ok for 
c--this interpolation as the reciprocal of infinity is zero.  
      i1 = num1 - 1 
  100 if((nu1.ge.nu1tab(i1)).or.(i1.le.1)) go to 150
           i1= i1 - 1 
           go to 100  
  150 continue
      if(i1.eq.num1) then 
           t1low = 0.0
      else  
           t1low = 1.0/float(nu1tab(i1+1))
      endif 
      t1high = 1.0/nu1tab(i1) 
      i2 = num2 - 1 
  200 if((nu2.ge.nu2tab(i2)).or.(i2.le.1)) go to 250
           i2= i2 - 1 
           go to 200  
  250 continue
      if(i2.eq.num2) then 
           t2low = 0.0
      else  
           t2low = 1.0/float(nu2tab(i2+1))
      endif 
      t2high = 1.0/nu2tab(i2) 
      work(1) = fcrit(i1+1,i2+1)
      work(2) = fcrit(i1,i2+1)
      work(3) = fcrit(i1+1,i2)
      work(4) = fcrit(i1,i2)
      dq1 = 1.0/float(nu1) ;
      dq2 = 2.0/float(nu2) ;
      test=shape1(t1low,t2low,t1high,t2high,work,dq1,dq2)
      f = (x1/nu1)/(x2/nu2) 
      if(f.gt.test) then
           ftest = .true. 
      else  
           ftest = .false.
      endif 
      return
      end 

c $Id$ 
