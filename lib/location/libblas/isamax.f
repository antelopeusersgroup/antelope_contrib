      integer function isamax(n,sx,incx)                                 isamax      2
c***begin prologue  isamax                                               isamax      3
c***revision date  811015   (yymmdd)                                     isamax      4
c***category no.  f1a                                                    isamax      5
c***keywords  blas,vector,largest component                              isamax      6
c***date written  october 1979                                           isamax      7
c***author lawson c. (jpl),hanson r. (sla),                              isamax      8
c                            kincaid d. (u texas), krogh f. (jpl)        isamax      9
c***purpose                                                              isamax     10
c    find largest component of s.p. vector                               isamax     11
c***description                                                          isamax     12
c                b l a s  subprogram                                     isamax     13
c    description of parameters                                           isamax     14
c                                                                        isamax     15
c     --input--                                                          isamax     16
c        n  number of elements in input vector(s)                        isamax     17
c       sx  single precision vector with n elements                      isamax     18
c     incx  storage spacing between elements of sx                       isamax     19
c                                                                        isamax     20
c     --output--                                                         isamax     21
c   isamax  smallest index (zero if n.le.0)                              isamax     22
c                                                                        isamax     23
c     find smallest index of maximum magnitude of single precision sx.   isamax     24
c     isamax =  first i, i = 1 to n, to minimize  abs(sx(1-incx+i*incx)  isamax     25
c                                                                        isamax     26
c***references                                                           isamax     27
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   isamax     28
c   *basic linear algebra subprograms for fortran usage*,                isamax     29
c  algorithm no. 539, transactions on mathematical software,             isamax     30
c  volume 5, number 3, september 1979, 308-323                           isamax     31
c***routines called  (none)                                              isamax     32
c***end prologue  isamax                                                 isamax     33
c                                                                        isamax     34
      real sx(1),smax,xmag                                               isamax     35
c***first executable statement  isamax                                   isamax     36
      isamax = 0                                                         isamax     37
      if(n.le.0) return                                                  isamax     38
      isamax = 1                                                         isamax     39
      if(n.le.1)return                                                   isamax     40
      if(incx.eq.1)goto 20                                               isamax     41
c                                                                        isamax     42
c        code for increments not equal to 1.                             isamax     43
c                                                                        isamax     44
      smax = abs(sx(1))                                                  isamax     45
      ns = n*incx                                                        isamax     46
      ii = 1                                                             isamax     47
          do 10 i=1,ns,incx                                              isamax     48
          xmag = abs(sx(i))                                              isamax     49
          if(xmag.le.smax) go to 5                                       isamax     50
          isamax = ii                                                    isamax     51
          smax = xmag                                                    isamax     52
    5     ii = ii + 1                                                    isamax     53
   10     continue                                                       isamax     54
      return                                                             isamax     55
c                                                                        isamax     56
c        code for increments equal to 1.                                 isamax     57
c                                                                        isamax     58
   20 smax = abs(sx(1))                                                  isamax     59
      do 30 i = 2,n                                                      isamax     60
         xmag = abs(sx(i))                                               isamax     61
         if(xmag.le.smax) go to 30                                       isamax     62
         isamax = i                                                      isamax     63
         smax = xmag                                                     isamax     64
   30 continue                                                           isamax     65
      return                                                             isamax     66
      end                                                                isamax     67

c $Id$ 
