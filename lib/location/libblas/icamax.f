      integer function icamax(n,cx,incx)                                 icamax      2
c***begin prologue  icamax                                               icamax      3
c***revision date  811015   (yymmdd)                                     icamax      4
c***category no.  f1a                                                    icamax      5
c***keywords  blas,vector,complex,component,largest component            icamax      6
c***date written  october 1979                                           icamax      7
c***author lawson c. (jpl),hanson r. (sla),                              icamax      8
c                            kincaid d. (u texas), krogh f. (jpl)        icamax      9
c***purpose                                                              icamax     10
c    find largest component of complex vector                            icamax     11
c***description                                                          icamax     12
c                b l a s  subprogram                                     icamax     13
c    description of parameters                                           icamax     14
c                                                                        icamax     15
c     --input--                                                          icamax     16
c        n  number of elements in input vector(s)                        icamax     17
c       cx  complex vector with n elements                               icamax     18
c     incx  storage spacing between elements of cx                       icamax     19
c                                                                        icamax     20
c     --output--                                                         icamax     21
c   icamax  smallest index (zero if n.le.0)                              icamax     22
c                                                                        icamax     23
c      returns the index of the component of cx having the               icamax     24
c      largest sum of magnitudes of real and imaginary parts.            icamax     25
c     icamax = first i, i = 1 to n, to minimize                          icamax     26
c        abs(real(cx(1-incx+i*incx))) + abs(imag(cx(1-incx+i*incx)))     icamax     27
c                                                                        icamax     28
c                                                                        icamax     29
c***references                                                           icamax     30
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   icamax     31
c   *basic linear algebra subprograms for fortran usage*,                icamax     32
c  algorithm no. 539, transactions on mathematical software,             icamax     33
c  volume 5, number 3, september 1979, 308-323                           icamax     34
c***routines called  (none)                                              icamax     35
c***end prologue  icamax                                                 icamax     36
c                                                                        icamax     37
      complex cx(1)                                                      icamax     38
c***first executable statement  icamax                                   icamax     39
      icamax = 0                                                         icamax     40
      if(n.le.0) return                                                  icamax     41
      icamax = 1                                                         icamax     42
      if(n .le. 1) return                                                icamax     43
      ns = n*incx                                                        icamax     44
      ii = 1                                                             icamax     45
      summax = abs(real(cx(1))) + abs(aimag(cx(1)))                      icamax     46
          do 20 i=1,ns,incx                                              icamax     47
          sumri = abs(real(cx(i))) + abs(aimag(cx(i)))                   icamax     48
          if(summax.ge.sumri) go to 10                                   icamax     49
          summax = sumri                                                 icamax     50
          icamax = ii                                                    icamax     51
   10     ii = ii + 1                                                    icamax     52
   20     continue                                                       icamax     53
      return                                                             icamax     54
      end                                                                icamax     55

c $Id$ 
