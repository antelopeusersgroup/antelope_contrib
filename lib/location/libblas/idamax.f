      integer function idamax(n,dx,incx)                                 idamax      2
c***begin prologue  idamax                                               idamax      3
c***revision date  811015   (yymmdd)                                     idamax      4
c***category no.  f1a                                                    idamax      5
c***keywords  blas,vector,double precision,largest component             idamax      6
c***date written  october 1979                                           idamax      7
c***author lawson c. (jpl),hanson r. (sla),                              idamax      8
c                            kincaid d. (u texas), krogh f. (jpl)        idamax      9
c***purpose                                                              idamax     10
c     find largest component of d.p. vector                              idamax     11
c***description                                                          idamax     12
c                b l a s  subprogram                                     idamax     13
c    description of parameters                                           idamax     14
c                                                                        idamax     15
c     --input--                                                          idamax     16
c        n  number of elements in input vector(s)                        idamax     17
c       dx  double precision vector with n elements                      idamax     18
c     incx  storage spacing between elements of dx                       idamax     19
c                                                                        idamax     20
c     --output--                                                         idamax     21
c   idamax  smallest index (zero if n.le.0)                              idamax     22
c                                                                        idamax     23
c     find smallest index of maximum magnitude of double precision dx.   idamax     24
c     idamax =  first i, i = 1 to n, to minimize  abs(dx(1-incx+i*incx)  idamax     25
c                                                                        idamax     26
c***references                                                           idamax     27
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   idamax     28
c   *basic linear algebra subprograms for fortran usage*,                idamax     29
c  algorithm no. 539, transactions on mathematical software,             idamax     30
c  volume 5, number 3, september 1979, 308-323                           idamax     31
c***routines called  (none)                                              idamax     32
c***end prologue  idamax                                                 idamax     33
c                                                                        idamax     34
      double precision dx(1),dmax,xmag                                   idamax     35
c***first executable statement  idamax                                   idamax     36
      idamax = 0                                                         idamax     37
      if(n.le.0) return                                                  idamax     38
      idamax = 1                                                         idamax     39
      if(n.le.1)return                                                   idamax     40
      if(incx.eq.1)goto 20                                               idamax     41
c                                                                        idamax     42
c        code for increments not equal to 1.                             idamax     43
c                                                                        idamax     44
      dmax = dabs(dx(1))                                                 idamax     45
      ns = n*incx                                                        idamax     46
      ii = 1                                                             idamax     47
          do 10 i = 1,ns,incx                                            idamax     48
          xmag = dabs(dx(i))                                             idamax     49
          if(xmag.le.dmax) go to 5                                       idamax     50
          idamax = ii                                                    idamax     51
          dmax = xmag                                                    idamax     52
    5     ii = ii + 1                                                    idamax     53
   10     continue                                                       idamax     54
      return                                                             idamax     55
c                                                                        idamax     56
c        code for increments equal to 1.                                 idamax     57
c                                                                        idamax     58
   20 dmax = dabs(dx(1))                                                 idamax     59
      do 30 i = 2,n                                                      idamax     60
          xmag = dabs(dx(i))                                             idamax     61
          if(xmag.le.dmax) go to 30                                      idamax     62
          idamax = i                                                     idamax     63
          dmax = xmag                                                    idamax     64
   30 continue                                                           idamax     65
      return                                                             idamax     66
      end                                                                idamax     67

c $Id$ 
