      function scasum(n,cx,incx)                                         scasum      2
c***begin prologue  scasum                                               scasum      3
c***revision date  811015   (yymmdd)                                     scasum      4
c***category no.  f1a                                                    scasum      5
c***keywords  blas,vector,complex,sum                                    scasum      6
c***date written  october 1979                                           scasum      7
c***author lawson c. (jpl),hanson r. (sla),                              scasum      8
c                            kincaid d. (u texas), krogh f. (jpl)        scasum      9
c***purpose                                                              scasum     10
c  sum of magnitudes of real and imaginary components of complex vector  scasum     11
c***description                                                          scasum     12
c                b l a s  subprogram                                     scasum     13
c    description of parameters                                           scasum     14
c                                                                        scasum     15
c     --input--                                                          scasum     16
c        n  number of elements in input vector(s)                        scasum     17
c       cx  complex vector with n elements                               scasum     18
c     incx  storage spacing between elements of cx                       scasum     19
c                                                                        scasum     20
c     --output--                                                         scasum     21
c   scasum  single precision result (zero if n.le.0)                     scasum     22
c                                                                        scasum     23
c     returns sums of magnitudes of real and imaginary parts of          scasum     24
c     components of cx.  note that this is not the l1 norm of cx.        scasum     25
c     casum = sum from 0 to n-1 of abs(real(cx(1+i*incx))) +             scasum     26
c             abs(imag(cx(1+i*incx)))                                    scasum     27
c                                                                        scasum     28
c                                                                        scasum     29
c***references                                                           scasum     30
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   scasum     31
c   *basic linear algebra subprograms for fortran usage*,                scasum     32
c  algorithm no. 539, transactions on mathematical software,             scasum     33
c  volume 5, number 3, september 1979, 308-323                           scasum     34
c***routines called  (none)                                              scasum     35
c***end prologue  scasum                                                 scasum     36
      complex cx(1)                                                      scasum     37
c***first executable statement  scasum                                   scasum     38
      scasum=0.                                                          scasum     39
      if(n .le. 0) return                                                scasum     40
      ns = n*incx                                                        scasum     41
          do 10 i=1,ns,incx                                              scasum     42
          scasum = scasum + abs(real(cx(i))) + abs(aimag(cx(i)))         scasum     43
   10     continue                                                       scasum     44
      return                                                             scasum     45
      end                                                                scasum     46

c $Id$ 
