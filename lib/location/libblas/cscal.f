      subroutine cscal(n,ca,cx,incx)                                     cscal       2
c***begin prologue  cscal                                                cscal       3
c***revision date  811015   (yymmdd)                                     cscal       4
c***category no.  f1a, m2                                                cscal       5
c***keywords  complex,blas,vector,scale                                  cscal       6
c***date written  october 1979                                           cscal       7
c***author lawson c. (jpl),hanson r. (sla),                              cscal       8
c                            kincaid d. (u texas), krogh f. (jpl)        cscal       9
c***purpose                                                              cscal      10
c    complex vector scale x = a*x                                        cscal      11
c***description                                                          cscal      12
c                b l a s  subprogram                                     cscal      13
c    description of parameters                                           cscal      14
c                                                                        cscal      15
c     --input--                                                          cscal      16
c        n  number of elements in input vector(s)                        cscal      17
c       ca  complex scale factor                                         cscal      18
c       cx  complex vector with n elements                               cscal      19
c     incx  storage spacing between elements of cx                       cscal      20
c                                                                        cscal      21
c     --output--                                                         cscal      22
c    cscal  complex result (unchanged if n.le.0)                         cscal      23
c                                                                        cscal      24
c     replace complex cx by complex ca*cx.                               cscal      25
c     for i = 0 to n-1, replace cx(1+i*incx) with  ca * cx(1+i*incx)     cscal      26
c                                                                        cscal      27
c                                                                        cscal      28
c***references                                                           cscal      29
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   cscal      30
c   *basic linear algebra subprograms for fortran usage*,                cscal      31
c  algorithm no. 539, transactions on mathematical software,             cscal      32
c  volume 5, number 3, september 1979, 308-323                           cscal      33
c***routines called  (none)                                              cscal      34
c***end prologue  cscal                                                  cscal      35
c                                                                        cscal      36
      complex ca,cx(1)                                                   cscal      37
c***first executable statement  cscal                                    cscal      38
      if(n .le. 0) return                                                cscal      39
      ns = n*incx                                                        cscal      40
          do 10 i = 1,ns,incx                                            cscal      41
          cx(i) = ca*cx(i)                                               cscal      42
   10     continue                                                       cscal      43
      return                                                             cscal      44
      end                                                                cscal      45

c $Id$ 
