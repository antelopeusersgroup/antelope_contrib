      subroutine dscal(n,da,dx,incx)                                     dscal       2
c***begin prologue  dscal                                                dscal       3
c***revision date  811015   (yymmdd)                                     dscal       4
c***category no.  f1a, m2                                                dscal       5
c***keywords                                                             dscal       6
c***date written  october 1979                                           dscal       7
c***author lawson c. (jpl),hanson r. (sla),                              dscal       8
c                            kincaid d. (u texas), krogh f. (jpl)        dscal       9
c***purpose                                                              dscal      10
c    d.p. vector scale x = a*x                                           dscal      11
c***description                                                          dscal      12
c                b l a s  subprogram                                     dscal      13
c    description of parameters                                           dscal      14
c                                                                        dscal      15
c     --input--                                                          dscal      16
c        n  number of elements in input vector(s)                        dscal      17
c       da  double precision scale factor                                dscal      18
c       dx  double precision vector with n elements                      dscal      19
c     incx  storage spacing between elements of dx                       dscal      20
c                                                                        dscal      21
c     --output--                                                         dscal      22
c       dx  double precision result (unchanged if n.le.0)                dscal      23
c                                                                        dscal      24
c     replace double precision dx by double precision da*dx.             dscal      25
c     for i = 0 to n-1, replace dx(1+i*incx) with  da * dx(1+i*incx)     dscal      26
c                                                                        dscal      27
c***references                                                           dscal      28
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   dscal      29
c   *basic linear algebra subprograms for fortran usage*,                dscal      30
c  algorithm no. 539, transactions on mathematical software,             dscal      31
c  volume 5, number 3, september 1979, 308-323                           dscal      32
c***routines called  (none)                                              dscal      33
c***end prologue  dscal                                                  dscal      34
c                                                                        dscal      35
      double precision da,dx(1)                                          dscal      36
c***first executable statement  dscal                                    dscal      37
      if(n.le.0)return                                                   dscal      38
      if(incx.eq.1)goto 20                                               dscal      39
c                                                                        dscal      40
c        code for increments not equal to 1.                             dscal      41
c                                                                        dscal      42
      ns = n*incx                                                        dscal      43
          do 10 i = 1,ns,incx                                            dscal      44
          dx(i) = da*dx(i)                                               dscal      45
   10     continue                                                       dscal      46
      return                                                             dscal      47
c                                                                        dscal      48
c        code for increments equal to 1.                                 dscal      49
c                                                                        dscal      50
c                                                                        dscal      51
c        clean-up loop so remaining vector length is a multiple of 5.    dscal      52
c                                                                        dscal      53
   20 m = mod(n,5)                                                       dscal      54
      if( m .eq. 0 ) go to 40                                            dscal      55
      do 30 i = 1,m                                                      dscal      56
        dx(i) = da*dx(i)                                                 dscal      57
   30 continue                                                           dscal      58
      if( n .lt. 5 ) return                                              dscal      59
   40 mp1 = m + 1                                                        dscal      60
      do 50 i = mp1,n,5                                                  dscal      61
        dx(i) = da*dx(i)                                                 dscal      62
        dx(i + 1) = da*dx(i + 1)                                         dscal      63
        dx(i + 2) = da*dx(i + 2)                                         dscal      64
        dx(i + 3) = da*dx(i + 3)                                         dscal      65
        dx(i + 4) = da*dx(i + 4)                                         dscal      66
   50 continue                                                           dscal      67
      return                                                             dscal      68
      end                                                                dscal      69

c $Id$ 
