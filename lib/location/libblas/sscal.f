      subroutine  sscal(n,sa,sx,incx)                                    sscal       2
c***begin prologue  sscal                                                sscal       3
c***revision date  811015   (yymmdd)                                     sscal       4
c***category no.  f1a, m2                                                sscal       5
c***keywords  blas,vector,scale                                          sscal       6
c***date written  october 1979                                           sscal       7
c***author lawson c. (jpl),hanson r. (sla),                              sscal       8
c                            kincaid d. (u texas), krogh f. (jpl)        sscal       9
c***purpose                                                              sscal      10
c   s.p. vector scale x = a*x                                            sscal      11
c***description                                                          sscal      12
c                b l a s  subprogram                                     sscal      13
c    description of parameters                                           sscal      14
c                                                                        sscal      15
c     --input--                                                          sscal      16
c        n  number of elements in input vector(s)                        sscal      17
c       sa  single precision scale factor                                sscal      18
c       sx  single precision vector with n elements                      sscal      19
c     incx  storage spacing between elements of sx                       sscal      20
c                                                                        sscal      21
c     --output--                                                         sscal      22
c       sx  single precision result (unchanged if n.le.0)                sscal      23
c                                                                        sscal      24
c     replace single precision sx by single precision sa*sx.             sscal      25
c     for i = 0 to n-1, replace sx(1+i*incx) with  sa * sx(1+i*incx)     sscal      26
c                                                                        sscal      27
c***references                                                           sscal      28
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   sscal      29
c   *basic linear algebra subprograms for fortran usage*,                sscal      30
c  algorithm no. 539, transactions on mathematical software,             sscal      31
c  volume 5, number 3, september 1979, 308-323                           sscal      32
c***routines called  (none)                                              sscal      33
c***end prologue  sscal                                                  sscal      34
c                                                                        sscal      35
      real sa,sx(1)                                                      sscal      36
c***first executable statement  sscal                                    sscal      37
      if(n.le.0)return                                                   sscal      38
      if(incx.eq.1)goto 20                                               sscal      39
c                                                                        sscal      40
c        code for increments not equal to 1.                             sscal      41
c                                                                        sscal      42
      ns = n*incx                                                        sscal      43
          do 10 i = 1,ns,incx                                            sscal      44
          sx(i) = sa*sx(i)                                               sscal      45
   10     continue                                                       sscal      46
      return                                                             sscal      47
c                                                                        sscal      48
c        code for increments equal to 1.                                 sscal      49
c                                                                        sscal      50
c                                                                        sscal      51
c        clean-up loop so remaining vector length is a multiple of 5.    sscal      52
c                                                                        sscal      53
   20 m = mod(n,5)                                                       sscal      54
      if( m .eq. 0 ) go to 40                                            sscal      55
      do 30 i = 1,m                                                      sscal      56
        sx(i) = sa*sx(i)                                                 sscal      57
   30 continue                                                           sscal      58
      if( n .lt. 5 ) return                                              sscal      59
   40 mp1 = m + 1                                                        sscal      60
      do 50 i = mp1,n,5                                                  sscal      61
        sx(i) = sa*sx(i)                                                 sscal      62
        sx(i + 1) = sa*sx(i + 1)                                         sscal      63
        sx(i + 2) = sa*sx(i + 2)                                         sscal      64
        sx(i + 3) = sa*sx(i + 3)                                         sscal      65
        sx(i + 4) = sa*sx(i + 4)                                         sscal      66
   50 continue                                                           sscal      67
      return                                                             sscal      68
      end                                                                sscal      69

c $Id$ 
