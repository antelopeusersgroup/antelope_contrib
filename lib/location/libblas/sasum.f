      real function sasum(n,sx,incx)                                     sasum       2
c***begin prologue  sasum                                                sasum       3
c***revision date  811015   (yymmdd)                                     sasum       4
c***category no.  f1a                                                    sasum       5
c***keywords  blas,vector,sum                                            sasum       6
c***date written  october 1979                                           sasum       7
c***author lawson c. (jpl),hanson r. (sla),                              sasum       8
c                            kincaid d. (u texas), krogh f. (jpl)        sasum       9
c***purpose                                                              sasum      10
c    sum of magnitudes of s.p vector components                          sasum      11
c***description                                                          sasum      12
c                b l a s  subprogram                                     sasum      13
c    description of parameters                                           sasum      14
c                                                                        sasum      15
c     --input--                                                          sasum      16
c        n  number of elements in input vector(s)                        sasum      17
c       sx  single precision vector with n elements                      sasum      18
c     incx  storage spacing between elements of sx                       sasum      19
c                                                                        sasum      20
c     --output--                                                         sasum      21
c    sasum  single precision result (zero if n.le.0)                     sasum      22
c                                                                        sasum      23
c     returns sum of magnitudes of single precision sx.                  sasum      24
c     sasum = sum from 0 to n-1 of  abs(sx(1+i*incx))                    sasum      25
c                                                                        sasum      26
c***references                                                           sasum      27
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   sasum      28
c   *basic linear algebra subprograms for fortran usage*,                sasum      29
c  algorithm no. 539, transactions on mathematical software,             sasum      30
c  volume 5, number 3, september 1979, 308-323                           sasum      31
c***routines called  (none)                                              sasum      32
c***end prologue  sasum                                                  sasum      33
c                                                                        sasum      34
      real sx(1)                                                         sasum      35
c***first executable statement  sasum                                    sasum      36
      sasum = 0.0e0                                                      sasum      37
      if(n.le.0)return                                                   sasum      38
      if(incx.eq.1)goto 20                                               sasum      39
c                                                                        sasum      40
c        code for increments not equal to 1.                             sasum      41
c                                                                        sasum      42
      ns = n*incx                                                        sasum      43
          do 10 i=1,ns,incx                                              sasum      44
          sasum = sasum + abs(sx(i))                                     sasum      45
   10     continue                                                       sasum      46
      return                                                             sasum      47
c                                                                        sasum      48
c        code for increments equal to 1.                                 sasum      49
c                                                                        sasum      50
c                                                                        sasum      51
c        clean-up loop so remaining vector length is a multiple of 6.    sasum      52
c                                                                        sasum      53
   20 m = mod(n,6)                                                       sasum      54
      if( m .eq. 0 ) go to 40                                            sasum      55
      do 30 i = 1,m                                                      sasum      56
        sasum = sasum + abs(sx(i))                                       sasum      57
   30 continue                                                           sasum      58
      if( n .lt. 6 ) return                                              sasum      59
   40 mp1 = m + 1                                                        sasum      60
      do 50 i = mp1,n,6                                                  sasum      61
        sasum = sasum + abs(sx(i)) + abs(sx(i + 1)) + abs(sx(i + 2))     sasum      62
     $  + abs(sx(i + 3)) + abs(sx(i + 4)) + abs(sx(i + 5))               sasum      63
   50 continue                                                           sasum      64
      return                                                             sasum      65
      end                                                                sasum      66

c $Id$ 
