      double precision function dasum(n,dx,incx)                         dasum       2
c***begin prologue  dasum                                                dasum       3
c***revision date  811015   (yymmdd)                                     dasum       4
c***category no.  f1a                                                    dasum       5
c***keywords  blas,vector,double precision,sum                           dasum       6
c***date written  october 1979                                           dasum       7
c***author lawson c. (jpl),hanson r. (sla),                              dasum       8
c                            kincaid d. (u texas), krogh f. (jpl)        dasum       9
c***purpose                                                              dasum      10
c    sum of magnitudes of d.p. vector components                         dasum      11
c***description                                                          dasum      12
c                b l a s  subprogram                                     dasum      13
c    description of parameters                                           dasum      14
c                                                                        dasum      15
c     --input--                                                          dasum      16
c        n  number of elements in input vector(s)                        dasum      17
c       dx  double precision vector with n elements                      dasum      18
c     incx  storage spacing between elements of dx                       dasum      19
c                                                                        dasum      20
c     --output--                                                         dasum      21
c    dasum  double precision result (zero if n.le.0)                     dasum      22
c                                                                        dasum      23
c     returns sum of magnitudes of double precision dx.                  dasum      24
c     dasum = sum from 0 to n-1 of dabs(dx(1+i*incx))                    dasum      25
c                                                                        dasum      26
c***references                                                           dasum      27
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   dasum      28
c   *basic linear algebra subprograms for fortran usage*,                dasum      29
c  algorithm no. 539, transactions on mathematical software,             dasum      30
c  volume 5, number 3, september 1979, 308-323                           dasum      31
c***routines called  (none)                                              dasum      32
c***end prologue  dasum                                                  dasum      33
c                                                                        dasum      34
      double precision dx(1)                                             dasum      35
c***first executable statement  dasum                                    dasum      36
      dasum = 0.d0                                                       dasum      37
      if(n.le.0)return                                                   dasum      38
      if(incx.eq.1)goto 20                                               dasum      39
c                                                                        dasum      40
c        code for increments not equal to 1.                             dasum      41
c                                                                        dasum      42
      ns = n*incx                                                        dasum      43
          do 10 i=1,ns,incx                                              dasum      44
          dasum = dasum + dabs(dx(i))                                    dasum      45
   10     continue                                                       dasum      46
      return                                                             dasum      47
c                                                                        dasum      48
c        code for increments equal to 1.                                 dasum      49
c                                                                        dasum      50
c                                                                        dasum      51
c        clean-up loop so remaining vector length is a multiple of 6.    dasum      52
c                                                                        dasum      53
   20 m = mod(n,6)                                                       dasum      54
      if( m .eq. 0 ) go to 40                                            dasum      55
      do 30 i = 1,m                                                      dasum      56
         dasum = dasum + dabs(dx(i))                                     dasum      57
   30 continue                                                           dasum      58
      if( n .lt. 6 ) return                                              dasum      59
   40 mp1 = m + 1                                                        dasum      60
      do 50 i = mp1,n,6                                                  dasum      61
         dasum = dasum + dabs(dx(i)) + dabs(dx(i+1)) + dabs(dx(i+2))     dasum      62
     $   + dabs(dx(i+3)) + dabs(dx(i+4)) + dabs(dx(i+5))                 dasum      63
   50 continue                                                           dasum      64
      return                                                             dasum      65
      end                                                                dasum      66

c $Id$ 
