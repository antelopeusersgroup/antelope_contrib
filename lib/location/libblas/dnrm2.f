      double precision function dnrm2 ( n, dx, incx)                     dnrm2       2
c***begin prologue  dnrm2                                                dnrm2       3
c***revision date  811015   (yymmdd)                                     dnrm2       4
c***category no.  f1a                                                    dnrm2       5
c***keywords  blas,vector,double precision,norm,length,l2 norm           dnrm2       6
c***date written  october 1979                                           dnrm2       7
c***author lawson c. (jpl),hanson r. (sla),                              dnrm2       8
c                            kincaid d. (u texas), krogh f. (jpl)        dnrm2       9
c***purpose                                                              dnrm2      10
c    euclidean length (l2 norm) of d.p. vector                           dnrm2      11
c***description                                                          dnrm2      12
c                b l a s  subprogram                                     dnrm2      13
c    description of parameters                                           dnrm2      14
c                                                                        dnrm2      15
c     --input--                                                          dnrm2      16
c        n  number of elements in input vector(s)                        dnrm2      17
c       dx  double precision vector with n elements                      dnrm2      18
c     incx  storage spacing between elements of dx                       dnrm2      19
c                                                                        dnrm2      20
c     --output--                                                         dnrm2      21
c    dnrm2  double precision result (zero if n.le.0)                     dnrm2      22
c                                                                        dnrm2      23
c     euclidean norm of the n-vector stored in dx() with storage         dnrm2      24
c     increment incx .                                                   dnrm2      25
c     if    n .le. 0 return with result = 0.                             dnrm2      26
c     if n .ge. 1 then incx must be .ge. 1                               dnrm2      27
c                                                                        dnrm2      28
c           c.l.lawson, 1978 jan 08                                      dnrm2      29
c                                                                        dnrm2      30
c     four phase method     using two built-in constants that are        dnrm2      31
c     hopefully applicable to all machines.                              dnrm2      32
c         cutlo = maximum of  dsqrt(u/eps)  over all known machines.     dnrm2      33
c         cuthi = minimum of  dsqrt(v)      over all known machines.     dnrm2      34
c     where                                                              dnrm2      35
c         eps = smallest no. such that eps + 1. .gt. 1.                  dnrm2      36
c         u   = smallest positive no.   (underflow limit)                dnrm2      37
c         v   = largest  no.            (overflow  limit)                dnrm2      38
c                                                                        dnrm2      39
c     brief outline of algorithm..                                       dnrm2      40
c                                                                        dnrm2      41
c     phase 1    scans zero components.                                  dnrm2      42
c     move to phase 2 when a component is nonzero and .le. cutlo         dnrm2      43
c     move to phase 3 when a component is .gt. cutlo                     dnrm2      44
c     move to phase 4 when a component is .ge. cuthi/m                   dnrm2      45
c     where m = n for x() real and m = 2*n for complex.                  dnrm2      46
c                                                                        dnrm2      47
c     values for cutlo and cuthi..                                       dnrm2      48
c     from the environmental parameters listed in the imsl converter     dnrm2      49
c     document the limiting values are as follows..                      dnrm2      50
c     cutlo, s.p.   u/eps = 2**(-102) for  honeywell.  close seconds are dnrm2      51
c                   univac and dec at 2**(-103)                          dnrm2      52
c                   thus cutlo = 2**(-51) = 4.44089e-16                  dnrm2      53
c     cuthi, s.p.   v = 2**127 for univac, honeywell, and dec.           dnrm2      54
c                   thus cuthi = 2**(63.5) = 1.30438e19                  dnrm2      55
c     cutlo, d.p.   u/eps = 2**(-67) for honeywell and dec.              dnrm2      56
c                   thus cutlo = 2**(-33.5) = 8.23181d-11                dnrm2      57
c     cuthi, d.p.   same as s.p.  cuthi = 1.30438d19                     dnrm2      58
c     data cutlo, cuthi / 8.232d-11,  1.304d19 /                         dnrm2      59
c     data cutlo, cuthi / 4.441e-16,  1.304e19 /                         dnrm2      60
c                                                                        dnrm2      61
c***references                                                           dnrm2      62
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   dnrm2      63
c   *basic linear algebra subprograms for fortran usage*,                dnrm2      64
c  algorithm no. 539, transactions on mathematical software,             dnrm2      65
c  volume 5, number 3, september 1979, 308-323                           dnrm2      66
c***routines called  (none)                                              dnrm2      67
c***end prologue  dnrm2                                                  dnrm2      68
      integer          next                                              dnrm2      69
      double precision   dx(1), cutlo, cuthi, hitest, sum, xmax,zero,one dnrm2      70
      data   zero, one /0.0d0, 1.0d0/                                    dnrm2      71
c                                                                        dnrm2      72
      data cutlo, cuthi / 8.232d-11,  1.304d19 /                         dnrm2      73
c***first executable statement  dnrm2                                    dnrm2      74
      if(n .gt. 0) go to 10                                              dnrm2      75
         dnrm2  = zero                                                   dnrm2      76
         go to 300                                                       dnrm2      77
c                                                                        dnrm2      78
   10 assign 30 to next                                                  dnrm2      79
      sum = zero                                                         dnrm2      80
      nn = n * incx                                                      dnrm2      81
c                                                 begin main loop        dnrm2      82
      i = 1                                                              dnrm2      83
   20    go to next,(30, 50, 70, 110)                                    dnrm2      84
   30 if( dabs(dx(i)) .gt. cutlo) go to 85                               dnrm2      85
      assign 50 to next                                                  dnrm2      86
      xmax = zero                                                        dnrm2      87
c                                                                        dnrm2      88
c                        phase 1.  sum is zero                           dnrm2      89
c                                                                        dnrm2      90
   50 if( dx(i) .eq. zero) go to 200                                     dnrm2      91
      if( dabs(dx(i)) .gt. cutlo) go to 85                               dnrm2      92
c                                                                        dnrm2      93
c                                prepare for phase 2.                    dnrm2      94
      assign 70 to next                                                  dnrm2      95
      go to 105                                                          dnrm2      96
c                                                                        dnrm2      97
c                                prepare for phase 4.                    dnrm2      98
c                                                                        dnrm2      99
  100 i = j                                                              dnrm2     100
      assign 110 to next                                                 dnrm2     101
      sum = (sum / dx(i)) / dx(i)                                        dnrm2     102
  105 xmax = dabs(dx(i))                                                 dnrm2     103
      go to 115                                                          dnrm2     104
c                                                                        dnrm2     105
c                   phase 2.  sum is small.                              dnrm2     106
c                             scale to avoid destructive underflow.      dnrm2     107
c                                                                        dnrm2     108
   70 if( dabs(dx(i)) .gt. cutlo ) go to 75                              dnrm2     109
c                                                                        dnrm2     110
c                     common code for phases 2 and 4.                    dnrm2     111
c                     in phase 4 sum is large.  scale to avoid overflow. dnrm2     112
c                                                                        dnrm2     113
  110 if( dabs(dx(i)) .le. xmax ) go to 115                              dnrm2     114
         sum = one + sum * (xmax / dx(i))**2                             dnrm2     115
         xmax = dabs(dx(i))                                              dnrm2     116
         go to 200                                                       dnrm2     117
c                                                                        dnrm2     118
  115 sum = sum + (dx(i)/xmax)**2                                        dnrm2     119
      go to 200                                                          dnrm2     120
c                                                                        dnrm2     121
c                                                                        dnrm2     122
c                  prepare for phase 3.                                  dnrm2     123
c                                                                        dnrm2     124
   75 sum = (sum * xmax) * xmax                                          dnrm2     125
c                                                                        dnrm2     126
c                                                                        dnrm2     127
c     for real or d.p. set hitest = cuthi/n                              dnrm2     128
c     for complex      set hitest = cuthi/(2*n)                          dnrm2     129
c                                                                        dnrm2     130
   85 hitest = cuthi/float( n )                                          dnrm2     131
c                                                                        dnrm2     132
c                   phase 3.  sum is mid-range.  no scaling.             dnrm2     133
c                                                                        dnrm2     134
      do 95 j =i,nn,incx                                                 dnrm2     135
      if(dabs(dx(j)) .ge. hitest) go to 100                              dnrm2     136
   95    sum = sum + dx(j)**2                                            dnrm2     137
      dnrm2 = dsqrt( sum )                                               dnrm2     138
      go to 300                                                          dnrm2     139
c                                                                        dnrm2     140
  200 continue                                                           dnrm2     141
      i = i + incx                                                       dnrm2     142
      if ( i .le. nn ) go to 20                                          dnrm2     143
c                                                                        dnrm2     144
c              end of main loop.                                         dnrm2     145
c                                                                        dnrm2     146
c              compute square root and adjust for scaling.               dnrm2     147
c                                                                        dnrm2     148
      dnrm2 = xmax * dsqrt(sum)                                          dnrm2     149
  300 continue                                                           dnrm2     150
      return                                                             dnrm2     151
      end                                                                dnrm2     152

c $Id$ 
