      real function snrm2 ( n, sx, incx)                                 snrm2       2
c***begin prologue  snrm2                                                snrm2       3
c***revision date  811015   (yymmdd)                                     snrm2       4
c***category no.  f1a                                                    snrm2       5
c***keywords  blas,vector,length,norm,l2 norm                            snrm2       6
c***date written  october 1979                                           snrm2       7
c***author lawson c. (jpl),hanson r. (sla),                              snrm2       8
c                            kincaid d. (u texas), krogh f. (jpl)        snrm2       9
c***purpose                                                              snrm2      10
c    euclidean length (l2 norm) of s.p. vector                           snrm2      11
c***description                                                          snrm2      12
c                b l a s  subprogram                                     snrm2      13
c    description of parameters                                           snrm2      14
c                                                                        snrm2      15
c     --input--                                                          snrm2      16
c        n  number of elements in input vector(s)                        snrm2      17
c       sx  single precision vector with n elements                      snrm2      18
c     incx  storage spacing between elements of sx                       snrm2      19
c                                                                        snrm2      20
c     --output--                                                         snrm2      21
c    snrm2  single precision result (zero if n.le.0)                     snrm2      22
c                                                                        snrm2      23
c     euclidean norm of the n-vector stored in sx() with storage         snrm2      24
c     increment incx .                                                   snrm2      25
c     if    n .le. 0 return with result = 0.                             snrm2      26
c     if n .ge. 1 then incx must be .ge. 1                               snrm2      27
c                                                                        snrm2      28
c           c.l.lawson, 1978 jan 08                                      snrm2      29
c                                                                        snrm2      30
c     four phase method     using two built-in constants that are        snrm2      31
c     hopefully applicable to all machines.                              snrm2      32
c         cutlo = maximum of  sqrt(u/eps)  over all known machines.      snrm2      33
c         cuthi = minimum of  sqrt(v)      over all known machines.      snrm2      34
c     where                                                              snrm2      35
c         eps = smallest no. such that eps + 1. .gt. 1.                  snrm2      36
c         u   = smallest positive no.   (underflow limit)                snrm2      37
c         v   = largest  no.            (overflow  limit)                snrm2      38
c                                                                        snrm2      39
c     brief outline of algorithm..                                       snrm2      40
c                                                                        snrm2      41
c     phase 1    scans zero components.                                  snrm2      42
c     move to phase 2 when a component is nonzero and .le. cutlo         snrm2      43
c     move to phase 3 when a component is .gt. cutlo                     snrm2      44
c     move to phase 4 when a component is .ge. cuthi/m                   snrm2      45
c     where m = n for x() real and m = 2*n for complex.                  snrm2      46
c                                                                        snrm2      47
c     values for cutlo and cuthi..                                       snrm2      48
c     from the environmental parameters listed in the imsl converter     snrm2      49
c     document the limiting values are as follows..                      snrm2      50
c     cutlo, s.p.   u/eps = 2**(-102) for  honeywell.  close seconds are snrm2      51
c                   univac and dec at 2**(-103)                          snrm2      52
c                   thus cutlo = 2**(-51) = 4.44089e-16                  snrm2      53
c     cuthi, s.p.   v = 2**127 for univac, honeywell, and dec.           snrm2      54
c                   thus cuthi = 2**(63.5) = 1.30438e19                  snrm2      55
c     cutlo, d.p.   u/eps = 2**(-67) for honeywell and dec.              snrm2      56
c                   thus cutlo = 2**(-33.5) = 8.23181d-11                snrm2      57
c     cuthi, d.p.   same as s.p.  cuthi = 1.30438d19                     snrm2      58
c     data cutlo, cuthi / 8.232d-11,  1.304d19 /                         snrm2      59
c     data cutlo, cuthi / 4.441e-16,  1.304e19 /                         snrm2      60
c                                                                        snrm2      61
c***references                                                           snrm2      62
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   snrm2      63
c   *basic linear algebra subprograms for fortran usage*,                snrm2      64
c  algorithm no. 539, transactions on mathematical software,             snrm2      65
c  volume 5, number 3, september 1979, 308-323                           snrm2      66
c***routines called  (none)                                              snrm2      67
c***end prologue  snrm2                                                  snrm2      68
      integer          next                                              snrm2      69
      real   sx(1),  cutlo, cuthi, hitest, sum, xmax, zero, one          snrm2      70
      data   zero, one /0.0e0, 1.0e0/                                    snrm2      71
c                                                                        snrm2      72
      data cutlo, cuthi / 4.441e-16,  1.304e19 /                         snrm2      73
c***first executable statement  snrm2                                    snrm2      74
      if(n .gt. 0) go to 10                                              snrm2      75
         snrm2  = zero                                                   snrm2      76
         go to 300                                                       snrm2      77
c                                                                        snrm2      78
   10 assign 30 to next                                                  snrm2      79
      sum = zero                                                         snrm2      80
      nn = n * incx                                                      snrm2      81
c                                                 begin main loop        snrm2      82
      i = 1                                                              snrm2      83
   20    go to next,(30, 50, 70, 110)                                    snrm2      84
   30 if( abs(sx(i)) .gt. cutlo) go to 85                                snrm2      85
      assign 50 to next                                                  snrm2      86
      xmax = zero                                                        snrm2      87
c                                                                        snrm2      88
c                        phase 1.  sum is zero                           snrm2      89
c                                                                        snrm2      90
   50 if( sx(i) .eq. zero) go to 200                                     snrm2      91
      if( abs(sx(i)) .gt. cutlo) go to 85                                snrm2      92
c                                                                        snrm2      93
c                                prepare for phase 2.                    snrm2      94
      assign 70 to next                                                  snrm2      95
      go to 105                                                          snrm2      96
c                                                                        snrm2      97
c                                prepare for phase 4.                    snrm2      98
c                                                                        snrm2      99
  100 i = j                                                              snrm2     100
      assign 110 to next                                                 snrm2     101
      sum = (sum / sx(i)) / sx(i)                                        snrm2     102
  105 xmax = abs(sx(i))                                                  snrm2     103
      go to 115                                                          snrm2     104
c                                                                        snrm2     105
c                   phase 2.  sum is small.                              snrm2     106
c                             scale to avoid destructive underflow.      snrm2     107
c                                                                        snrm2     108
   70 if( abs(sx(i)) .gt. cutlo ) go to 75                               snrm2     109
c                                                                        snrm2     110
c                     common code for phases 2 and 4.                    snrm2     111
c                     in phase 4 sum is large.  scale to avoid overflow. snrm2     112
c                                                                        snrm2     113
  110 if( abs(sx(i)) .le. xmax ) go to 115                               snrm2     114
         sum = one + sum * (xmax / sx(i))**2                             snrm2     115
         xmax = abs(sx(i))                                               snrm2     116
         go to 200                                                       snrm2     117
c                                                                        snrm2     118
  115 sum = sum + (sx(i)/xmax)**2                                        snrm2     119
      go to 200                                                          snrm2     120
c                                                                        snrm2     121
c                                                                        snrm2     122
c                  prepare for phase 3.                                  snrm2     123
c                                                                        snrm2     124
   75 sum = (sum * xmax) * xmax                                          snrm2     125
c                                                                        snrm2     126
c                                                                        snrm2     127
c     for real or d.p. set hitest = cuthi/n                              snrm2     128
c     for complex      set hitest = cuthi/(2*n)                          snrm2     129
c                                                                        snrm2     130
   85 hitest = cuthi/float( n )                                          snrm2     131
c                                                                        snrm2     132
c                   phase 3.  sum is mid-range.  no scaling.             snrm2     133
c                                                                        snrm2     134
      do 95 j =i,nn,incx                                                 snrm2     135
      if(abs(sx(j)) .ge. hitest) go to 100                               snrm2     136
   95    sum = sum + sx(j)**2                                            snrm2     137
      snrm2 = sqrt( sum )                                                snrm2     138
      go to 300                                                          snrm2     139
c                                                                        snrm2     140
  200 continue                                                           snrm2     141
      i = i + incx                                                       snrm2     142
      if ( i .le. nn ) go to 20                                          snrm2     143
c                                                                        snrm2     144
c              end of main loop.                                         snrm2     145
c                                                                        snrm2     146
c              compute square root and adjust for scaling.               snrm2     147
c                                                                        snrm2     148
      snrm2 = xmax * sqrt(sum)                                           snrm2     149
  300 continue                                                           snrm2     150
      return                                                             snrm2     151
      end                                                                snrm2     152

c $Id$ 
