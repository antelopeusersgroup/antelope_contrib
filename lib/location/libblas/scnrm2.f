      real function scnrm2( n, cx, incx)                                 scnrm2      2
c***begin prologue  scnrm2                                               scnrm2      3
c***revision date  811015   (yymmdd)                                     scnrm2      4
c***category no.  f1a                                                    scnrm2      5
c***keywords  blas,vector,complex,unitary norm,norm                      scnrm2      6
c***date written  october 1979                                           scnrm2      7
c***author lawson c. (jpl),hanson r. (sla),                              scnrm2      8
c                            kincaid d. (u texas), krogh f. (jpl)        scnrm2      9
c***purpose                                                              scnrm2     10
c    unitary norm of complex vector                                      scnrm2     11
c***description                                                          scnrm2     12
c                b l a s  subprogram                                     scnrm2     13
c    description of parameters                                           scnrm2     14
c                                                                        scnrm2     15
c     --input--                                                          scnrm2     16
c        n  number of elements in input vector(s)                        scnrm2     17
c       cx  complex vector with n elements                               scnrm2     18
c     incx  storage spacing between elements of cx                       scnrm2     19
c                                                                        scnrm2     20
c     --output--                                                         scnrm2     21
c   scnrm2  single precision result (zero if n.le.0)                     scnrm2     22
c                                                                        scnrm2     23
c     unitary norm of the complex n-vector stored in cx() with storage   scnrm2     24
c     increment incx .                                                   scnrm2     25
c     if    n .le. 0 return with result = 0.                             scnrm2     26
c     if n .ge. 1 then incx must be .ge. 1                               scnrm2     27
c                                                                        scnrm2     28
c           c.l.lawson , 1978 jan 08                                     scnrm2     29
c                                                                        scnrm2     30
c     four phase method     using two built-in constants that are        scnrm2     31
c     hopefully applicable to all machines.                              scnrm2     32
c         cutlo = maximum of  sqrt(u/eps)  over all known machines.      scnrm2     33
c         cuthi = minimum of  sqrt(v)      over all known machines.      scnrm2     34
c     where                                                              scnrm2     35
c         eps = smallest no. such that eps + 1. .gt. 1.                  scnrm2     36
c         u   = smallest positive no.   (underflow limit)                scnrm2     37
c         v   = largest  no.            (overflow  limit)                scnrm2     38
c                                                                        scnrm2     39
c     brief outline of algorithm..                                       scnrm2     40
c                                                                        scnrm2     41
c     phase 1    scans zero components.                                  scnrm2     42
c     move to phase 2 when a component is nonzero and .le. cutlo         scnrm2     43
c     move to phase 3 when a component is .gt. cutlo                     scnrm2     44
c     move to phase 4 when a component is .ge. cuthi/m                   scnrm2     45
c     where m = n for x() real and m = 2*n for complex.                  scnrm2     46
c                                                                        scnrm2     47
c     values for cutlo and cuthi..                                       scnrm2     48
c     from the environmental parameters listed in the imsl converter     scnrm2     49
c     document the limiting values are as follows..                      scnrm2     50
c     cutlo, s.p.   u/eps = 2**(-102) for  honeywell.  close seconds are scnrm2     51
c                   univac and dec at 2**(-103)                          scnrm2     52
c                   thus cutlo = 2**(-51) = 4.44089e-16                  scnrm2     53
c     cuthi, s.p.   v = 2**127 for univac, honeywell, and dec.           scnrm2     54
c                   thus cuthi = 2**(63.5) = 1.30438e19                  scnrm2     55
c     cutlo, d.p.   u/eps = 2**(-67) for honeywell and dec.              scnrm2     56
c                   thus cutlo = 2**(-33.5) = 8.23181d-11                scnrm2     57
c     cuthi, d.p.   same as s.p.  cuthi = 1.30438d19                     scnrm2     58
c     data cutlo, cuthi / 8.232d-11,  1.304d19 /                         scnrm2     59
c     data cutlo, cuthi / 4.441e-16,  1.304e19 /                         scnrm2     60
c                                                                        scnrm2     61
c***references                                                           scnrm2     62
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   scnrm2     63
c   *basic linear algebra subprograms for fortran usage*,                scnrm2     64
c  algorithm no. 539, transactions on mathematical software,             scnrm2     65
c  volume 5, number 3, september 1979, 308-323                           scnrm2     66
c***routines called  (none)                                              scnrm2     67
c***end prologue  scnrm2                                                 scnrm2     68
      logical imag, scale                                                scnrm2     69
      integer          next                                              scnrm2     70
      real         cutlo, cuthi, hitest, sum, xmax, absx, zero, one      scnrm2     71
      complex      cx(1)                                                 scnrm2     72
      data         zero, one /0.0e0, 1.0e0/                              scnrm2     73
c                                                                        scnrm2     74
      data cutlo, cuthi / 4.441e-16,  1.304e19 /                         scnrm2     75
c***first executable statement  scnrm2                                   scnrm2     76
      if(n .gt. 0) go to 10                                              scnrm2     77
         scnrm2  = zero                                                  scnrm2     78
         go to 300                                                       scnrm2     79
c                                                                        scnrm2     80
   10 assign 30 to next                                                  scnrm2     81
      sum = zero                                                         scnrm2     82
      nn = n * incx                                                      scnrm2     83
c                                                 begin main loop        scnrm2     84
      do 210 i=1,nn,incx                                                 scnrm2     85
         absx = abs(real(cx(i)))                                         scnrm2     86
         imag = .false.                                                  scnrm2     87
         go to next,(30, 50, 70, 90, 110)                                scnrm2     88
   30 if( absx .gt. cutlo) go to 85                                      scnrm2     89
      assign 50 to next                                                  scnrm2     90
      scale = .false.                                                    scnrm2     91
c                                                                        scnrm2     92
c                        phase 1.  sum is zero                           scnrm2     93
c                                                                        scnrm2     94
   50 if( absx .eq. zero) go to 200                                      scnrm2     95
      if( absx .gt. cutlo) go to 85                                      scnrm2     96
c                                                                        scnrm2     97
c                                prepare for phase 2.                    scnrm2     98
      assign 70 to next                                                  scnrm2     99
      go to 105                                                          scnrm2    100
c                                                                        scnrm2    101
c                                prepare for phase 4.                    scnrm2    102
c                                                                        scnrm2    103
  100 assign 110 to next                                                 scnrm2    104
      sum = (sum / absx) / absx                                          scnrm2    105
  105 scale = .true.                                                     scnrm2    106
      xmax = absx                                                        scnrm2    107
      go to 115                                                          scnrm2    108
c                                                                        scnrm2    109
c                   phase 2.  sum is small.                              scnrm2    110
c                             scale to avoid destructive underflow.      scnrm2    111
c                                                                        scnrm2    112
   70 if( absx .gt. cutlo ) go to 75                                     scnrm2    113
c                                                                        scnrm2    114
c                     common code for phases 2 and 4.                    scnrm2    115
c                     in phase 4 sum is large.  scale to avoid overflow. scnrm2    116
c                                                                        scnrm2    117
  110 if( absx .le. xmax ) go to 115                                     scnrm2    118
         sum = one + sum * (xmax / absx)**2                              scnrm2    119
         xmax = absx                                                     scnrm2    120
         go to 200                                                       scnrm2    121
c                                                                        scnrm2    122
  115 sum = sum + (absx/xmax)**2                                         scnrm2    123
      go to 200                                                          scnrm2    124
c                                                                        scnrm2    125
c                                                                        scnrm2    126
c                  prepare for phase 3.                                  scnrm2    127
c                                                                        scnrm2    128
   75 sum = (sum * xmax) * xmax                                          scnrm2    129
c                                                                        scnrm2    130
   85 assign 90 to next                                                  scnrm2    131
      scale = .false.                                                    scnrm2    132
c                                                                        scnrm2    133
c     for real or d.p. set hitest = cuthi/n                              scnrm2    134
c     for complex      set hitest = cuthi/(2*n)                          scnrm2    135
c                                                                        scnrm2    136
      hitest = cuthi/float( n )                                          scnrm2    137
c                                                                        scnrm2    138
c                   phase 3.  sum is mid-range.  no scaling.             scnrm2    139
c                                                                        scnrm2    140
   90 if(absx .ge. hitest) go to 100                                     scnrm2    141
         sum = sum + absx**2                                             scnrm2    142
  200 continue                                                           scnrm2    143
c                  control selection of real and imaginary parts.        scnrm2    144
c                                                                        scnrm2    145
      if(imag) go to 210                                                 scnrm2    146
         absx = abs(aimag(cx(i)))                                        scnrm2    147
         imag = .true.                                                   scnrm2    148
      go to next,(  50, 70, 90, 110 )                                    scnrm2    149
c                                                                        scnrm2    150
  210 continue                                                           scnrm2    151
c                                                                        scnrm2    152
c              end of main loop.                                         scnrm2    153
c              compute square root and adjust for scaling.               scnrm2    154
c                                                                        scnrm2    155
      scnrm2 = sqrt(sum)                                                 scnrm2    156
      if(scale) scnrm2 = scnrm2 * xmax                                   scnrm2    157
  300 continue                                                           scnrm2    158
      return                                                             scnrm2    159
      end                                                                scnrm2    160

c $Id$ 
