      subroutine srotmg (sd1,sd2,sx1,sy1,sparam)                         srotmg      2
c***begin prologue  srotmg                                               srotmg      3
c***revision date  811015   (yymmdd)                                     srotmg      4
c***category no.  f1a                                                    srotmg      5
c***keywords  blas,vector,givens,rotation                                srotmg      6
c***date written  march 1978                                             srotmg      7
c***author lawson c., hanson r., kincaid d., krogh f.                    srotmg      8
c***purpose                                                              srotmg      9
c  construct s.p. modified givens transformation                         srotmg     10
c***description                                                          srotmg     11
c                b l a s  subprogram                                     srotmg     12
c    description of parameters                                           srotmg     13
c                                                                        srotmg     14
c     --input--                                                          srotmg     15
c      sd1  single precision scalar used to define a1 below              srotmg     16
c      sd2  single precision scalar used to define a2 below              srotmg     17
c      sb1  single precision scalar defining a1 below                    srotmg     18
c      sb2  single precision scalar defining a2 below                    srotmg     19
c   sparam  s.p. 5-vector. sparam(1)=sflag defined below.                srotmg     20
c           locations 2-5 contain the rotation matrix.                   srotmg     21
c                                                                        srotmg     22
c     --output--                                                         srotmg     23
c      sd1  changed to represent the effect of the transformation        srotmg     24
c     sd2  changed to represent the effect of the transformation         srotmg     25
c     sb1  changed to represent the effect of the transformation         srotmg     26
c     sb2  unchanged                                                     srotmg     27
c                                                                        srotmg     28
c     construct the modified givens transformation matrix h which zeros  srotmg     29
c     the second component of the 2-vector  (sqrt(sd1)*sx1,sqrt(sd2)*    srotmg     30
c     sy2)**t.                                                           srotmg     31
c     with sparam(1)=sflag, h has one of the following forms..           srotmg     32
c                                                                        srotmg     33
c     sflag=-1.e0     sflag=0.e0        sflag=1.e0     sflag=-2.e0       srotmg     34
c                                                                        srotmg     35
c       (sh11  sh12)    (1.e0  sh12)    (sh11  1.e0)    (1.e0  0.e0)     srotmg     36
c     h=(          )    (          )    (          )    (          )     srotmg     37
c       (sh21  sh22),   (sh21  1.e0),   (-1.e0 sh22),   (0.e0  1.e0).    srotmg     38
c     locations 2-5 of sparam contain sh11,sh21,sh12, and sh22           srotmg     39
c     respectively. (values of 1.e0, -1.e0, or 0.e0 implied by the       srotmg     40
c     value of sparam(1) are not stored in sparam.)                      srotmg     41
c                                                                        srotmg     42
c***references                                                           srotmg     43
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   srotmg     44
c   *basic linear algebra subprograms for fortran usage*,                srotmg     45
c  algorithm no. 539, transactions on mathematical software,             srotmg     46
c  volume 5, number 3, september 1979, 308-323                           srotmg     47
c***routines called  (none)                                              srotmg     48
c***end prologue  srotmg                                                 srotmg     49
c                                                                        srotmg     50
      dimension sparam(5)                                                srotmg     51
      data zero,one,two /0.e0,1.e0,2.e0/                                 srotmg     52
      data gam,gamsq,rgamsq/4096.e0,1.67772e7,5.96046e-8/                srotmg     53
c***first executable statement  srotmg                                   srotmg     54
      if(.not. sd1 .lt. zero) go to 10                                   srotmg     55
c       go zero-h-d-and-sx1..                                            srotmg     56
          go to 60                                                       srotmg     57
   10 continue                                                           srotmg     58
c     case-sd1-nonnegative                                               srotmg     59
      sp2=sd2*sy1                                                        srotmg     60
      if(.not. sp2 .eq. zero) go to 20                                   srotmg     61
          sflag=-two                                                     srotmg     62
          go to 260                                                      srotmg     63
c     regular-case..                                                     srotmg     64
   20 continue                                                           srotmg     65
      sp1=sd1*sx1                                                        srotmg     66
      sq2=sp2*sy1                                                        srotmg     67
      sq1=sp1*sx1                                                        srotmg     68
c                                                                        srotmg     69
      if(.not. abs(sq1) .gt. abs(sq2)) go to 40                          srotmg     70
          sh21=-sy1/sx1                                                  srotmg     71
          sh12=sp2/sp1                                                   srotmg     72
c                                                                        srotmg     73
          su=one-sh12*sh21                                               srotmg     74
c                                                                        srotmg     75
          if(.not. su .le. zero) go to 30                                srotmg     76
c         go zero-h-d-and-sx1..                                          srotmg     77
               go to 60                                                  srotmg     78
   30     continue                                                       srotmg     79
               sflag=zero                                                srotmg     80
               sd1=sd1/su                                                srotmg     81
               sd2=sd2/su                                                srotmg     82
               sx1=sx1*su                                                srotmg     83
c         go scale-check..                                               srotmg     84
               go to 100                                                 srotmg     85
   40 continue                                                           srotmg     86
          if(.not. sq2 .lt. zero) go to 50                               srotmg     87
c         go zero-h-d-and-sx1..                                          srotmg     88
               go to 60                                                  srotmg     89
   50     continue                                                       srotmg     90
               sflag=one                                                 srotmg     91
               sh11=sp1/sp2                                              srotmg     92
               sh22=sx1/sy1                                              srotmg     93
               su=one+sh11*sh22                                          srotmg     94
               stemp=sd2/su                                              srotmg     95
               sd2=sd1/su                                                srotmg     96
               sd1=stemp                                                 srotmg     97
               sx1=sy1*su                                                srotmg     98
c         go scale-check                                                 srotmg     99
               go to 100                                                 srotmg    100
c     procedure..zero-h-d-and-sx1..                                      srotmg    101
   60 continue                                                           srotmg    102
          sflag=-one                                                     srotmg    103
          sh11=zero                                                      srotmg    104
          sh12=zero                                                      srotmg    105
          sh21=zero                                                      srotmg    106
          sh22=zero                                                      srotmg    107
c                                                                        srotmg    108
          sd1=zero                                                       srotmg    109
          sd2=zero                                                       srotmg    110
          sx1=zero                                                       srotmg    111
c         return..                                                       srotmg    112
          go to 220                                                      srotmg    113
c     procedure..fix-h..                                                 srotmg    114
   70 continue                                                           srotmg    115
      if(.not. sflag .ge. zero) go to 90                                 srotmg    116
c                                                                        srotmg    117
          if(.not. sflag .eq. zero) go to 80                             srotmg    118
          sh11=one                                                       srotmg    119
          sh22=one                                                       srotmg    120
          sflag=-one                                                     srotmg    121
          go to 90                                                       srotmg    122
   80     continue                                                       srotmg    123
          sh21=-one                                                      srotmg    124
          sh12=one                                                       srotmg    125
          sflag=-one                                                     srotmg    126
   90 continue                                                           srotmg    127
      go to igo,(120,150,180,210)                                        srotmg    128
c     procedure..scale-check                                             srotmg    129
  100 continue                                                           srotmg    130
  110     continue                                                       srotmg    131
          if(.not. sd1 .le. rgamsq) go to 130                            srotmg    132
               if(sd1 .eq. zero) go to 160                               srotmg    133
               assign 120 to igo                                         srotmg    134
c              fix-h..                                                   srotmg    135
               go to 70                                                  srotmg    136
  120          continue                                                  srotmg    137
               sd1=sd1*gam**2                                            srotmg    138
               sx1=sx1/gam                                               srotmg    139
               sh11=sh11/gam                                             srotmg    140
               sh12=sh12/gam                                             srotmg    141
          go to 110                                                      srotmg    142
  130 continue                                                           srotmg    143
  140     continue                                                       srotmg    144
          if(.not. sd1 .ge. gamsq) go to 160                             srotmg    145
               assign 150 to igo                                         srotmg    146
c              fix-h..                                                   srotmg    147
               go to 70                                                  srotmg    148
  150          continue                                                  srotmg    149
               sd1=sd1/gam**2                                            srotmg    150
               sx1=sx1*gam                                               srotmg    151
               sh11=sh11*gam                                             srotmg    152
               sh12=sh12*gam                                             srotmg    153
          go to 140                                                      srotmg    154
  160 continue                                                           srotmg    155
  170     continue                                                       srotmg    156
          if(.not. abs(sd2) .le. rgamsq) go to 190                       srotmg    157
               if(sd2 .eq. zero) go to 220                               srotmg    158
               assign 180 to igo                                         srotmg    159
c              fix-h..                                                   srotmg    160
               go to 70                                                  srotmg    161
  180          continue                                                  srotmg    162
               sd2=sd2*gam**2                                            srotmg    163
               sh21=sh21/gam                                             srotmg    164
               sh22=sh22/gam                                             srotmg    165
          go to 170                                                      srotmg    166
  190 continue                                                           srotmg    167
  200     continue                                                       srotmg    168
          if(.not. abs(sd2) .ge. gamsq) go to 220                        srotmg    169
               assign 210 to igo                                         srotmg    170
c              fix-h..                                                   srotmg    171
               go to 70                                                  srotmg    172
  210          continue                                                  srotmg    173
               sd2=sd2/gam**2                                            srotmg    174
               sh21=sh21*gam                                             srotmg    175
               sh22=sh22*gam                                             srotmg    176
          go to 200                                                      srotmg    177
  220 continue                                                           srotmg    178
          if(sflag)250,230,240                                           srotmg    179
  230     continue                                                       srotmg    180
               sparam(3)=sh21                                            srotmg    181
               sparam(4)=sh12                                            srotmg    182
               go to 260                                                 srotmg    183
  240     continue                                                       srotmg    184
               sparam(2)=sh11                                            srotmg    185
               sparam(5)=sh22                                            srotmg    186
               go to 260                                                 srotmg    187
  250     continue                                                       srotmg    188
               sparam(2)=sh11                                            srotmg    189
               sparam(3)=sh21                                            srotmg    190
               sparam(4)=sh12                                            srotmg    191
               sparam(5)=sh22                                            srotmg    192
  260 continue                                                           srotmg    193
          sparam(1)=sflag                                                srotmg    194
          return                                                         srotmg    195
      end                                                                srotmg    196

c $Id$ 
