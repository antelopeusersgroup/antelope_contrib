      subroutine drotmg (dd1,dd2,dx1,dy1,dparam)                         drotmg      2
c***begin prologue  drotmg                                               drotmg      3
c***revision date  811015   (yymmdd)                                     drotmg      4
c***category no.  f1a                                                    drotmg      5
c***keywords  blas,vector,double precision,rotation,givens               drotmg      6
c***date written  march 1978                                             drotmg      7
c***author lawson c., hanson r., kincaid d., krogh f.                    drotmg      8
c***purpose                                                              drotmg      9
c    construct d.p. modified givens transformation                       drotmg     10
c***description                                                          drotmg     11
c                b l a s  subprogram                                     drotmg     12
c    description of parameters                                           drotmg     13
c                                                                        drotmg     14
c     --input--                                                          drotmg     15
c      dd1  double precision scalar                                      drotmg     16
c      dd2  double precision scalar                                      drotmg     17
c      dx1  double precision scalar                                      drotmg     18
c      dx2  double precision scalar                                      drotmg     19
c   dparam  d.p. 5-vector. dparam(1)=dflag defined below.                drotmg     20
c             elements 2-5  define the transformation matrix h.          drotmg     21
c                                                                        drotmg     22
c     --output--                                                         drotmg     23
c      dd1  changed to represent the effect of the transformation        drotmg     24
c      dd2  changed to reflect the transformation                        drotmg     25
c      dx1  changed to reflect the transformation                        drotmg     26
c      dx2  unchanged                                                    drotmg     27
c                                                                        drotmg     28
c     construct the modified givens transformation matrix h which zeros  drotmg     29
c     the second component of the 2-vector  (dsqrt(dd1)*dx1,dsqrt(dd2)*  drotmg     30
c     dy2)**t.                                                           drotmg     31
c     with dparam(1)=dflag, h has one of the following forms..           drotmg     32
c                                                                        drotmg     33
c     dflag=-1.d0     dflag=0.d0        dflag=1.d0     dflag=-2.d0       drotmg     34
c                                                                        drotmg     35
c       (dh11  dh12)    (1.d0  dh12)    (dh11  1.d0)    (1.d0  0.d0)     drotmg     36
c     h=(          )    (          )    (          )    (          )     drotmg     37
c       (dh21  dh22),   (dh21  1.d0),   (-1.d0 dh22),   (0.d0  1.d0).    drotmg     38
c     locations 2-5 of dparam contain dh11, dh21, dh12, and dh22         drotmg     39
c     respectively. (values of 1.d0, -1.d0, or 0.d0 implied by the       drotmg     40
c     value of dparam(1) are not stored in dparam.)                      drotmg     41
c                                                                        drotmg     42
c***references                                                           drotmg     43
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   drotmg     44
c   *basic linear algebra subprograms for fortran usage*,                drotmg     45
c  algorithm no. 539, transactions on mathematical software,             drotmg     46
c  volume 5, number 3, september 1979, 308-323                           drotmg     47
c***routines called  (none)                                              drotmg     48
c***end prologue  drotmg                                                 drotmg     49
c                                                                        drotmg     50
      double precision gam,one,rgamsq,dd2,dh11,dh21,dparam,dp2,          drotmg     51
     1 dq2,du,dy1,zero,gamsq,dd1,dflag,dh12,dh22,dp1,dq1,                drotmg     52
     2 dtemp,dx1,two                                                     drotmg     53
      dimension dparam(5)                                                drotmg     54
      data zero,one,two /0.d0,1.d0,2.d0/                                 drotmg     55
      data gam,gamsq,rgamsq/4096.d0,16777216.d0,5.9604645d-8/            drotmg     56
c***first executable statement  drotmg                                   drotmg     57
      if(.not. dd1 .lt. zero) go to 10                                   drotmg     58
c       go zero-h-d-and-dx1..                                            drotmg     59
          go to 60                                                       drotmg     60
   10 continue                                                           drotmg     61
c     case-dd1-nonnegative                                               drotmg     62
      dp2=dd2*dy1                                                        drotmg     63
      if(.not. dp2 .eq. zero) go to 20                                   drotmg     64
          dflag=-two                                                     drotmg     65
          go to 260                                                      drotmg     66
c     regular-case..                                                     drotmg     67
   20 continue                                                           drotmg     68
      dp1=dd1*dx1                                                        drotmg     69
      dq2=dp2*dy1                                                        drotmg     70
      dq1=dp1*dx1                                                        drotmg     71
c                                                                        drotmg     72
      if(.not. dabs(dq1) .gt. dabs(dq2)) go to 40                        drotmg     73
          dh21=-dy1/dx1                                                  drotmg     74
          dh12=dp2/dp1                                                   drotmg     75
c                                                                        drotmg     76
          du=one-dh12*dh21                                               drotmg     77
c                                                                        drotmg     78
          if(.not. du .le. zero) go to 30                                drotmg     79
c         go zero-h-d-and-dx1..                                          drotmg     80
               go to 60                                                  drotmg     81
   30     continue                                                       drotmg     82
               dflag=zero                                                drotmg     83
               dd1=dd1/du                                                drotmg     84
               dd2=dd2/du                                                drotmg     85
               dx1=dx1*du                                                drotmg     86
c         go scale-check..                                               drotmg     87
               go to 100                                                 drotmg     88
   40 continue                                                           drotmg     89
          if(.not. dq2 .lt. zero) go to 50                               drotmg     90
c         go zero-h-d-and-dx1..                                          drotmg     91
               go to 60                                                  drotmg     92
   50     continue                                                       drotmg     93
               dflag=one                                                 drotmg     94
               dh11=dp1/dp2                                              drotmg     95
               dh22=dx1/dy1                                              drotmg     96
               du=one+dh11*dh22                                          drotmg     97
               dtemp=dd2/du                                              drotmg     98
               dd2=dd1/du                                                drotmg     99
               dd1=dtemp                                                 drotmg    100
               dx1=dy1*du                                                drotmg    101
c         go scale-check                                                 drotmg    102
               go to 100                                                 drotmg    103
c     procedure..zero-h-d-and-dx1..                                      drotmg    104
   60 continue                                                           drotmg    105
          dflag=-one                                                     drotmg    106
          dh11=zero                                                      drotmg    107
          dh12=zero                                                      drotmg    108
          dh21=zero                                                      drotmg    109
          dh22=zero                                                      drotmg    110
c                                                                        drotmg    111
          dd1=zero                                                       drotmg    112
          dd2=zero                                                       drotmg    113
          dx1=zero                                                       drotmg    114
c         return..                                                       drotmg    115
          go to 220                                                      drotmg    116
c     procedure..fix-h..                                                 drotmg    117
   70 continue                                                           drotmg    118
      if(.not. dflag .ge. zero) go to 90                                 drotmg    119
c                                                                        drotmg    120
          if(.not. dflag .eq. zero) go to 80                             drotmg    121
          dh11=one                                                       drotmg    122
          dh22=one                                                       drotmg    123
          dflag=-one                                                     drotmg    124
          go to 90                                                       drotmg    125
   80     continue                                                       drotmg    126
          dh21=-one                                                      drotmg    127
          dh12=one                                                       drotmg    128
          dflag=-one                                                     drotmg    129
   90 continue                                                           drotmg    130
      go to igo,(120,150,180,210)                                        drotmg    131
c     procedure..scale-check                                             drotmg    132
  100 continue                                                           drotmg    133
  110     continue                                                       drotmg    134
          if(.not. dd1 .le. rgamsq) go to 130                            drotmg    135
               if(dd1 .eq. zero) go to 160                               drotmg    136
               assign 120 to igo                                         drotmg    137
c              fix-h..                                                   drotmg    138
               go to 70                                                  drotmg    139
  120          continue                                                  drotmg    140
               dd1=dd1*gam**2                                            drotmg    141
               dx1=dx1/gam                                               drotmg    142
               dh11=dh11/gam                                             drotmg    143
               dh12=dh12/gam                                             drotmg    144
          go to 110                                                      drotmg    145
  130 continue                                                           drotmg    146
  140     continue                                                       drotmg    147
          if(.not. dd1 .ge. gamsq) go to 160                             drotmg    148
               assign 150 to igo                                         drotmg    149
c              fix-h..                                                   drotmg    150
               go to 70                                                  drotmg    151
  150          continue                                                  drotmg    152
               dd1=dd1/gam**2                                            drotmg    153
               dx1=dx1*gam                                               drotmg    154
               dh11=dh11*gam                                             drotmg    155
               dh12=dh12*gam                                             drotmg    156
          go to 140                                                      drotmg    157
  160 continue                                                           drotmg    158
  170     continue                                                       drotmg    159
          if(.not. dabs(dd2) .le. rgamsq) go to 190                      drotmg    160
               if(dd2 .eq. zero) go to 220                               drotmg    161
               assign 180 to igo                                         drotmg    162
c              fix-h..                                                   drotmg    163
               go to 70                                                  drotmg    164
  180          continue                                                  drotmg    165
               dd2=dd2*gam**2                                            drotmg    166
               dh21=dh21/gam                                             drotmg    167
               dh22=dh22/gam                                             drotmg    168
          go to 170                                                      drotmg    169
  190 continue                                                           drotmg    170
  200     continue                                                       drotmg    171
          if(.not. dabs(dd2) .ge. gamsq) go to 220                       drotmg    172
               assign 210 to igo                                         drotmg    173
c              fix-h..                                                   drotmg    174
               go to 70                                                  drotmg    175
  210          continue                                                  drotmg    176
               dd2=dd2/gam**2                                            drotmg    177
               dh21=dh21*gam                                             drotmg    178
               dh22=dh22*gam                                             drotmg    179
          go to 200                                                      drotmg    180
  220 continue                                                           drotmg    181
          if(dflag)250,230,240                                           drotmg    182
  230     continue                                                       drotmg    183
               dparam(3)=dh21                                            drotmg    184
               dparam(4)=dh12                                            drotmg    185
               go to 260                                                 drotmg    186
  240     continue                                                       drotmg    187
               dparam(2)=dh11                                            drotmg    188
               dparam(5)=dh22                                            drotmg    189
               go to 260                                                 drotmg    190
  250     continue                                                       drotmg    191
               dparam(2)=dh11                                            drotmg    192
               dparam(3)=dh21                                            drotmg    193
               dparam(4)=dh12                                            drotmg    194
               dparam(5)=dh22                                            drotmg    195
  260 continue                                                           drotmg    196
          dparam(1)=dflag                                                drotmg    197
          return                                                         drotmg    198
      end                                                                drotmg    199

c $Id$ 
