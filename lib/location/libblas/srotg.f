      subroutine srotg(sa,sb,sc,ss)                                      srotg       2
c***begin prologue  srotg                                                srotg       3
c***revision date  811015   (yymmdd)                                     srotg       4
c***category no.  f1a                                                    srotg       5
c***keywords  blas,vector,plane,givens,rotation                          srotg       6
c***date written  october 1979                                           srotg       7
c***author lawson c. (jpl),hanson r. (sla),                              srotg       8
c                            kincaid d. (u texas), krogh f. (jpl)        srotg       9
c***purpose                                                              srotg      10
c    construct s.p. plane givens rotation                                srotg      11
c***description                                                          srotg      12
c                b l a s  subprogram                                     srotg      13
c    description of parameters                                           srotg      14
c                                                                        srotg      15
c     --input--                                                          srotg      16
c       sa  single precision scalar                                      srotg      17
c       sb  single precision scalar                                      srotg      18
c                                                                        srotg      19
c     --output--                                                         srotg      20
c       sa  single precision result r                                    srotg      21
c       sb  single precision result z                                    srotg      22
c       sc  single precision result                                      srotg      23
c       ss  single precision result                                      srotg      24
c                                                                        srotg      25
c     designed by c.l.lawson, jpl, 1977 sept 08                          srotg      26
c                                                                        srotg      27
c                                                                        srotg      28
c     construct the givens transformation                                srotg      29
c                                                                        srotg      30
c         ( sc  ss )                                                     srotg      31
c     g = (        ) ,    sc**2 + ss**2 = 1 ,                            srotg      32
c         (-ss  sc )                                                     srotg      33
c                                                                        srotg      34
c     which zeros the second entry of the 2-vector  (sa,sb)**t.          srotg      35
c                                                                        srotg      36
c     the quantity r = (+/-)sqrt(sa**2 + sb**2) overwrites sa in         srotg      37
c     storage.  the value of sb is overwritten by a value z which        srotg      38
c     allows sc and ss to be recovered by the following algorithm%       srotg      39
c           if z=1  set  sc=0.  and  ss=1.                               srotg      40
c           if abs(z) .lt. 1  set  sc=sqrt(1-z**2)  and  ss=z            srotg      41
c           if abs(z) .gt. 1  set  sc=1/z  and  ss=sqrt(1-sc**2)         srotg      42
c                                                                        srotg      43
c     normally, the subprogram srot(n,sx,incx,sy,incy,sc,ss) will        srotg      44
c     next be called to apply the transformation to a 2 by n matrix.     srotg      45
c                                                                        srotg      46
c ------------------------------------------------------------------     srotg      47
c                                                                        srotg      48
c***references                                                           srotg      49
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   srotg      50
c   *basic linear algebra subprograms for fortran usage*,                srotg      51
c  algorithm no. 539, transactions on mathematical software,             srotg      52
c  volume 5, number 3, september 1979, 308-323                           srotg      53
c***routines called  (none)                                              srotg      54
c***end prologue  srotg                                                  srotg      55
c                                                                        srotg      56
c***first executable statement  srotg                                    srotg      57
      if (abs(sa) .le. abs(sb)) go to 10                                 srotg      58
c                                                                        srotg      59
c *** here abs(sa) .gt. abs(sb) ***                                      srotg      60
c                                                                        srotg      61
      u = sa + sa                                                        srotg      62
      v = sb / u                                                         srotg      63
c                                                                        srotg      64
c     note that u and r have the sign of sa                              srotg      65
c                                                                        srotg      66
      r = sqrt(.25 + v**2) * u                                           srotg      67
c                                                                        srotg      68
c     note that sc is positive                                           srotg      69
c                                                                        srotg      70
      sc = sa / r                                                        srotg      71
      ss = v * (sc + sc)                                                 srotg      72
      sb = ss                                                            srotg      73
      sa = r                                                             srotg      74
      return                                                             srotg      75
c                                                                        srotg      76
c *** here abs(sa) .le. abs(sb) ***                                      srotg      77
c                                                                        srotg      78
   10 if (sb .eq. 0.) go to 20                                           srotg      79
      u = sb + sb                                                        srotg      80
      v = sa / u                                                         srotg      81
c                                                                        srotg      82
c     note that u and r have the sign of sb                              srotg      83
c     (r is immediately stored in sa)                                    srotg      84
c                                                                        srotg      85
      sa = sqrt(.25 + v**2) * u                                          srotg      86
c                                                                        srotg      87
c     note that ss is positive                                           srotg      88
c                                                                        srotg      89
      ss = sb / sa                                                       srotg      90
      sc = v * (ss + ss)                                                 srotg      91
      if (sc .eq. 0.) go to 15                                           srotg      92
      sb = 1. / sc                                                       srotg      93
      return                                                             srotg      94
   15 sb = 1.                                                            srotg      95
      return                                                             srotg      96
c                                                                        srotg      97
c *** here sa = sb = 0. ***                                              srotg      98
c                                                                        srotg      99
   20 sc = 1.                                                            srotg     100
      ss = 0.                                                            srotg     101
      return                                                             srotg     102
c                                                                        srotg     103
      end                                                                srotg     104

c $Id$ 
