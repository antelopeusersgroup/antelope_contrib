      subroutine drotg(da,db,dc,ds)                                      drotg       2
c***begin prologue  drotg                                                drotg       3
c***revision date  811015   (yymmdd)                                     drotg       4
c***category no.  f1a                                                    drotg       5
c***keywords  blas,vector,double precision,rotation,givens               drotg       6
c***date written  october 1979                                           drotg       7
c***author lawson c. (jpl),hanson r. (sla),                              drotg       8
c                            kincaid d. (u texas), krogh f. (jpl)        drotg       9
c***purpose                                                              drotg      10
c   construct d.p. plane givens rotation                                 drotg      11
c***description                                                          drotg      12
c                b l a s  subprogram                                     drotg      13
c    description of parameters                                           drotg      14
c                                                                        drotg      15
c     --input--                                                          drotg      16
c       da  double precision scalar                                      drotg      17
c       db  double precision scalar                                      drotg      18
c                                                                        drotg      19
c     --output--                                                         drotg      20
c       da  double precision result r                                    drotg      21
c       db  double precision result z                                    drotg      22
c       dc  double precision result                                      drotg      23
c       ds  double precision result                                      drotg      24
c                                                                        drotg      25
c     designed by c.l.lawson, jpl, 1977 sept 08                          drotg      26
c                                                                        drotg      27
c                                                                        drotg      28
c     construct the givens transformation                                drotg      29
c                                                                        drotg      30
c         ( dc  ds )                                                     drotg      31
c     g = (        ) ,    dc**2 + ds**2 = 1 ,                            drotg      32
c         (-ds  dc )                                                     drotg      33
c                                                                        drotg      34
c     which zeros the second entry of the 2-vector  (da,db)**t .         drotg      35
c                                                                        drotg      36
c     the quantity r = (+/-)dsqrt(da**2 + db**2) overwrites da in        drotg      37
c     storage.  the value of db is overwritten by a value z which        drotg      38
c     allows dc and ds to be recovered by the following algorithm%       drotg      39
c           if z=1  set  dc=0.d0  and  ds=1.d0                           drotg      40
c           if dabs(z) .lt. 1  set  dc=dsqrt(1-z**2)  and  ds=z          drotg      41
c           if dabs(z) .gt. 1  set  dc=1/z  and  ds=dsqrt(1-dc**2)       drotg      42
c                                                                        drotg      43
c     normally, the subprogram drot(n,dx,incx,dy,incy,dc,ds) will        drotg      44
c     next be called to apply the transformation to a 2 by n matrix.     drotg      45
c                                                                        drotg      46
c ------------------------------------------------------------------     drotg      47
c                                                                        drotg      48
c***references                                                           drotg      49
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   drotg      50
c   *basic linear algebra subprograms for fortran usage*,                drotg      51
c  algorithm no. 539, transactions on mathematical software,             drotg      52
c  volume 5, number 3, september 1979, 308-323                           drotg      53
c***routines called   (none)                                             drotg      54
c***end prologue  drotg                                                  drotg      55
c                                                                        drotg      56
      double precision  da, db, dc, ds, u, v, r                          drotg      57
c***first executable statement  drotg                                    drotg      58
      if (dabs(da) .le. dabs(db)) go to 10                               drotg      59
c                                                                        drotg      60
c *** here dabs(da) .gt. dabs(db) ***                                    drotg      61
c                                                                        drotg      62
      u = da + da                                                        drotg      63
      v = db / u                                                         drotg      64
c                                                                        drotg      65
c     note that u and r have the sign of da                              drotg      66
c                                                                        drotg      67
      r = dsqrt(.25d0 + v**2) * u                                        drotg      68
c                                                                        drotg      69
c     note that dc is positive                                           drotg      70
c                                                                        drotg      71
      dc = da / r                                                        drotg      72
      ds = v * (dc + dc)                                                 drotg      73
      db = ds                                                            drotg      74
      da = r                                                             drotg      75
      return                                                             drotg      76
c                                                                        drotg      77
c *** here dabs(da) .le. dabs(db) ***                                    drotg      78
c                                                                        drotg      79
   10 if (db .eq. 0.d0) go to 20                                         drotg      80
      u = db + db                                                        drotg      81
      v = da / u                                                         drotg      82
c                                                                        drotg      83
c     note that u and r have the sign of db                              drotg      84
c     (r is immediately stored in da)                                    drotg      85
c                                                                        drotg      86
      da = dsqrt(.25d0 + v**2) * u                                       drotg      87
c                                                                        drotg      88
c     note that ds is positive                                           drotg      89
c                                                                        drotg      90
      ds = db / da                                                       drotg      91
      dc = v * (ds + ds)                                                 drotg      92
      if (dc .eq. 0.d0) go to 15                                         drotg      93
      db = 1.d0 / dc                                                     drotg      94
      return                                                             drotg      95
   15 db = 1.d0                                                          drotg      96
      return                                                             drotg      97
c                                                                        drotg      98
c *** here da = db = 0.d0 ***                                            drotg      99
c                                                                        drotg     100
   20 dc = 1.d0                                                          drotg     101
      ds = 0.d0                                                          drotg     102
      return                                                             drotg     103
c                                                                        drotg     104
      end                                                                drotg     105

c $Id$ 
