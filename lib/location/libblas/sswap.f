      subroutine  sswap (n,sx,incx,sy,incy)                              sswap       2
c***begin prologue  sswap                                                sswap       3
c***revision date  811015   (yymmdd)                                     sswap       4
c***category no.  f1a                                                    sswap       5
c***keywords  blas,vector,interchange,swap                               sswap       6
c***date written  october 1979                                           sswap       7
c***author lawson c. (jpl),hanson r. (sla),                              sswap       8
c                            kincaid d. (u texas), krogh f. (jpl)        sswap       9
c***purpose                                                              sswap      10
c    interchange s.p vectors                                             sswap      11
c***description                                                          sswap      12
c                b l a s  subprogram                                     sswap      13
c    description of parameters                                           sswap      14
c                                                                        sswap      15
c     --input--                                                          sswap      16
c        n  number of elements in input vector(s)                        sswap      17
c       sx  single precision vector with n elements                      sswap      18
c     incx  storage spacing between elements of sx                       sswap      19
c       sy  single precision vector with n elements                      sswap      20
c     incy  storage spacing between elements of sy                       sswap      21
c                                                                        sswap      22
c     --output--                                                         sswap      23
c       sx  input vector sy (unchanged if n.le.0)                        sswap      24
c       sy  input vector sx (unchanged if n.le.0)                        sswap      25
c                                                                        sswap      26
c     interchange single precision sx and single precision sy.           sswap      27
c     for i = 0 to n-1, interchange  sx(lx+i*incx) and sy(ly+i*incy),    sswap      28
c     where lx = 1 if incx .ge. 0, else lx = (-incx)*n, and ly is        sswap      29
c     defined in a similar way using incy.                               sswap      30
c                                                                        sswap      31
c***references                                                           sswap      32
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   sswap      33
c   *basic linear algebra subprograms for fortran usage*,                sswap      34
c  algorithm no. 539, transactions on mathematical software,             sswap      35
c  volume 5, number 3, september 1979, 308-323                           sswap      36
c***routines called  (none)                                              sswap      37
c***end prologue  sswap                                                  sswap      38
c                                                                        sswap      39
      real sx(1),sy(1),stemp1,stemp2,stemp3                              sswap      40
c***first executable statement  sswap                                    sswap      41
      if(n.le.0)return                                                   sswap      42
      if(incx.eq.incy) if(incx-1) 5,20,60                                sswap      43
    5 continue                                                           sswap      44
c                                                                        sswap      45
c       code for unequal or nonpositive increments.                      sswap      46
c                                                                        sswap      47
      ix = 1                                                             sswap      48
      iy = 1                                                             sswap      49
      if(incx.lt.0)ix = (-n+1)*incx + 1                                  sswap      50
      if(incy.lt.0)iy = (-n+1)*incy + 1                                  sswap      51
      do 10 i = 1,n                                                      sswap      52
        stemp1 = sx(ix)                                                  sswap      53
        sx(ix) = sy(iy)                                                  sswap      54
        sy(iy) = stemp1                                                  sswap      55
        ix = ix + incx                                                   sswap      56
        iy = iy + incy                                                   sswap      57
   10 continue                                                           sswap      58
      return                                                             sswap      59
c                                                                        sswap      60
c       code for both increments equal to 1                              sswap      61
c                                                                        sswap      62
c                                                                        sswap      63
c       clean-up loop so remaining vector length is a multiple of 3.     sswap      64
c                                                                        sswap      65
   20 m = mod(n,3)                                                       sswap      66
      if( m .eq. 0 ) go to 40                                            sswap      67
      do 30 i = 1,m                                                      sswap      68
        stemp1 = sx(i)                                                   sswap      69
        sx(i) = sy(i)                                                    sswap      70
        sy(i) = stemp1                                                   sswap      71
   30 continue                                                           sswap      72
      if( n .lt. 3 ) return                                              sswap      73
   40 mp1 = m + 1                                                        sswap      74
      do 50 i = mp1,n,3                                                  sswap      75
        stemp1 = sx(i)                                                   sswap      76
        stemp2 = sx(i+1)                                                 sswap      77
        stemp3 = sx(i+2)                                                 sswap      78
        sx(i) = sy(i)                                                    sswap      79
        sx(i+1) = sy(i+1)                                                sswap      80
        sx(i+2) = sy(i+2)                                                sswap      81
        sy(i) = stemp1                                                   sswap      82
        sy(i+1) = stemp2                                                 sswap      83
        sy(i+2) = stemp3                                                 sswap      84
   50 continue                                                           sswap      85
      return                                                             sswap      86
   60 continue                                                           sswap      87
c                                                                        sswap      88
c     code for equal, positive, nonunit increments.                      sswap      89
c                                                                        sswap      90
      ns = n*incx                                                        sswap      91
        do 70 i=1,ns,incx                                                sswap      92
        stemp1 = sx(i)                                                   sswap      93
        sx(i) = sy(i)                                                    sswap      94
        sy(i) = stemp1                                                   sswap      95
   70   continue                                                         sswap      96
      return                                                             sswap      97
      end                                                                sswap      98

c $Id$ 
