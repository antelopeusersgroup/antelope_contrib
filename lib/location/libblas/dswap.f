      subroutine dswap(n,dx,incx,dy,incy)                                dswap       2
c***begin prologue  dswap                                                dswap       3
c***revision date  811015   (yymmdd)                                     dswap       4
c***category no.  f1a                                                    dswap       5
c***keywords  blas,vector,double precision,swap,interchange              dswap       6
c***date written  october 1979                                           dswap       7
c***author lawson c. (jpl),hanson r. (sla),                              dswap       8
c                            kincaid d. (u texas), krogh f. (jpl)        dswap       9
c***purpose                                                              dswap      10
c     interchange d.p. vectors                                           dswap      11
c***description                                                          dswap      12
c                b l a s  subprogram                                     dswap      13
c    description of parameters                                           dswap      14
c                                                                        dswap      15
c     --input--                                                          dswap      16
c        n  number of elements in input vector(s)                        dswap      17
c       dx  double precision vector with n elements                      dswap      18
c     incx  storage spacing between elements of dx                       dswap      19
c       dy  double precision vector with n elements                      dswap      20
c     incy  storage spacing between elements of dy                       dswap      21
c                                                                        dswap      22
c     --output--                                                         dswap      23
c       dx  input vector dy (unchanged if n.le.0)                        dswap      24
c       dy  input vector dx (unchanged if n.le.0)                        dswap      25
c                                                                        dswap      26
c     interchange double precision dx and double precision dy.           dswap      27
c     for i = 0 to n-1, interchange  dx(lx+i*incx) and dy(ly+i*incy),    dswap      28
c     where lx = 1 if incx .ge. 0, else lx = (-incx)*n, and ly is        dswap      29
c     defined in a similar way using incy.                               dswap      30
c                                                                        dswap      31
c***references                                                           dswap      32
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   dswap      33
c   *basic linear algebra subprograms for fortran usage*,                dswap      34
c  algorithm no. 539, transactions on mathematical software,             dswap      35
c  volume 5, number 3, september 1979, 308-323                           dswap      36
c***routines called  (none)                                              dswap      37
c***end prologue  dswap                                                  dswap      38
c                                                                        dswap      39
      double precision dx(1),dy(1),dtemp1,dtemp2,dtemp3                  dswap      40
c***first executable statement  dswap                                    dswap      41
      if(n.le.0)return                                                   dswap      42
      if(incx.eq.incy) if(incx-1) 5,20,60                                dswap      43
    5 continue                                                           dswap      44
c                                                                        dswap      45
c       code for unequal or nonpositive increments.                      dswap      46
c                                                                        dswap      47
      ix = 1                                                             dswap      48
      iy = 1                                                             dswap      49
      if(incx.lt.0)ix = (-n+1)*incx + 1                                  dswap      50
      if(incy.lt.0)iy = (-n+1)*incy + 1                                  dswap      51
      do 10 i = 1,n                                                      dswap      52
        dtemp1 = dx(ix)                                                  dswap      53
        dx(ix) = dy(iy)                                                  dswap      54
        dy(iy) = dtemp1                                                  dswap      55
        ix = ix + incx                                                   dswap      56
        iy = iy + incy                                                   dswap      57
   10 continue                                                           dswap      58
      return                                                             dswap      59
c                                                                        dswap      60
c       code for both increments equal to 1                              dswap      61
c                                                                        dswap      62
c                                                                        dswap      63
c       clean-up loop so remaining vector length is a multiple of 3.     dswap      64
c                                                                        dswap      65
   20 m = mod(n,3)                                                       dswap      66
      if( m .eq. 0 ) go to 40                                            dswap      67
      do 30 i = 1,m                                                      dswap      68
        dtemp1 = dx(i)                                                   dswap      69
        dx(i) = dy(i)                                                    dswap      70
        dy(i) = dtemp1                                                   dswap      71
   30 continue                                                           dswap      72
      if( n .lt. 3 ) return                                              dswap      73
   40 mp1 = m + 1                                                        dswap      74
      do 50 i = mp1,n,3                                                  dswap      75
        dtemp1 = dx(i)                                                   dswap      76
        dtemp2 = dx(i+1)                                                 dswap      77
        dtemp3 = dx(i+2)                                                 dswap      78
        dx(i) = dy(i)                                                    dswap      79
        dx(i+1) = dy(i+1)                                                dswap      80
        dx(i+2) = dy(i+2)                                                dswap      81
        dy(i) = dtemp1                                                   dswap      82
        dy(i+1) = dtemp2                                                 dswap      83
        dy(i+2) = dtemp3                                                 dswap      84
   50 continue                                                           dswap      85
      return                                                             dswap      86
   60 continue                                                           dswap      87
c                                                                        dswap      88
c     code for equal, positive, nonunit increments.                      dswap      89
c                                                                        dswap      90
      ns = n*incx                                                        dswap      91
        do 70 i=1,ns,incx                                                dswap      92
        dtemp1 = dx(i)                                                   dswap      93
        dx(i) = dy(i)                                                    dswap      94
        dy(i) = dtemp1                                                   dswap      95
   70   continue                                                         dswap      96
      return                                                             dswap      97
      end                                                                dswap      98

c $Id$ 
