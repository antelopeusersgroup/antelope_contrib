      subroutine daxpy(n,da,dx,incx,dy,incy)                             daxpy       2
c***begin prologue  daxpy                                                daxpy       3
c***revision date  811015   (yymmdd)                                     daxpy       4
c***category no.  f1a                                                    daxpy       5
c***keywords  blas,vector,double precision                               daxpy       6
c***date written  october 1979                                           daxpy       7
c***author lawson c. (jpl),hanson r. (sla),                              daxpy       8
c                            kincaid d. (u texas), krogh f. (jpl)        daxpy       9
c***purpose                                                              daxpy      10
c  d.p computation y = a*x + y                                           daxpy      11
c***description                                                          daxpy      12
c                b l a s  subprogram                                     daxpy      13
c    description of parameters                                           daxpy      14
c                                                                        daxpy      15
c     --input--                                                          daxpy      16
c        n  number of elements in input vector(s)                        daxpy      17
c       da  double precision scalar multiplier                           daxpy      18
c       dx  double precision vector with n elements                      daxpy      19
c     incx  storage spacing between elements of dx                       daxpy      20
c       dy  double precision vector with n elements                      daxpy      21
c     incy  storage spacing between elements of dy                       daxpy      22
c                                                                        daxpy      23
c     --output--                                                         daxpy      24
c       dy  double precision result (unchanged if n.le.0)                daxpy      25
c                                                                        daxpy      26
c     overwrite double precision dy with double precision da*dx + dy.    daxpy      27
c     for i = 0 to n-1, replace  dy(ly+i*incy) with da*dx(lx+i*incx) +   daxpy      28
c       dy(ly+i*incy), where lx = 1 if incx .ge. 0, else lx = (-incx)*n  daxpy      29
c       and ly is defined in a similar way using incy.                   daxpy      30
c                                                                        daxpy      31
c***references                                                           daxpy      32
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   daxpy      33
c   *basic linear algebra subprograms for fortran usage*,                daxpy      34
c  algorithm no. 539, transactions on mathematical software,             daxpy      35
c  volume 5, number 3, september 1979, 308-323                           daxpy      36
c***routines called   (none)                                             daxpy      37
c***end prologue  daxpy                                                  daxpy      38
c                                                                        daxpy      39
      double precision dx(1),dy(1),da                                    daxpy      40
c***first executable statement  daxpy                                    daxpy      41
      if(n.le.0.or.da.eq.0.d0) return                                    daxpy      42
      if(incx.eq.incy) if(incx-1) 5,20,60                                daxpy      43
    5 continue                                                           daxpy      44
c                                                                        daxpy      45
c        code for nonequal or nonpositive increments.                    daxpy      46
c                                                                        daxpy      47
      ix = 1                                                             daxpy      48
      iy = 1                                                             daxpy      49
      if(incx.lt.0)ix = (-n+1)*incx + 1                                  daxpy      50
      if(incy.lt.0)iy = (-n+1)*incy + 1                                  daxpy      51
      do 10 i = 1,n                                                      daxpy      52
        dy(iy) = dy(iy) + da*dx(ix)                                      daxpy      53
        ix = ix + incx                                                   daxpy      54
        iy = iy + incy                                                   daxpy      55
   10 continue                                                           daxpy      56
      return                                                             daxpy      57
c                                                                        daxpy      58
c        code for both increments equal to 1                             daxpy      59
c                                                                        daxpy      60
c                                                                        daxpy      61
c        clean-up loop so remaining vector length is a multiple of 4.    daxpy      62
c                                                                        daxpy      63
   20 m = mod(n,4)                                                       daxpy      64
      if( m .eq. 0 ) go to 40                                            daxpy      65
      do 30 i = 1,m                                                      daxpy      66
        dy(i) = dy(i) + da*dx(i)                                         daxpy      67
   30 continue                                                           daxpy      68
      if( n .lt. 4 ) return                                              daxpy      69
   40 mp1 = m + 1                                                        daxpy      70
      do 50 i = mp1,n,4                                                  daxpy      71
        dy(i) = dy(i) + da*dx(i)                                         daxpy      72
        dy(i + 1) = dy(i + 1) + da*dx(i + 1)                             daxpy      73
        dy(i + 2) = dy(i + 2) + da*dx(i + 2)                             daxpy      74
        dy(i + 3) = dy(i + 3) + da*dx(i + 3)                             daxpy      75
   50 continue                                                           daxpy      76
      return                                                             daxpy      77
c                                                                        daxpy      78
c        code for equal, positive, nonunit increments.                   daxpy      79
c                                                                        daxpy      80
   60 continue                                                           daxpy      81
      ns = n*incx                                                        daxpy      82
          do 70 i=1,ns,incx                                              daxpy      83
          dy(i) = da*dx(i) + dy(i)                                       daxpy      84
   70     continue                                                       daxpy      85
      return                                                             daxpy      86
      end                                                                daxpy      87

c $Id$ 
