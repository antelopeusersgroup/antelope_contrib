      subroutine saxpy(n,sa,sx,incx,sy,incy)                             saxpy       2
c***begin prologue  saxpy                                                saxpy       3
c***revision date  811015   (yymmdd)                                     saxpy       4
c***category no.  f1a                                                    saxpy       5
c***keywords  blas,vector,sum plus                                       saxpy       6
c***date written  october 1979                                           saxpy       7
c***author lawson c. (jpl),hanson r. (sla),                              saxpy       8
c                            kincaid d. (u texas), krogh f. (jpl)        saxpy       9
c***purpose                                                              saxpy      10
c  s.p. computation y = a*x + y                                          saxpy      11
c***description                                                          saxpy      12
c                b l a s  subprogram                                     saxpy      13
c    description of parameters                                           saxpy      14
c                                                                        saxpy      15
c     --input--                                                          saxpy      16
c        n  number of elements in input vector(s)                        saxpy      17
c       sa  single precision scalar multiplier                           saxpy      18
c       sx  single precision vector with n elements                      saxpy      19
c     incx  storage spacing between elements of sx                       saxpy      20
c       sy  single precision vector with n elements                      saxpy      21
c     incy  storage spacing between elements of sy                       saxpy      22
c                                                                        saxpy      23
c     --output--                                                         saxpy      24
c       sy  single precision result (unchanged if n.le.0)                saxpy      25
c                                                                        saxpy      26
c     overwrite single precision sy with single precision sa*sx +sy.     saxpy      27
c     for i = 0 to n-1, replace  sy(ly+i*incy) with sa*sx(lx+i*incx) +   saxpy      28
c       sy(ly+i*incy), where lx = 1 if incx .ge. 0, else lx = (-incx)*n  saxpy      29
c       and ly is defined in a similar way using incy.                   saxpy      30
c                                                                        saxpy      31
c***references                                                           saxpy      32
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   saxpy      33
c   *basic linear algebra subprograms for fortran usage*,                saxpy      34
c  algorithm no. 539, transactions on mathematical software,             saxpy      35
c  volume 5, number 3, september 1979, 308-323                           saxpy      36
c***routines called    (none)                                            saxpy      37
c***end prologue  saxpy                                                  saxpy      38
c                                                                        saxpy      39
      real sx(1),sy(1),sa                                                saxpy      40
c***first executable statement  saxpy                                    saxpy      41
      if(n.le.0.or.sa.eq.0.e0) return                                    saxpy      42
      if(incx.eq.incy) if(incx-1) 5,20,60                                saxpy      43
    5 continue                                                           saxpy      44
c                                                                        saxpy      45
c        code for nonequal or nonpositive increments.                    saxpy      46
c                                                                        saxpy      47
      ix = 1                                                             saxpy      48
      iy = 1                                                             saxpy      49
      if(incx.lt.0)ix = (-n+1)*incx + 1                                  saxpy      50
      if(incy.lt.0)iy = (-n+1)*incy + 1                                  saxpy      51
      do 10 i = 1,n                                                      saxpy      52
        sy(iy) = sy(iy) + sa*sx(ix)                                      saxpy      53
        ix = ix + incx                                                   saxpy      54
        iy = iy + incy                                                   saxpy      55
   10 continue                                                           saxpy      56
      return                                                             saxpy      57
c                                                                        saxpy      58
c        code for both increments equal to 1                             saxpy      59
c                                                                        saxpy      60
c                                                                        saxpy      61
c        clean-up loop so remaining vector length is a multiple of 4.    saxpy      62
c                                                                        saxpy      63
   20 m = mod(n,4)                                                       saxpy      64
      if( m .eq. 0 ) go to 40                                            saxpy      65
      do 30 i = 1,m                                                      saxpy      66
        sy(i) = sy(i) + sa*sx(i)                                         saxpy      67
   30 continue                                                           saxpy      68
      if( n .lt. 4 ) return                                              saxpy      69
   40 mp1 = m + 1                                                        saxpy      70
      do 50 i = mp1,n,4                                                  saxpy      71
        sy(i) = sy(i) + sa*sx(i)                                         saxpy      72
        sy(i + 1) = sy(i + 1) + sa*sx(i + 1)                             saxpy      73
        sy(i + 2) = sy(i + 2) + sa*sx(i + 2)                             saxpy      74
        sy(i + 3) = sy(i + 3) + sa*sx(i + 3)                             saxpy      75
   50 continue                                                           saxpy      76
      return                                                             saxpy      77
c                                                                        saxpy      78
c        code for equal, positive, nonunit increments.                   saxpy      79
c                                                                        saxpy      80
   60 continue                                                           saxpy      81
      ns = n*incx                                                        saxpy      82
          do 70 i=1,ns,incx                                              saxpy      83
          sy(i) = sa*sx(i) + sy(i)                                       saxpy      84
   70     continue                                                       saxpy      85
      return                                                             saxpy      86
      end                                                                saxpy      87

c $Id$ 
