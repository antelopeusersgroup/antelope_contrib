      subroutine dcopy(n,dx,incx,dy,incy)                                dcopy       2
c***begin prologue  dcopy                                                dcopy       3
c***revision date  811015   (yymmdd)                                     dcopy       4
c***category no.  f1a                                                    dcopy       5
c***keywords  blas,vector,double precision,copy                          dcopy       6
c***date written  october 1979                                           dcopy       7
c***author lawson c. (jpl),hanson r. (sla),                              dcopy       8
c                            kincaid d. (u texas), krogh f. (jpl)        dcopy       9
c***purpose                                                              dcopy      10
c   d.p. vector copy y = x                                               dcopy      11
c***description                                                          dcopy      12
c                b l a s  subprogram                                     dcopy      13
c    description of parameters                                           dcopy      14
c                                                                        dcopy      15
c     --input--                                                          dcopy      16
c        n  number of elements in input vector(s)                        dcopy      17
c       dx  double precision vector with n elements                      dcopy      18
c     incx  storage spacing between elements of dx                       dcopy      19
c       dy  double precision vector with n elements                      dcopy      20
c     incy  storage spacing between elements of dy                       dcopy      21
c                                                                        dcopy      22
c     --output--                                                         dcopy      23
c       dy  copy of vector dx (unchanged if n.le.0)                      dcopy      24
c                                                                        dcopy      25
c     copy double precision dx to double precision dy.                   dcopy      26
c     for i = 0 to n-1, copy dx(lx+i*incx) to dy(ly+i*incy),             dcopy      27
c     where lx = 1 if incx .ge. 0, else lx = (-incx)*n, and ly is        dcopy      28
c     defined in a similar way using incy.                               dcopy      29
c                                                                        dcopy      30
c***references                                                           dcopy      31
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   dcopy      32
c   *basic linear algebra subprograms for fortran usage*,                dcopy      33
c  algorithm no. 539, transactions on mathematical software,             dcopy      34
c  volume 5, number 3, september 1979, 308-323                           dcopy      35
c***routines called  (none)                                              dcopy      36
c***end prologue  dcopy                                                  dcopy      37
c                                                                        dcopy      38
      double precision dx(1),dy(1)                                       dcopy      39
c***first executable statement  dcopy                                    dcopy      40
      if(n.le.0)return                                                   dcopy      41
      if(incx.eq.incy) if(incx-1) 5,20,60                                dcopy      42
    5 continue                                                           dcopy      43
c                                                                        dcopy      44
c        code for unequal or nonpositive increments.                     dcopy      45
c                                                                        dcopy      46
      ix = 1                                                             dcopy      47
      iy = 1                                                             dcopy      48
      if(incx.lt.0)ix = (-n+1)*incx + 1                                  dcopy      49
      if(incy.lt.0)iy = (-n+1)*incy + 1                                  dcopy      50
      do 10 i = 1,n                                                      dcopy      51
        dy(iy) = dx(ix)                                                  dcopy      52
        ix = ix + incx                                                   dcopy      53
        iy = iy + incy                                                   dcopy      54
   10 continue                                                           dcopy      55
      return                                                             dcopy      56
c                                                                        dcopy      57
c        code for both increments equal to 1                             dcopy      58
c                                                                        dcopy      59
c                                                                        dcopy      60
c        clean-up loop so remaining vector length is a multiple of 7.    dcopy      61
c                                                                        dcopy      62
   20 m = mod(n,7)                                                       dcopy      63
      if( m .eq. 0 ) go to 40                                            dcopy      64
      do 30 i = 1,m                                                      dcopy      65
        dy(i) = dx(i)                                                    dcopy      66
   30 continue                                                           dcopy      67
      if( n .lt. 7 ) return                                              dcopy      68
   40 mp1 = m + 1                                                        dcopy      69
      do 50 i = mp1,n,7                                                  dcopy      70
        dy(i) = dx(i)                                                    dcopy      71
        dy(i + 1) = dx(i + 1)                                            dcopy      72
        dy(i + 2) = dx(i + 2)                                            dcopy      73
        dy(i + 3) = dx(i + 3)                                            dcopy      74
        dy(i + 4) = dx(i + 4)                                            dcopy      75
        dy(i + 5) = dx(i + 5)                                            dcopy      76
        dy(i + 6) = dx(i + 6)                                            dcopy      77
   50 continue                                                           dcopy      78
      return                                                             dcopy      79
c                                                                        dcopy      80
c        code for equal, positive, nonunit increments.                   dcopy      81
c                                                                        dcopy      82
   60 continue                                                           dcopy      83
      ns=n*incx                                                          dcopy      84
          do 70 i=1,ns,incx                                              dcopy      85
          dy(i) = dx(i)                                                  dcopy      86
   70     continue                                                       dcopy      87
      return                                                             dcopy      88
      end                                                                dcopy      89

c $Id$ 
