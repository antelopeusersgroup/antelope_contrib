      double precision function ddot(n,dx,incx,dy,incy)                  ddot        2
c***begin prologue  ddot                                                 ddot        3
c***revision date  811015   (yymmdd)                                     ddot        4
c***category no.  f1a                                                    ddot        5
c***keywords  blas,vector,double precision,inner product,                ddot        6
c             dot product                                                ddot        7
c***date written  october 1979                                           ddot        8
c***author lawson c. (jpl),hanson r. (sla),                              ddot        9
c                            kincaid d. (u texas), krogh f. (jpl)        ddot       10
c***purpose                                                              ddot       11
c   d.p. inner product of d.p. vectors                                   ddot       12
c***description                                                          ddot       13
c                b l a s  subprogram                                     ddot       14
c    description of parameters                                           ddot       15
c                                                                        ddot       16
c     --input--                                                          ddot       17
c        n  number of elements in input vector(s)                        ddot       18
c       dx  double precision vector with n elements                      ddot       19
c     incx  storage spacing between elements of dx                       ddot       20
c       dy  double precision vector with n elements                      ddot       21
c     incy  storage spacing between elements of dy                       ddot       22
c                                                                        ddot       23
c     --output--                                                         ddot       24
c     ddot  double precision dot product (zero if n.le.0)                ddot       25
c                                                                        ddot       26
c     returns the dot product of double precision dx and dy.             ddot       27
c     ddot = sum for i = 0 to n-1 of  dx(lx+i*incx) * dy(ly+i*incy)      ddot       28
c     where lx = 1 if incx .ge. 0, else lx = (-incx)*n, and ly is        ddot       29
c     defined in a similar way using incy.                               ddot       30
c                                                                        ddot       31
c***references                                                           ddot       32
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   ddot       33
c   *basic linear algebra subprograms for fortran usage*,                ddot       34
c  algorithm no. 539, transactions on mathematical software,             ddot       35
c  volume 5, number 3, september 1979, 308-323                           ddot       36
c***routines called  (none)                                              ddot       37
c***end prologue  ddot                                                   ddot       38
c                                                                        ddot       39
      double precision dx(1),dy(1)                                       ddot       40
c***first executable statement  ddot                                     ddot       41
      ddot = 0.d0                                                        ddot       42
      if(n.le.0)return                                                   ddot       43
      if(incx.eq.incy) if(incx-1) 5,20,60                                ddot       44
    5 continue                                                           ddot       45
c                                                                        ddot       46
c         code for unequal or nonpositive increments.                    ddot       47
c                                                                        ddot       48
      ix = 1                                                             ddot       49
      iy = 1                                                             ddot       50
      if(incx.lt.0)ix = (-n+1)*incx + 1                                  ddot       51
      if(incy.lt.0)iy = (-n+1)*incy + 1                                  ddot       52
      do 10 i = 1,n                                                      ddot       53
         ddot = ddot + dx(ix)*dy(iy)                                     ddot       54
        ix = ix + incx                                                   ddot       55
        iy = iy + incy                                                   ddot       56
   10 continue                                                           ddot       57
      return                                                             ddot       58
c                                                                        ddot       59
c        code for both increments equal to 1.                            ddot       60
c                                                                        ddot       61
c                                                                        ddot       62
c        clean-up loop so remaining vector length is a multiple of 5.    ddot       63
c                                                                        ddot       64
   20 m = mod(n,5)                                                       ddot       65
      if( m .eq. 0 ) go to 40                                            ddot       66
      do 30 i = 1,m                                                      ddot       67
         ddot = ddot + dx(i)*dy(i)                                       ddot       68
   30 continue                                                           ddot       69
      if( n .lt. 5 ) return                                              ddot       70
   40 mp1 = m + 1                                                        ddot       71
      do 50 i = mp1,n,5                                                  ddot       72
         ddot = ddot + dx(i)*dy(i) + dx(i+1)*dy(i+1) +                   ddot       73
     $   dx(i + 2)*dy(i + 2) + dx(i + 3)*dy(i + 3) + dx(i + 4)*dy(i + 4) ddot       74
   50 continue                                                           ddot       75
      return                                                             ddot       76
c                                                                        ddot       77
c         code for positive equal increments .ne.1.                      ddot       78
c                                                                        ddot       79
   60 continue                                                           ddot       80
      ns = n*incx                                                        ddot       81
          do 70 i=1,ns,incx                                              ddot       82
          ddot = ddot + dx(i)*dy(i)                                      ddot       83
   70     continue                                                       ddot       84
      return                                                             ddot       85
      end                                                                ddot       86

c $Id$ 
