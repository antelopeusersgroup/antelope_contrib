      real function sdot(n,sx,incx,sy,incy)                              sdot        2
c***begin prologue  sdot                                                 sdot        3
c***revision date  811015   (yymmdd)                                     sdot        4
c***category no.  f1a                                                    sdot        5
c***keywords  blas,vector,dot product,inner product                      sdot        6
c***date written  october 1979                                           sdot        7
c***author lawson c. (jpl),hanson r. (sla),                              sdot        8
c                            kincaid d. (u texas), krogh f. (jpl)        sdot        9
c***purpose                                                              sdot       10
c   s.p. inner product of s.p. vectors                                   sdot       11
c***description                                                          sdot       12
c                b l a s  subprogram                                     sdot       13
c    description of parameters                                           sdot       14
c                                                                        sdot       15
c     --input--                                                          sdot       16
c        n  number of elements in input vector(s)                        sdot       17
c       sx  single precision vector with n elements                      sdot       18
c     incx  storage spacing between elements of sx                       sdot       19
c       sy  single precision vector with n elements                      sdot       20
c     incy  storage spacing between elements of sy                       sdot       21
c                                                                        sdot       22
c     --output--                                                         sdot       23
c     sdot  single precision dot product (zero if n.le.0)                sdot       24
c                                                                        sdot       25
c     returns the dot product of single precision sx and sy.             sdot       26
c     sdot = sum for i = 0 to n-1 of  sx(lx+i*incx) * sy(ly+i*incy),     sdot       27
c     where lx = 1 if incx .ge. 0, else lx = (-incx)*n, and ly is        sdot       28
c     defined in a similar way using incy.                               sdot       29
c                                                                        sdot       30
c***references                                                           sdot       31
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   sdot       32
c   *basic linear algebra subprograms for fortran usage*,                sdot       33
c  algorithm no. 539, transactions on mathematical software,             sdot       34
c  volume 5, number 3, september 1979, 308-323                           sdot       35
c***routines called  (none)                                              sdot       36
c***end prologue  sdot                                                   sdot       37
c                                                                        sdot       38
      real sx(1),sy(1)                                                   sdot       39
c***first executable statement  sdot                                     sdot       40
      sdot = 0.0e0                                                       sdot       41
      if(n.le.0)return                                                   sdot       42
      if(incx.eq.incy) if(incx-1)5,20,60                                 sdot       43
    5 continue                                                           sdot       44
c                                                                        sdot       45
c        code for unequal increments or nonpositive increments.          sdot       46
c                                                                        sdot       47
      ix = 1                                                             sdot       48
      iy = 1                                                             sdot       49
      if(incx.lt.0)ix = (-n+1)*incx + 1                                  sdot       50
      if(incy.lt.0)iy = (-n+1)*incy + 1                                  sdot       51
      do 10 i = 1,n                                                      sdot       52
        sdot = sdot + sx(ix)*sy(iy)                                      sdot       53
        ix = ix + incx                                                   sdot       54
        iy = iy + incy                                                   sdot       55
   10 continue                                                           sdot       56
      return                                                             sdot       57
c                                                                        sdot       58
c        code for both increments equal to 1                             sdot       59
c                                                                        sdot       60
c                                                                        sdot       61
c        clean-up loop so remaining vector length is a multiple of 5.    sdot       62
c                                                                        sdot       63
   20 m = mod(n,5)                                                       sdot       64
      if( m .eq. 0 ) go to 40                                            sdot       65
      do 30 i = 1,m                                                      sdot       66
        sdot = sdot + sx(i)*sy(i)                                        sdot       67
   30 continue                                                           sdot       68
      if( n .lt. 5 ) return                                              sdot       69
   40 mp1 = m + 1                                                        sdot       70
      do 50 i = mp1,n,5                                                  sdot       71
        sdot = sdot + sx(i)*sy(i) + sx(i + 1)*sy(i + 1) +                sdot       72
     $   sx(i + 2)*sy(i + 2) + sx(i + 3)*sy(i + 3) + sx(i + 4)*sy(i + 4) sdot       73
   50 continue                                                           sdot       74
      return                                                             sdot       75
c                                                                        sdot       76
c        code for positive equal increments .ne.1.                       sdot       77
c                                                                        sdot       78
   60 continue                                                           sdot       79
      ns=n*incx                                                          sdot       80
      do 70 i=1,ns,incx                                                  sdot       81
        sdot = sdot + sx(i)*sy(i)                                        sdot       82
   70   continue                                                         sdot       83
      return                                                             sdot       84
      end                                                                sdot       85

c $Id$ 
