      subroutine  scopy(n,sx,incx,sy,incy)                               scopy       2
c***begin prologue  scopy                                                scopy       3
c***revision date  811015   (yymmdd)                                     scopy       4
c***category no.  f1a                                                    scopy       5
c***keywords  blas,vector,copy                                           scopy       6
c***date written  october 1979                                           scopy       7
c***author lawson c. (jpl),hanson r. (sla),                              scopy       8
c                            kincaid d. (u texas), krogh f. (jpl)        scopy       9
c***purpose                                                              scopy      10
c   copy s.p. vector y = x                                               scopy      11
c***description                                                          scopy      12
c                b l a s  subprogram                                     scopy      13
c    description of parameters                                           scopy      14
c                                                                        scopy      15
c     --input--                                                          scopy      16
c        n  number of elements in input vector(s)                        scopy      17
c       sx  single precision vector with n elements                      scopy      18
c     incx  storage spacing between elements of sx                       scopy      19
c       sy  single precision vector with n elements                      scopy      20
c     incy  storage spacing between elements of sy                       scopy      21
c                                                                        scopy      22
c     --output--                                                         scopy      23
c       sy  copy of vector sx (unchanged if n.le.0)                      scopy      24
c                                                                        scopy      25
c     copy single precision sx to single precision sy.                   scopy      26
c     for i = 0 to n-1, copy  sx(lx+i*incx) to sy(ly+i*incy),            scopy      27
c     where lx = 1 if incx .ge. 0, else lx = (-incx)*n, and ly is        scopy      28
c     defined in a similar way using incy.                               scopy      29
c                                                                        scopy      30
c***references                                                           scopy      31
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   scopy      32
c   *basic linear algebra subprograms for fortran usage*,                scopy      33
c  algorithm no. 539, transactions on mathematical software,             scopy      34
c  volume 5, number 3, september 1979, 308-323                           scopy      35
c***routines called  (none)                                              scopy      36
c***end prologue  scopy                                                  scopy      37
c                                                                        scopy      38
      real sx(1),sy(1)                                                   scopy      39
c***first executable statement  scopy                                    scopy      40
      if(n.le.0)return                                                   scopy      41
      if(incx.eq.incy) if(incx-1) 5,20,60                                scopy      42
    5 continue                                                           scopy      43
c                                                                        scopy      44
c        code for unequal or nonpositive increments.                     scopy      45
c                                                                        scopy      46
      ix = 1                                                             scopy      47
      iy = 1                                                             scopy      48
      if(incx.lt.0)ix = (-n+1)*incx + 1                                  scopy      49
      if(incy.lt.0)iy = (-n+1)*incy + 1                                  scopy      50
      do 10 i = 1,n                                                      scopy      51
        sy(iy) = sx(ix)                                                  scopy      52
        ix = ix + incx                                                   scopy      53
        iy = iy + incy                                                   scopy      54
   10 continue                                                           scopy      55
      return                                                             scopy      56
c                                                                        scopy      57
c        code for both increments equal to 1                             scopy      58
c                                                                        scopy      59
c                                                                        scopy      60
c        clean-up loop so remaining vector length is a multiple of 7.    scopy      61
c                                                                        scopy      62
   20 m = mod(n,7)                                                       scopy      63
      if( m .eq. 0 ) go to 40                                            scopy      64
      do 30 i = 1,m                                                      scopy      65
        sy(i) = sx(i)                                                    scopy      66
   30 continue                                                           scopy      67
      if( n .lt. 7 ) return                                              scopy      68
   40 mp1 = m + 1                                                        scopy      69
      do 50 i = mp1,n,7                                                  scopy      70
        sy(i) = sx(i)                                                    scopy      71
        sy(i + 1) = sx(i + 1)                                            scopy      72
        sy(i + 2) = sx(i + 2)                                            scopy      73
        sy(i + 3) = sx(i + 3)                                            scopy      74
        sy(i + 4) = sx(i + 4)                                            scopy      75
        sy(i + 5) = sx(i + 5)                                            scopy      76
        sy(i + 6) = sx(i + 6)                                            scopy      77
   50 continue                                                           scopy      78
      return                                                             scopy      79
c                                                                        scopy      80
c        code for equal, positive, nonunit increments.                   scopy      81
c                                                                        scopy      82
   60 continue                                                           scopy      83
      ns = n*incx                                                        scopy      84
          do 70 i=1,ns,incx                                              scopy      85
          sy(i) = sx(i)                                                  scopy      86
   70     continue                                                       scopy      87
      return                                                             scopy      88
      end                                                                scopy      89

c $Id$ 
