      subroutine srotm (n,sx,incx,sy,incy,sparam)                        srotm       2
c***begin prologue  srotm                                                srotm       3
c***revision date  811015   (yymmdd)                                     srotm       4
c***category no.  f1a                                                    srotm       5
c***keywords  blas,vector,givens,rotation                                srotm       6
c***date written  october 1979                                           srotm       7
c***author lawson c. (jpl),hanson r. (sla),                              srotm       8
c                            kincaid d. (u texas), krogh f. (jpl)        srotm       9
c***purpose                                                              srotm      10
c   apply s.p. modified givens transformation                            srotm      11
c***description                                                          srotm      12
c                b l a s  subprogram                                     srotm      13
c    description of parameters                                           srotm      14
c                                                                        srotm      15
c     --input--                                                          srotm      16
c        n  number of elements in input vector(s)                        srotm      17
c       sx  single precision vector with n elements                      srotm      18
c     incx  storage spacing between elements of sx                       srotm      19
c       sy  single precision vector with n elements                      srotm      20
c     incy  storage spacing between elements of sy                       srotm      21
c   sparam  5-element vector. sparam(1) is sflag described below.        srotm      22
c             locations 2-5 of sparam contain elements of the            srotm      23
c              transformation matrix h described below.                  srotm      24
c                                                                        srotm      25
c     --output--                                                         srotm      26
c       sx  rotated vector (unchanged if n.le.0)                         srotm      27
c       sy  rotated vector (unchanged if n.le.0)                         srotm      28
c                                                                        srotm      29
c     apply the modified givens transformation, h, to the 2 by n matrix  srotm      30
c                                                                        srotm      31
c     (sx**t) , where **t indicates transpose. the elements of sx are in srotm      32
c     (sy**t)                                                            srotm      33
c                                                                        srotm      34
c     sx(lx+i*incx), i = 0 to n-1, where lx = 1 if incx .ge. 0, else     srotm      35
c     lx = (-incx)*n, and similarly for sy using using ly and incy.      srotm      36
c     with sparam(1)=sflag, h has one of the following forms..           srotm      37
c                                                                        srotm      38
c     sflag=-1.e0     sflag=0.e0        sflag=1.e0     sflag=-2.e0       srotm      39
c                                                                        srotm      40
c       (sh11  sh12)    (1.e0  sh12)    (sh11  1.e0)    (1.e0  0.e0)     srotm      41
c     h=(          )    (          )    (          )    (          )     srotm      42
c       (sh21  sh22),   (sh21  1.e0),   (-1.e0 sh22),   (0.e0  1.e0).    srotm      43
c     see  srotmg for a description of data storage in sparam.           srotm      44
c                                                                        srotm      45
c                                                                        srotm      46
c***references                                                           srotm      47
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   srotm      48
c   *basic linear algebra subprograms for fortran usage*,                srotm      49
c  algorithm no. 539, transactions on mathematical software,             srotm      50
c  volume 5, number 3, september 1979, 308-323                           srotm      51
c***routines called  (none)                                              srotm      52
c***end prologue  srotm                                                  srotm      53
c                                                                        srotm      54
      dimension sx(1),sy(1),sparam(5)                                    srotm      55
      data zero,two/0.e0,2.e0/                                           srotm      56
c***first executable statement  srotm                                    srotm      57
      sflag=sparam(1)                                                    srotm      58
      if(n .le. 0 .or.(sflag+two.eq.zero)) go to 140                     srotm      59
          if(.not.(incx.eq.incy.and. incx .gt.0)) go to 70               srotm      60
c                                                                        srotm      61
               nsteps=n*incx                                             srotm      62
               if(sflag) 50,10,30                                        srotm      63
   10          continue                                                  srotm      64
               sh12=sparam(4)                                            srotm      65
               sh21=sparam(3)                                            srotm      66
                    do 20 i=1,nsteps,incx                                srotm      67
                    w=sx(i)                                              srotm      68
                    z=sy(i)                                              srotm      69
                    sx(i)=w+z*sh12                                       srotm      70
                    sy(i)=w*sh21+z                                       srotm      71
   20               continue                                             srotm      72
               go to 140                                                 srotm      73
   30          continue                                                  srotm      74
               sh11=sparam(2)                                            srotm      75
               sh22=sparam(5)                                            srotm      76
                    do 40 i=1,nsteps,incx                                srotm      77
                    w=sx(i)                                              srotm      78
                    z=sy(i)                                              srotm      79
                    sx(i)=w*sh11+z                                       srotm      80
                    sy(i)=-w+sh22*z                                      srotm      81
   40               continue                                             srotm      82
               go to 140                                                 srotm      83
   50          continue                                                  srotm      84
               sh11=sparam(2)                                            srotm      85
               sh12=sparam(4)                                            srotm      86
               sh21=sparam(3)                                            srotm      87
               sh22=sparam(5)                                            srotm      88
                    do 60 i=1,nsteps,incx                                srotm      89
                    w=sx(i)                                              srotm      90
                    z=sy(i)                                              srotm      91
                    sx(i)=w*sh11+z*sh12                                  srotm      92
                    sy(i)=w*sh21+z*sh22                                  srotm      93
   60               continue                                             srotm      94
               go to 140                                                 srotm      95
   70     continue                                                       srotm      96
          kx=1                                                           srotm      97
          ky=1                                                           srotm      98
          if(incx .lt. 0) kx=1+(1-n)*incx                                srotm      99
          if(incy .lt. 0) ky=1+(1-n)*incy                                srotm     100
c                                                                        srotm     101
          if(sflag)120,80,100                                            srotm     102
   80     continue                                                       srotm     103
          sh12=sparam(4)                                                 srotm     104
          sh21=sparam(3)                                                 srotm     105
               do 90 i=1,n                                               srotm     106
               w=sx(kx)                                                  srotm     107
               z=sy(ky)                                                  srotm     108
               sx(kx)=w+z*sh12                                           srotm     109
               sy(ky)=w*sh21+z                                           srotm     110
               kx=kx+incx                                                srotm     111
               ky=ky+incy                                                srotm     112
   90          continue                                                  srotm     113
          go to 140                                                      srotm     114
  100     continue                                                       srotm     115
          sh11=sparam(2)                                                 srotm     116
          sh22=sparam(5)                                                 srotm     117
               do 110 i=1,n                                              srotm     118
               w=sx(kx)                                                  srotm     119
               z=sy(ky)                                                  srotm     120
               sx(kx)=w*sh11+z                                           srotm     121
               sy(ky)=-w+sh22*z                                          srotm     122
               kx=kx+incx                                                srotm     123
               ky=ky+incy                                                srotm     124
  110          continue                                                  srotm     125
          go to 140                                                      srotm     126
  120     continue                                                       srotm     127
          sh11=sparam(2)                                                 srotm     128
          sh12=sparam(4)                                                 srotm     129
          sh21=sparam(3)                                                 srotm     130
          sh22=sparam(5)                                                 srotm     131
               do 130 i=1,n                                              srotm     132
               w=sx(kx)                                                  srotm     133
               z=sy(ky)                                                  srotm     134
               sx(kx)=w*sh11+z*sh12                                      srotm     135
               sy(ky)=w*sh21+z*sh22                                      srotm     136
               kx=kx+incx                                                srotm     137
               ky=ky+incy                                                srotm     138
  130          continue                                                  srotm     139
  140     continue                                                       srotm     140
          return                                                         srotm     141
          end                                                            srotm     142

c $Id$ 
