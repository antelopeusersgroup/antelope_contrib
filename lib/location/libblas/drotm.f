      subroutine drotm (n,dx,incx,dy,incy,dparam)                        drotm       2
c***begin prologue  drotm                                                drotm       3
c***revision date  811015   (yymmdd)                                     drotm       4
c***category no.  f1a                                                    drotm       5
c***keywords  blas,vector,double precision,rotation,givens               drotm       6
c***date written  october 1979                                           drotm       7
c***author  lawson c. (jpl), hanson r. (sla),                            drotm       8
c                            kincaid d. (u texas), krogh f. (jpl)        drotm       9
c***purpose                                                              drotm      10
c   apply d.p. modified givens transformation                            drotm      11
c***description                                                          drotm      12
c                b l a s  subprogram                                     drotm      13
c    description of parameters                                           drotm      14
c                                                                        drotm      15
c     --input--                                                          drotm      16
c        n  number of elements in input vector(s)                        drotm      17
c       dx  double precision vector with n elements                      drotm      18
c     incx  storage spacing between elements of dx                       drotm      19
c       dy  double precision vector with n elements                      drotm      20
c     incy  storage spacing between elements of dy                       drotm      21
c   dparam  5-element d.p. vector. dparam(1) is dflag described below    drotm      22
c            elements 2-5 form the transformation matrix h.              drotm      23
c                                                                        drotm      24
c     --output--                                                         drotm      25
c       dx  rotated vector (unchanged if n.le.0)                         drotm      26
c       dy  rotated vector (unchanged if n.le.0)                         drotm      27
c                                                                        drotm      28
c     apply the modified givens transformation, h, to the 2 by n matrix  drotm      29
c                                                                        drotm      30
c     (dx**t) , where **t indicates transpose. the elements of dx are in drotm      31
c     (dy**t)                                                            drotm      32
c                                                                        drotm      33
c     dx(lx+i*incx), i = 0 to n-1, where lx = 1 if incx .ge. 0, else     drotm      34
c     lx = (-incx)*n, and similarly for sy using ly and incy.            drotm      35
c     with dparam(1)=dflag, h has one of the following forms..           drotm      36
c                                                                        drotm      37
c     dflag=-1.d0     dflag=0.d0        dflag=1.d0     dflag=-2.d0       drotm      38
c                                                                        drotm      39
c       (dh11  dh12)    (1.d0  dh12)    (dh11  1.d0)    (1.d0  0.d0)     drotm      40
c     h=(          )    (          )    (          )    (          )     drotm      41
c       (dh21  dh22),   (dh21  1.d0),   (-1.d0 dh22),   (0.d0  1.d0).    drotm      42
c     see drotmg for a description of data storage in dparam.            drotm      43
c                                                                        drotm      44
c                                                                        drotm      45
c***references                                                           drotm      46
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   drotm      47
c   *basic linear algebra subprograms for fortran usage*,                drotm      48
c  algorithm no. 539, transactions on mathematical software,             drotm      49
c  volume 5, number 3, september 1979, 308-323                           drotm      50
c***routines called  (none)                                              drotm      51
c***end prologue  drotm                                                  drotm      52
c                                                                        drotm      53
      double precision dflag,dh12,dh22,dx,two,z,dh11,dh21,               drotm      54
     1 dparam,dy,w,zero                                                  drotm      55
      dimension dx(1),dy(1),dparam(5)                                    drotm      56
      data zero,two/0.d0,2.d0/                                           drotm      57
c***first executable statement  drotm                                    drotm      58
      dflag=dparam(1)                                                    drotm      59
      if(n .le. 0 .or.(dflag+two.eq.zero)) go to 140                     drotm      60
          if(.not.(incx.eq.incy.and. incx .gt.0)) go to 70               drotm      61
c                                                                        drotm      62
               nsteps=n*incx                                             drotm      63
               if(dflag) 50,10,30                                        drotm      64
   10          continue                                                  drotm      65
               dh12=dparam(4)                                            drotm      66
               dh21=dparam(3)                                            drotm      67
                    do 20 i=1,nsteps,incx                                drotm      68
                    w=dx(i)                                              drotm      69
                    z=dy(i)                                              drotm      70
                    dx(i)=w+z*dh12                                       drotm      71
                    dy(i)=w*dh21+z                                       drotm      72
   20               continue                                             drotm      73
               go to 140                                                 drotm      74
   30          continue                                                  drotm      75
               dh11=dparam(2)                                            drotm      76
               dh22=dparam(5)                                            drotm      77
                    do 40 i=1,nsteps,incx                                drotm      78
                    w=dx(i)                                              drotm      79
                    z=dy(i)                                              drotm      80
                    dx(i)=w*dh11+z                                       drotm      81
                    dy(i)=-w+dh22*z                                      drotm      82
   40               continue                                             drotm      83
               go to 140                                                 drotm      84
   50          continue                                                  drotm      85
               dh11=dparam(2)                                            drotm      86
               dh12=dparam(4)                                            drotm      87
               dh21=dparam(3)                                            drotm      88
               dh22=dparam(5)                                            drotm      89
                    do 60 i=1,nsteps,incx                                drotm      90
                    w=dx(i)                                              drotm      91
                    z=dy(i)                                              drotm      92
                    dx(i)=w*dh11+z*dh12                                  drotm      93
                    dy(i)=w*dh21+z*dh22                                  drotm      94
   60               continue                                             drotm      95
               go to 140                                                 drotm      96
   70     continue                                                       drotm      97
          kx=1                                                           drotm      98
          ky=1                                                           drotm      99
          if(incx .lt. 0) kx=1+(1-n)*incx                                drotm     100
          if(incy .lt. 0) ky=1+(1-n)*incy                                drotm     101
c                                                                        drotm     102
          if(dflag)120,80,100                                            drotm     103
   80     continue                                                       drotm     104
          dh12=dparam(4)                                                 drotm     105
          dh21=dparam(3)                                                 drotm     106
               do 90 i=1,n                                               drotm     107
               w=dx(kx)                                                  drotm     108
               z=dy(ky)                                                  drotm     109
               dx(kx)=w+z*dh12                                           drotm     110
               dy(ky)=w*dh21+z                                           drotm     111
               kx=kx+incx                                                drotm     112
               ky=ky+incy                                                drotm     113
   90          continue                                                  drotm     114
          go to 140                                                      drotm     115
  100     continue                                                       drotm     116
          dh11=dparam(2)                                                 drotm     117
          dh22=dparam(5)                                                 drotm     118
               do 110 i=1,n                                              drotm     119
               w=dx(kx)                                                  drotm     120
               z=dy(ky)                                                  drotm     121
               dx(kx)=w*dh11+z                                           drotm     122
               dy(ky)=-w+dh22*z                                          drotm     123
               kx=kx+incx                                                drotm     124
               ky=ky+incy                                                drotm     125
  110          continue                                                  drotm     126
          go to 140                                                      drotm     127
  120     continue                                                       drotm     128
          dh11=dparam(2)                                                 drotm     129
          dh12=dparam(4)                                                 drotm     130
          dh21=dparam(3)                                                 drotm     131
          dh22=dparam(5)                                                 drotm     132
               do 130 i=1,n                                              drotm     133
               w=dx(kx)                                                  drotm     134
               z=dy(ky)                                                  drotm     135
               dx(kx)=w*dh11+z*dh12                                      drotm     136
               dy(ky)=w*dh21+z*dh22                                      drotm     137
               kx=kx+incx                                                drotm     138
               ky=ky+incy                                                drotm     139
  130          continue                                                  drotm     140
  140     continue                                                       drotm     141
          return                                                         drotm     142
          end                                                            drotm     143

c $Id$ 
