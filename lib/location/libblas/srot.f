      subroutine srot(n,sx,incx,sy,incy,sc,ss)                           srot        2
c***begin prologue  srot                                                 srot        3
c***revision date  811015   (yymmdd)                                     srot        4
c***category no.  f1a                                                    srot        5
c***keywords  blas,vector,givens,rotation                                srot        6
c***date written  october 1979                                           srot        7
c***author lawson c. (jpl),hanson r. (sla),                              srot        8
c                            kincaid d. (u texas), krogh f. (jpl)        srot        9
c                            kincaid d. (u texas), krogh f. (jpl)        srot       10
c***purpose                                                              srot       11
c  apply s.p. givens rotation                                            srot       12
c***description                                                          srot       13
c                b l a s  subprogram                                     srot       14
c    description of parameters                                           srot       15
c                                                                        srot       16
c     --input--                                                          srot       17
c        n  number of elements in input vector(s)                        srot       18
c       sx  single precision vector with n elements                      srot       19
c     incx  storage spacing between elements of sx                       srot       20
c       sy  single precision vector with n elements                      srot       21
c     incy  storage spacing between elements of sy                       srot       22
c       sc  element of rotation matrix                                   srot       23
c       ss  element of rotation matrix                                   srot       24
c                                                                        srot       25
c     --output--                                                         srot       26
c       sx  rotated vector sx (unchanged if n.le.0)                      srot       27
c       sy  rotated vector sy (unchanged if n.le.0)                      srot       28
c                                                                        srot       29
c     multiply the 2 x 2 matrix  ( sc ss) times the 2 x n matrix (sx**t) srot       30
c                                (-ss sc)                        (sy**t) srot       31
c     where **t indicates transpose.    the elements of sx are in        srot       32
c     sx(lx+i*incx), i = 0 to n-1, where lx = 1 if incx .ge. 0, else     srot       33
c     lx = (-incx)*n, and similarly for sy using ly and incy.            srot       34
c                                                                        srot       35
c***references                                                           srot       36
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   srot       37
c   *basic linear algebra subprograms for fortran usage*,                srot       38
c  algorithm no. 539, transactions on mathematical software,             srot       39
c  volume 5, number 3, september 1979, 308-323                           srot       40
c***routines called  (none)                                              srot       41
c***end prologue  srot                                                   srot       42
c                                                                        srot       43
      real             sx,sy,sc,ss,zero,one,w,z                          srot       44
      dimension sx(1),sy(1)                                              srot       45
      data zero,one/0.e0,1.e0/                                           srot       46
c***first executable statement  srot                                     srot       47
      if(n .le. 0 .or. (ss .eq. zero .and. sc .eq. one)) go to 40        srot       48
      if(.not. (incx .eq. incy .and. incx .gt. 0)) go to 20              srot       49
c                                                                        srot       50
           nsteps=incx*n                                                 srot       51
           do 10 i=1,nsteps,incx                                         srot       52
                w=sx(i)                                                  srot       53
                z=sy(i)                                                  srot       54
                sx(i)=sc*w+ss*z                                          srot       55
                sy(i)=-ss*w+sc*z                                         srot       56
   10           continue                                                 srot       57
           go to 40                                                      srot       58
c                                                                        srot       59
   20 continue                                                           srot       60
           kx=1                                                          srot       61
           ky=1                                                          srot       62
c                                                                        srot       63
           if(incx .lt. 0) kx=1-(n-1)*incx                               srot       64
           if(incy .lt. 0) ky=1-(n-1)*incy                               srot       65
c                                                                        srot       66
           do 30 i=1,n                                                   srot       67
                w=sx(kx)                                                 srot       68
                z=sy(ky)                                                 srot       69
                sx(kx)=sc*w+ss*z                                         srot       70
                sy(ky)=-ss*w+sc*z                                        srot       71
                kx=kx+incx                                               srot       72
                ky=ky+incy                                               srot       73
   30           continue                                                 srot       74
   40 continue                                                           srot       75
c                                                                        srot       76
      return                                                             srot       77
      end                                                                srot       78

c $Id$ 
