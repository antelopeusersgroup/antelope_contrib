      subroutine drot(n,dx,incx,dy,incy,dc,ds)                           drot        2
c***begin prologue  drot                                                 drot        3
c***revision date  811015   (yymmdd)                                     drot        4
c***category no.  f1a                                                    drot        5
c***keywords  blas,vector,double precision,rotation,givens               drot        6
c***date written  october 1979                                           drot        7
c***author lawson c. (jpl),hanson r. (sla),                              drot        8
c                            kincaid d. (u texas), krogh f. (jpl)        drot        9
c***purpose                                                              drot       10
c    apply d.p. givens rotation                                          drot       11
c***description                                                          drot       12
c                b l a s  subprogram                                     drot       13
c    description of parameters                                           drot       14
c                                                                        drot       15
c     --input--                                                          drot       16
c        n  number of elements in input vector(s)                        drot       17
c       dx  double precision vector with n elements                      drot       18
c     incx  storage spacing between elements of dx                       drot       19
c       dy  double precision vector with n elements                      drot       20
c     incy  storage spacing between elements of dy                       drot       21
c       dc  d.p. element of rotation matrix                              drot       22
c       ds  d.p. element of rotation matrix                              drot       23
c                                                                        drot       24
c     --output--                                                         drot       25
c       dx  rotated vector (unchanged if n.le.0)                         drot       26
c       dy  rotated vector (unchanged if n.le.0)                         drot       27
c                                                                        drot       28
c     multiply the 2 x 2 matrix  ( dc ds) times the 2 x n matrix (dx**t) drot       29
c                                (-ds dc)                        (dy**t) drot       30
c     where **t indicates transpose.    the elements of dx are in        drot       31
c     dx(lx+i*i cx), i = 0 to n-1, where lx = 1 if i cx .ge. 0, else     drot       32
c     lx = (-incx)*n, and similarly for dy using ly and incy.            drot       33
c                                                                        drot       34
c***references                                                           drot       35
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   drot       36
c   *basic linear algebra subprograms for fortran usage*,                drot       37
c  algorithm no. 539, transactions on mathematical software,             drot       38
c  volume 5, number 3, september 1979, 308-323                           drot       39
c***routines called (none)                                               drot       40
c***end prologue  drot                                                   drot       41
c                                                                        drot       42
      double precision dx,dy,dc,ds,zero,one,w,z                          drot       43
      dimension dx(1),dy(1)                                              drot       44
      data zero,one/0.d0,1.d0/                                           drot       45
c***first executable statement  drot                                     drot       46
      if(n .le. 0 .or. (ds .eq. zero .and. dc .eq. one)) go to 40        drot       47
      if(.not. (incx .eq. incy .and. incx .gt. 0)) go to 20              drot       48
c                                                                        drot       49
           nsteps=incx*n                                                 drot       50
           do 10 i=1,nsteps,incx                                         drot       51
                w=dx(i)                                                  drot       52
                z=dy(i)                                                  drot       53
                dx(i)=dc*w+ds*z                                          drot       54
                dy(i)=-ds*w+dc*z                                         drot       55
   10           continue                                                 drot       56
           go to 40                                                      drot       57
c                                                                        drot       58
   20 continue                                                           drot       59
           kx=1                                                          drot       60
           ky=1                                                          drot       61
c                                                                        drot       62
           if(incx .lt. 0) kx=1-(n-1)*incx                               drot       63
           if(incy .lt. 0) ky=1-(n-1)*incy                               drot       64
c                                                                        drot       65
           do 30 i=1,n                                                   drot       66
                w=dx(kx)                                                 drot       67
                z=dy(ky)                                                 drot       68
                dx(kx)=dc*w+ds*z                                         drot       69
                dy(ky)=-ds*w+dc*z                                        drot       70
                kx=kx+incx                                               drot       71
                ky=ky+incy                                               drot       72
   30           continue                                                 drot       73
   40 continue                                                           drot       74
c                                                                        drot       75
      return                                                             drot       76
      end                                                                drot       77

c $Id$ 
