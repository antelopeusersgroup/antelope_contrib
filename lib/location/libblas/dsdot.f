      double precision function dsdot(n,sx,incx,sy,incy)                 dsdot       2
c***begin prologue  dsdot                                                dsdot       3
c***revision date  811015   (yymmdd)                                     dsdot       4
c***category no.  f1a                                                    dsdot       5
c***keywords  blas,vector,double precision,dot product,                  dsdot       6
c             inner product                                              dsdot       7
c***date written  october 1979                                           dsdot       8
c***author lawson c. (jpl),hanson r. (sla),                              dsdot       9
c                            kincaid d. (u texas), krogh f. (jpl)        dsdot      10
c***purpose                                                              dsdot      11
c   d.p inner product of s.p. vectors                                    dsdot      12
c***description                                                          dsdot      13
c                b l a s  subprogram                                     dsdot      14
c    description of parameters                                           dsdot      15
c                                                                        dsdot      16
c     --input--                                                          dsdot      17
c        n  number of elements in input vector(s)                        dsdot      18
c       sx  single precision vector with n elements                      dsdot      19
c     incx  storage spacing between elements of sx                       dsdot      20
c       sy  single precision vector with n elements                      dsdot      21
c     incy  storage spacing between elements of sy                       dsdot      22
c                                                                        dsdot      23
c     --output--                                                         dsdot      24
c    dsdot  double precision dot product (zero if n.le.0)                dsdot      25
c                                                                        dsdot      26
c     returns d.p. dot product accumulated in d.p., for s.p. sx and sy   dsdot      27
c     dsdot = sum for i = 0 to n-1 of  sx(lx+i*incx) * sy(ly+i*incy),    dsdot      28
c     where lx = 1 if incx .ge. 0, else lx = (-incx)*n, and ly is        dsdot      29
c     defined in a similar way using incy.                               dsdot      30
c                                                                        dsdot      31
c                                                                        dsdot      32
c***references                                                           dsdot      33
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   dsdot      34
c   *basic linear algebra subprograms for fortran usage*,                dsdot      35
c  algorithm no. 539, transactions on mathematical software,             dsdot      36
c  volume 5, number 3, september 1979, 308-323                           dsdot      37
c***routines called  (none)                                              dsdot      38
c***end prologue  dsdot                                                  dsdot      39
c                                                                        dsdot      40
      real sx(1),sy(1)                                                   dsdot      41
c***first executable statement  dsdot                                    dsdot      42
      dsdot = 0.d0                                                       dsdot      43
      if(n .le. 0)return                                                 dsdot      44
      if(incx.eq.incy.and.incx.gt.0) go to 20                            dsdot      45
      kx = 1                                                             dsdot      46
      ky = 1                                                             dsdot      47
      if(incx.lt.0) kx = 1+(1-n)*incx                                    dsdot      48
      if(incy.lt.0) ky = 1+(1-n)*incy                                    dsdot      49
          do 10 i = 1,n                                                  dsdot      50
          dsdot = dsdot + dble(sx(kx))*dble(sy(ky))                      dsdot      51
          kx = kx + incx                                                 dsdot      52
          ky = ky + incy                                                 dsdot      53
   10 continue                                                           dsdot      54
      return                                                             dsdot      55
   20 continue                                                           dsdot      56
      ns = n*incx                                                        dsdot      57
          do 30 i=1,ns,incx                                              dsdot      58
          dsdot = dsdot + dble(sx(i))*dble(sy(i))                        dsdot      59
   30     continue                                                       dsdot      60
      return                                                             dsdot      61
      end                                                                dsdot      62

c $Id$ 
