      real function sdsdot(n,sb,sx,incx,sy,incy)                         sdsdot      2
c***begin prologue  sdsdot                                               sdsdot      3
c***revision date  811015   (yymmdd)                                     sdsdot      4
c***category no.  f1a                                                    sdsdot      5
c***keywords  blas,vector,inner product,dot product                      sdsdot      6
c***date written  october 1979                                           sdsdot      7
c***author lawson c. (jpl),hanson r. (sla),                              sdsdot      8
c                            kincaid d. (u texas), krogh f. (jpl)        sdsdot      9
c***purpose                                                              sdsdot     10
c   s.p. result with inner product accumulated in d.p.                   sdsdot     11
c***description                                                          sdsdot     12
c                b l a s  subprogram                                     sdsdot     13
c    description of parameters                                           sdsdot     14
c                                                                        sdsdot     15
c     --input--                                                          sdsdot     16
c        n  number of elements in input vector(s)                        sdsdot     17
c       sb  single precision scalar to be added to inner product         sdsdot     18
c       sx  single precision vector with n elements                      sdsdot     19
c     incx  storage spacing between elements of sx                       sdsdot     20
c       sy  single precision vector with n elements                      sdsdot     21
c     incy  storage spacing between elements of sy                       sdsdot     22
c                                                                        sdsdot     23
c     --output--                                                         sdsdot     24
c   sdsdot  single precision dot product (zero if n.le.0)                sdsdot     25
c                                                                        sdsdot     26
c     returns s.p. result with dot product accumulated in d.p.           sdsdot     27
c     sdsdot = sb + sum for i = 0 to n-1 of sx(lx+i*incx)*sy(ly+i*incy)  sdsdot     28
c     where lx = 1 if incx .ge. 0, else lx = (-incx)*n, and ly is        sdsdot     29
c     defined in a similar way using incy.                               sdsdot     30
c                                                                        sdsdot     31
c                                                                        sdsdot     32
c***references                                                           sdsdot     33
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   sdsdot     34
c   *basic linear algebra subprograms for fortran usage*,                sdsdot     35
c  algorithm no. 539, transactions on mathematical software,             sdsdot     36
c  volume 5, number 3, september 1979, 308-323                           sdsdot     37
c***routines called   (none)                                             sdsdot     38
c***end prologue  sdsdot                                                 sdsdot     39
c                                                                        sdsdot     40
      real              sx(1),sy(1),sb                                   sdsdot     41
      double precision dsdot                                             sdsdot     42
c***first executable statement  sdsdot                                   sdsdot     43
      dsdot = dble(sb)                                                   sdsdot     44
      if(n .le. 0) go to 30                                              sdsdot     45
      if(incx.eq.incy.and.incx.gt.0) go to 40                            sdsdot     46
      kx = 1                                                             sdsdot     47
      ky = 1                                                             sdsdot     48
      if(incx.lt.0) kx = 1+(1-n)*incx                                    sdsdot     49
      if(incy.lt.0) ky = 1+(1-n)*incy                                    sdsdot     50
          do 10 i = 1,n                                                  sdsdot     51
          dsdot = dsdot + dble(sx(kx))*dble(sy(ky))                      sdsdot     52
          kx = kx + incx                                                 sdsdot     53
          ky = ky + incy                                                 sdsdot     54
   10     continue                                                       sdsdot     55
   30 sdsdot = sngl(dsdot)                                               sdsdot     56
      return                                                             sdsdot     57
   40 continue                                                           sdsdot     58
      ns = n*incx                                                        sdsdot     59
          do 50 i=1,ns,incx                                              sdsdot     60
          dsdot = dsdot + dble(sx(i))*dble(sy(i))                        sdsdot     61
   50     continue                                                       sdsdot     62
      sdsdot = sngl(dsdot)                                               sdsdot     63
      return                                                             sdsdot     64
      end                                                                sdsdot     65

c $Id$ 
