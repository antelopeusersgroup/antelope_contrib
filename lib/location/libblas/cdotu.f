      complex function cdotu(n,cx,incx,cy,incy)                          cdotu       2
c***begin prologue  cdotu                                                cdotu       3
c***revision date  811015   (yymmdd)                                     cdotu       4
c***category no.  f1a                                                    cdotu       5
c***keywords  complex,blas,vector,inner product,dot product              cdotu       6
c***date written  october 1979                                           cdotu       7
c***author lawson c. (jpl),hanson r. (sla),                              cdotu       8
c                            kincaid d. (u texas), krogh f. (jpl)        cdotu       9
c***purpose                                                              cdotu      10
c   inner product of complex vectors                                     cdotu      11
c***description                                                          cdotu      12
c                b l a s  subprogram                                     cdotu      13
c    description of parameters                                           cdotu      14
c                                                                        cdotu      15
c     --input--                                                          cdotu      16
c        n  number of elements in input vector(s)                        cdotu      17
c       cx  complex vector with n elements                               cdotu      18
c     incx  storage spacing between elements of cx                       cdotu      19
c       cy  complex vector with n elements                               cdotu      20
c     incy  storage spacing between elements of cy                       cdotu      21
c                                                                        cdotu      22
c     --output--                                                         cdotu      23
c    cdotu  complex result (zero if n.le.0)                              cdotu      24
c                                                                        cdotu      25
c     returns the dot product for complex cx and cy, no conjugation      cdotu      26
c     cdotu = sum for i = 0 to n-1 of  cx(lx+i*incx) * cy(ly+i*incy),    cdotu      27
c     where lx = 1 if incx .ge. 0, else lx = (-incx)*n, and ly is        cdotu      28
c     defined in a similar way using incy.                               cdotu      29
c                                                                        cdotu      30
c                                                                        cdotu      31
c***references                                                           cdotu      32
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   cdotu      33
c   *basic linear algebra subprograms for fortran usage*,                cdotu      34
c  algorithm no. 539, transactions on mathematical software,             cdotu      35
c  volume 5, number 3, september 1979, 308-323                           cdotu      36
c***routines called  (none)                                              cdotu      37
c***end prologue  cdotu                                                  cdotu      38
c                                                                        cdotu      39
      complex cx(1),cy(1)                                                cdotu      40
c***first executable statement  cdotu                                    cdotu      41
      cdotu = (0.,0.)                                                    cdotu      42
      if(n .le. 0)return                                                 cdotu      43
      if(incx.eq.incy.and.incx.gt.0) go to 20                            cdotu      44
      kx = 1                                                             cdotu      45
      ky = 1                                                             cdotu      46
      if(incx.lt.0) kx = 1+(1-n)*incx                                    cdotu      47
      if(incy.lt.0) ky = 1+(1-n)*incy                                    cdotu      48
          do 10 i = 1,n                                                  cdotu      49
          cdotu = cdotu + cx(kx)*cy(ky)                                  cdotu      50
          kx = kx + incx                                                 cdotu      51
          ky = ky + incy                                                 cdotu      52
   10     continue                                                       cdotu      53
      return                                                             cdotu      54
   20 continue                                                           cdotu      55
      ns = n*incx                                                        cdotu      56
          do 30 i=1,ns,incx                                              cdotu      57
          cdotu = cdotu + cx(i)*cy(i)                                    cdotu      58
   30     continue                                                       cdotu      59
      return                                                             cdotu      60
      end                                                                cdotu      61

c $Id$ 
