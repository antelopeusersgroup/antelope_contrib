      complex function cdotc(n,cx,incx,cy,incy)                          cdotc       2
c***begin prologue  cdotc                                                cdotc       3
c***revision date  811015   (yymmdd)                                     cdotc       4
c***category no.  f1a                                                    cdotc       5
c***keywords  complex,blas,vector,dot product,inner product              cdotc       6
c***date written  october 1979                                           cdotc       7
c***author lawson c. (jpl),hanson r. (sla),                              cdotc       8
c                            kincaid d. (u texas), krogh f. (jpl)        cdotc       9
c***purpose                                                              cdotc      10
c  dot product of complex vectors, uses complx conjugate of first vector cdotc      11
c***description                                                          cdotc      12
c                b l a s  subprogram                                     cdotc      13
c    description of parameters                                           cdotc      14
c                                                                        cdotc      15
c     --input--                                                          cdotc      16
c        n  number of elements in input vector(s)                        cdotc      17
c       cx  complex vector with n elements                               cdotc      18
c     incx  storage spacing between elements of cx                       cdotc      19
c       cy  complex vector with n elements                               cdotc      20
c     incy  storage spacing between elements of cy                       cdotc      21
c                                                                        cdotc      22
c     --output--                                                         cdotc      23
c    cdotc  complex result (zero if n.le.0)                              cdotc      24
c                                                                        cdotc      25
c     returns the dot product for complex cx and cy, uses conjugate(cx)  cdotc      26
c     cdotc = sum for i = 0 to n-1 of conj(cx(lx+i*incx))*cy(ly+i*incy)  cdotc      27
c     where lx = 1 if incx .ge. 0, else lx = (-incx)*n, and ly is        cdotc      28
c     defined in a similar way using incy.                               cdotc      29
c                                                                        cdotc      30
c                                                                        cdotc      31
c***references                                                           cdotc      32
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   cdotc      33
c   *basic linear algebra subprograms for fortran usage*,                cdotc      34
c  algorithm no. 539, transactions on mathematical software,             cdotc      35
c  volume 5, number 3, september 1979, 308-323                           cdotc      36
c***routines called   (none)                                             cdotc      37
c***end prologue  cdotc                                                  cdotc      38
c                                                                        cdotc      39
      complex cx(1),cy(1)                                                cdotc      40
c***first executable statement  cdotc                                    cdotc      41
      cdotc = (0.,0.)                                                    cdotc      42
      if(n .le. 0)return                                                 cdotc      43
      if(incx.eq.incy.and.incx.gt.0) go to 20                            cdotc      44
      kx = 1                                                             cdotc      45
      ky = 1                                                             cdotc      46
      if(incx.lt.0) kx = 1+(1-n)*incx                                    cdotc      47
      if(incy.lt.0) ky = 1+(1-n)*incy                                    cdotc      48
          do 10 i = 1,n                                                  cdotc      49
          cdotc = cdotc + conjg(cx(kx))*cy(ky)                           cdotc      50
          kx = kx + incx                                                 cdotc      51
          ky = ky + incy                                                 cdotc      52
   10     continue                                                       cdotc      53
      return                                                             cdotc      54
   20 continue                                                           cdotc      55
      ns = n*incx                                                        cdotc      56
          do 30 i=1,ns,incx                                              cdotc      57
          cdotc = conjg(cx(i))*cy(i) + cdotc                             cdotc      58
   30     continue                                                       cdotc      59
      return                                                             cdotc      60
      end                                                                cdotc      61

c $Id$ 
