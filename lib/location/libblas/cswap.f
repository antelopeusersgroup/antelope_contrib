      subroutine cswap(n,cx,incx,cy,incy)                                cswap       2
c***begin prologue  cswap                                                cswap       3
c***revision date  811015   (yymmdd)                                     cswap       4
c***category no.  f1a                                                    cswap       5
c***keywords  complex,blas,vector,interchange                            cswap       6
c***date written  october 1979                                           cswap       7
c***author lawson c. (jpl),hanson r. (sla),                              cswap       8
c                            kincaid d. (u texas), krogh f. (jpl)        cswap       9
c***purpose                                                              cswap      10
c     interchange complex vectors                                        cswap      11
c***description                                                          cswap      12
c                b l a s  subprogram                                     cswap      13
c    description of parameters                                           cswap      14
c                                                                        cswap      15
c     --input--                                                          cswap      16
c        n  number of elements in input vector(s)                        cswap      17
c       cx  complex vector with n elements                               cswap      18
c     incx  storage spacing between elements of cx                       cswap      19
c       cy  complex vector with n elements                               cswap      20
c     incy  storage spacing between elements of cy                       cswap      21
c                                                                        cswap      22
c     --output--                                                         cswap      23
c       cx  input vector cy (unchanged if n.le.0)                        cswap      24
c       cy  input vector cx (unchanged if n.le.0)                        cswap      25
c                                                                        cswap      26
c     interchange complex cx and complex cy                              cswap      27
c     for i = 0 to n-1, interchange  cx(lx+i*incx) and cy(ly+i*incy),    cswap      28
c     where lx = 1 if incx .gt. 0, else lx = (-incx)*n, and ly is        cswap      29
c     defined in a similar way using incy.                               cswap      30
c                                                                        cswap      31
c                                                                        cswap      32
c***references                                                           cswap      33
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   cswap      34
c   *basic linear algebra subprograms for fortran usage*,                cswap      35
c  algorithm no. 539, transactions on mathematical software,             cswap      36
c  volume 5, number 3, september 1979, 308-323                           cswap      37
c***routines called  (none)                                              cswap      38
c***end prologue  cswap                                                  cswap      39
c                                                                        cswap      40
      complex cx(1),cy(1),ctemp                                          cswap      41
c***first executable statement  cswap                                    cswap      42
      if(n .le. 0)return                                                 cswap      43
      if(incx.eq.incy.and.incx.gt.0) go to 20                            cswap      44
      kx = 1                                                             cswap      45
      ky = 1                                                             cswap      46
      if(incx.lt.0) kx = 1+(1-n)*incx                                    cswap      47
      if(incy.lt.0) ky = 1+(1-n)*incy                                    cswap      48
          do 10 i = 1,n                                                  cswap      49
          ctemp = cx(kx)                                                 cswap      50
          cx(kx) = cy(ky)                                                cswap      51
          cy(ky) = ctemp                                                 cswap      52
          kx = kx + incx                                                 cswap      53
          ky = ky + incy                                                 cswap      54
   10 continue                                                           cswap      55
      return                                                             cswap      56
   20 continue                                                           cswap      57
      ns = n*incx                                                        cswap      58
          do 30 i=1,ns,incx                                              cswap      59
          ctemp = cx(i)                                                  cswap      60
          cx(i) = cy(i)                                                  cswap      61
          cy(i) = ctemp                                                  cswap      62
   30     continue                                                       cswap      63
      return                                                             cswap      64
      end                                                                cswap      65

c $Id$ 
