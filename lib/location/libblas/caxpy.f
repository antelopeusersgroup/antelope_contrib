      subroutine caxpy(n,ca,cx,incx,cy,incy)                             caxpy       2
c***begin prologue  caxpy                                                caxpy       3
c***revision date  811015   (yymmdd)                                     caxpy       4
c***category no.  f1a                                                    caxpy       5
c***keywords  blas,complex                                               caxpy       6
c***date written  october 1979                                           caxpy       7
c***author lawson c. (jpl),hanson r. (sla),                              caxpy       8
c                            kincaid d. (u texas), krogh f. (jpl)        caxpy       9
c***purpose                                                              caxpy      10
c  complex computation y = a*x + y                                       caxpy      11
c***description                                                          caxpy      12
c                b l a s  subprogram                                     caxpy      13
c    description of parameters                                           caxpy      14
c                                                                        caxpy      15
c     --input--                                                          caxpy      16
c        n  number of elements in input vector(s)                        caxpy      17
c       ca  complex scalar multiplier                                    caxpy      18
c       cx  complex vector with n elements                               caxpy      19
c     incx  storage spacing between elements of cx                       caxpy      20
c       cy  complex vector with n elements                               caxpy      21
c     incy  storage spacing between elements of cy                       caxpy      22
c                                                                        caxpy      23
c     --output--                                                         caxpy      24
c       cy  complex result (unchanged if n.le.0)                         caxpy      25
c                                                                        caxpy      26
c     overwrite complex cy with complex  ca*cx + cy.                     caxpy      27
c     for i = 0 to n-1, replace  cy(ly+i*incy) with ca*cx(lx+i*incx) +   caxpy      28
c       cy(ly+i*incy), where lx = 1 if incx .ge. 0, else lx = (-incx)*n  caxpy      29
c       and ly is defined in a similar way using incy.                   caxpy      30
c                                                                        caxpy      31
c                                                                        caxpy      32
c***references                                                           caxpy      33
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   caxpy      34
c   *basic linear algebra subprograms for fortran usage*,                caxpy      35
c  algorithm no. 539, transactions on mathematical software,             caxpy      36
c  volume 5, number 3, september 1979, 308-323                           caxpy      37
c***routines called    (none)                                            caxpy      38
c***end prologue  caxpy                                                  caxpy      39
c                                                                        caxpy      40
      complex cx(1),cy(1),ca                                             caxpy      41
c***first executable statement  caxpy                                    caxpy      42
      canorm = abs(real(ca)) + abs(aimag(ca))                            caxpy      43
      if(n.le.0.or.canorm.eq.0.e0) return                                caxpy      44
      if(incx.eq.incy.and.incx.gt.0) go to 20                            caxpy      45
      kx = 1                                                             caxpy      46
      ky = 1                                                             caxpy      47
      if(incx.lt.0) kx = 1+(1-n)*incx                                    caxpy      48
      if(incy.lt.0) ky = 1+(1-n)*incy                                    caxpy      49
          do 10 i = 1,n                                                  caxpy      50
          cy(ky) = cy(ky) + ca*cx(kx)                                    caxpy      51
          kx = kx + incx                                                 caxpy      52
          ky = ky + incy                                                 caxpy      53
   10 continue                                                           caxpy      54
      return                                                             caxpy      55
   20 continue                                                           caxpy      56
      ns = n*incx                                                        caxpy      57
          do 30 i=1,ns,incx                                              caxpy      58
          cy(i) = ca*cx(i) + cy(i)                                       caxpy      59
   30     continue                                                       caxpy      60
      return                                                             caxpy      61
      end                                                                caxpy      62

c $Id$ 
