      subroutine ccopy(n,cx,incx,cy,incy)                                ccopy       2
c***begin prologue  ccopy                                                ccopy       3
c***revision date  811015   (yymmdd)                                     ccopy       4
c***category no.  f1a                                                    ccopy       5
c***keywords  complex,blas,vector,copy,vector copy                       ccopy       6
c***date written  october 1979                                           ccopy       7
c***author lawson c. (jpl),hanson r. (sla),                              ccopy       8
c                            kincaid d. (u texas), krogh f. (jpl)        ccopy       9
c***purpose                                                              ccopy      10
c   complex vector copy y = x                                            ccopy      11
c***description                                                          ccopy      12
c                b l a s  subprogram                                     ccopy      13
c    description of parameters                                           ccopy      14
c                                                                        ccopy      15
c     --input--                                                          ccopy      16
c        n  number of elements in input vector(s)                        ccopy      17
c       cx  complex vector with n elements                               ccopy      18
c     incx  storage spacing between elements of cx                       ccopy      19
c       cy  complex vector with n elements                               ccopy      20
c     incy  storage spacing between elements of cy                       ccopy      21
c                                                                        ccopy      22
c     --output--                                                         ccopy      23
c       cy  copy of vector cx (unchanged if n.le.0)                      ccopy      24
c                                                                        ccopy      25
c     copy complex cx to complex cy.                                     ccopy      26
c     for i = 0 to n-1, copy cx(lx+i*incx) to cy(ly+i*incy),             ccopy      27
c     where lx = 1 if incx .ge. 0, else lx = (-incx)*n, and ly is        ccopy      28
c     defined in a similar way using incy.                               ccopy      29
c                                                                        ccopy      30
c                                                                        ccopy      31
c***references                                                           ccopy      32
c  lawson c.l., hanson r.j., kincaid d.r., krogh f.t.,                   ccopy      33
c   *basic linear algebra subprograms for fortran usage*,                ccopy      34
c  algorithm no. 539, transactions on mathematical software,             ccopy      35
c  volume 5, number 3, september 1979, 308-323                           ccopy      36
c***routines called  (none)                                              ccopy      37
c***end prologue  ccopy                                                  ccopy      38
c                                                                        ccopy      39
      complex cx(1),cy(1)                                                ccopy      40
c***first executable statement  ccopy                                    ccopy      41
      if(n .le. 0)return                                                 ccopy      42
      if(incx.eq.incy.and.incx.gt.0) go to 20                            ccopy      43
      kx = 1                                                             ccopy      44
      ky = 1                                                             ccopy      45
      if(incx.lt.0) kx = 1+(1-n)*incx                                    ccopy      46
      if(incy.lt.0) ky = 1+(1-n)*incy                                    ccopy      47
          do 10 i = 1,n                                                  ccopy      48
          cy(ky) = cx(kx)                                                ccopy      49
          kx = kx + incx                                                 ccopy      50
          ky = ky + incy                                                 ccopy      51
   10 continue                                                           ccopy      52
      return                                                             ccopy      53
   20 continue                                                           ccopy      54
      ns = n*incx                                                        ccopy      55
          do 30 i=1,ns,incx                                              ccopy      56
          cy(i) = cx(i)                                                  ccopy      57
   30     continue                                                       ccopy      58
      return                                                             ccopy      59
      end                                                                ccopy      60

c $Id$ 
