      complex function cdcdot(n,cb,cx,incx,cy,incy)                      cdcdot      2
c***begin prologue  cdcdot                                               cdcdot      3
c***category no.  f1a                                                    cdcdot      4
c***purpose  complex inner product  --see sdsdot                         cdcdot      5
c***refer to sdsdot                                                      cdcdot      6
c              this function is the complex equivalent of sdsdot         cdcdot      7
c               in blas.  it is used by cgeir.                           cdcdot      8
c***routines called  (none)                                              cdcdot      9
c***end prologue  cdcdot                                                 cdcdot     10
      integer n,incx,incy,i,kx,ky                                        cdcdot     11
      complex cx(1),cy(1),cb                                             cdcdot     12
      double precision dsdotr,dsdoti,dt1,dt2,dt3,dt4                     cdcdot     13
c***first executable statement  cdcdot                                   cdcdot     14
      dsdotr=real(cb)                                                    cdcdot     15
      dsdoti=aimag(cb)                                                   cdcdot     16
      if(n.le.0) go to 10                                                cdcdot     17
      kx=1                                                               cdcdot     18
      ky=1                                                               cdcdot     19
      if(incx.lt.0) kx=1+(1-n)*incx                                      cdcdot     20
      if(incy.lt.0) ky=1+(1-n)*incy                                      cdcdot     21
      do 5 i=1,n                                                         cdcdot     22
        dt1=dble(real(cx(kx)))                                           cdcdot     23
        dt2=dble(real(cy(ky)))                                           cdcdot     24
        dt3=dble(aimag(cx(kx)))                                          cdcdot     25
        dt4=dble(aimag(cy(ky)))                                          cdcdot     26
      dsdotr=dsdotr+(dt1*dt2)-(dt3*dt4)                                  cdcdot     27
      dsdoti=dsdoti+(dt1*dt4)+(dt3*dt2)                                  cdcdot     28
      kx=kx+incx                                                         cdcdot     29
      ky=ky+incy                                                         cdcdot     30
    5 continue                                                           cdcdot     31
   10 cdcdot=cmplx(sngl(dsdotr),sngl(dsdoti))                            cdcdot     32
      return                                                             cdcdot     33
      end                                                                cdcdot     34

c $Id$ 
