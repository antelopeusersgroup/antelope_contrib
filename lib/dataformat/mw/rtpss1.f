      subroutine rtpss1(efn,nndim,ndata,mm,mdim,nnhefn,ctpw,theta,
     $                  flag,sym,efnh,evh,fv1,fv2,fv3,ierr,work)
c
c  reduced band matrix dpss core routine
c
c  calls tdmev,upefrr,efnnr
c
      implicit double precision (a-h,o-z)
      dimension efn(nndim,nnhefn),efnh(mdim,nnhefn),theta(nnhefn),
     $          evh(nnhefn),fv1(mdim),fv2(mdim),fv3(mdim),work(1)
      logical flag,neven,noedge
      parameter (sr2=1.4142135623731d0)
      ndim=nndim
      nhefn=nnhefn
      neven=mod(ndata,2).eq.0
      noedge=neven.or.(sym.lt.0.)
      cent=dble(ndata-1)/2.
c
c  perform symmetry reduction to half size
c
      do 600 nf=1,mm
        n=nf-1
c
c  main diagonal, diagonal of eigenfunction matrix
c
        fv1(nf)=ctpw*(cent-dble(n))**2
c
c  sub diagonal
c
        fv2(nf)=dble(n*(ndata-n))/2.
  600   fv3(nf)=fv2(nf)*fv2(nf)
      if(neven)fv1(mm)=ctpw*(cent-dble(mdim-1))**2+
     $         sym*dble(mdim*(ndata-mdim))/2.
      if(noedge)goto 700
      fv2(mm)=sr2*fv2(mm)
      fv3(mm)=2.*fv3(mm)
  700 call tdmev(fv1,fv2,fv3,mm,mdim,evh,efnh,nhefn,ierr,work)
      if(noedge)goto 740
      do 720 k=1,nhefn
  720   efnh(mm,k)=sr2*efnh(mm,k)
  740 do 760 k=1,nhefn
        kr=nhefn-k+1
        k2=2*k-1
        theta(k2)=evh(kr)
        call upefrr(efnh(1,kr),ndata,mm,sym,efn(1,k2),flag)
        call efnnr(efn(1,k2),ndata,mm,ndata,flag)
  760   continue
      return
      end

c $Id$ 
