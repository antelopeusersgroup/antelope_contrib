      subroutine compos1(npts,nfft,ne,kspec,tbw,evalu,mxdata,dpss,se,
     $                   spec,mxfreq,yk,mxwt,wt,w,dpswp,sc)
      double precision evalu,dpss,sie,dpsw0,om,tpi,ct,st,sumn,sumd
      double complex t0,t1
      complex yk,dpswp
      dimension evalu(kspec),dpss(mxdata,kspec),se(ne),spec(ne),
     $          yk(mxfreq,kspec),wt(mxwt,kspec),sc(ne),w(nfft),
     $          dpswp(-(nfft/npts+1)*kspec:(nfft/npts+1)*kspec,kspec)
      parameter (pi=3.141592653589793d0,tpi=2.*pi)
c      
c  calls sft
c
      nfft2=nfft/2
      xnfft=nfft
      nw=int(float(nfft)/float(npts)*tbw)
      sie=0.
      do 5 i=1,kspec
    5   sie=sie+1./evalu(i)
      sien=sie/npts
c     compute dpsw over inner domain
      do 20 j=1,kspec
        dpsw0=0.
        do 10 i=1,npts
   10     dpsw0=dpsw0+dpss(i,j)
      	dpswp(1,j)=dpsw0
      	do 20 i=2,nw+1
      	  om=tpi*float(i-1)/xnfft
      	  call sft(dpss(1,j),npts,om,ct,st)
      	  dpswp(i,j)=cmplx(ct,-st)
   20     dpswp(2-i,j)=conjg(dpswp(i,j))
c  compute estimated fisher information for integration weights
      if(ne.le.nfft2+1)then
        w(1)=se(1)/spec(1)**2
        w(nfft2+1)=se(nfft2+1)/spec(nfft2+1)**2
        do 30 i=2,ne-1
   30     w(i)=4.*se(i)/spec(i)**2
        do 31 i=nfft2+2,nfft
   31     w(i)=w(nfft-i+2)
      elseif(ne.eq.nfft)then
        do 32 i=1,ne
   32     w(i)=se(i)/spec(i)**2
      endif
      do 70 i=1,ne
c  compute integral of fisher information over inner domain
      	sumd=0.
        do 40 j=i-nw,i+nw	
      	  jj=j
      	  if(jj.lt.1)jj=jj+nfft
      	  if(jj.gt.nfft)jj=jj-nfft
   40     sumd=sumd+w(jj)
c  compute integral of high resolution estimate weighted by
c  fisher information over inner domain
      	sumn=0.
      	do 60 j=i-nw,i+nw
      	  ii=i+1-j
      	  jj=j
      	  if(jj.lt.1)jj=jj+nfft
      	  if(jj.gt.nfft)jj=jj-nfft
      	  t0=0.
      	  do 50 k=1,kspec
      	    t1=wt(i,k)*dpswp(ii,k)*yk(jj,k)
      	    do 50 kk=1,kspec
   50 	      t0=t0+t1*wt(i,kk)*conjg(dpswp(ii,kk)*yk(jj,kk))
   60     sumn=sumn+w(jj)*dreal(t0)
   70   sc(i)=sien*sumn/sumd
      return
      end

c $Id$ 
