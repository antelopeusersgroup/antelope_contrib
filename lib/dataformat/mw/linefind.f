      subroutine linefind(npts,nfft,kspec,mlin,nf,flo,fup,pc,p,
     $                    mxdata,dpss,mxfreq,yk,itemp,dpsw0,nl,
     $                    xmul,ilin)
c
c  finds all frequencies with critical probabilities above pc
c  computes line parameters for those frequencies
c
c  inputs:
c    npts  - number of data points
c    nfft  - length of the fft
c    kspec - number of windows
c    mlin  - maximum number of lines allowed
c    nf    - number of entries in flo and fup
c    flo   - vector of lower frequencies for search
c    fup   - vector of upper frequencies for search
c    pc    - critical probability for search
c    p     - vector of length nfft containing the f-test
c            probabilities
c    mxdata- leading dimension of dpss
c    dpss  - double precision array containing kspec prolate windows
c            of length npts each
c    mxfreq- leading dimension of yk
c    yk    - complex array of tapered data fft's of length nfft
c            by kspec
c    itemp - work vector of length at least 10*mlin
c    dpsw0 - work vector of length at least 2*kspec
c
c  outputs:
c    nl    - number of lines found
c    xmul  - complex vector of mean values for the spectra at the line
c            frequencies
c    ilin  - vector of indices of the lines
c
c*******************************************************************
      double precision dpss,dpsw0,dp0sum
      double complex xmu
      complex yk,xmul
      dimension dpss(mxdata,kspec),yk(mxfreq,kspec),p(nfft),
     $          ilin(mlin),dpsw0(kspec),itemp(10*mlin),xmul(mlin),
     $          flo(nf),fup(nf)
c
      nfft2=nfft/2
      xnfft=nfft
c  find indices of lines which exceed pc in p
      ni=0
      do 15 n=1,nf
        nlo=ifix(xnfft*flo(n))+1
        if(nlo.lt.1)nlo=nlo+nfft
        nup=ifix(xnfft*fup(n))+1
        if(nup.eq.nfft2+1)nup=nfft2
        if(nup.lt.1)nup=nup+nfft
        if(nup.ge.nlo)then
          do 10 i=nlo,nup
            if(p(i).ge.pc)then
              ni=ni+1
              itemp(ni)=i
            endif
   10       continue
        else
          do 11 i=nlo,nfft
            if(p(i).ge.pc)then
              ni=ni+1
              itemp(ni)=i
            endif
   11       continue
          do 12 i=1,nup
            if(p(i).ge.pc)then
              ni=ni+1
              itemp(ni)=i
            endif
   12       continue
        endif
   15   continue
      if(ni.eq.0)then
        nl=0
        return
      endif
c  find peak location and save frequencies and mean values
      k=1
      nl=0
   19 continue
      kl=1
c  check for wide peak in p
      do 20 kk=k+1,ni
        if(itemp(kk)-itemp(kk-1).ne.1)goto 21
   20   kl=kl+1
   21 continue
      xmax=p(itemp(k))
      jm=itemp(k)
      do 30 i=2,kl
        jj=itemp(k+i-1)
        if(p(jj).gt.xmax)then
          xmax=p(jj)
          jm=jj
        endif
   30   continue
      nl=nl+1
      if(nl.gt.mlin)then
        write(6,*)' too many lines'
        nl=mlin
        return
      endif
      ilin(nl)=jm
      k=k+kl
      if(k.gt.ni)goto 40
      goto 19
c  compute mean values for lines
   40 dp0sum=0.
      do 50 j=1,kspec
        dpsw0(j)=0.
        do 45 i=1,npts
   45     dpsw0(j)=dpsw0(j)+dpss(i,j)
   50   dp0sum=dp0sum+dpsw0(j)**2
      do 70 i=1,nl
        xmu=0.
        do 60 j=1,kspec
   60     xmu=xmu+dpsw0(j)*yk(ilin(i),j)
   70   xmul(i)=xmu/dp0sum
      return
      end

c $Id$ 
