      subroutine powspc ( npts, data, nfft, dt, tbw, 
     +                    freqf, spec, errsp, xi, seavg, ljack, ierr  )
c
c  computes power spectrum of a real data sequence using
c  multiple prolate window eigenfunction expansion
c  removes line components at 0, 50, 60, 100, 120, 180, 240, 300 Hz.
c  This version does not calculate error spectra
c
c  inputs:
c
c  npts             integer number of data points in data
c  data(npts)       real data vector
c  nfft             number of frequency points desired (at least 2*npts)
c  dt               real sample interval in seconds (only used for line removal)
c  tbw              real time-bandwidth product
c
c  outputs:
c
c  freqf(nfft/2+1)  real frequency vector with nyquist frequency = 0.5
c  spec(nfft/2+1)   real power spectra vector
c  ersp(nfft/2+1)   real jackknife error vector.  Errors are
c		    defined as multiplicative factor on spec.
c  xi               real variance efficiency
c  seavg            real average stability of estimate
c  ljack	    logical switch.  When true return
c		    jackknife error estimates in errsp
c  ierr		   error code return.  
c  
c
c
c  calls input,rtpss,pssev,eigenft,ftest,mdfd,linefind,reshape,
c        adaptwt,adaptwtl
c
c*************************************************************
c
c     parameter (MXDATA=___,MXLIN=___,MXSPEC=__)
c     parameter (MXFREQ=2*MXDATA,MXFREQ2=MXFREQ/2+1)
c     parameter (MWSPACE=9*MXDATA+3*MXSPEC+MXDATA*MXSPEC/2+11)
c
      include 'mwspec_par.i'
c       
      double precision evalu,evalu1,dpss,bw,atol,rtol,
     $                 PI,TPI,sum
      logical ljack
      complex yk,xmul
      dimension yk(MXFREQ,MXSPEC),xmul(MXLIN),
     $          data(MXDATA),evalu(MXSPEC),
     $          evalu1(MXSPEC),dpss(MXDATA,MXSPEC),work(MWSPACE),
     $          wsave(4*MXFREQ+15),freql(MXLIN),
     $          freqf(MXFREQ2),f(MXFREQ2),p(MXFREQ2),
     $          flo(MXLIN),fup(MXLIN),iwork(10*MXLIN),
     $          ilin(MXLIN),slin(MXLIN,MXSPEC),spec(MXFREQ2),
     $          se(MXFREQ2),wt(MXFREQ2,MXSPEC),
     $          errsp(MXFREQ2),specl(MXLIN),sel(MXLIN),
     $          wtl(MXLIN,MXSPEC),errspl(MXLIN)
      parameter (PI=3.141592653589793d0,TPI=2.*PI,deg=180./PI)
      data kspec/MXSPEC/
c
      ierr = 0
      if(npts.gt.MXDATA)then
	ierr = 1
        return
      endif
c
c  force fft length to even number of values
c
      if(mod(nfft,2).ne.0)nfft=nfft+1
c
c  force fft length to at least twice npts to avoid circular
c  convolutions
c
      if ( nfft .lt. 2*npts ) then
        nfft = 2 * npts
        ierr = -1
      endif
      if ((kspec .gt. ( 2. * tbw ) - 1. ).or.( kspec .gt. MXSPEC ))then
        kspec = min( int(( 2. * tbw) - 1. ) , MXSPEC )
c        ierr = -2
      endif
      xnpts=npts
      xnfft=nfft
      xkspec=kspec
      nfft2=nfft/2
      nyq=nfft2+1
      bw=tbw/xnpts
      do 15 i=1,nyq
   15   freqf(i)=float(i-1)/xnfft
c  ***************************************************************
c
c  compute kspec discrete prolate spheroidal sequences for use as
c  data windows along with the associated eigenvalues
c
      rtol=1.d-8
      atol=1.d-13
      call rtpss(MXDATA,npts,kspec,bw,dpss,evalu1,iflag,work)
      call pssev(MXDATA,npts,kspec,bw,rtol,atol,dpss,evalu,evalu1)
      if(iflag.ne.0)then
        ierr = 2
	return
      endif
c
c  compute estimate of variance efficiency
c
      xi=0.
      do 20 i=1,npts
        sum=0.
        do 19 j=1,kspec
   19     sum=sum+dpss(i,j)**2
        sum=sum/kspec
   20   xi=xi+sum**2
      xi=1./(xnpts*xi)
c
c calculate eigen-ft's yk using prolate windows dpss
c
      call eigenft(npts,nfft,kspec,data,MXDATA,dpss,
     $             wsave,MXFREQ,yk)
c
c  compute f-test for spectral line components and probabilities
c  corresponding to f
c
      pc=max(0.9,(xnpts-10.)/xnpts)

      call ftest(npts,nyq,kspec,MXDATA,dpss,MXFREQ,yk,work,f)

      do 35 i=1,nyq
        call mdfd(f(i),2,2*(kspec-1),p(i),ier) 
   35   continue
c
c  remove dc component
c
      freqnyq = freqf(nyq)/dt
      nf=1
      flo(nf) = 0.0 
      fup(nf) = freqf(10)
c
c  if 50 hz possible remove it
c
      if (  freqnyq .gt. 50.) then
         nf = nf + 1
         flo(nf) = ( .5 * 48. ) / freqnyq
         fup(nf) = ( .5 * 52. ) / freqnyq
      endif
c
c  if 60 hz possible remove it
c
      if (  freqnyq .gt. 60.) then
         nf = nf + 1
         flo(nf) = ( .5 * 58. ) / freqnyq
         fup(nf) = ( .5 * 62. ) / freqnyq
      endif
c
c  if 100 hz possible remove it
c
      if (  freqnyq .gt. 100.) then
         nf = nf + 1
         flo(nf) = ( .5 * 96. ) / freqnyq
         fup(nf) = ( .5 * 104. ) / freqnyq
      endif
c
c  if 120 hz possible remove it
c
      if (  freqnyq .gt. 120.) then
         nf = nf + 1
         flo(nf) = ( .5 * 116. ) / freqnyq
         fup(nf) = ( .5 * 124. ) / freqnyq
      endif
c
c  if 180 hz possible remove it
c
      if (  freqnyq .gt. 180.) then
         nf = nf + 1
         flo(nf) = ( .5 * 176. ) / freqnyq
         fup(nf) = ( .5 * 184. ) / freqnyq
      endif
c
c  if 240 hz possible remove it
c
      if (  freqnyq .gt. 240.) then
         nf = nf + 1
         flo(nf) = ( .5 * 236. ) / freqnyq
         fup(nf) = ( .5 * 244. ) / freqnyq
      endif
c
c  if 300 hz possible remove it
c
      if (  freqnyq .gt. 300.) then
         nf = nf + 1
         flo(nf) = ( .5 * 296. ) / freqnyq
         fup(nf) = ( .5 * 304. ) / freqnyq
      endif
c
c  find line parameters for all significant lines
c  reshape eigenspectra around significant spectral lines
c
      call linefind(npts,nfft,kspec,MXLIN,nf,flo,fup,pc,p,
     $              MXDATA,dpss,MXFREQ,yk,iwork,work,nl,
     $              xmul,ilin)
      if(nl.gt.0)then
c
c  add negative frequency terms for reshaping
c
        nll=nl
        do 55 i=1,nl
          if((ilin(i).eq.1).or.(ilin(i).eq.nyq))goto 55
          nll=nll+1
          ilin(nll)=nfft-ilin(i)+2
          xmul(nll)=conjg(xmul(i))
   55   continue
        call reshape(npts,nfft,kspec,nll,MXDATA,dpss,ilin,
     $               xmul,MXFREQ,yk,wsave,work,MXLIN,slin)
      endif
      do 58 i=1,nl
   58   freql(i)=float(ilin(i)-1)/xnfft
c
c  compute adaptively weighted spectrum
c
      sum=0.
      do 72 i=1,npts
   72   sum=sum+data(i)**2
      dvar=sum/xnpts
      call adaptwt(nfft,nyq,kspec,dvar,evalu1,MXFREQ,yk,
     $             work,MXFREQ2,spec,se,wt,errsp,ljack)
c
c  double power in positive frequencies
c
      do 74 i=2,nfft2
   74   spec(i)=2.*spec(i)
      if(nll.gt.0)then
        call adaptwtl(nfft,nll,kspec,dvar,evalu1,ilin,MXLIN,
     $                slin,work,specl,sel,wtl,errspl)
        no=nll
        do 75 i=1,nll
   75     if((ilin(i).eq.1).or.(ilin(i).eq.nyq))no=no-1
        no=no/2
        do 76 i=1,nl
   76     if((ilin(i).ne.1).and.(ilin(i).ne.nyq))specl(i)=
     $                                           specl(i)+specl(i+no)
      endif
c
c  scale spectrum to meet parseval's theorem
c
      sum=0.
      do 77 i=1,nyq
   77   sum=sum+spec(i)
      do 78 i=1,nl
   78   sum=sum+specl(i)
      sscal=xnfft*dvar/sum
      do 79 i=1,nyq
   79   spec(i)=sscal*spec(i)
      do 80 i=1,nl
   80   specl(i)=sscal*specl(i)
      seavg=0.
      do 85 i=1,nyq
   85   seavg=seavg+se(i)
      do 86 i=1,nl
   86   seavg=seavg+sel(i)
      seavg=seavg/(2.*xkspec*(nyq+nl))
c
c
      return
      end

