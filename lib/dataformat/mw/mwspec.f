      PROGRAM MWSPEC
c
c  computes power spectrum of a real data sequence using
c  multiple prolate window eigenfunction expansion
c
c  calls input,arfilt,rtpss,pssev,eigenft,ftest,mdfd,linefind,reshape,
c        adaptwt,adaptwtl,sft,compos
c
c*************************************************************
c
      parameter (MXDATA=5000,MXLIN=100,MXSPEC=14,MARF=50)
      parameter (MXFREQ=2*MXDATA,MXFREQ2=MXFREQ/2+1)
      parameter (MWSPACE=9*MXDATA+3*MXSPEC+MXDATA*MXSPEC/2+11)
      character prompt*80
      character filnam*80,form*1
      double precision evalu,evalu1,dpss,bw,atol,rtol,arf,om,
     $                 PI,TPI,ct,st,sum
      logical ljack
      complex yk,xmul
      dimension yk(MXFREQ,MXSPEC),xmul(MXLIN),
     $          data(MXDATA),arf(MARF),evalu(MXSPEC),
     $          evalu1(MXSPEC),dpss(MXDATA,MXSPEC),work(MWSPACE),
     $          wsave(4*MXFREQ+15),freql(MXLIN),
     $          freqf(MXFREQ2),f(MXFREQ2),p(MXFREQ2),
     $          flo(MXLIN),fup(MXLIN),iwork(10*MXLIN),
     $          ilin(MXLIN),slin(MXLIN,MXSPEC),spec(MXFREQ2),
     $          se(MXFREQ2),wt(MXFREQ2,MXSPEC),
     $          errsp(MXFREQ2),specl(MXLIN),sel(MXLIN),
     $          wtl(MXLIN,MXSPEC),errspl(MXLIN),
     $          upw(MXFREQ2)
      parameter (PI=3.141592653589793d0,TPI=2.*PI,deg=180./PI)
      data ljack/.true./
      data form/'f'/
c***************************************************************
c   set up data vector
      write(6,1)
    1 format(' input the data filename:',$)
      read(5,2)filnam
    2 format(a)
      write(6,5)
    5 format(' input the number of data points to skip and read:',$)
      read(5,*)nskip,npts
      call input(filnam,form,nblock,nskip,npts,data)
      write(6,*)npts,' data values input'
      if(npts.gt.MXDATA)then
        write(6,*)' fatal error-too many data values input'
        stop
      endif
      prompt(1:80)='input the ar filter length (0 for none)'
      nar = iask(prompt,0)
c      write(6,6)
c    6 format(' input the ar filter length (0 for none):',$)
c      read(5,*)nar
      if(nar.gt.0)then
        call arfilt(npts,nar,data,work(nar+2),work)
        do 7 i=1,nar
    7     arf(i)=work(i)
      endif
c  **************************************************************
c   input spectrum parameters
c
      ntemp = 2*npts
      prompt(1:80) = 'input the fft length'
      nfft = iask(prompt,ntemp)
c      write(6,11)
c   11 format(' input the fft length:',$)
c      read(5,*)nfft
c  force fft length to even number of values
      if(mod(nfft,2).ne.0)nfft=nfft+1
c  force fft length to at least twice npts to avoid circular
c  convolutions
      if(nfft.lt.2*npts)then
        nfft=2*npts
        write(6,*)' fft length increased to ',nfft
      endif
      rtemp = 4.
      prompt(1:80) = 'input the time-bandwidth product'
      tbw = rask(prompt,rtemp)
c      write(6,12)
c   12 format(' input the time-bandwidth product:',$)
c      read(5,*)tbw
      ntemp = 2.*tbw - 1.
      prompt(1:80) = 'input the number of eigenspectra'
      kspec = iask(prompt,ntemp)
c      write(6,13)
c   13 format(' input the number of eigenspectra:',$)
c      read(5,*)kspec
      if((kspec.gt.2.*tbw).or.(kspec.gt.MXSPEC))then
        kspec=min(int(2.*tbw),MXSPEC)
        write(6,*)' number of spectra reduced to',kspec
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
      rtol=1.d-8
      atol=1.d-14
      call rtpss(MXDATA,npts,kspec,bw,dpss,evalu1,iflag,work)
      call pssev(MXDATA,npts,kspec,bw,rtol,atol,dpss,evalu,evalu1)
      if(iflag.ne.0)then
        write(6,*)' fatal error while computing dpss  iflag=',iflag
        stop
      endif
c  compute estimate of variance efficiency
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
      write(6,26)pc
   26 format(' critical probability level=',g15.7)
      call ftest(npts,nyq,kspec,MXDATA,dpss,MXFREQ,yk,work,f)
      write(6,30)
   30 format(' input the f-test filename:',$)
      read(5,2)filnam
      open (unit=10,file=filnam,status='new')
      kl=0
      do 35 i=1,nyq
        call mdfd(f(i),2,2*(kspec-1),p(i),ier) 
        write(10,900)freqf(i),f(i),p(i)
        if(p(i).ge.pc)then
          if(kl.eq.0)write(6,37)
          write(6,900)freqf(i),f(i),p(i)
          kl=1
        endif
   35   continue
      if(kl.eq.0)write(6,*)' no significant lines detected'
   37 format(4x,'frequency',5x,'f-statistic',4x,'probability')
      close (unit=10)
      pctemp = 1.
      prompt(1:80)='input critical probability for reshaping'
      prompt(42:80)='(1.0 for none)'
      pc = rask(prompt,pctemp)
c      write(6,41)
c   41 format(' input the critical probability for reshaping:',$)
c      read(5,*)pc
      if(pc.eq.1.)then
        nl=0
        goto 70
      endif
      write(6,43)
   43 format(' input the lower and upper frequency bounds (nyq)')
      nf=0
      do 50 i=1,MXLIN
        read(5,*,end=51)flo(i),fup(i)
        if(fup(i).eq.999.)then
          goto 51
        elseif(fup(i).eq..5)then
          nf=nf+1
          goto 51
        else
          nf=nf+1
        endif
   50   continue
   51 continue
c
c  find line parameters for all significant lines
c  reshape eigenspectra around significant spectral lines
c
      call linefind(npts,nfft,kspec,MXLIN,nf,flo,fup,pc,p,
     $              MXDATA,dpss,MXFREQ,yk,iwork,work,nl,
     $              xmul,ilin)
      if(nl.gt.0)then
c  add negative frequency terms for reshaping
        nll=nl
        do 55 i=1,nl
          if((ilin(i).eq.1).or.(ilin(i).eq.nyq))goto 55
          nll=nll+1
          ilin(nll)=nfft-ilin(i)+2
          xmul(nll)=conjg(xmul(i))
   55     continue
        call reshape(npts,nfft,kspec,nll,MXDATA,dpss,ilin,
     $               xmul,MXFREQ,yk,wsave,work,MXLIN,slin)
      endif
      do 58 i=1,nl
   58   freql(i)=float(ilin(i)-1)/xnfft
c
c  compute adaptively weighted spectrum
c
   70 continue
      write(6,71)xi
   71 format(' variance efficiency=',g15.7)
      sum=0.
      do 72 i=1,npts
   72   sum=sum+data(i)**2
      dvar=sum/xnpts
      call adaptwt(nfft,nyq,kspec,dvar,evalu1,MXFREQ,yk,
     $             work,MXFREQ2,spec,se,wt,errsp,ljack)
c  double power in positive frequencies
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
c  scale spectrum to meet parseval's theorem
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
      write(6,91)seavg
   91 format(' average stability of estimate=',g15.7)
      if(nar.gt.0)then
        do 95 i=1,nyq
          om=TPI*freqf(i)
          call sft(arf,nar,om,ct,st)
   95     upw(i)=1./(ct**2+st**2)
      else
        do 96 i=1,nyq
   96     upw(i)=1.
      endif
      write(6,97)
   97 format(' input the averaged spectrum filename:',$)
      read(5,2)filnam
      open (unit=10,file=filnam,status='new')
      k=1
      do 98 i=1,nyq
        if((nl.ne.0).and.(freql(k).eq.freqf(i)))then
          write(10,900)freql(k),upw(i)*specl(k),sel(k),
     $                 upw(i)*specl(k)/errspl(k),
     $                 upw(i)*specl(k)*errspl(k)
          k=k+1
        endif
   98   write(10,900)freqf(i),upw(i)*spec(i),se(i),
     $               upw(i)*spec(i)/errsp(i),
     $               upw(i)*spec(i)*errsp(i)
      close(unit=10)
  900 format(x,5g15.7)
      stop
      end

c $Id$ 
