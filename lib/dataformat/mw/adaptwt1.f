      subroutine adaptwt1(nfft,ne,kspec,dvar,evalu1,mxfreq,yk,
     $                    evalu,sqev,bk,sk,eigsp,delsp,mxwt,spec,
     $                    se,wt,err,jack)
c
c  calls r1mach,fzero
c
      double precision evalu,sqev,evalu1,sie,sumwt,eigsum,sumdel,var
      logical jack
      complex yk
      dimension evalu(kspec),sqev(kspec),evalu1(kspec),
     $          yk(mxfreq,kspec),se(ne),spec(ne),wt(mxwt,kspec),
     $          err(ne),eigsp(kspec),delsp(kspec),
     $          bk(kspec),sk(kspec)
      external fspec
c
      nfft2=nfft/2
      xnfft=nfft
      xkspec=kspec
      sie=0.
      do 10 j=1,kspec
        bk(j)=dvar*evalu1(j)
        evalu(j)=1.-evalu1(j)
        sqev(j)=sqrt(evalu(j))
   10   sie=sie+1./evalu(j)
      siek=sie/xkspec
c
c  step through frequencies and find weighted spectrum iteratively
c  for each
      small=r1mach(1)
      big=r1mach(2)
      rerr=4.*r1mach(4)
      aerr=1.e6*small
      do 40 i=1,ne
        b=big
        c=small
        do 30 j=1,kspec
          sk(j)=siek*evalu(j)*abs(yk(i,j))**2
          b=min(b,sk(j))
          c=max(c,sk(j))
30	  continue
        r=0.5*(sk(1)+sk(2))
        call fzero(fspec,b,c,r,rerr,aerr,iflag,evalu,bk,kspec)
        if(iflag.ne.1)then
          write(6,*)' spectrum recursion did not converge'
          write(6,*)' frequency=',float(i-1)/xnfft,' iflag=',iflag
        endif
   40   spec(i)=b
c  compute weights
      do 50 i=1,ne
        se(i)=0.
        sumwt=0.
        do 45 j=1,kspec
          wt(i,j)=sqev(j)*spec(i)/(evalu(j)*spec(i)+bk(j))
          sumwt=sumwt+wt(i,j)**2
          if((i.eq.1).or.(i.eq.nfft2+1))then
            se(i)=se(i)+evalu(j)*wt(i,j)**2
          else
            se(i)=se(i)+2.*evalu(j)*wt(i,j)**2
          endif
   45     continue
        sumwt=sqrt(sumwt)
        do 50 j=1,kspec
   50     wt(i,j)=wt(i,j)/sumwt
      if (.not.jack) return
c
c  calculate jackknife estimate of 95% confidence limits
c
      do 110 i=1,ne
        eigsum=0.
        do 70 j=1,kspec
          eigsp(j)=siek*evalu(j)*(wt(i,j)*abs(yk(i,j)))**2
   70     eigsum=eigsum+eigsp(j)
        sumdel=0.
        do 90 j=1,kspec
          delsp(j)=log(eigsum-eigsp(j))
   90     sumdel=sumdel+delsp(j)
        sumdel=sumdel/xkspec
        var=0.
        do 100 j=1,kspec
  100     var=var+(sumdel-delsp(j))**2
        var=(xkspec-1.)*var/xkspec
  110   err(i)=exp(1.96*sqrt(var))
      return
      end

c $Id$ 
