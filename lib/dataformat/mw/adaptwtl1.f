      subroutine adaptwtl1(nfft,kspec,nlin,dvar,evalu1,ilin,
     $                     mxlin,slin,evalu,sqev,bk,sk,eigsp,delsp,
     $                     spec,se,wt,err)
c
c  calls r1mach,fzero
c
      double precision evalu,sqev,evalu1,sie,sumwt,eigsum,sumdel,var
      dimension evalu(kspec),sqev(kspec),evalu1(kspec),
     $          slin(mxlin,kspec),se(nlin),spec(nlin),
     $          wt(mxlin,kspec),err(nlin),eigsp(kspec),delsp(kspec),
     $          bk(kspec),sk(kspec),ilin(nlin)
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
c  step through lines and find weighted spectrum iteratively
c  for each
      small=r1mach(1)
      big=r1mach(2)
      rerr=4.*r1mach(4)
      aerr=1.e6*small
      do 40 i=1,nlin
        b=big
        c=small
        do 30 j=1,kspec
          sk(j)=siek*evalu(j)*slin(i,j)
          b=min(b,sk(j))
   30     c=max(c,sk(j))
        r=0.5*(sk(1)+sk(2))
        call fzero(fspec,b,c,r,rerr,aerr,iflag,evalu,bk,kspec)
        if(iflag.ne.1)then
          write(6,*)' line recursion did not converge'
          write(6,*)' frequency=',float(ilin(i)-1)/xnfft,
     $              ' iflag=',iflag
        endif
   40   spec(i)=b
c  compute weights
      do 50 i=1,nlin
        se(i)=0.
        do 45 j=1,kspec
          wt(i,j)=sqev(j)*spec(i)/(evalu(j)*spec(i)+bk(j))
          sumwt=sumwt+wt(i,j)**2
          if((ilin(i).eq.1).or.(ilin(i).eq.nfft2+1))then
            se(i)=se(i)+evalu(j)*wt(i,j)**2
          else
            se(i)=se(i)+2.*evalu(j)*wt(i,j)**2
          endif
   45     continue
        sumwt=sqrt(sumwt)
        do 50 j=1,kspec
   50     wt(i,j)=wt(i,j)/sumwt
c
c  calculate jackknife estimate of 95% confidence limits
c
      do 110 i=1,nlin
        eigsum=0.
        do 70 j=1,kspec
          eigsp(j)=siek*evalu(j)*wt(i,j)**2*slin(i,j)
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
