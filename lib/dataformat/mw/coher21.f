      subroutine coher21(ne,kspec,evalu,mxfreq,mxspec,yk,spec,
     $                   mxwt,wt,eigcr,eigsp,delc2,c2,phase,
     $                   c2lo,c2hi,pherr)
      double precision evalu,spec1,spec2,sumdelc2,delsp1,delsp2
      double complex cross,sumdelph,delcr
      complex yk,eigcr
      dimension evalu(kspec),yk(mxfreq,mxspec,2),
     $          spec(mxfreq,2),wt(mxwt,mxspec,2),
     $          c2(ne),phase(ne),c2lo(ne),
     $          c2hi(ne),pherr(ne),eigcr(kspec),
     $          eigsp(kspec,2),delc2(kspec)
      parameter (pi=3.141592653589793d0,tpi=2.*pi)
      arctanh(x)=log((1+x)/(1-x))/2.
      xkspec=kspec
      do 60 i=1,ne
c  compute individual cross- and auto-eigenspectra
        do 15 j=1,kspec
   15     eigcr(j)=evalu(j)*wt(i,j,1)*wt(i,j,2)*conjg(yk(i,j,1))*
     $             yk(i,j,2)
        do 20 n=1,2
          do 20 j=1,kspec
   20       eigsp(j,n)=evalu(j)*(wt(i,j,n)*abs(yk(i,j,n)))**2
c  compute mw coherence and phase
        cross=0.
        spec1=0.
        spec2=0.
        do 30 j=1,kspec
          cross=cross+eigcr(j)
          spec1=spec1+eigsp(j,1)
   30     spec2=spec2+eigsp(j,2)        
        c2(i)=abs(cross)**2/(spec1*spec2)
        phase(i)=atan2(dimag(cross),dreal(cross))
c  compute jackknife estimates of variance
        sumdelc2=0.
        sumdelph=0.
        do 40 j=1,kspec
          delcr=cross-eigcr(j)
          delsp1=spec1-eigsp(j,1)
          delsp2=spec2-eigsp(j,2)
          if ((sngl(abs(delcr)/sqrt(delsp1*delsp2))).lt.(.999999)) then
            delc2(j)=arctanh(sngl(abs(delcr)/sqrt(delsp1*delsp2)))
          else
            delc2(j)=arctanh(.999999)
          end if
          sumdelc2=sumdelc2+delc2(j)
   40     sumdelph=sumdelph+delcr/abs(delcr)
        sumdelc2=sumdelc2/xkspec
        sumdelph=sumdelph/xkspec
        var=0.
        do 50 j=1,kspec
   50     var=var+(sumdelc2-delc2(j))**2
        var=sqrt((xkspec-1.)*var/xkspec)
c
c  correct for zero divide problem in arctanh
c
        if ((sqrt(c2(i))).lt.(.999999)) then
          g=arctanh(sqrt(c2(i)))
        else
          g=arctanh(.999999)
        end if
c
c  correct for lower error bar being higher than the mean
c
        gtanhl = tanh(g-1.96*var)
        if (gtanhl.lt.0) then
          c2lo(i) = 0
        else        
          c2lo(i)=gtanhl**2
        end if
c
        c2hi(i)=tanh(g+1.96*var)**2
        if(abs(sumdelph).le.1.)then
          pherr(i)=sqrt(2.*(xkspec-1.)*(1.-abs(sumdelph)))
        else
          pherr(i)=tpi
        endif
   60   continue
      return
      end

c $Id$ 
