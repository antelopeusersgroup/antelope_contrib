      subroutine mwsvd1(nf,kspec,nser,jack,scal,swch,evalu,mxfreq,
     $                  mxspec,yk,mxwt,wt,mxser,sqev,x,work,s,e,v,vd,
     $                  sv,svd,spec,specd,del,sv2,sv2j,apc,apcj,ppc,
     $                  ppcj,c2,c2lo,c2hi)
c
c  calls zsvdc
c
      double precision evalu,sqev,siek,sum,var,sumdelc2,sumdela
      double complex x,work,s,e,v,vd,sumdelph
      complex yk,t0
      logical jack,scal,swch
      dimension evalu(kspec),sqev(kspec),yk(mxfreq,mxspec,nser),
     $          wt(mxwt,mxspec,nser),sv(nser),sv2(nser),sv2j(nser),
     $          apc(mxser,nser),apcj(mxser,nser),
     $          ppc(mxser,nser),ppcj(mxser,nser),c2(mxser,nser),
     $          c2lo(mxser,nser),c2hi(mxser,nser),x(kspec,nser),
     $          work(kspec),s(nser),e(nser),v(nser,nser),spec(nser),
     $          svd(nser,kspec),vd(nser,nser,kspec),specd(nser,kspec),
     $          del(kspec)
      parameter (pi=3.141592653589793,tpi=2.*pi)
      arctanh(xx)=log((1.+xx)/(1.-xx))/2.
c
      xkspec=kspec
      siek=0.
      do 10 i=1,kspec
        sqev(i)=sqrt(evalu(i))
   10   siek=siek+1./evalu(i)
      siek=sqrt(siek/xkspec)
c
c  set up spectral matrix
c
      do 20 j=1,nser
        do 20 i=1,kspec
   20     x(i,j)=siek*sqev(i)*wt(nf,i,j)*yk(nf,i,j)
      if(swch.or.scal)then
        do 25 j=1,nser
          sum=0.
          do 24 i=1,kspec
   24       sum=sum+abs(x(i,j))**2
   25     spec(j)=sum
      endif
c
c  optional preweighting
c
      if(scal)then
        do 30 j=1,nser
          scale=1./sqrt(spec(j))
          do 30 i=1,kspec
   30       x(i,j)=scale*x(i,j)
      endif
c
c  compute svd
c
      job=00
      if(swch)job=01
      call zsvdc(x,kspec,kspec,nser,s,e,u,1,v,nser,work,job,info)
      if(info.ne.0)write(6,*)' svd failure at frequency',nf,
     $                       ' info=',info
c
c  store singular values
c
      sum=0.
      do 40 i=1,nser
        sv(i)=dreal(s(i))
        sv2(i)=dreal(s(i))**2
   40   sum=sum+sv2(i)
      sv2(nser+1)=sum
      if((.not.swch).and.(.not.jack))return
c
c  compute jackknife error estimates
c
      if(jack)then
c  compute delete one estimates of singular values and right vectors
        do 50 n=1,kspec
          k=1
          do 46 i=1,kspec
            if(i.eq.n)goto 46
            do 45 j=1,nser
   45         x(k,j)=siek*sqev(i)*wt(nf,i,j)*yk(nf,i,j)
            k=k+1
   46       continue
        if(swch.or.scal)then
          do 47 j=1,nser
   47       specd(j,n)=spec(j)-
     $                 (siek*sqev(n)*wt(nf,n,j)*abs(yk(nf,n,j)))**2
        endif
        if(scal)then
          do 48 j=1,nser
             scale=1./sqrt(specd(j,n))
             do 48 i=1,kspec-1
   48          x(i,j)=scale*x(i,j)
        endif
        call zsvdc(x,kspec,kspec-1,nser,s,e,u,1,vd(1,1,n),nser,work,
     $             job,info)
        if(info.ne.0)write(6,*)' jackknife svd failure at frequency',
     $                         nf,' info=',info
        do 50 j=1,nser
   50     svd(j,n)=dreal(s(j))
        if(.not.swch)goto 81
c  compute delete one estimates of cross spectrum of 
c  eofs and eigentransforms
        if(scal)then
          do 60 j=1,nser
            do 60 i=1,nser
              do 60 k=1,kspec
   60           vd(i,j,k)=svd(j,k)**2*conjg(vd(i,j,k))*sqrt(specd(i,k))
        else
          do 61 j=1,nser
            do 61 i=1,nser
              do 61 k=1,kspec
   61           vd(i,j,k)=svd(j,k)**2*conjg(vd(i,j,k))
        endif
c  compute jackknife estimates of variance for pc amplitudes,
c  squared coherence, and phase
        do 80 i=1,nser
          do 80 j=1,nser
            sumdelc2=0.
            sumdelph=0.
            do 70 k=1,kspec
              temp=(sngl(abs(vd(i,j,k)))/(svd(j,k)*sqrt(specd(i,k))))         
              if (temp.lt.(.999999)) then
                del(k)=arctanh(temp)
              else
                del(k)=arctanh(.999999)
              end if
              sumdelc2=sumdelc2+del(k)
   70         sumdelph=sumdelph+vd(i,j,k)/abs(vd(i,j,k))
            sumdelc2=sumdelc2/xkspec
            sumdelph=sumdelph/xkspec
            var=0.
            do 72 k=1,kspec
   72         var=var+(sumdelc2-del(k))**2
            var=(xkspec-1.)*var/xkspec
            c2lo(i,j)=sqrt(var)
            if(abs(sumdelph).le.1.)then
              ppcj(i,j)=sqrt(2.*(xkspec-1.)*(1.-abs(sumdelph)))
            else
              ppcj(i,j)=tpi
            endif
            sumdela=0.
            do 75 k=1,kspec
              del(k)=log(abs(vd(i,j,k))/svd(j,k))
   75         sumdela=sumdela+del(k)
            sumdela=sumdela/xkspec
            var=0.
            do 77 k=1,kspec
   77         var=var+(sumdela-del(k))**2
            var=(xkspec-1.)*var/xkspec
   80       apcj(i,j)=exp(sqrt(var))
c  compute jackknife error on squared eigenvalues
   81   continue
        do 100 i=1,nser
          sum=0.
          do 95 j=1,kspec
            del(j)=log(svd(i,j)**2)
   95       sum=sum+del(j)
          sum=sum/xkspec
          var=0.
          do 97 j=1,kspec
   97       var=var+(sum-del(j))**2
          var=(xkspec-1.)*var/xkspec
  100     sv2j(i)=exp(sqrt(var))
        if(.not.swch)return
      endif
c
c  scale singular vectors so that first row of v is real
c
      do 110 j=1,nser
        t0=conjg(v(1,j))/abs(v(1,j))
        do 110 i=1,nser
  110     v(i,j)=t0*v(i,j)
c
c  compute eof amplitude and phase
c
      do 120 j=1,nser
        do 120 i=1,nser
          apc(i,j)=sv(j)*abs(v(i,j))
  120     ppc(i,j)=atan2(-dimag(v(i,j)),dreal(v(i,j)))
      if(scal)then
        do 121 i=1,nser
          scale=sqrt(spec(i))
          do 121 j=1,nser
  121       apc(i,j)=scale*apc(i,j)
      endif
c
c  compute squared coherences between eofs and data, along
c  with 95% confidence limits on coherence
c
      if(scal)then
        do 130 j=1,nser
          do 130 i=1,nser
  130       c2(i,j)=sv2(j)*abs(v(i,j))**2
      else
        do 131 j=1,nser
          do 131 i=1,nser
  131       c2(i,j)=sv2(j)*abs(v(i,j))**2/spec(i)
      endif
      if(jack)then
        do 140 j=1,nser
          do 140 i=1,nser
            if (sqrt(c2(i,j)).lt.(.999999)) then
              g=arctanh(sqrt(c2(i,j)))
            else
              g=arctanh(.999999)
            end if
            c2hi(i,j)=tanh(g+1.96*c2lo(i,j))**2
c
c  correct for lower error bar being higher than the mean
c
            gtanhl = tanh(g-1.96*c2lo(i,j))
            if (gtanhl.lt.0) then
              c2lo(i,j) = 0
            else        
              c2lo(i,j)=gtanhl**2
            end if
c
  140   continue
      endif
      return
      end

c $Id$ 
