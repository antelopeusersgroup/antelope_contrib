      subroutine mwtr1(ne,kspec,nser,jack,evalu,mxfreq,mxspec,yk,
     $                 mxwt,wt,sqev,a,b,x,rr,trd1,trd1s,trd1ws,
     $                 wtj,cd1,c2,trans,c2lo,c2hi,trerr)
c
c  calls qr
c
      double precision evalu,sqev,a,b,x,szz,rr,rho,wtjs,syy,
     $                 cd1av,var
      complex yk,trd1s,trd1,trd1ws,trans,t0,t1
      logical jack
      dimension evalu(kspec),yk(mxfreq,mxspec,nser),
     $          wt(mxwt,mxspec,nser),a(2*kspec,2*nser),
     $          b(2*kspec),x(2*nser),trd1s(nser),
     $          rr(2*nser,2*nser),trd1(nser,kspec),
     $          wtj(kspec),trd1ws(nser),c2(ne),
     $          trans(mxwt,nser-1),c2lo(ne),
     $          c2hi(ne),cd1(kspec),sqev(kspec),
     $          trerr(mxwt,nser-1)
      arctanh(z)=log((1.+z)/(1.-z))/2.
      nser1=nser-1
      kspec1=kspec-1
      do 1 i=1,kspec
    1   sqev(i)=sqrt(evalu(i))
      do 100 n=1,ne
c  solve for transfer functions by qr decomposition
        do 10 i=1,kspec
          b(i)=sqev(i)*wt(n,i,1)*real(yk(n,i,1))
          b(i+kspec)=sqev(i)*wt(n,i,1)*aimag(yk(n,i,1))
          do 10 j=1,nser1
            a(i,j)=sqev(i)*wt(n,i,j+1)*real(yk(n,i,j+1))
            a(i+kspec,j+nser1)=a(i,j)
            a(i,j+nser1)=-sqev(i)*wt(n,i,j+1)*aimag(yk(n,i,j+1))
   10       a(i+kspec,j)=-a(i,j+nser1)
        call qr(2*kspec,2*kspec,2*nser1,a,b,x,szz)
        if(szz.lt.0.)then
          write(6,*)' singular qr system detected'
          stop
        endif
        do 20 i=1,nser1
          trd1s(i)=0.
          trd1ws(i)=0.
   20     trans(n,i)=cmplx(x(i),x(i+nser1))
      if(jack)then
c  save upper triangular part
          do 25 i=1,2*nser1
            do 25 j=i,2*nser1
   25         rr(i,j)=a(i,j)
c  compute jackknife estimate of transfer function standard error
c  compute delete one solutions
          do 40 i=1,kspec
            k=1
            do 34 ii=1,kspec
              if(ii.eq.i)goto 34
              b(k)=sqev(ii)*wt(n,ii,1)*real(yk(n,ii,1))
              b(k+kspec1)=sqev(ii)*wt(n,ii,1)*aimag(yk(n,ii,1))
              do 32 j=1,nser1
                a(k,j)=sqev(ii)*wt(n,ii,j+1)*real(yk(n,ii,j+1))
                a(k+kspec1,j+nser1)=a(k,j)
                a(k,j+nser1)=-sqev(ii)*wt(n,ii,j+1)*aimag(yk(n,ii,j+1))
   32           a(k+kspec1,j)=-a(k,j+nser1)
              k=k+1
   34         continue
            call qr(2*kspec,2*kspec1,2*nser1,a,b,x,rho)
            cd1(i)=rho
            do 40 ii=1,nser1
              trd1(ii,i)=cmplx(x(ii),x(ii+nser1))
   40         trd1s(ii)=trd1s(ii)+trd1(ii,i)
c  compute inverse of hat matrix
c  solve for inverse of upper triangular part of a by back substitution
          do 46 j=1,2*nser1
            do 46 ii=2*nser1,1,-1
              a(ii,j)=0.
              if(ii.eq.j)a(ii,j)=1.
              do 44 jj=ii+1,2*nser1
   44           a(ii,j)=a(ii,j)-rr(ii,jj)*a(jj,j)
   46         a(ii,j)=a(ii,j)/rr(ii,ii)
          do 50 i=1,2*nser1
            do 50 j=1,2*nser1
              rr(i,j)=0.
              do 50 ii=1,2*nser1
   50           rr(i,j)=rr(i,j)+a(i,ii)*a(j,ii)
c  compute weight vector
          wtjs=0.
          do 60 i=1,kspec
            t1=0.
            do 54 ii=1,nser1
              t0=0.
              do 52 jj=1,nser1
   52           t0=t0+cmplx(rr(ii,jj),-rr(ii,jj+nser1))*
     $                wt(n,i,jj+1)*conjg(yk(n,i,jj+1))
   54         t1=t1+wt(n,i,ii+1)*yk(n,i,ii+1)*t0
            wtj(i)=real(t1)
            wtjs=wtjs+wtj(i)
            do 60 ii=1,nser1
   60         trd1ws(ii)=trd1ws(ii)+wtj(i)*trd1(ii,i)
c  form variance
          do 70 ii=1,nser1
   70       trerr(n,ii)=0.
          do 75 i=1,kspec
            do 75 ii=1,nser1
              t0=(wtjs-kspec*wtj(i))*trans(n,ii)+trd1s(ii)-trd1ws(ii)-
     $           kspec*(1.-wtj(i))*trd1(ii,i)
   75       trerr(n,ii)=trerr(n,ii)+abs(t0)**2/(kspec*(kspec-nser1))
          do 80 ii=1,nser1
   80       trerr(n,ii)=sqrt(trerr(n,ii))
c  compute multiple coherence
        endif
        syy=0.
        do 85 i=1,kspec
   85     syy=syy+evalu(i)*(wt(n,i,1)*abs(yk(n,i,1)))**2
        c2(n)=max((syy-szz)/syy,0.d0)
c  compute jackknifed estimate of coherence 95% confidence interval 
        if(jack)then
          cd1av=0.
          do 90 i=1,kspec
            syyd1=syy-evalu(i)*(wt(n,i,1)*abs(yk(n,i,1)))**2
            c2d1=max((syyd1-cd1(i))/syyd1,0.) 
            if ((sqrt(c2d1)).lt.(.999999)) then
              cd1(i)=arctanh(sqrt(c2d1))
            else
              cd1(i)=arctanh(.999999)
            end if
   90       cd1av=cd1av+cd1(i)
          cd1av=cd1av/kspec
          var=0.
          do 95 i=1,kspec
   95       var=var+(cd1av-cd1(i))**2
          err=sqrt((kspec-1.)*var/kspec)
c
c  correct for divide by zero in arctanh
c
          if (sqrt(c2(n)).lt.(.999999)) then
            c=arctanh(sqrt(c2(n)))
          else
            c=arctanh(.999999)
          end if
c
c  correct for lower error bar being higher than the mean
c
          ctanhl = tanh(c-1.96*err)
          if (ctanhl.lt.0) then
            c2lo(i) = 0
          else        
            c2lo(i)=ctanhl**2
          end if
c
          c2hi(n)=tanh(c+1.96*err)**2
        endif
  100   continue
      return
      end

c $Id$ 
