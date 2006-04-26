      subroutine hext(smt,ev,mcov,delm,wroot,wegnv,windx)
      real*4 mcov(6,6)
      dimension smt(6),ev(3,3)
      dimension ah(6,1),temp(1,6),se(1,1),wvec(4)
      dimension ah1(6,1),ah2(6,1),ah2d(6,2),temp2(2,6),w(2,2)
      dimension wev(2,2),indx(3),wroot(2,3),windx(2,3)
      dimension wegnv(2,2,3),delm(3)
      equivalence (ah2d(1,1),ah1(1,1)),(ah2d(1,2),ah2(1,1))
      data indx/1,3,6/
c
c     computation of error on eigenvalue
c
      do 5 i=1,3
c
c     form ahat matrix, p.356 of hext
c
      call ahat(ev(1,i),ev(1,i),ah)
c
c     form ahat(transpose)* at*a inv
c
      call mult(ah,-1,mcov,1,temp,1,6,6,6,6,1,0)
c
c     form ahat(t) * a(t)*a inv * ahat
c
      call mult(temp,1,ah,1,se,1,6,1,1,6,1,0)
c
5     delm(i)=sqrt(se(1,1))
c
c     computation of error elipse on eigenvector
c
      do 10 i=1,3
      i1=i+1
      i2=i+2
      if(i1.gt.3) i1=i1-3
      if(i2.gt.3) i2=i2-3
c
c     calculate wihat using eq. 5.6 in hext(1963).
c     first compute ahats
c
      call ahat(ev(1,i1),ev(1,i),ah1)
      call ahat(ev(1,i2),ev(1,i),ah2)
c
c     scale the ahats
c
      in0=indx(i)
      in1=indx(i1)
      in2=indx(i2)
      scale1=1.0/(smt(in0)-smt(in1))
      scale2=1.0/(smt(in0)-smt(in2))
c
      do 15 j=1,6
      ah1(j,1)=scale1*ah1(j,1)
15    ah2(j,1)=scale2*ah2(j,1)
c
c     form ah2d(t) * a(t)*a inv
c
      call mult(ah2d,-1,mcov,1,temp2,2,6,6,6,6,2,0)
c
c     form ah2d(t) * a(t)*a inv * ah2d
c
      call mult(temp2,1,ah2d,1,w,2,6,2,2,6,2,0)
c
c     form matrix to be diagonalized
c
      wvec(1)=w(1,1)
      wvec(2)=w(1,2)
      wvec(3)=w(2,2)
c
      call eigens(wvec,wev,2,0)
c
      wroot(1,i)=wvec(1)
      wroot(2,i)=wvec(3)
      wegnv(1,1,i)=wev(1,1)
      wegnv(2,1,i)=wev(2,1)
      wegnv(1,2,i)=wev(1,2)
      wegnv(2,2,i)=wev(2,2)
c
      windx(1,i)=in1
      windx(2,i)=in2
c
10    continue
c
      return
      end
