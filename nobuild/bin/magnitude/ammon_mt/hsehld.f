c
c     routine from Chuck Langston - momvert
c
c ------------------------ hsehld --------------------------------
      subroutine hsehld(nn,nsiz,a,vec,eig)
c         nn= size of matrix
c         a = matrix (only lower triangle is used + this is destroyed)
c         eig = returned eigenvalues in algebraic descending order
c         vec = returned eigenvectors in columns
c         ind = error return indicator
c              0 for normal return
c              1 sum of eigenvalues not equal to trace
c              2 sum of eigenvalues squared not equal to norm
c              3 both of these errors
c     the following dimensioned variables are equivalenced
      dimension a(nsiz,nsiz),vec(nsiz,nsiz)
      dimension eig(nsiz,5)
      dimension neig(100)
      if(nsiz.gt.100) go to 565
      n = iabs(nn)
      nn1 = n - 1
      do 1 i = 1, nn1
      ii = i + 1
      do 1 j = ii,n
  1   a(j,i) = a(i,j)
c     reset error return indicator
      ind=0
      if(n .eq. 0) go to 560
      n1=n-1
      n2=n-2
c     compute the trace and euclidian norm of the input matrix
c     later check against sum and sum of squares of eigenvalues
      enorm=0.
      trace=0.
      do 110 j=1,n
      do 100 i=j,n
  100 enorm=enorm+a(i,j)**2
      trace=trace+a(j,j)
  110 enorm=enorm-.5*a(j,j)**2
      enorm=enorm+enorm
      eig(1,3)=a(1,1)
      if(n2) 280,270,120
  120 do 260 nr=1,n2
      b=a(nr+1,nr)
      s=0.
      do 130 i=nr,n2
  130 s=s+a(i+2,nr)**2
c     prepare for possible bypass of transformation
      a(nr+1,nr)=0.
      if(s) 250,250,140
  140 s=s+b*b
      sgn=+1.
      if(b) 150,160,160
  150 sgn=-1.
  160 sqrts=sqrt(s)
      d=sgn/(sqrts+sqrts)
      temp=sqrt(.5+b*d)
      eig(nr,5)=temp
      a(nr+1,nr)=temp
      b=-sgn*sqrts
c     d is factor of proportionality. now compute and save w vector.
      d=d/temp
c     extra singly subscripted w vector used for speed.
      do 170 i=nr,n2
      temp=d*a(i+2,nr)
      eig(i+1,5)=temp
  170 a(i+2,nr)=temp
c     premultiply vector w by matrix a to obtain p vector.
c     simultaneously accumulate dot product wp,(the scalar k)
      wtaw=0.
      do 220 i=nr,n1
      sum=0.
      do 180 j=nr,i
  180 sum=sum+a(i+1,j+1)*eig(j,5)
      i1=i+1
      if(n1-i1) 210,190,190
  190 do 200 j=i1,n1
  200 sum=sum+a(j+1,i+1)*eig(j,5)
  210 eig(i,2)=sum
  220 wtaw=wtaw+sum*eig(i,5)
c     p vector and scalar k now stored. next compute q vector
      do 230 i=nr,n1
  230 eig(i,2)=eig(i,2)-wtaw*eig(i,5)
c     now form pap matrix, required part
      do 240 j=nr,n1
      qj=eig(j,2)
      wj=eig(j,5)
      do 240 i=j,n1
  240 a(i+1,j+1)=a(i+1,j+1)-2.*(eig(i,5)*qj+wj*eig(i,2))
  250 eig(nr,2)=b
      eig(nr,4)=b**2
  260 eig(nr+1,3)=a(nr+1,nr+1)
  270 b=a(n,n-1)
      eig(n-1,2)=b
      eig(n-1,4)=b**2
      eig(n,3)=a(n,n)
  280 eig(n,4)=0.
c     adjoin an identity matrix to be postmultiplied by rotations.
      do 300 i=1,n
      do 290 j=1,n
  290 vec(i,j)=0.
  300 vec(i,i)=1.
      m=n
      sum=0.
      npas=1
      go to 400
  310 sum=sum+shift
      cosa=1.
      g=eig(1,3)-shift
      pp=g
      ppbs=pp*pp+eig(1,4)
      ppbr=sqrt(ppbs)
      do 370 j=1,m
      cosap=cosa
      if(ppbs .ne. 0.) go to 320
      sina=0.
      sina2=0.
      cosa=1.
      go to 350
  320 sina=eig(j,2)/ppbr
      if(abs(sina) .lt. 1.0e-25)sina=0.0
      sina2=eig(j,4)/ppbs
      cosa=pp/ppbr
c     postmultiply identity by p-transpose matrix
      nt=j+npas
      if(nt .ge. n) nt=n
  330 do 340 i=1,nt
      temp=cosa*vec(i,j)+sina*vec(i,j+1)
      if(abs(temp) .lt. 1.0e-38) temp=0.0
      vec(i,j+1)=-sina*vec(i,j)+cosa*vec(i,j+1)
  340 vec(i,j)=temp
  350 dia=eig(j+1,3)-shift
      u=sina2*(g+dia)
      eig(j,3)=g+u
      g=dia-u
      pp=dia*cosa-sina*cosap*eig(j,2)
      if(j .ne. m) go to 360
      eig(j,2)=sina*pp
      eig(j,4)=sina2*pp**2
      go to 380
  360 ppbs=pp**2+eig(j+1,4)
      ppbr=sqrt(ppbs)
      eig(j,2)=sina*ppbr
  370 eig(j,4)=sina2*ppbs
  380 eig(m+1,3)=g
c     test for convergence of last diagonal element
      npas=npas+1
      if(eig(m,4).gt.1.e-21) go to 410
  390 eig(m+1,1)=eig(m+1,3)+sum
  400 eig(m,2)=0.
      eig(m,4)=0.
      m=m-1
      if(m .eq. 0) go to 430
      if(eig(m,4).le.1.e-21) go to 390
c     take root of corner 2 by 2 nearest to lower diagonal in value
c     as estimate of eigenvalue to use for shift
  410 a2=eig(m+1,3)
      r2=.5*a2
      r1=.5*eig(m,3)
      r12=r1+r2
      dif=r1-r2
      temp=sqrt(dif**2+eig(m,4))
      r1=r12+temp
      r2=r12-temp
      dif=abs(a2-r1)-abs(a2-r2)
      if(dif .lt. 0.) go to 420
      shift=r2
      go to 310
  420 shift=r1
      go to 310
  430 eig(1,1)=eig(1,3)+sum
c     use sortd2 to sort the eigenvalues
      call sortd2(eig,n,neig)
      do 701 i=1,n
701   eig(i,5)=neig(i)
      if(nn.lt.0) go to 555
      if(n1 .eq. 0) go to 500
c     initialize auxiliary tables required for rearranging the vectors
      do 440 k=1,n
      eig(k,4)=k
  440 eig(k,3)=k
      do 490 l=1,n1
      nv=eig(l,5)
      np=eig(nv,4)
      if(np.eq.l) go to 490
      lv=eig(l,3)
      eig(np,3)=lv
      eig(lv,4)=np
      do 480 i=1,n
      temp=vec(i,l)
      vec(i,l)=vec(i,np)
  480 vec(i,np)=temp
  490 continue
  500 esum=0.
      essq=0.
c     back transform the vectors of the triple diagonal matrix
      do 550 nrr=1,n
      k=n1
  510 k=k-1
      if(k .le. 0) go to 540
      sum=0.
      do 520 i=k,n1
  520 sum=sum+vec(i+1,nrr)*a(i+1,k)
      sum=sum+sum
      do 530 i=k,n1
  530 vec(i+1,nrr)=vec(i+1,nrr)-sum*a(i+1,k)
      go to 510
  540 esum=esum+eig(nrr,1)
  550 essq=essq+eig(nrr,1)**2
      temp=abs(128.*trace)
      if((abs(trace-esum)+temp)-temp .ne. 0.) ind=ind+1
      temp=256.*enorm
      if((abs(enorm-essq)+temp)-temp .ne. 0.) ind=ind+2
  555 continue
  560 return
  565 write(8,600)
  600 format(1h0,50hmessage from hsehld routine array size exceeds 100)
      stop
      end
c ------------------------ sortd2 --------------------------------
      subroutine sortd2(a,n,idx)
      dimension a(n),idx(n)
      if (n.eq.1) go to 65
      if (n.le.0) go to 60
      do 1 i = 1,n
      idx(i) = i
    1 continue
      n2 = n/2
      n21 = n2 + 2
      ict=1
      i=2
   11 n1=n21-i
      nn=n
      ik=n1
   15 c=a(ik)
      ic=idx(ik)
  100 jk=2*ik
      if (jk.gt.nn) go to 140
      if (jk.eq.nn) go to 120
c     if (a(jk+1)-a(jk)) 110,120,120
       if (a(jk+1).ge.a(jk)) go to 120
  110 jk=jk+1
c 120 if (a(jk)-c) 200,140,140
  120 if (a(jk).ge. c) go to 140
  200 a(ik)=a(jk)
      idx(ik)=idx(jk)
      ik=jk
      go to 100
  140 a(ik)=c
      idx(ik)=ic
      go to (3,45) ,ict
    3 if (i.ge.n2) go to 35
      i=i+1
      go to 11
   35 ict=2
      np2=n+2
      i=2
   37 n1=np2-i
      nn=n1
      ik=1
      go to 15
  45  continue
      t = a(1)
      a(1) = a(n1)
      a(n1) = t
      it = idx(1)
      idx(1) = idx(n1)
    4 idx(n1) = it
      if (i.ge.n) go to 55
      i=i+1
      go to 37
   55 return
   60 write(8,500)
  500 format(1h0,51herror return from sortd2--n less than or equal to 0)
      stop
   65 idx(1)=1
      return
      end
c
c
