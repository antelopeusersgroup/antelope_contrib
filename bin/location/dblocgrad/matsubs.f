      subroutine dinv(a,tdif,ma,natot,ndim,xdif,usvd,vsvd,
     +qsvd,tmp2)
c     invert differential traveltime matrix

      implicit real*8 (a-h,o-z)
      dimension a(ma,ndim),tdif(ma),xdif(ndim),
     +usvd(ma,ndim),vsvd(ndim,ndim),qsvd(ndim)
      dimension tmp2(ndim)
      data perq/0.01d-9/
      isvd=1
      mq=ndim+1
      ierr=0

c     decompose a into usvd*qsvd*vtsvd
      call svd(ma,ndim,natot,ndim,a,ndim,usvd,
     +vsvd,qsvd,isvd)
      call eigsrt(qsvd,vsvd,ndim,ndim,usvd,ma,natot)

c**   solve for xdif
      call mult(tdif,usvd,tmp2,1,ma,ndim,1,natot,ndim)
c     discard singular values smaller than perq
      do 560 j=2,ndim
         if (qsvd(j)/qsvd(1).lt.perq) mq=j
 560  continue
      do 3500 k=1,mq-1
            tmp2(k)=tmp2(k)/qsvd(k)
3500  continue
      do 3505 k=mq,ndim
            xx=qsvd(k)/qsvd(1)
            print *,'singular value ',k,' =  ',tmp2(k),
     +      'ratio ',xx,' is smaller than ',perq
            tmp2(k)=0.d0
 3505 continue
      nused=mq-1
      call mult(vsvd,tmp2,xdif,ndim,ndim,1,ndim,ndim,1)

      return
      end

      subroutine eigsrt(d,v,np,n,u,mp,m)
c     sorts eigenvalues and eigenvectors from tqli
      implicit real*8 (a-h,o-z)
      dimension d(np),v(np,np),u(mp,*)

      do 13 i=1,n-1
        k=i
        p=d(i)
        do 11 j=i+1,n
           if (d(j).ge.p) then
              k=j
              p=d(j)
           endif
 11     continue
        if (k.ne.i) then
           d(k)=d(i)
           d(i)=p
           do 12 j=1,n
                 p=v(j,i)
                 v(j,i)=v(j,k)
                 v(j,k)=p
 12        continue
           if (mp.gt.1) then
              do 14 j=1,m
                 p=u(j,i)
                 u(j,i)=u(j,k)
                 u(j,k)=p
 14           continue
           endif
        endif
 13   continue
      return
      end
      subroutine mult(a,b,c,nphy,mphy,lphy,nlog,mlog,llog)
c$$$$$ calls no other routines 
c  routine multiplies matrices a and b, putting the answer into c
c  a  is n by m  and b is m by l in dimension
      implicit real*8 (a-h,o-z)
      dimension a(nphy,mphy),b(mphy,lphy),c(nphy,lphy) 
      n=nlog
      m=mlog
      l=llog
      do 5 j=1,l
         do 10 i=1,n
            x=0.d0
            do 15 k=1,m
               x=x+a(i,k)*b(k,j)
 15         continue
            c(i,j)=x
 10      continue
  5   continue
      return
      end

c**********************
c      lm=l*m     
c      l1=0
c      do 1 j=1,lm,m     
c      do 1 i=1,n 
c      k1=i
c      k2=j
c      x=0.d0
c      do 2 k=1,m 
c      x=x+a(k1)*b(k2)   
c      k1=k1+n    
c 2    k2=k2+1    
c      l1=l1+1    
c 1    c(l1)=x    
c      return     
c      end
      subroutine svd(mphys,nphys,mlog,nlog,a,nda,u,v,q,index)      

c$$$$$ calls no other routines 
c  singular value decomposition)  for algo program see wilkinson+reinsch 
c  handbook for automatic computation vol 2 - linear algebra, pp140-144  
c  translated from algol by r.l.parker
c  the matrix a(m,n) is decomposed.  singular values in q, pre-matrix in u,     
c  post-matrix in v (NOT Vtranspose!!!)
c  index may be 1,2,3 or 4.  if 1, find u,v. if 2, find     
c  only u. if 3, find only v. if 4, find neither. in all cases, the array  u    
c  must be supplied as it is used as working space for the routine.      
c  can specify dimension nda in case alot of columns of a are zeroes
c  and you want to only the nonzero columns( there are nda nonzero 
c  columns). svd uses nda consecutive columns.
      implicit real*8 (a-h,o-z)
      dimension a(mphys,nda),u(mphys,nphys),v(nphys,nphys),q(nphys)
c     dimension a(m,n),u(m,n),v(n,n),q(n)    
      dimension e(500)  
c***** looks like n must be less than 500?

c  
      m=mlog
      n=nlog
      eps=5.0d-15
      tol=1.0d-291
      do 1100 i=1,m     
      do 1100 j=1,n     
 1100 u(i,j)=a(i,j)     
c  householder reduction to bi-diagonal form 
      g=0.0d0      
      x=0.0d0      
      do 2900 i=1,n     
      e(i)=g     
      s=0.0d0      
      l=i+1      
      do 2100 j=i,m     
 2100 s=u(j,i)**2 + s   
      if (s .lt. tol) go to 2500      
      f=u(i,i)   
      g=-dsign(dsqrt(s),f)
      h=f*g - s  
      u(i,i)=f - g      
      if (l.gt.n) go to 2501   
      do 2400 j=l,n     
      s=0.0d0      
      do 2200 k=i,m     
 2200 s=u(k,i)*u(k,j) + s      
      f=s/h      
      do 2300 k=i,m     
 2300 u(k,j)=u(k,j) + f*u(k,i) 
 2400 continue   
      go to 2501 
 2500 g=0.0d0      
c  
 2501 continue   
      q(i)=g     
      s=0.0d0      
      if (l.gt.n) go to 2601   
      do 2600 j=l,n     
 2600 s=u(i,j)**2 + s   
 2601 if (s.lt.tol) go to 2800 
      f=u(i,i+1) 
      g=-dsign(dsqrt(s),f)
      h=f*g - s  
      u(i,i+1)=f - g    
      if (l.gt.n) go to 2651   
      do 2650 j=l,n     
 2650 e(j)=u(i,j)/h     
 2651 continue   
      if (l.gt.m) go to 2850   
      do 2700 j=l,m     
      s=0.0d0      
      if (l.gt.n) go to 2700   
      do 2670 k=l,n     
 2670 s=u(j,k)*u(i,k) + s      
      do 2690 k=l,n     
 2690 u(j,k)=u(j,k) + s*e(k)   
 2700 continue   
      go to 2850 
 2800 g=0.0d0      
 2850 y=dabs(q(i)) + dabs(e(i))  
      if (y .gt. x) x=y 
 2900 continue   
c  
c  accumulation of right-hand transforms (v) 
c  
      go to (3000,3701,3000,3701),index      
 3000 continue   
      do 3700 iback=1,n 
      i=n+1-iback
      if (g .eq. 0.0d0) go to 3500      
      h=u(i,i+1)*g      
      if (l.gt.n) go to 3500   
      do 3100 j=l,n     
 3100 v(j,i)=u(i,j)/h   
      do 3400 j=l,n     
      s=0.0d0      
      do 3200 k=l,n     
 3200 s=u(i,k)*v(k,j) + s      
      do 3300 k=l,n     
 3300 v(k,j)=v(k,j) + s*v(k,i) 
 3400 continue   
 3500 continue   
      if (l.gt.n) go to 3601   
      do 3600 j=l,n     
      v(j,i)=0.0d0 
 3600 v(i,j)=0.0d0 
 3601 v(i,i)=1.0d0 
      g=e(i)     
      l=i 
 3700 continue   
 3701 continue   
c  
c  accumulation of left-hand transforms      
      go to (4000,4000,4701,4701),index      
 4000 continue   
      do 4700 iback=1,n 
      i=n+1-iback
      l=i+1      
      g=q(i)     
      if (l.gt.n) go to 4101   
      do 4100 j=l,n     
 4100 u(i,j)=0.0d0 
 4101 if (g.eq. 0.0d0) go to  4500      
      h=u(i,i)*g 
      if (l.gt.n) go to 4401   
      do 4400 j=l,n     
      s=0.0d0      
      do 4200 k=l,m     
 4200 s=u(k,i)*u(k,j) + s      
      f=s/h      
      do 4300 k=i,m     
 4300 u(k,j)=u(k,j) + f*u(k,i) 
 4400 continue   
 4401 continue   
      do 4550 j=i,m     
 4550 u(j,i)=u(j,i)/g   
      go to 4700 
 4500 continue   
      do 4600 j=i,m     
 4600 u(j,i)=0.0d0 
 4700 u(i,i)=u(i,i) + 1.0d0      
c  
c  diagonalization of bi-diagonal form
 4701 eps=eps*x  
      do 9000 kback=1,n 
      k=n+1-kback
c  test f-splitting     
 5000 continue   
      do 5100 lback=1,k 
      l=k+1-lback
      if (dabs(e(l)).le. eps) go to 6500      
      if (dabs(q(l-1)) .le. eps) go to 6000   
 5100 continue   
c  cancellation of e(l), if l.gt. 1   
 6000 c=0.0d0      
      s=1.0d0 
      l1=l - 1   
      do 6200 i=l,k     
      f=s*e(i)   
      e(i)=c*e(i) 
      if (dabs(f) .le. eps) go to 6500 
      g=q(i)     
      q(i)=dsqrt(f*f + g*g)     
      h=q(i)     
      c=g/h      
      s=-f/h     
      go to (6050,6050,6200,6200),index      
 6050 continue   
      do 6100 j=1,m     
      y=u(j,l1)  
      z=u(j,i)   
      u(j,l1)=y*c + z*s 
      u(j,i)=-y*s + z*c 
 6100 continue   
 6200 continue   
c  test f-convergence   
 6500 z=q(k)     
      if (l .eq. k) go to  8000
c  shift from bottom 2 x 2 minor      
      x=q(l)     
      y=q(k-1)   
      g=e(k-1)   
      h=e(k)     
      f=((y-z)*(y+z) + (g-h)*(g+h))/(2.0d0*h*y)
      g=dsqrt(f*f + 1.0d0) 
      f=((x-z)*(x+z) + h*(y/(f + dsign(g,f))-h))/x   
c  next q-r transformation     
      c=1.0d0      
      s=1.0d0      
      lplus=l + 1
      do 7500 i=lplus,k 
      g=e(i)     
      y=q(i)     
      h=s*g      
      g=c*g      
      z=dsqrt(f*f + h*h) 
      e(i-1)=z   
      c=f/z      
      s=h/z      
      f=x*c + g*s
      g=-x*s + g*c      
      h=y*s      
      y=y*c      
      go to (7100,7201,7100,7201),index      
 7100 do 7200 j=1,n     
      x=v(j,i-1) 
      z=v(j,i)   
      v(j,i-1)=x*c + z*s
      v(j,i)=-x*s + z*c 
 7200 continue   
 7201 z=dsqrt(f*f + h*h) 
      q(i-1)=z   
      c=f/z      
      s=h/z      
      f=c*g + s*y
      x=-s*g + c*y      
      go to (7300,7300,7500,7500),index      
 7300 do 7400 j=1,m     
      y=u(j,i-1) 
      z=u(j,i)   
      u(j,i-1)=y*c + z*s
      u(j,i)=-y*s + z*c 
 7400 continue   
 7500 continue   
      e(l)=0.0d0   
      e(k)=f     
      q(k)=x     
      go to  5000
c  convergence   
 8000 if (z .ge. 0.0d0) go to 9000      
c  q is made non-negative      
      q(k)=-z    
      go to (8100,9000,8100,9000),index      
 8100 do 8200 j=1,n     
 8200 v(j,k)=-v(j,k)    
 9000 continue   
      return     
      end     

      subroutine mfeout (a,mda,m,n,title,mode,lun)
c     for matrix output with labeling
c
c     a() matrix to be output
c         mda  1st physical dim of array a
c         m    logical dim rows of a
c         n    logical dim columns of a
c     mode     ==1 for 4p8f12.0 format for v matrix
c              ==2 for 8e15.8 format for candidate soln
c              ==0 for 8(3x,g11.4) format
c     lun      unit number to write output to
c
      implicit real*8 (a-h,o-z)
      dimension a(mda,n)
      logical notblk
      character *6 ihead(2),iblank
      character *20 title
      data maxcol/8/, iblank/' '/,ihead(1)/' col'/,ihead(2)/'soln'/       
c
      if (mode.eq.0) then
         write (lun,85) title
      else if (mode.eq.2) then
         write (lun,80)
      else if (mode.eq.1) then
         write (lun,70)
      endif
c
      nblock=n/maxcol
      last=n-nblock*maxcol
      ncol=maxcol
      j1=1
c
c**   main loop starts
c
 30   continue
      if (nblock.le.0) then
         if (last.le.0) return
         ncol=last
         last=0
      endif
      
      j2=j1+ncol-1
      if (mode.ne.0) then
        write (lun,90) (ihead(mode),j,j=j1,j2)
      else
        write (lun,90) (ihead(1),j,j=j1,j2)
      endif
c
      do 60 i=1,m
          if (mode.eq.0) then
             write (lun,120) i,(a(i,j),j=j1,j2)
          else if (mode.eq.2) then 
             write (lun,110) i,(a(i,j),j=j1,j2)
          else if (mode.eq.1) then
             write (lun,100) i,(a(i,j),j=j1,j2)
          endif
 60   continue                
c
      j1=j1+maxcol
      nblock=nblock-1
      goto 30
c
 70   format ('v-matrix of the singular value decomposition')
 80   format ('sequence of candidate solutions', 1X)
 85   format (a20)
 90   format (5x,8(3x,a4,i4,3x)/1x)
 100  format (1x,i3,1x,4p8f12.0)
 110  format (1x,i3,1x,8e15.8)
 120  format (1x,i3,1x,8(3x,g11.4))
      end


c $Id$ 
