      subroutine qr(ndime,m, n, a, b, x, resq)
      implicit double precision (a-h, o-z)
c$$$$  calls no other routines
c  solves over-determined least-squares problem  ax = b
c  where  a  is an  m by n  matrix,  b  is an m-vector .
c  resq  is the sum of squared residuals of optimal solution.  also used
c  to signal error conditions - if -2 , system is underdetermined,  if
c  -1,  system is singular.
c  method - successive householder rotations.  see lawson+hanson - solv
c  -ing least squares problems.
c  routine will also work when m=n.
c*****   caution -  a and b  are overwritten by this routine.
      dimension a(ndime,1),b(1),x(1)
      double precision sum,dot
c
      resq=-2.0d0
      if (m.lt.n) return
c   loop ending on 1800 rotates  a  into upper triangular form
      do 1800 j=1,n
c  find constants for rotation and diagonal entry
      sq=0.0d0
      do 1100 i=j,m
 1100 sq=a(i,j)**2 + sq
      qv1=-dsign(dsqrt(sq),a(j,j))
      u1=a(j,j) - qv1
      a(j,j)=qv1
      j1=j + 1
      if (j1.gt.n) go to 1500
c  rotate remaining columns of sub-matrix
      do 1400 jj=j1,n
      dot=u1*a(j,jj)
      do 1200 i=j1,m
 1200 dot=a(i,jj)*a(i,j) + dot
      const=dot/dabs(qv1*u1)
      do 1300 i=j1,m
 1300 a(i,jj)=a(i,jj) - const*a(i,j)
      a(j,jj)=a(j,jj) - const*u1
 1400 continue
c  rotate  b  vector
 1500 dot=u1*b(j)
      if (j1.gt.m) go to 1610
      do 1600 i=j1,m
 1600 dot=b(i)*a(i,j) + dot
 1610 const=dot/dabs(qv1*u1)
      b(j)=b(j) - const*u1
      if (j1.gt.m) go to 1800
      do 1700 i=j1,m
 1700 b(i)=b(i) - const*a(i,j)
 1800 continue
c  solve triangular system by back-substitution.
      resq=-1.0d0
      do 2200 ii=1,n
      i=n-ii+1
      sum=b(i)
      if (ii.eq.1) go to 2110
      i1=i+1
      do 2100 j=i1,n
 2100 sum=sum - a(i,j)*x(j)
 2110 if (a(i,i).eq. 0.0d0) return
 2200 x(i)=sum/a(i,i)
c  find residual in overdetermined case.
      resq=0.0d0
      if (m.eq.n) return
      i1=n+1
      do 2300 i=i1,m
 2300 resq=b(i)**2 + resq
      return
      end

c $Id$ 
