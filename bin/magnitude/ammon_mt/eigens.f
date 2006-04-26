      subroutine eigens(a,r,n,mv)
c
c     purpose:
c         compute eigenvalues and eigenvectors of a real symmetric
c         matrix
c
c     usage:
c         call eigen(a,r,n,mv)
c
c     description of parameters:
c         a - original matrix (symmetric), destroyed in computation
c             resultant eigenvalues are developed in dagonal of
c             matrix a in descending order.
c         r - resultant matrix of eigenvectors (stored comunmwixe,
c             in same sequence as eigenvalues
c         n - order of matrix a and r
c         mv- input code
c         0 compute eigen values and eigenvectors
c         1 compute eigenvalues only (r need not be dimensioned
c           but must still appeat in calling
c           sequence)
c
c     remarks::
c         origenal matrix a must be real symmetric (storage mode 1)
c         matrix r cannot be in same place as a
c
c     subroutine and functions required:
c         none
c
c     method:
c         diagonalization method originated by jacobi and adapted
c         by von neumann for lage computers as found in 'mathematical
c         methods for digital computers', edited by a. ralston and
c         h.s.wilf, john riley and sons, new york, 1962, chapter 7.
c
      dimension a(1000),r(1000)
c-----generate identity matrix
      range=1.0e-6
      if(mv-1)10,25,10
 10   iq=-n
      do 20 j=1,n
      iq=iq+n
      do 20 i=1,n
      ij=iq+i
      r(ij)=0.
      if(i-j)20,15,20
 15   r(ij)=1.
 20   continue
c-----compute initial and final norms
 25   anorm=0.
      do 35 i=1,n
      do 35 j=i,n
      if(i-j)30,35,30
 30   ia=i+(j*j-j)/2
      anorm=anorm+a(ia)*a(ia)
 35   continue
      if(anorm)165,165,40
 40   anorm=1.414*sqrt(anorm)
      anrmx=anorm*range/float(n)
c-----initialize indicators and compute threshold, thr
      ind=0
      thr=anorm
 45   thr=thr/float(n)
 50   l=1
 55   m=l+1
c-----compute sin and cos
 60   mq=(m*m-m)/2
      lq=(l*l-l)/2
      lm=l+mq
      if(abs(a(lm))-thr)130,65,65
 65   ind=1
      ll=l+lq
      mm=m+mq
      x=0.5*(a(ll)-a(mm))
      y=-a(lm)/sqrt(a(lm)*a(lm)+x*x)
      if(x)70,75,75
 70   y=-y
 75   argq=1.0-y*y
      if(argq.lt.0.0) argq=0.0
      sinx=y/sqrt(2.0*(1.0+(sqrt(argq))))
      sinx2=sinx*sinx
      argq=1.0-sinx2
      if(argq.lt.0.0) argq=0.0
      cosx=sqrt(argq)
      cosx2=cosx*cosx
      sincs=sinx*cosx
c-----rotate l and m column
      ilq=n*(l-1)
      imq=n*(m-1)
      do 125 i=1,n
      iq=(i*i-i)/2
      if(i-l) 80,115,80
 80   if(i-m) 85,115,90
 85   im=i+mq
      go to 95
 90   im=m+iq
 95   if(i-l) 100,105,105
 100  il=i+lq
      go to 110
 105  il=l+iq
 110  x=a(il)*cosx-a(im)*sinx
      a(im)=a(il)*sinx+a(im)*cosx
      a(il)=x
 115  if(mv-1) 120,125,120
 120  ilr=ilq+i
      imr=imq+i
      x=r(ilr)*cosx-r(imr)*sinx
      r(imr)=r(ilr)*sinx+r(imr)*cosx
      r(ilr)=x
 125  continue
      x=2.0*a(lm)*sincs
      y=a(ll)*cosx2+a(mm)*sinx2-x
      x=a(ll)*sinx2+a(mm)*cosx2+x
      a(lm)=(a(ll)-a(mm))*sincs+a(lm)*(cosx2-sinx2)
      a(ll)=y
      a(mm)=x
c-----test for completion; m=last column
 130  if(m-n) 135,140,135
 135  m=m+1
      go to 60
c-----test for l = second from last column
 140  if(l-(n-1)) 145,150,145
 145  l=l+1
      go to 55
 150  if(ind-1) 160,155,160
 155  ind=0
      go to 50
c------compare threshold with final norm
 160  if(thr-anrmx) 165,165,45
c------sort eigenvalues and eigenvectors
 165  iq=-n
      do 185 i=1,n
      iq=iq+n
      ll=i+(i*i-i)/2
      jq=n*(i-2)
      do 185 j=i,n
      jq=jq+n
      mm=j+(j*j-j)/2
      if(a(ll)-a(mm)) 170,185,185
 170  x=a(ll)
      a(ll)=a(mm)
      a(mm)=x
      if(mv-1) 175,185,175
 175  do 180 k=1,n
      ilr=iq+k
      imr=jq+k
      x=r(ilr)
      r(ilr)=r(imr)
 180  r(imr)=x
 185  continue
      return
      end
