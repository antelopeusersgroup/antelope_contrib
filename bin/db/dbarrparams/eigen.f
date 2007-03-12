



      subroutine eigen(a,r,n,mv)
c
c Computes the eigenvalues and eigenvectors of a real symmetric matrix.
c
c Usage:
c       call eigen(a,r,n,mv)
c
c Description of parameters:
c       "a" - original matrix (symmetric), destroyed in computation.
c             Resultant eigenvalues are developed in diagonal of
c             matrix in descending order.
c       "r" - resultant matrix of eigenvectors (stored columnwise, in
c	      same order as eigenvalues)
c       "n" - order of matrices "a" and "r"
c      "mv" - input code
c             .ne.1  compute egenvalues and eigenvectors
c             .eq.1  compute eigenvalues only ("r" need not be dimensioned
c             but must still appear in calling sequence)
c
c Remarks:
c       Original matrix "a" must be real symmetric (storage mode =1)
c       stored either as the upper triangular part by columns or the
c       lower triangular part by rows (including main diagonal)
c       Matrix "a" cannot be in same location as matrix "r" 
c
c Method:
c       Diagonalization method originated by Jacobi and adapted by
c       von Neumann as found in "Mathematical
c       Methods for Digital Computers" edited by Ralston and Wilf,
c       John Wiley and sons, New York, 1962, chapter 7.
c       This program copied from "Cluster Analysis for Applications",
c       by Anderberg, p 242.
c
c ...................................................................
c
      dimension a(1),r(1)
c
c ...................................................................
c
c     If a double precision version of this routine is desired, the
c     c in column 1 should be removed from the double precision
c     statement which follows.
c    double precision a,r,anorm,anrmx,thr,x,y,sinx,sinx2,cosx,cosx2,
c   *                 sincs,range
c
c     The double precision version of this subroutine must also contain
c     double precision fortran functions. sqrt in statements 40,68,75
c     and 78 must be changed to dsqrt. abs in statement 62 must be
c     changed to dabs. The constant in statement 5 should be changed to
c     1.0d-12.
c
c ...................................................................
c
c generate identity matrix
c

	character*130 rcsid
	data rcsid/'$Header$'/

 5    range     = 1.0e-12
      if (mv-1) 10,25,10
 10   iq        = -n
      do 20 j   = 1,n
      iq        = iq + n
      do 20 i   = 1,n
      ij        = iq + i
      r(ij)     = 0.
      if (i-j) 20,15,20
 15   r(ij)     = 1.
 20   continue
c
c compute initial and final norms (anorm and anormx)
c
 25   anorm     = 0.
      do 35 i   = 1,n
      do 35 j   = i,n
      if (i-j) 30,35,30
 30   ia        = i + (j*j-j)/2
      anorm     = anorm + a(ia)*a(ia)
 35   continue
      if (anorm) 165,165,40
 40   anorm     = 1.414e0*sqrt(anorm)
      anrmx     = anorm*range/float(n)
c
c initialize indicators and compute threshold thr
c
      ind       = 0
      thr       = anorm
 45   thr       = thr/float(n)
 50   l         = 1
 55   m         = l + 1
c
c compute sin and cos
c
 60   mq        = (m*m-m)/2
      lq        = (l*l-l)/2
      lm        = l + mq
 62   if (abs(a(lm))-thr) 130,65,65
 65   ind       = 1
      ll        = l + lq
      mm        = m + mq
      x         = .5*(a(ll) - a(mm))
 68   y         = -a(lm)/sqrt(a(lm)*a(lm)+x*x)
      if (x) 70,75,75
 70   y         = -y
 75   sinx      = y/sqrt(2.*(1.+(sqrt(1.-y*y))))
      sinx2     = sinx*sinx
 78   cosx      = sqrt(1.-sinx2)
      cosx2     = cosx*cosx
      sincs     = sinx*cosx
c
c rotate l and m columns
c
      ilq       = n*(l-1)
      imq       = n*(m-1)
      do 125 i  = 1,n
      iq        = (i*i-i)/2
      if (i-l) 80,115,80
 80   if (i-m) 85,115,90
 85   im        = i + mq
      go to 95
 90   im        = m + iq
 95   if (i-l) 100,105,105
 100  il        = i + lq
      go to 110
 105  il        = l + iq
 110  x         = a(il)*cosx - a(im)*sinx
      a(im)     = a(il)*sinx + a(im)*cosx
      a(il)     = x
 115  if (mv-1) 120,125,120
 120  ilr       = ilq + i
      imr       = imq + i
      x = r(ilr)*cosx - r(imr)*sinx
      r(imr)    = r(ilr)*sinx + r(imr)*cosx
      r(ilr)    = x
 125  continue
      x         = 2.*a(lm)*sincs
      y         = a(ll)*cosx2 + a(mm)*sinx2 - x
      x         = a(ll)*sinx2 + a(mm)*cosx2 + x
      a(lm)     = (a(ll)-a(mm))*sincs + a(lm)*(cosx2-sinx2)
      a(ll)     = y
      a(mm)     = x
c
c tests for completion
c
c test for m = last column
c
 130  if (m-n) 135,140,135
 135  m         = m + 1
      go to 60
c
c test for l = second from last column
c
 140  if (l-(n-1))  145,150,145
 145  l         = l + 1
      go to 55
 150  if (ind-1)  160,155,160
 155  ind       = 0
      go to 50
c
c compare threshold with final norm
c
 160  if (thr-anrmx)  165,165,45
c
c sort eigenvalues and eigenvectors
c
 165  iq        = -n
      do 185 i  = 1,n
      iq        = iq + n
      ll        = i + (i*i-i)/2
      jq        = n*(i-2)
      do 185 j  = i,n
      jq        = jq + n
      mm        = j + (j*j-j)/2
      if (a(ll)-a(mm))  170,185,185
 170  x         = a(ll)
      a(ll)     = a(mm)
      a(mm)     = x
      if (mv-1) 175,185,175
 175  do 180 k  = 1,n
      ilr       = iq + k
      imr       = jq + k
      x         = r(ilr)
      r(ilr)    = r(imr)
 180  r(imr)    = x
 185  continue
c
      return
      end

c $Id$ 
