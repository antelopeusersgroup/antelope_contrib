c   imsl routine name   - mdfd
c
c-----------------------------------------------------------------------
c
c   computer            - cray/single
c
c   latest revision     - august 1, 1981
c
c   purpose             - f probability distribution function
c
c   usage               - call mdfd (f,n1,n2,p,ier)
c
c   arguments    f      - input constant to which integration is
c                           performed. f must be greater than or equal
c                           to zero.
c                n1     - input first degree of freedom. a positive
c                           integer.
c                n2     - input second degree of freedom, a positive
c                           integer.
c                p      - output probability that a random variable
c                           following the f distribution with degrees
c                           of freedom n1 and n2 will be less than or
c                           equal to input f.
c                ier    - error parameter. (output)
c                           terminal error
c                             ier = 129 indicates either n1 or n2 is
c                               less than one or n1+n2 is greater than
c                               20,000.  p is set to positive machine
c                               infinity.
c                             ier = 130 indicates f is less than zero.
c                               p is set to positive machine infinity.
c
c   precision/hardware  - single/all
c
c   reqd. imsl routines - merrc=erfc,uertst,ugetio
c
c   notation            - information on special notation and
c                           conventions is available in the manual
c                           introduction or through imsl routine uhelp
c
c
c   warranty            - imsl warrants only that imsl testing has been
c                           applied to this code. no other warranty,
c                           expressed or implied, is applicable.
c
c-----------------------------------------------------------------------
c
      subroutine mdfd   (f,n1,n2,p,ier)
c
c                                  specifications for arguments
      integer            n1,n2,ier
      real f,p
c                                  specifications for local variables
      integer            i1,i2p,i2,i,l2,mnm,mxm
      real acons,a,bige,b,cbr1,cbr2,c,dpl,dp,f1f,f1,f2p,
     1                   f2,r1d3,r2d9,r2dpi,rinfp,sts,s,temp1,temp,
     2                   theta,vp,x1,x2,xi
      data               rinfp/1.e37/
      data               r2d9/.22222222222222/,r1d3/.33333333333333/
c                                  r2dpi = 2/pi
      data               r2dpi/.63661977236758/
      data               bige/85.195648/,acons/1.e32/
c                                  first executable statement
c                                  test for invalid input
      mxm = max(n1,n2)
      mnm = min(n1,n2)
      if (mnm.lt.1.or.mxm.gt.(20000-mnm)) go to 100
      if (f.lt.0.0) go to 105
      ier = 0
      if (f.eq.0.0) go to 115
      f1 = n1
      f2 = n2
      dp = 0.0
      vp = f1+f2-2.0
      f1f = f1*f
      f2p = f2+f1f
      x1 = f2/f2p
      x2 = 1.0-x1
      if (x2.eq.0.0) go to 115
      if ((n1/2)*2-n1.eq.0.and.n1.le.500) go to 5
      if ((n2/2)*2-n2.eq.0.and.n2.le.500) go to 30
      if (n1+n2.le.500) go to 55
      f1 = r2d9/f1
      f2 = r2d9/f2
      cbr1 = r1d3*log(f)
      if (abs(cbr1).gt.bige) go to 120
      cbr1 = exp(cbr1)
      cbr2 = cbr1*cbr1
      s = (cbr1*(1.0-f2)-1.0+f1)/sqrt(f1+cbr2*f2)
      p=.70710678118655
      p = .5*erfc(-p*s)
      go to 95
c                                  n1 is even and less than 500
    5 temp1 = 0.
      temp = .5*f2*log(x1)
      if (n1.eq.2) go to 25
      i1 = n1-2
      xi = f1
      do 10 i2=2,i1,2
         l2 = i2
         xi = xi-2.
         vp = vp-2.
         dp = x2*vp/xi*(1.+dp)
         if (dp.gt.acons) go to 15
   10 continue
      go to 25
   15 if (l2.ge.i1) go to 25
      dpl = log(dp)
      i2p = l2+2
      xi = f1-i2p
      do 20 i2=i2p,i1,2
         vp = vp-2.
         dpl = dpl+log(x2*vp/xi)
         xi = xi-2.
   20 continue
      temp = temp+dpl
      if (abs(temp).le.bige) temp1 = exp(temp)
      p = 1.-temp1
      go to 95
   25 if (abs(temp).le.bige) temp1 = exp(temp)
      p = 1.0-temp1*(1.0+dp)
      go to 95
c                                  n2 is even and less than 500
   30 temp1 = 0.
      temp = .5*f1*log(x2)
      if (n2.eq.2) go to 50
      i1 = n2-2
      xi = f2
      do 35 i2=2,i1,2
         l2 = i2
         xi = xi-2.
         vp = vp-2.
         dp = x1*vp/xi*(1.+dp)
         if (dp.gt.acons) go to 40
   35 continue
      go to 50
   40 if (l2.ge.i1) go to 50
      dpl = log(dp)
      i2p = l2+2
      xi = f2-i2p
      do 45 i2=i2p,i1,2
         vp = vp-2.
         dpl = dpl+log(x1*vp/xi)
         xi = xi-2.
   45 continue
      temp = temp+dpl
      if (abs(temp).le.bige) temp1 = exp(temp)
      p = temp1
      go to 95
   50 if (abs(temp).le.bige) temp1 = exp(temp)
      p = temp1*(1.+dp)
      go to 95
c                                  sum of dfs are le 500 and odd
   55 dp = sqrt(f1f/f2)
      theta = atan(dp)
      sts = f1f/f2p
      a = 0.0
      b = 0.0
      if (n2.eq.1) go to 70
      if (n2.eq.3) go to 65
      i1 = n2-3
      xi = f2
      do 60 i2=2,i1,2
         xi = xi-2.
         a = x1*(xi-1.0)/xi*(1.0+a)
   60 continue
   65 a = x1*dp*(1.0+a)
   70 a = a+theta
      if (n1.eq.1) go to 90
      if (n1.eq.3) go to 80
      i1 = n1-3
      xi = f1
      do 75 i2=2,i1,2
         xi = xi-2.
         vp = vp-2.
         b = sts*vp/xi*(1.0+b)
   75 continue
   80 b = dp*x1*(1.0+b)
      if (n2.eq.1) go to 90
      i2 = n2/2
      c = 1.0
      do 85 i=1,i2
         b = b*x1*c/(c-0.5)
         c = c+1.0
   85 continue
   90 p = r2dpi*(a-b)
   95 if (p.lt.0.0) p = 0.0
      if (p.gt.1.0) p = 1.0
      go to 9005
  100 ier = 129
      go to 110
  105 ier = 130
  110 p = rinfp
      go to 9000
  115 p = 0.0
      go to 9005
  120 p = .5
      go to 9005
 9000 continue
c      call uertst (ier,6hmdfd  )
 9005 return
      end

c $Id$ 
