c   imsl routine name   - merrc=erfc
c
c-----------------------------------------------------------------------
c
c   computer            - cray/single
c
c   latest revision     - august 1, 1981
c
c   purpose             - evaluate the complemented error function
c
c   usage               - result = erfc(y)
c
c   arguments    y      - input argument of the complemented error
c                           function.
c                erfc   - output value of the complemented error
c                           function.
c
c   precision/hardware  - single/all
c                         note - erfc may not be supplied by imsl if it
c                           resides in the mathematical subprogram
c                           library supplied by the manufacturer.
c
c   reqd. imsl routines - none required
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
      function erfc(y)
c                                  specifications for arguments
      real y
c                                  specifications for local variables
      integer            isw,i
      dimension          p(5),q(3),p1(8),q1(7),p2(5),q2(4)
      real p,q,p1,q1,p2,q2,xmin,xlarge,ssqpi,x,
     *                   res,xsq,xnum,xden,xi
c                                  coefficients for 0.0 .le. y .lt.
c                                  .477
      data               p(1)/-.44422647396874/,
     1                   p(2)/10.731707253648/,
     2                   p(3)/15.915606197771/,
     3                   p(4)/374.81624081284/,
     4                   p(5)/2.5612422994823e-02/
      data               q(1)/17.903143558843/,
     1                   q(2)/124.82892031581/,
     2                   q(3)/332.17224470532/
c                                  coefficients for .477 .le. y
c                                  .le. 4.0
      data               p1(1)/7.2117582508831/,
     1                   p1(2)/43.162227222057/,
     2                   p1(3)/152.98928504694/,
     3                   p1(4)/339.32081673434/,
     4                   p1(5)/451.91895371187/,
     5                   p1(6)/300.45926102016/,
     6                   p1(7)/-1.3686485738272e-07/,
     7                   p1(8)/.56419551747897/
      data               q1(1)/77.000152935229/,
     1                   q1(2)/277.58544474399/,
     2                   q1(3)/638.98026446563/,
     3                   q1(4)/931.35409485061/,
     4                   q1(5)/790.95092532790/,
     5                   q1(6)/300.45926095698/,
     6                   q1(7)/12.782727319629/
c                                  coefficients for 4.0 .lt. y
      data               p2(1)/-.22695659353969/,
     1                   p2(2)/-4.9473091062325e-02/,
     2                   p2(3)/-2.9961070770354e-03/,
     3                   p2(4)/-2.2319245973418e-02/,
     4                   p2(5)/-2.7866130860965e-01/
      data               q2(1)/1.0516751070679/,
     1                   q2(2)/.19130892610783/,
     2                   q2(3)/1.0620923052847e-02/,
     3                   q2(4)/1.9873320181714/
c                                  constants
      data               xmin/1.0e-8/,xlarge/5.6875/
c                                  erfc(xbig) .approx. setap
      data               xbig/25.90625/
      data               ssqpi/.56418958354776/
c                                  first executable statement
      x = y
      isw = 1
      if (x.ge.0.0) go to 5
      isw = -1
      x = -x
    5 if (x.lt..477) go to 10
      if (x.le.4.0) go to 30
      if (isw .gt. 0) go to 40
      if (x.lt.xlarge) go to 45
      res = 2.0
      go to 65
c                                  abs(y) .lt. .477, evaluate
c                                  approximation for erfc
   10 if (x.lt.xmin) go to 20
      xsq = x*x
      xnum = p(5)
      do 15 i = 1,4
         xnum = xnum*xsq+p(i)
   15 continue
      xden = ((q(1)+xsq)*xsq+q(2))*xsq+q(3)
      res = x*xnum/xden
      go to 25
   20 res = x*p(4)/q(3)
   25 if (isw.eq.-1) res = -res
      res = 1.0e0-res
      go to 65
c                                  .477 .le. abs(y) .le. 4.0
c                                  evaluate approximation for erfc
   30 xsq = x*x
      xnum = p1(7)*x+p1(8)
      xden = x+q1(7)
      do 35 i=1,6
         xnum = xnum*x+p1(i)
         xden = xden*x+q1(i)
   35 continue
      res = xnum/xden
      go to 55
c                                  4.0 .lt. abs(y), evaluate
c                                  minimax approximation for erfc
   40 if (x.gt.xbig) go to 60
   45 xsq = x*x
      xi = 1.0e0/xsq
      xnum = p2(4)*xi+p2(5)
      xden = xi+q2(4)
      do 50 i = 1,3
         xnum = xnum*xi+p2(i)
         xden = xden*xi+q2(i)
   50 continue
      res = (ssqpi+xi*xnum/xden)/x
   55 res = res*exp(-xsq)
      if (isw.eq.-1) res = 2.0e0-res
      go to 65
   60 res = 0.0
   65 erfc = res
      return
      end

c $Id$ 
