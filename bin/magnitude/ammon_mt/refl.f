      subroutine refl(p,f,tplyr,btlyr,rpp,rps,rsp,rss,rsh)
      integer tplyr,btlyr
      complex p,f
      complex rpp,rps,rsp,rss,rsh
c
c        compute reflectivity - reflection from a
c        plane wave incident from above, on a stack of plane, parallel,
c        homogeneous layers
c        for a p, sv or sh wave incident
c        top interface is top of layer tplyr
c        layer btlyr is half space, with radiation condition
c        given frequency and phase slowness.
c        Rd  in (7.36) for reflection from buried source, tplyr is
c        src layer (see Fig. 7.2) use recursive construction scheme (6.17)
c
c          arguments...
c
c        f,p - prescribed freq (hz) & horizontal phase slowness (c is
c            not restricted to be greater than alfa or beta)
c            both may be complex
c
c        passed in common /model/
c        alfa,beta,qp,qs,rho and thik contain the medium properties for
c
c
c
c
c        commons and declarations
c
c
      include 'kennet.inc'
c
c        complex declarations
c
      complex i,zero,one,two,quartr,w
      complex t11,t12,t21,t22,l11,l12,l21,l22,tsh,lsh
c     complex*16 det
      complex det
      complex x11,x12,x21,x22,y11,y12,y21,y22,xsh,ysh
      complex rndpp,rndps,rndsp,rndss,rndsh
      complex phtp,phts,phtpp,phtps,phtss
      real twopi,eps
      integer lyr,nif,cnvnif
      external cphs
      complex cphs
      data twopi,eps/6.2831853,.001/,i,zero/(0.,1.),(0.,0.)/,
     & one,two/(1.,0.),(2.,0.)/,quartr/(0.25,0.)/
c
c
      w = twopi*f
c     if(f .eq. (0.,0.)) w = (1.0e-6,0.)
c
c        handle exceptional cases first...
c        if btlyr = tplyr there is no reflection from half space
c        if tplyr > btlyr there is a screw up, return zeroes
c
      if ( tplyr .ge. btlyr ) then
         rpp = zero
         rps = zero
         rsp = zero
         rss = zero
         rsh = zero
         return
       endif
c
c        now go on to handle normal cases...
c        initialize tup and rdown matricies for the stack with
c        bottom interface matricies
c
      nif = btlyr - 1
      cnvnif = cnvrsn(nif)
      if ( cnvnif .eq. allphs ) then
         rndpp = rdpp(nif)
         rndss = rdss(nif)
         rndps = rdps(nif)
         rndsp = rdsp(nif)
         rndsh = rdsh(nif)
       else if ( cnvnif .eq. prmphs ) then
         rndpp = rdpp(nif)
         rndss = rdss(nif)
         rndps = zero
         rndsp = zero
         rndsh = rdsh(nif)
       else if ( cnvnif .eq. cnvphs ) then
         rndps = rdps(nif)
         rndsp = rdsp(nif)
         rndpp = zero
         rndss = zero
         rndsh = rdsh(nif)
       endif
c
c        now do the  bottom up recursion for tup and rdown
c
      do 10 lyr = btlyr-1, tplyr+1, -1
         nif = lyr - 1
c
c        use the two way phase delay through the layer
c        to/from the next interface and mult by reflection coeff
c
         phtp = cphs( -i*w*xi(lyr)*thik(lyr) )
         phts = cphs( -i*w*eta(lyr)*thik(lyr) )
c         mult reflection coeff by corresponding phase delay
c         using recursive construction scheme (6.16)
         phtpp = phtp * phtp
         phtps = phtp * phts
         phtss = phts * phts
         rndpp = rndpp * phtpp
         rndss = rndss * phtss
         rndps = rndps * phtps
         rndsp = rndsp * phtps
         rndsh = rndsh * phtss


c
c        form the reverberation operator for the layer
c
         cnvnif = cnvrsn(nif)
         if ( cnvnif .eq. allphs ) then
            t11 = rupp(nif)
            t22 = russ(nif)
            t12 = rups(nif)
            t21 = rusp(nif)
            tsh = rush(nif)
          else if ( cnvnif .eq. prmphs ) then
            t11 = rupp(nif)
            t22 = russ(nif)
            t12 = zero
            t21 = zero
            tsh = rush(nif)
          else if ( cnvnif .eq. cnvphs ) then
            t12 = rups(nif)
            t21 = rusp(nif)
            t11 = zero
            t22 = zero
            tsh = rush(nif)
          endif
c         form [I - Rd Ru]**-1
         if ( reverb(lyr) .eq. allrvb ) then
            l11 = one - (rndpp*t11 + rndps*t21)
            l22 = one - (rndsp*t12 + rndss*t22)
            l12 = - (rndpp*t12 + rndps*t22)
            l21 = - (rndsp*t11 + rndss*t21)
            det = ( l11*l22 - l12*l21 )
	    det = one / det
            l12 = -l12*det
            l21 = -l21*det
            t11 = l11*det
            l11 = l22*det
            l22 = t11
            lsh = one / ( one - rndsh*tsh )
         else if ( reverb(lyr) .eq. onervb ) then
            l11 = one + (rndpp*t11 + rndps*t21)
            l22 = one + (rndsp*t12 + rndss*t22)
            l12 =  (rndpp*t12 + rndps*t22)
            l21 =  (rndsp*t11 + rndss*t21)
            lsh = one + rndsh*tsh
         else if ( reverb(lyr) .eq. norvb ) then
            l11 = one
            l22 = one
            l12 = zero
            l21 = zero
            lsh = one
          endif
c
c        now finish the recursion (6.17), adding the next interface
c
         if ( cnvnif .eq. allphs ) then
            x11 = tupp(nif)
            x22 = tuss(nif)
            x12 = tups(nif)
            x21 = tusp(nif)
            xsh = tush(nif)
            y11 = rdpp(nif)
            y22 = rdss(nif)
            y12 = rdps(nif)
            y21 = rdsp(nif)
            ysh = rdsh(nif)
          else if ( cnvnif .eq. prmphs ) then
            x11 = tupp(nif)
            x22 = tuss(nif)
            x12 = zero
            x21 = zero
            xsh = tush(nif)
            y11 = rdpp(nif)
            y22 = rdss(nif)
            y12 = zero
            y21 = zero
            ysh = rdsh(nif)
          else if ( cnvnif .eq. cnvphs ) then
            x12 = tups(nif)
            x21 = tusp(nif)
            x11 = zero
            x22 = zero
            xsh = tush(nif)
            y12 = rdps(nif)
            y21 = rdsp(nif)
            y11 = zero
            y22 = zero
            ysh = rdsh(nif)
          endif
c
c        t11 = l11*tdpp(nif) + l21*tdsp(nif)
c        t12 = l11*tdps(nif) + l21*tdss(nif)
c        t21 = l12*tdpp(nif) + l22*tdsp(nif)
c        t22 = l12*tdps(nif) + l22*tdss(nif)
         t11 = l11*x11 + l21*x12
         t12 = l11*x21 + l21*x22
         t21 = l12*x11 + l22*x12
         t22 = l12*x21 + l22*x22
         tsh = lsh * xsh
         l11 = rndpp*t11 + rndps*t21
         l12 = rndpp*t12 + rndps*t22
         l21 = rndsp*t11 + rndss*t21
         l22 = rndsp*t12 + rndss*t22
         lsh = rndsh * tsh
c        rndpp = rdpp(nif) + tupp(nif)*l11 + tups(nif)*l21
c        rndss = rdss(nif) + tusp(nif)*l12 + tuss(nif)*l22
c        rndps = rdps(nif) + tupp(nif)*l12 + tups(nif)*l22
c        rndsp = rdsp(nif) + tusp(nif)*l11 + tuss(nif)*l21
         rndpp = y11 + x11*l11 + x12*l21
         rndss = y22 + x21*l12 + x22*l22
         rndps = y12 + x11*l12 + x12*l22
         rndsp = y21 + x21*l11 + x22*l21
         rndsh = ysh + xsh*lsh
c
10    continue
c
         rpp = rndpp
         rps = rndps
         rsp = rndsp
         rss = rndss
         rsh = rndsh
c
c
c
      return
      end
