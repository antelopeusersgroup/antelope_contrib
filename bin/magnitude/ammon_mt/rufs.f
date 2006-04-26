      subroutine rufs(p,f,btlyr,rpp,rps,rsp,rss,rsh)
      integer btlyr
      complex p,f
      complex rpp,rps,rsp,rss,rsh
c
c        compute reflectivity - reflection from a
c        plane wave incident from below, on a stack of plane, parallel,
c        homogeneous layers bounded above by free surface
c        for a p, sv or sh wave incident
c        layer btlyr is half space, with radiation condition
c        given frequency and phase slowness.
c        for regional synthetics, btlyr is source layer
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
      complex rnupp,rnups,rnusp,rnuss,rnush
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
c        initialize rupfs matrix for the stack with
c        free surface reflection matrix set in ifmat
c
      nif = 0
      cnvnif = cnvrsn(nif)
      if ( cnvnif .eq. allphs ) then
         rnupp = ruppfs
         rnuss = russfs
         rnups = rupsfs
         rnusp = ruspfs
         rnush = rushfs
       else if ( cnvnif .eq. prmphs ) then
         rnupp = ruppfs
         rnuss = russfs
         rnups = zero
         rnusp = zero
         rnush = rushfs
       else if ( cnvnif .eq. cnvphs ) then
         rnups = rupsfs
         rnusp = ruspfs
         rnupp = zero
         rnuss = zero
         rnush = rushfs
       endif
c
c        now do the top down recursion for rupfs
c
      do 10 lyr = 1, btlyr-1
         nif = lyr
c
c        use the two way phase delay through the layer
c        to/from the next interface
c        using recursive construction scheme (6.16)
c
         phtp = cphs( -i*w*xi(lyr)*thik(lyr) )
         phts = cphs( -i*w*eta(lyr)*thik(lyr) )
         phtpp = phtp * phtp
         phtps = phtp * phts
         phtss = phts * phts
         rnupp = rnupp * phtpp
         rnuss = rnuss * phtss
         rnups = rnups * phtps
         rnusp = rnusp * phtps
         rnush = rnush * phtss

c
c        form the reverberation operator for the layer
c        Rd from ifmat.f
c
         cnvnif = cnvrsn(nif)
         if ( cnvnif .eq. allphs ) then
            t11 = rdpp(nif)
            t22 = rdss(nif)
            t12 = rdps(nif)
            t21 = rdsp(nif)
            tsh = rdsh(nif)
          else if ( cnvnif .eq. prmphs ) then
            t11 = rdpp(nif)
            t22 = rdss(nif)
            t12 = zero
            t21 = zero
            tsh = rdsh(nif)
          else if ( cnvnif .eq. cnvphs ) then
            t12 = rdps(nif)
            t21 = rdsp(nif)
            t11 = zero
            t22 = zero
            tsh = rdsh(nif)
          endif
c               f
c         [I - Ru Rd]**-1
         if ( reverb(lyr) .eq. allrvb ) then
            l11 = one - (rnupp*t11 + rnups*t21)
            l22 = one - (rnusp*t12 + rnuss*t22)
            l12 = - (rnupp*t12 + rnups*t22)
            l21 = - (rnusp*t11 + rnuss*t21)
            det = ( l11*l22 - l12*l21 )
	    det = one / det
            l12 = -l12*det
            l21 = -l21*det
            t11 = l11*det
            l11 = l22*det
            l22 = t11
            lsh = one / ( one - rnush*tsh )
         else if ( reverb(lyr) .eq. onervb ) then
            l11 = one + (rnupp*t11 + rnups*t21)
            l22 = one + (rnusp*t12 + rnuss*t22)
            l12 =  (rnupp*t12 + rnups*t22)
            l21 =  (rnusp*t11 + rnuss*t21)
            lsh = one + rnush*tsh
         else if ( reverb(lyr) .eq. norvb ) then
            l11 = one
            l22 = one
            l12 = zero
            l21 = zero
            lsh = one
          endif
c
c        now finish the recursion, adding the next interface
c
         if ( cnvnif .eq. allphs ) then
            x11 = tdpp(nif)
            x22 = tdss(nif)
            x12 = tdps(nif)
            x21 = tdsp(nif)
            xsh = tdsh(nif)
            y11 = rupp(nif)
            y22 = russ(nif)
            y12 = rups(nif)
            y21 = rusp(nif)
            ysh = rush(nif)
          else if ( cnvnif .eq. prmphs ) then
            x11 = tdpp(nif)
            x22 = tdss(nif)
            x12 = zero
            x21 = zero
            xsh = tdsh(nif)
            y11 = rupp(nif)
            y22 = russ(nif)
            y12 = zero
            y21 = zero
            ysh = rush(nif)
          else if ( cnvnif .eq. cnvphs ) then
            x12 = tdps(nif)
            x21 = tdsp(nif)
            x11 = zero
            x22 = zero
            xsh = tdsh(nif)
            y12 = rups(nif)
            y21 = rusp(nif)
            y11 = zero
            y22 = zero
            ysh = rush(nif)
          endif
c
c        t11 = l11*tupp(nif) + l21*tusp(nif)
c        t12 = l11*tups(nif) + l21*tuss(nif)
c        t21 = l12*tupp(nif) + l22*tusp(nif)
c        t22 = l12*tups(nif) + l22*tuss(nif)
c
c               f
c         [I - Ru Rd]**-1 Td
c
         t11 = l11*x11 + l21*x12
         t12 = l11*x21 + l21*x22
         t21 = l12*x11 + l22*x12
         t22 = l12*x21 + l22*x22
         tsh = lsh * xsh
         l11 = rnupp*t11 + rnups*t21
         l12 = rnupp*t12 + rnups*t22
         l21 = rnusp*t11 + rnuss*t21
         l22 = rnusp*t12 + rnuss*t22
         lsh = rnush * tsh
c
c               f
c        Ru [I - Ru Rd]**-1 Td
c
c        rnupp = rupp(nif) + tdpp(nif)*l11 + tdps(nif)*l21
c        rnuss = russ(nif) + tdsp(nif)*l12 + tdss(nif)*l22
c        rnups = rups(nif) + tdpp(nif)*l12 + tdps(nif)*l22
c        rnusp = rusp(nif) + tdsp(nif)*l11 + tdss(nif)*l21
c        (6.9) Ru = Ru + Td Ru [I - Ru Rd]**-1 Td
         rnupp = y11 + x11*l11 + x12*l21
         rnuss = y22 + x21*l12 + x22*l22
         rnups = y12 + x11*l12 + x12*l22
         rnusp = y21 + x21*l11 + x22*l21
         rnush = ysh + xsh*lsh
c
10    continue
c
         rpp = rnupp
         rps = rnups
         rsp = rnusp
         rss = rnuss
         rsh = rnush
c
c
c
      return
      end
