      subroutine rcvrfn(p,f,nlyrs,dvp,dvs,drp,drs,dts,rpp,rps,rsp,rss,rsh)
      integer nlyrs
      complex p,f
      complex dvp,dvs,drp,drs,dts
      complex rpp,rps,rsp,rss,rsh
c
c        compute receiver function - free surface displacement from a
c        plane wave incident from below, on a stack of plane, parallel,
c        homogeneous layers (6.4) ~       0s ~       0s
c        gives 1st part of (7.36) W[I - Rd   R]**-1 Tu
c        for a p, sv or sh wave incident (6.18) Tu, Ru
c        interface 0 is top of layer 1, a free surface,
c        layer n is half space
c        given frequency and phase slowness.
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
c        nlyrs - total number of layers, layer nlyrs is
c            the half space
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
      complex i,zero,one,w
      complex t11,t12,t21,t22,l11,l12,l21,l22,tsh,lsh
c     complex*16 det
      complex det
      complex x11,x12,x21,x22,y11,y12,y21,y22,xsh,ysh
      complex r11,r12,r21,r22,s11,s12,s21,s22,ssh
      complex tnupp,tnups,tnusp,tnuss,tnush
      complex rndpp,rndps,rndsp,rndss,rndsh
      complex rnupp,rnups,rnusp,rnuss,rnush
      complex phtp,phts,phtpp,phtps,phtss
      real twopi
      integer lyr,nif,cnvnif
      external cphs
      complex cphs
      data twopi/6.2831853/,i,zero/(0.,1.),(0.,0.)/,one/(1.,0.)/
c
c
      w = twopi*f
c
c     handle the special case of a half space
c     use free surface displacement operator (5.69) from ifmat
c
c
      if ( nlyrs .eq. 1 ) then
         dvp = dvpfs
         dvs = dvsfs
         drp = drpfs
         drs = drsfs
         dts = dtshfs
	 rpp = ruppfs
	 rsp = ruspfs
	 rps = rupsfs
	 rss = russfs
	 rsh = rushfs
         return
       endif
c
c        initialize tup and rdown matrices for the stack with
c        bottom interface matrices
c        cnvrsn set in kennett.f
c        allphs - all converted phases at layer using trans/ref matrices
c                 in ifmat.f
c        prmphs - no converted phases, only PP, SS reflections
c        cnvphs - only converted phases
c
      nif = nlyrs-1
      cnvnif = cnvrsn(nif)
      if ( cnvnif .eq. allphs ) then
         tnupp = tupp(nif)
         tnuss = tuss(nif)
         tnups = tups(nif)
         tnusp = tusp(nif)
         tnush = tush(nif)
         rndpp = rdpp(nif)
         rndss = rdss(nif)
         rndps = rdps(nif)
         rndsp = rdsp(nif)
         rndsh = rdsh(nif)
         rnupp = rupp(nif)
         rnuss = russ(nif)
         rnups = rups(nif)
         rnusp = rusp(nif)
         rnush = rush(nif)
       else if ( cnvnif .eq. prmphs ) then
         tnupp = tupp(nif)
         tnuss = tuss(nif)
         tnups = zero
         tnusp = zero
         tnush = tush(nif)
         rndpp = rdpp(nif)
         rndss = rdss(nif)
         rndps = zero
         rndsp = zero
         rndsh = rdsh(nif)
         rnupp = rupp(nif)
         rnuss = russ(nif)
         rnups = zero
         rnusp = zero
         rnush = rush(nif)
       else if ( cnvnif .eq. cnvphs ) then
         tnups = tups(nif)
         tnusp = tusp(nif)
         tnupp = zero
         tnuss = zero
         tnush = tush(nif)
         rndps = rdps(nif)
         rndsp = rdsp(nif)
         rndpp = zero
         rndss = zero
         rndsh = rdsh(nif)
         rnups = rups(nif)
         rnusp = rusp(nif)
         rnupp = zero
         rnuss = zero
         rnush = rush(nif)
       endif
c
c        now do the  bottom up recursion for tup and rdown
c
      do 10 lyr = nlyrs-1, 2, -1
c     nif is next layer above lyr
         nif = lyr - 1
c
c        use the two way phase delay through the layer
c        to/from the next interface (3.11, 3.12)
c
         phtp = cphs( -i*w*xi(lyr)*thik(lyr) )
         phts = cphs( -i*w*eta(lyr)*thik(lyr) )
         phtpp = phtp * phtp
         phtps = phtp * phts
         phtss = phts * phts
c 
c         mult reflection coeff by corresponding phase delay using
c         recursive construction scheme (6.16) needed to transmit
c         up and down from reflection
c
         rndpp = rndpp * phtpp
         rndss = rndss * phtss
         rndps = rndps * phtps
         rndsp = rndsp * phtps
         rndsh = rndsh * phtss
c
c         for transmission, only need to transmit once
c
         tnupp = tnupp * phtp
         tnuss = tnuss * phts
         tnups = tnups * phtp
         tnusp = tnusp * phts
         tnush = tnush * phts

c
c        form the reverberation operator for the layer
c        nif is layer above lyr, so tii is Ru(nif)
c
         cnvnif = cnvrsn(nif)
         if ( cnvnif .eq. allphs ) then
            r11 = rupp(nif)
            r22 = russ(nif)
            r12 = rups(nif)
            r21 = rusp(nif)
            rsh = rush(nif)
          else if ( cnvnif .eq. prmphs ) then
            r11 = rupp(nif)
            r22 = russ(nif)
            r12 = zero
            r21 = zero
            rsh = rush(nif)
          else if ( cnvnif .eq. cnvphs ) then
            r12 = rups(nif)
            r21 = rusp(nif)
            r11 = zero
            r22 = zero
            rsh = rush(nif)
          endif
c
c          form (I - Rd(lyr)*Ru(nif)]**-1 for upward tranmission
c          and reflection matrices Tu, Ru (6.4, 6.18)
c
         if ( reverb(lyr) .eq. allrvb ) then
            l11 = one - (rndpp*r11 + rndps*r21)
            l22 = one - (rndsp*r12 + rndss*r22)
            l12 = - (rndpp*r12 + rndps*r22)
            l21 = - (rndsp*r11 + rndss*r21)
            det = ( l11*l22 - l12*l21 )
c            find inverse
	    det = one / det
            l12 = -l12*det
            l21 = -l21*det
            t11 = l11*det
            l11 = l22*det
            l22 = t11
            lsh = one / ( one - rndsh*rsh )
         else if ( reverb(lyr) .eq. onervb ) then
            l11 = one + (rndpp*r11 + rndps*r21)
            l22 = one + (rndsp*r12 + rndss*r22)
            l12 =  (rndpp*r12 + rndps*r22)
            l21 =  (rndsp*r11 + rndss*r21)
            lsh = one + rndsh*rsh
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
c         post multiply reverberation operator for Tu (6.4)
c
         t11 = l11*tnupp + l12*tnusp
         t22 = l21*tnups + l22*tnuss
         t21 = l21*tnupp + l22*tnusp
         t12 = l11*tnups + l12*tnuss
         tsh = lsh * tnush
c
c                               -1                    T
c        Ru = Ru + TdRu[I - RdRu] Tu  ; but use Td = Tu  relation
c
	 s11 = r11*t11 + r12*t21
	 s12 = r11*t12 + r12*t22
	 s21 = r21*t11 + r22*t21
	 s22 = r21*t12 + r22*t22
	 ssh = rsh*tsh
	 rnupp = rnupp + tnupp*s11 + tnusp*s21
	 rnups = rnups + tnupp*s12 + tnusp*s22
	 rnusp = rnusp + tnups*s11 + tnuss*s21
	 rnuss = rnuss + tnups*s12 + tnuss*s22
	 rnush = rnush + tnush*ssh
c
c        tnupp = tupp(nif)*t11 + tups(nif)*t21
c        tnuss = tusp(nif)*t12 + tuss(nif)*t22
c        tnups = tupp(nif)*t12 + tups(nif)*t22
c        tnusp = tusp(nif)*t11 + tuss(nif)*t21
c         pre multiply for Tu (6.4, 6.18)
c         Tu(z-) = Tu(AB)[I - Rd(BC)Ru(AB)]**-1 Tu(BC)
c
         tnupp = x11*t11 + x12*t21
         tnuss = x21*t12 + x22*t22
         tnups = x11*t12 + x12*t22
         tnusp = x21*t11 + x22*t21
         tnush = xsh * tsh
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
c         Rd(AC) (6.3)
         rndpp = y11 + x11*l11 + x12*l21
         rndss = y22 + x21*l12 + x22*l22
         rndps = y12 + x11*l12 + x12*l22
         rndsp = y21 + x21*l11 + x22*l21
         rndsh = ysh + xsh*lsh
c
10    continue
c
c        use the two way phase delay through the top layer (3.11,3.12)
c
         phtp = cphs( -i*w*xi(lyr)*thik(lyr) )
         phts = cphs( -i*w*eta(lyr)*thik(lyr) )
         phtpp = phtp * phtp
         phtps = phtp * phts
         phtss = phts * phts
         tnupp = tnupp * phtp
         tnuss = tnuss * phts
         tnups = tnups * phtp
         tnusp = tnusp * phts
         tnush = tnush * phts
         rndpp = rndpp * phtpp
         rndss = rndss * phtss
         rndps = rndps * phtps
         rndsp = rndsp * phtps
         rndsh = rndsh * phtss
c
c        form the reverberation operator for the top layer
c        ru__fs was calculated in ifmat.f
c
         cnvnif = cnvrsn(0)
         if ( cnvnif .eq. allphs ) then
            r11 = ruppfs
            r22 = russfs
            r12 = rupsfs
            r21 = ruspfs
            rsh = rushfs
          else if ( cnvnif .eq. prmphs ) then
            r11 = ruppfs
            r22 = russfs
            r12 = zero
            r21 = zero
            rsh = rushfs
          else if ( cnvnif .eq. cnvphs ) then
            r12 = rupsfs
            r21 = ruspfs
            r11 = zero
            r22 = zero
            rsh = rushfs
          endif
c                    I ~
c         form [I - Rd R]**-1
         if ( reverb(lyr) .eq. allrvb ) then
            l11 = one - (rndpp*r11 + rndps*r21)
            l22 = one - (rndsp*r12 + rndss*r22)
            l12 = - (rndpp*r12 + rndps*r22)
            l21 = - (rndsp*r11 + rndss*r21)
            det = ( l11*l22 - l12*l21 )
	    det = one / det
            l12 = -l12*det
            l21 = -l21*det
            t11 = l11*det
            l11 = l22*det
            l22 = t11
            lsh = one / ( one - rndsh*rsh )
         else if ( reverb(lyr) .eq. onervb ) then
            l11 = one + (rndpp*r11 + rndps*r21)
            l22 = one + (rndsp*r12 + rndss*r22)
            l12 =  (rndpp*r12 + rndps*r22)
            l21 =  (rndsp*r11 + rndss*r21)
            lsh = one + rndsh*rsh
         else if ( reverb(lyr) .eq. norvb ) then
            l11 = one
            l22 = one
            l12 = zero
            l21 = zero
            lsh = one
          endif
c
c        now add the free surface displacement
c        1st part of (7.36) with source at bottom of stack
c
         t11 = l11*tnupp + l12*tnusp
         t22 = l21*tnups + l22*tnuss
         t21 = l21*tnupp + l22*tnusp
         t12 = l11*tnups + l12*tnuss
         tsh = lsh*tnush
         dvp = dvpfs*t11 + dvsfs*t21
         dvs = dvpfs*t12 + dvsfs*t22
         drp = drpfs*t11 + drsfs*t21
         drs = drpfs*t12 + drsfs*t22
         dts = dtshfs*tsh
c
c        now form the net Ru operator using the transpose trick for Td again
c
         s11 = r11*t11 + r12*t21
         s12 = r11*t12 + r12*t22
         s21 = r21*t11 + r22*t21
         s22 = r21*t12 + r22*t22
         ssh = rsh*tsh
         rnupp = rnupp + tnupp*s11 + tnusp*s21
         rnups = rnups + tnupp*s12 + tnusp*s22
         rnusp = rnusp + tnups*s11 + tnuss*s21
         rnuss = rnuss + tnups*s12 + tnuss*s22
         rnush = rnush + tnush*ssh
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
