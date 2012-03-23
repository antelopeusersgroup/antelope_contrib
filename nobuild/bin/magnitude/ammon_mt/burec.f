      subroutine burec(p,f,nlyrs,irlyr,recdep,dvp,dvs,drp,drs,dts)
      integer nlyrs,irlyr
      complex p,f
      complex dvp,dvs,drp,drs,dts
c
c        compute receiver function for buried receiver from a
c        plane wave incident from below, on a stack of plane, parallel,
c        homogeneous layers (6.4) 
c        gives 1st part of (7.34) 
c        (Mu + Mdr*Ru(fR)[I-Rd(RS)Ru(fR]**-1 Tu(RS)
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
c        irlyr - layer containing receiver (note receiver must be at
c                or above source depth)
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
      complex tnupp,tnups,tnusp,tnuss,tnush
      complex rndpp,rndps,rndsp,rndss,rndsh
      complex phtp,phts,phtpp,phtps,phtss
      complex ruppfr,russfr,rupsfr,ruspfr,rushfr
      complex dvpfr,dvsfr,drpfr,drsfr,dtshfr
      complex mnd11,mnd12,mnd21,mnd22
      real twopi,recdep
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
c     d__fs terms defined in ifmat.f (5.69)
         dvp = dvpfs
         dvs = dvsfs
         drp = drpfs
         drs = drsfs
         dts = dtshfs
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
       endif
c
c        this do 10 loop uses recursion construction scheme
c        using the  bottom up recursion (6.16; 6.18) for tup and rdown
c        Rd(RS), Tu(RS) to be used in construction of
c        (Mu + Mdr*Ru(fR)[I-Rd(RS)Ru(fR]**-1 Tu(RS)
c
c
      do 10 lyr = nlyrs-1, irlyr+1, -1
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
c         store for each layer
c
	 stnupp(lyr) = tnupp
	 stnups(lyr) = tnups
	 stnusp(lyr) = tnusp
	 stnuss(lyr) = tnuss
	 stnush(lyr) = tnush
	 srndpp(lyr) = rndpp
	 srndps(lyr) = rndps
	 srndsp(lyr) = rndsp
	 srndss(lyr) = rndss
	 srndsh(lyr) = rndsh
c
c        form the reverberation operator for the layer
c        nif is layer above lyr, so tii is Ru(nif)
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
c
c          form [I - Rd(lyr)*Ru(nif)]**-1 for upward tranmission
c          and reflection matrices Tu, Ru (6.4, 6.18)
c
         if ( reverb(lyr) .eq. allrvb ) then
            l11 = one - (rndpp*t11 + rndps*t21)
            l22 = one - (rndsp*t12 + rndss*t22)
            l12 = - (rndpp*t12 + rndps*t22)
            l21 = - (rndsp*t11 + rndss*t21)
            det = ( l11*l22 - l12*l21 )
c            find inverse
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
c         [I-Rd*Ru]**-1 Tu
c
         t11 = l11*tnupp + l12*tnusp
         t22 = l21*tnups + l22*tnuss
         t21 = l21*tnupp + l22*tnusp
         t12 = l11*tnups + l12*tnuss
         tsh = lsh * tnush
c
c        tnupp = tupp(nif)*t11 + tups(nif)*t21
c        tnuss = tusp(nif)*t12 + tuss(nif)*t22
c        tnups = tupp(nif)*t12 + tups(nif)*t22
c        tnusp = tusp(nif)*t11 + tuss(nif)*t21
c
c         pre multiply for Tu (6.4, 6.18) in next layer
c         and store for next layer calculation
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
c
c         calculate Rd for next layer Rd(AC) (6.3)
c        
         rndpp = y11 + x11*l11 + x12*l21
         rndss = y22 + x21*l12 + x22*l22
         rndps = y12 + x11*l12 + x12*l22
         rndsp = y21 + x21*l11 + x22*l21
         rndsh = ysh + xsh*lsh
c
10    continue
c
c        use the two way phase delay through the top or
c        receiver layer (3.11,3.12)
c
c        note: on next two lines eventually want thik(lyr) - recdep
c
         phtp = cphs( -i*w*xi(lyr)*(thik(lyr) - recdep) )
         phts = cphs( -i*w*eta(lyr)*(thik(lyr) - recdep) )
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
c        form the reverberation operator, Ru(fR) for the layers
c        between the receiver and free surface
c        ru__fs was calculated in ifmat.f
c
      call rufs(p,f,lyr,ruppfr,rupsfr,ruspfr,russfr,rushfr)
c
c     note: may need phase delay here if receiver is located
c           below interface (i.e. recdep ne 0)
c
        phtp = cphs( -i*w*xi(lyr)*recdep )
        phts = cphs( -i*w*eta(lyr)*recdep )
        phtpp = phtp * phtp
        phtps = phtp * phts
        phtss = phts * phts
        ruppfr = ruppfr * phtpp
        rupsfr = rupsfr * phtps
        ruspfr = ruspfr * phtps
        russfr = russfr * phtss
        rushfr = rushfr * phtss
c
c           
c         form [I - Rd(RS)*Ru(fR)]**-1
c
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
c        now add the free surface displacement
c        post multiply by Tu(RS)
c        1st part of (7.35) with source at bottom of stack
c
         t11 = l11*tnupp + l12*tnusp
         t22 = l21*tnups + l22*tnuss
         t21 = l21*tnupp + l22*tnusp
         t12 = l11*tnups + l12*tnuss
         tsh = lsh*tnush
c
c        pre multiply by (MuR + MdR*Ru(fr)) 
c        Mu and Md from ifmat.f
c
      mnd11 = md11(lyr)
      mnd12 = md12(lyr)
      mnd21 = md21(lyr)
      mnd22 = md22(lyr)
      dvpfr = mu11(lyr) + (mnd11*ruppfr + mnd12*ruspfr)
      dvsfr = mu12(lyr) + (mnd11*rupsfr + mnd12*russfr)
      drpfr = mu21(lyr) + (mnd21*ruppfr + mnd22*ruspfr)
      drsfr = mu22(lyr) + (mnd21*rupsfr + mnd22*russfr)
      dtshfr = zsh(lyr) + (zsh(lyr)*tsh)
c
         dvp = dvpfr*t11 + dvsfr*t21
         dvs = dvpfr*t12 + dvsfr*t22
         drp = drpfr*t11 + drsfr*t21
         drs = drpfr*t12 + drsfr*t22
         dts = dtshfr*tsh
c
c
c
      return
      end
