      subroutine ifmat(psvsh,p,f,nlyrs)
      integer psvsh,nlyrs
      complex p,f
c
c        compute kennett's interface matrices for n layer model
c        for a p, sv or sh wave incident
c        interface 0 is top of layer 1, a free surface, compute
c        reflection operator, and free surface displacement operator
c        layer n is half space
c        compute frequency-independent ru, rd, tu, td at interfaces
c        given frequency and phase slowness (see p. 106 kennett).
c        e.g.
c 
c             | Rd(pp) Rd(ps) |
c        Rd = |               | (5.15)
c             | Rd(sp) Rd(ss) |
c
c
c          arguments...
c        psvsh = 1,2,3 for an incident p, sv or sh wave.
c
c        f,p - prescribed freq (hz) & horizontal phase slowness (c is
c            not restricted to be greater than alfa or beta)
c            both may be complex
c
c        passed in common /model/
c        alfa,beta,qp,qs,rho and thik contain the medium properties for
c            layers 1 thru nlyrs (the halfspace)
c
c        nlyrs - total number of layers, layer nlyrs is
c            the half space
c
c
      logical psvwav,shwave
c
c        commons and declarations
c
c
       include  'kennet.inc'
c
c        complex declarations
c
      complex mum11,mum12,mum21,mum22,mup11,mup12,mup21,mup22
      complex mdm11,mdm12,mdm21,mdm22,mdp11,mdp12,mdp21,mdp22
      complex num11,num12,num21,num22,nup11,nup12,nup21,nup22
      complex ndm11,ndm12,ndm21,ndm22,ndp11,ndp12,ndp21,ndp22
      complex xip,xim,etap,etam,epap,epam,epbp,epbm,mum,mup
      complex alfam,alfap,betam,betap,rhom,rhop
c
      complex i,zero,one,two,quartr,w
      complex t1,t2,zshp,zshm
      complex t11,t12,t21,t22,det,l11,l12,l21,l22
      real twopi,eps
      integer lyr
      intrinsic csqrt
      complex vslow
      external vslow
      data twopi,eps/6.2831853,.001/,i,zero/(0.,1.),(0.,0.)/,
     & one,two/(1.,0.),(2.,0.)/,quartr/(0.25,0.)/
c
c
      w = twopi*f
c     if(f .eq. (0.,0.)) w = (1.0e-6,0.)
      shwave = psvsh .eq. 3
      psvwav = psvsh .le. 2
c
c
c
      alfam = alfa(1)
      betam = beta(1)
      rhom = rho(1)
      mum = betam*betam * rhom
c     vslow is vertical slowness pv = sqrt(c-2 - p2)
      xim = vslow(alfam,p,f)
      etam = vslow(betam,p,f)
      xi(1) = xim
      eta(1) =  etam
c     define eta normalization factors (3.32, 3.33)
c
      epam = one / csqrt( two*rhom*xim )
      epbm = one / csqrt( two*rhom*etam )
c
c     if ( real( one/(alfam*alfam) ) .gt. real( p*p) ) then
c 	   epam = one / csqrt( two*rhom*xim )
c     else
c	   epam = one / csqrt( i*two*rhom*xim )
c     endif
c     if ( real( one/(betam*betam) ) .gt. real( p*p) ) then
c	   epbm = one / csqrt( two*rhom*etam )
c     else
c	   epbm = one / csqrt( i*two*rhom*etam )
c     endif
c
      t1 = two * mum * p
      t2 = t1 * p - rhom
c
c     form layer 1 matrices for free surface and interface 1
c     Mu,d, Nu,d up and downgoing disp and stress matrices for
c     P,SV waves (3.37), make up D, for b=Dv transformation
c
      mdm11 = i * xim * epam
      mum11 = - mdm11
      mdm12 = p * epbm
      mum12 = mdm12
      mdm21 = p * epam
      mum21 = mdm21
      mdm22 = i * etam * epbm
      mum22 = - mdm22
      ndm11 = t2 * epam
      num11 = ndm11
      ndm12 = t1 * mdm22
      num12 = -ndm12
      ndm21 = t1 * mdm11
      num21 = -ndm21
      ndm22 = t2 * epbm
      num22 = ndm22
      zshm = mum * etam
c
c     calculate the free surface reflection matrix (5.66), and 
c     free surface displacement operator (5.69).
c     free-surface reflection matrix (5.65) due to incident 
c     upgoing wave
c
      det = ndm11*ndm22 - ndm12*ndm21
      det = one/det
c     -Nd0**-1
      t11 = -ndm22*det
      t22 = -ndm11*det
      t12 = ndm12*det
      t21 = ndm21*det
c     ~
c     R
      ruppfs = t11*num11 + t12*num21
      rupsfs = t11*num12 + t12*num22
      ruspfs = t21*num11 + t22*num21
      russfs = t21*num12 + t22*num22
c     SH free-surface reflection (5.66H)
      rushfs = one
c 
c     free-surface displacement matrix due to incident upgoing
c                              ~
c     wave (5.69) W = Mu0 + Dd0R
c
      dvpfs = mum11 + mdm11*ruppfs + mdm12*ruspfs
      drpfs = mum21 + mdm21*ruppfs + mdm22*ruspfs
      dvsfs = mum12 + mdm11*rupsfs + mdm12*russfs
      drsfs = mum22 + mdm21*rupsfs + mdm22*russfs
c     SH downgoing amplitude doubling at free-surface (5.70H)
c     but with energy normaliaztion factro correcting (5.70H)
      dtshfs = two * epbm / betam
c
c        now do the interfaces, and save below matrices into above matrices
c        before starting next interface
c        construct (3.37) displacement and stress matrices
c        Mu,d, Nu,d partitions of D matrix
c
      do 10 lyr = 1, nlyrs-1
c
         alfap = alfa(lyr+1)
         betap = beta(lyr+1)
         rhop = rho(lyr+1)
         mup = betap*betap * rhop
         xip = vslow(alfap,p,f)
         etap = vslow(betap,p,f)
         xi(lyr+1) = xip
         eta(lyr+1) =  etap
c
c        (3.32) normalization
         epap = one / csqrt( two*rhop*xip )
         epbp = one / csqrt( two*rhop*etap )
c
c 	 if ( real( one/(alfap*alfap) ) .gt. real( p*p) ) then
c	      epap = one / csqrt( two*rhop*xip )
c	 else
c	      epap = one / csqrt( i*two*rhop*xip )
c	 endif
c	 if ( real( one/(betap*betap) ) .gt. real( p*p) ) then
c	      epbp = one / csqrt( two*rhop*etap )
c	 else
c	      epbp = one / csqrt( i*two*rhop*etap )
c	 endif
c
         t1 = two * mup * p
         t2 = t1 * p - rhop
c
c        Mu,d, Nu,d (3.37)
c
         mdp11 = i * xip * epap
         mup11 = - mdp11
         mdp12 = p * epbp
         mup12 = mdp12
         mdp21 = p * epap
         mup21 = mdp21
         mdp22 = i * etap * epbp
         mup22 = - mdp22
         ndp11 = t2*epap
         nup11 = ndp11
         ndp12 = t1 * mdp22
         nup12 = -ndp12
         ndp21 = t1 * mdp11
         nup21 = -ndp21
         ndp22 = t2*epbp
         nup22 = ndp22
         zshp = mup * etap
c
c          calculate coupling matrix Q and its inverse for transmission 
c          coefficients for incident up and downgoing waves (5.13)
c
         t11 = mum11*ndp11 + mum21*ndp21 - num11*mdp11 - num21*mdp21
         t21 = mum12*ndp11 + mum22*ndp21 - num12*mdp11 - num22*mdp21
         t12 = mum11*ndp12 + mum21*ndp22 - num11*mdp12 - num21*mdp22
         t22 = mum12*ndp12 + mum22*ndp22 - num12*mdp12 - num22*mdp22
         det = t11*t22 - t12*t21
	 det = one/det
         l12 = -t12*det
         l21 = -t21*det
         l22 = t11*det
         l11 = t22*det
c
c         calculate up/down tranmission matrices (5.13 - 5.19)
c         Tu(I) = (Td(i))**T (5.24)
c
         tdpp(lyr) = i*l11
         tdps(lyr) = i*l12
         tdsp(lyr) = i*l21
         tdss(lyr) = i*l22
         tupp(lyr) = i*l11
         tups(lyr) = i*l21
         tusp(lyr) = i*l12
         tuss(lyr) = i*l22
         tush(lyr) = two*csqrt(zshp*zshm)/(zshp+zshm)
         tdsh(lyr) = tush(lyr)
c
c          calculate up/down reflectivity matrices (5.19, 5.24)
c
         t11 = mdm11*ndp11 + mdm21*ndp21 - ndm11*mdp11 - ndm21*mdp21
         t21 = mdm12*ndp11 + mdm22*ndp21 - ndm12*mdp11 - ndm22*mdp21
         t12 = mdm11*ndp12 + mdm21*ndp22 - ndm11*mdp12 - ndm21*mdp22
         t22 = mdm12*ndp12 + mdm22*ndp22 - ndm12*mdp12 - ndm22*mdp22
         rdpp(lyr) = - t11*l11 - t12*l21
         rdps(lyr) = - t11*l12 - t12*l22
         rdsp(lyr) = - t21*l11 - t22*l21
         rdss(lyr) = - t21*l12 - t22*l22
         rdsh(lyr) = (zshm - zshp)/(zshm + zshp)
c
         t11 = mum11*nup11 + mum21*nup21 - num11*mup11 - num21*mup21
         t21 = mum12*nup11 + mum22*nup21 - num12*mup11 - num22*mup21
         t12 = mum11*nup12 + mum21*nup22 - num11*mup12 - num21*mup22
         t22 = mum12*nup12 + mum22*nup22 - num12*mup12 - num22*mup22
         rupp(lyr) = - l11*t11 - l12*t21
         rups(lyr) = - l11*t12 - l12*t22
         rusp(lyr) = - l21*t11 - l22*t21
         russ(lyr) = - l21*t12 - l22*t22
         rush(lyr) = - rdsh(lyr)
c
c     copy the n and m matrices if this is source layer
c
         if ( lyr .eq. srclyr ) then
	    xis = xim
	    etas = etam
	    epas = epam
	    epbs = epbm
            nus11 = num11
            nus12 = num12
            nus21 = num21
            nus22 = num22
            nussh = -i*rhom*betam*etam*epbm
            nds11 = ndm11
            nds12 = ndm12
            nds21 = ndm21
            nds22 = ndm22
            ndssh = -nussh
            mus11 = mum11
            mus12 = mum12
            mus21 = mum21
            mus22 = mum22
            mussh = epbm/betam
            mds11 = mdm11
            mds12 = mdm12
            mds21 = mdm21
            mds22 = mdm22
            mdssh = mussh
            rhos = rhom
            alfas = alfam
            betas = betam
          endif
c
c	 copy the above values to storage for  inversion
c        copy the below values to above values for next interface
c
	 mu(lyr) = mum
	 epa(lyr) = epam
	 epb(lyr) = epbm
         nu11(lyr) = num11
         nu12(lyr) = num12
         nu21(lyr) = num21
         nu22(lyr) = num22
         nd11(lyr) = ndm11
         nd12(lyr) = ndm12
         nd21(lyr) = ndm21
         nd22(lyr) = ndm22
         mu11(lyr) = mum11
         mu12(lyr) = mum12
         mu21(lyr) = mum21
         mu22(lyr) = mum22
         md11(lyr) = mdm11
         md12(lyr) = mdm12
         md21(lyr) = mdm21
         md22(lyr) = mdm22
	 zsh(lyr) = zshm
         alfam = alfap
         betam = betap
         rhom = rhop
         mum = mup
         xim = xip
         etam = etap
         epam = epap
         epbm = epbp
         num11 = nup11
         num12 = nup12
         num21 = nup21
         num22 = nup22
         ndm11 = ndp11
         ndm12 = ndp12
         ndm21 = ndp21
         ndm22 = ndp22
         mum11 = mup11
         mum12 = mup12
         mum21 = mup21
         mum22 = mup22
         mdm11 = mdp11
         mdm12 = mdp12
         mdm21 = mdp21
         mdm22 = mdp22
	 zshm = zshp
10    continue
c
c	copy the layer matrices for halfspace to inversion storage
c
	 mu(nlyrs) = mup
	 epa(nlyrs) = epam
	 epb(nlyrs) = epbm
         nu11(nlyrs) = nup11
         nu12(nlyrs) = nup12
         nu21(nlyrs) = nup21
         nu22(nlyrs) = nup22
         nd11(nlyrs) = ndp11
         nd12(nlyrs) = ndp12
         nd21(nlyrs) = ndp21
         nd22(nlyrs) = ndp22
         mu11(nlyrs) = mup11
         mu12(nlyrs) = mup12
         mu21(nlyrs) = mup21
         mu22(nlyrs) = mup22
         md11(nlyrs) = mdp11
         md12(nlyrs) = mdp12
         md21(nlyrs) = mdp21
         md22(nlyrs) = mdp22
	 zsh(nlyrs) = zshp
c
c
      return
      end
