      subroutine disptf(p,f,islyr,d,nd,irlyr,recdep,nlyrs)
         complex p,f
         integer islyr,nlyrs,irlyr,nd
         real d(nd),recdep
c
c     compute the displacement transfer function of the medium
c     to upgoing and down going waves for a given
c     slowness p and frequency f
c     source is in layer islyr, and depth d from top of layer
c     model has total nlyrs including half space below
c
      include 'kennet.inc'
      complex rfspp,rfsps,rfssp,rfsss,rfssh
      complex rslpp,rslps,rslsp,rslss,rslsh
      complex t11,t12,t21,t22,l11,l12,l21,l22,lsh
      complex xpp,xps,xsp,xss,xsh,ypp,yps,ysp,yss,ysh
      complex  rdvpup,rdvsup,rdrpup,rdrsup,rdtsup
c     complex*16 det
      complex det
      complex i,one,w,twopi,php,phs,phpp,phps,phss,oo,to
      integer id
      real depth
      complex cphs
      external cphs,rcvrfn,rufs,refl,burec
      data i,one,twopi/(0.,1.),(1.,0.),(6.2831853,0.)/
      data oo,to/(1.,1.),(2.,1.)/
      w = twopi * f
c
c      compute receiver function, free surface displacement
c      from a plane wave incident from a source layer (islyr)
c      1st part of (7.36)
c      use rcvrfs for surface receiver and Rurs
c      burec for buried receiver, and rufs for Rufs
c
      if(irlyr .eq. 1 .and. recdep .eq. 0.) then
c     call rcvrfs(p,f,islyr,rdvpup,rdrpup,rdvsup,rdrsup,rdtsup,
c    *                      rfspp,rfsps,rfssp,rfsss,rfssh)
      call rcvrfs(p,f,islyr,rdvpup,rdrpup,rdvsup,rdrsup,rdtsup,
     *                      rfspp,rfsps,rfssp,rfsss,rfssh)
      else
      call brcvrfs(p,f,islyr,irlyr,recdep,
     *		 rdvpup,rdvsup,rdrpup,rdrsup,rdtsup,
     *           rfspp,rfsps,rfssp,rfsss,rfssh)
c
c      compute relectivity from a plane wave incident from 
c      below, see Rufs in (7.36)
c
c     call rufs(p,f,islyr,rfspp,rfsps,rfssp,rfsss,rfssh)
      endif
c
c      Rd(SL) in (7.36)
c
      call refl(p,f,islyr,nlyrs,rslpp,rslps,rslsp,rslss,rslsh)
c     call refltd(p,f,islyr,nlyrs,rslpp,rslps,rslsp,rslss,rslsh)
c
c     assemble the terms with appropriate phase delays for the required depths
c
      do 10 id = 1, nd
      if ( islyr .eq. irlyr ) then
	depth =  d(id) - recdep 
      else
	depth = d(id)
      endif
      php = cphs( -i*w*xi(islyr)*depth )
      phs = cphs( -i*w*eta(islyr)*depth )
c      phpp = cphs( -i*2.*w*xi(islyr)*depth )
c      phss = cphs( -i*2.*w*eta(islyr)*depth )
c      phps = cphs( -i*w*(xi(islyr)+eta(islyr))*depth )
      phpp = php * php
      phss = phs * phs
      phps = php * phs
c
c     the receiver function or buried receiver for the path to the surface/receiver
c
      dvpup(id) = rdvpup * php
      dvsup(id) = rdvsup * phs
      drpup(id) = rdrpup * php
      drsup(id) = rdrsup * phs
      dtsup(id) = rdtsup * phs
c
c      relectivity from a plane wave incident from 
c      below, see Rufs in (7.36)
c
      xpp = rfspp * phpp
      xps = rfsps * phps
      xsp = rfssp * phps
      xss = rfsss * phss
      xsh = rfssh * phss
c
c      Rd(SL) in (7.36)
c
      php = cphs( -i*w*xi(islyr)*(thik(islyr)-d(id)) )
      phs = cphs( -i*w*eta(islyr)*(thik(islyr)-d(id)) )
c      phpp = cphs( -i*2.*w*xi(islyr)*(thik(islyr)-d(id)) )
c      phss = cphs( -i*2.*w*eta(islyr)*(thik(islyr)-d(id)) )
c      phps = cphs( -i*w*(xi(islyr)+eta(islyr))*(thik(islyr)-d(id)) )
      phpp = php * php
      phss = phs * phs
      phps = php * phs
      ypp = rslpp * phpp
      yps = rslps * phps
      ysp = rslsp * phps
      yss = rslss * phss
      ysh = rslsh * phss
c
c     l11 = one - rslpp*rfspp - rslps*rfssp
c     l12 = -rslpp*rfsps - rslps*rfsss
c     l21 = -rslsp*rfspp - rslss*rfssp
c     l22 = one - rslsp*rfsps - rslss*rfsss
c
c     the following 8 lines serve to keep the dynamic
c     range of the numbers in l to a reasonable level
c     basically adding and subtracting one, and letting
c     the truncation in normalization before a floating add
c     take care of numbers too small to be significant
c     physically, but able to cause underflow in products
c     that follow, bombing the code needlessly....
c     if machine sets underflow to zero, this could be
c     removed, and returned to the inital straightforward
c     coding.
c
c                   SL  fS
c       form [I - Rd  Ru  ]**-1 in (7.36)
c
      l11 = -oo - ypp*xpp - yps*xsp
      l12 = -oo - ypp*xps - yps*xss
      l21 = -oo - ysp*xpp - yss*xsp
      l22 = -oo - ysp*xps - yss*xss
      l11 = to + l11
      l12 = oo + l12
      l21 = oo + l21
      l22 = to + l22
      det = l11*l22 - l12*l21
      det = one / det
      l12 = -l12*det
      l21 = -l21*det
      t11 = l11*det
      l11 = l22*det
      l22 = t11
      lsh = one / ( one - ysh*xsh )
c
c      mult by W[I - Rd(0S)*R(0f)]**-1 Tu(0S)  from rcvrfn
c      or Mur + Mdr*Ru(fR)[I - Rd(RS)*Ru(fR)]**-1 Tu(RS)
c
      t11 = dvpup(id)*l11 + dvsup(id)*l21
      t12 = dvpup(id)*l12 + dvsup(id)*l22
      t21 = drpup(id)*l11 + drsup(id)*l21
      t22 = drpup(id)*l12 + drsup(id)*l22
      dvpup(id) = t11
      dvsup(id) = t12
      drpup(id) = t21
      drsup(id) = t22
      dtsup(id) = dtsup(id)*lsh
c
c                         sL
c      post multiply by Rd
c
      dvpdn(id) = t11*ypp + t12*ysp
      dvsdn(id) = t11*yps + t12*yss
      drpdn(id) = t21*ypp + t22*ysp
      drsdn(id) = t21*yps + t22*yss
      dtsdn(id) = dtsup(id)*ysh
10    continue
c
      return
      end
