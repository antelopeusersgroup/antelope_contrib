      subroutine rayle  (nn,tleng, alpan,betan,rhon,c, alpam,betam,rhom,
     2                   thikm, nl, tranf, iwave)
c
c   a program to calculate the crustal transfer functions of p wave and
c     shear wave(sv) for the receiver crust.
c
c     iwave = a parameter to specify the p or sv wave component.
c	      if = 1 : for P(z) wave component.
c             if = 2 : for P(x) wave component.
c             if = 3 : for transfer ratio, P(x)/P(z) (radial receiver function)
c             if = 4 : for SV(x) wave component.
c
      double precision alpam(1),betam(1),rhom(1),thikm(1),e(4,4),
     2                 alpan,apnsq,betan,btnsq,c,csq,ralpn,rbetn,gaman,
     3                 rhon,omega,pk,apmsq,btmsq,ralpm,rbetm,gamam,
     4                 gamm1,gamsq,gm1sq,pm,qm,rocsq,sinpm,sinqm,
     5                 cospm,cosqm, anorm
      complex a(4,4),t(4,4),p(4,4),tranf(1),jwp,jus,jup,jws,frayl,cimag
c
c debug output
c	write (0,*) "RAYLE: hfsp=",alpan,betan,rhon," phvel=",c
c	write (0,*) "  nl=",nl," lyr1=",alpam(1),betam(1),rhom(1),thikm(1)
c	write (0,*) "  nn=",nn," tlen=",tleng," iwave=",iwave

      nsub1=nl-1
      apnsq=alpan**2
      btnsq=betan**2
      csq=c**2
      ralpn=dsqrt(csq/apnsq-1.0d+00)
      rbetn=dsqrt(csq/btnsq-1.0d+00)
      gaman=2.0d+00*btnsq/csq
      cimag = cmplx(0.,1.)
c
c.....construction of stress-displacement vector transform matrix (e).
c
      e(1,1)=-2.0d+00*btnsq/apnsq
      e(1,2)=0.0d+00
      e(1,3)=1.0d+00/(rhon*apnsq)
      e(1,4)=0.0d+00
      e(2,1)=0.0d0
      e(2,2)=csq*(gaman-1.0d+00)/(apnsq*ralpn)
      e(2,3)=0.0d+00
      e(2,4)=1.0d+00/(rhon*apnsq*ralpn)
      e(3,1)=(gaman-1.0d+00)/(gaman*rbetn)
      e(3,2)=0.0d0
      e(3,3)=-1.0d+00/(rhon*csq*gaman*rbetn)
      e(3,4)=0.0d+00
      e(4,1)=0.0d0
      e(4,2)=1.0d+00
      e(4,3)=0.0d0
      e(4,4)=1.0d+00/(rhon*csq*gaman)
c
c
c...formulation of propagator matrix for each layer and consequtive
c   matrix product from bottom to top free surface.
c
      do 200 i=2,nn
      freq = float(i-1)/tleng
      omega = 6.2831853072d+00*dble(freq)
      pk = omega/c
c
      do 100 j=1,nsub1
c
      m = nl - j
c
      apmsq=alpam(m)**2
      btmsq=betam(m)**2
      ralpm=dsqrt(csq/apmsq-1.0d+00)
      rbetm=dsqrt(csq/btmsq-1.0d+00)
      gamam=2.0d+00*btmsq/csq
      gamm1=(gamam-1.0d+00)
      gamsq=gamam**2
      gm1sq=gamm1**2
      rocsq=rhom(m)*csq
      pm=pk*ralpm*thikm(m)
      qm=pk*rbetm*thikm(m)
      sinpm=dsin(pm)
      sinqm=dsin(qm)
      cospm=dcos(pm)
      cosqm=dcos(qm)
c
c.....construction of progator matrix (a) from bottom to top.
c
      a(1,1)=gamam*cospm-gamm1*cosqm
      a(1,2)=cimag*(gamm1*sinpm/ralpm+gamam*rbetm*sinqm)
      a(1,3)=-(cospm-cosqm)/rocsq
      a(1,4)=cimag*(sinpm/ralpm+rbetm*sinqm)/rocsq
      a(2,1)=-cimag*(gamam*ralpm*sinpm+gamm1*sinqm/rbetm)
      a(2,2)=-gamm1*cospm+gamam*cosqm
      a(2,3)=cimag*(ralpm*sinpm+sinqm/rbetm)/rocsq
      a(2,4)=a(1,3)
      a(3,1)=rocsq*gamam*gamm1*(cospm-cosqm)
      a(3,2)=cimag*rocsq*(gm1sq*sinpm/ralpm+gamsq*rbetm*sinqm)
      a(3,3)=a(2,2)
      a(3,4)=a(1,2)
      a(4,1)=cimag*rocsq*(gamsq*ralpm*sinpm+gm1sq*sinqm/rbetm)
      a(4,2)=a(3,1)
      a(4,3)=a(2,1)
      a(4,4)=a(1,1)
c
c.....first propagator matrix (a), layer index (n-1).
c
      if (j - 1)  40, 40, 50
c
   40    do 101 k=1,4
         do 101 l=1,4
         t(k,l) = cmplx(0.,0.)
         do 101 n =1,4
         t(k,l) = e(k,n)*a(n,l) + t(k,l)
  101    continue
      go to 100
c
c
c.....continuous propagator matrix multiplication, i.e., (p) = (t)*(a).
c
   50    do 102 k=1,4
         do 102 l=1,4
         p(k,l) = cmplx(0.,0.)
         do 102 n=1,4
         p(k,l) = t(k,n)*a(n,l) + p(k,l)
  102    continue
c
         do 103 k=1,4
         do 103 l=1,4
         t(k,l) = p(k,l)
         p(k,l) = cmplx(0.,0.)
  103    continue
c
c
  100 continue
c
c.....calculation of secular function of rayleigh wave \frayl\ and
c     transfer functions of p or sv waves.
c
      jwp = t(3,1) - t(4,1)
      jup = t(3,2) - t(4,2)
      jws = t(1,1) - t(2,1)
      jus = t(1,2) - t(2,2)
c
      frayl = jws*jup - jus*jwp
c
      go to (33,34,35,32), iwave
c
c...4..sv wave horizontal component, receiver crust.
c
   32 anorm = c/betan
      tranf(i) = anorm*jus/frayl
      go to 200
c
c...1..p wave vertical component, receiver crust.
c
   33 anorm = 2.0d+00*c/alpan
      tranf(i) = anorm*(-jwp/frayl)
      go to 200
c
c...2..p wave horizontal component, receiver crust.
c
   34 anorm = 2.0d+00*rhon*alpan/c
      tranf(i) = anorm*((t(4,2) - t(3,2))/frayl)
      go to 200
c
c..3..P wave transfer ratio P(x)/P(z)
c
  35  anorm = (rhon*alpan*alpan)/(c*c)
      tranf(i) = anorm*(-(t(4,2) - t(3,2))/jwp)
c
  200 continue
      tranf(1) = cmplx(0.0, 0.0)
c
      return
      end
