      subroutine ginterp(ze,jeq,zs,jst,dist,pout,tout,ierr)
c     finds p and t for a given x using guy's interps.
c     must call subroutines readmod,finlay, and table first.
c     calls interps(in this file) to do linear interpolation
c     between table points.  calls subroutine time.
c     input parameters:
c     ze=depth of event       jeq=model layer containing ze
c     zs=depth of station     jst=model layer containing zs
c     dist=epicentral distance   
c     output arguments:
c     pout=ray parameter (negative for direct ray)
c     tout=time               ierr=0 means no errors
c     call readmod and finlay first to find jeq,jst

      implicit real*8(a-h,o-z)
      parameter(mnod=25)
      common /mod/g(mnod),b(mnod),z(mnod),u(mnod),nodes
      common /travtable/ptab(200),xtab(200),ttab(200),maxpindex
       ierr=-5
       pout=0.0d0
       tout=10000.d0


c      if table starts at too great a distance
c      generate a new table
       if (xtab(1).gt.dist) then
 11       continue
          ptab(1)=ptab(1)/2.0d0
          call time(ze,jeq,zs,jst,ptab(1),xtab(1),ttab(1),tau,ierr)
          if (xtab(1).gt.dist) goto 11
       endif
c      find closest points in table
       del1=xtab(1)-dist
       do 20 j=2,maxpindex
         del2=xtab(j)-dist
         if((del1*del2).lt.0.d0) then
           call interps(ze,jeq,zs,jst,dist,j,pbar,tbar,interr)
c           print *,'ran interp',j
           if (tbar.lt.tout) then
             tout=tbar
             pout=pbar
             ierr=interr
           endif
         endif
         del1=del2
 20    continue
  
c       print *,dist,tout,pout
       return
       end


      subroutine interps(ze,jeq,zs,jst,dist,k,pout,ans,ierr)
c     interpolates for time(tbar) and p(pbar) at dist which is
c     between x(k) and x(k-1) using linear interpolation or 
c     bisection as appropriate.
c     input parameters:
c     ze=depth of event       jeq=model layer containing ze
c     zs=depth of station     jst=model layer containing zs
c     dist=epicentral distance  k=dist is between xtab(k)&xtab(k-1)   
c     output arguments:
c     pout=ray parameter (negative for direct ray)
c     ans=time               ierr=0 means no errors
c     calls subroutine time.
      
      implicit real*8 (a-h,o-z)
      parameter(mnod=25)
      common /mod/g(mnod),b(mnod),z(mnod),u(mnod),nodes
      common /travtable/ptab(200),xtab(200),ttab(200),maxpindex
      data  tol,xjump/5.d-16,1.d-4/
c      open (unit=15,file='debint')
      ans=20000.d0
      it=0
      bb=ptab(k)
      fb=xtab(k)-dist
      c=ptab(k-1)
      fc=xtab(k-1)-dist
      s=c
      fs=fc
c*** bisect ***
 5    h=0.5d0*(bb+c)
c      write (15,87)bb,fb,c,fc
c 87   format(4(f25.18,1x))
      t=h*tol
c*** check for convergence ***
      db=dabs(fb)
      dc=dabs(fc)
      it=it+1
      if(dabs(h-bb).lt.dabs(t)) then
c          close (15)
          goto 35
      endif
      if(db.le.dc) goto 10
      y=bb
      fy=fb
      gg=bb
      fg=fb
      s=c
      fs=fc
      goto 15
 10   y=s
      fy=fs
      gg=c
      fg=fc
      s=bb
      fs=fb
 15   if(fy.eq.fs) goto 20
      bb=(s*fy-y*fs)/(fy-fs)
      if(dabs(bb-s).lt.t) bb=s+dsign(t,gg-s)
      if((bb-h)*(s-bb).lt.0.d0) bb=h
      goto 25
 20   bb=h
 25   call time(ze,jeq,zs,jst,bb,xtry,ttry,tau,ierr)
      if(ierr.lt.0) return
      fb=xtry-dist
      if(fg*fb.lt.0.d0) goto 30
      c=s
      fc=fs
      goto 5
 30   c=gg
      fc=fg
      goto 5
c*** when unable to find arrival, linearly interpolates for time
 35   if(dmin1(dc,db).gt.xjump) then
         call time(ze,jeq,zs,jst,bb,xb,tb,tau,ierr)
         call time(ze,jeq,zs,jst,c,xc,tc,tau,ierr)
         ans=(tc*fb-tb*fc)/(fb-fc)
         pout=bb
         return
      endif
      ans=ttry
      pout=bb
      return
      end
      subroutine hderiv(ev,arti,stn,levs,sig,wt,sc,mdim,
     +   natot,a,tres,ma,save,ierr)
c     ev=input current event location
c     arti=input arrival times (p or s )
c     stn=input station coordinates
c     levs=input slowness at station elev and node index
c     sig=input station weight
c     wt=input arrival time weight
c     sc=input station correction
c     mdim=input max stations array dimension
c     natot=output total arrivals (p and s)
c     a=output partial derivative matrix
c     tres=ouput residual/std dev or weight
c     ma=2*mdim
c     this version has pick weighting(8/22/90)
c     this version has expanded save array to save wt,residual,
c     epdis,rayparameter,azimuth (11/30/87 7:00a)
c     raytrace,calculate hypocentral derivative matrix elements
c     save(1,i)=weight
c         (2,i)=residual
c         (3,i)=epicentral distance
c         (4,i)=ray parameter
c         (5,i)=azimuth from station to event
c     (11/28/87 5:30)
c     error conditions:
c        ierr=2 model not deep enough
c        ierr=4 a moho pick


      implicit real*8 (a-h,o-z)
      parameter (pion180=3.14159265358979d0/180.d0)
      parameter(mnod=25)
      common /mod/g(mnod),b(mnod),z(mnod),u(mnod),nodes
      common /travtable/ptab(200),xtab(200),ttab(200),maxpindex
      parameter(ndim=4)
      dimension ev(ndim),arti(mdim),stn(ndim,mdim),wt(mdim),
     &levs(mdim),sig(mdim),sc(mdim),save(5,mdim),a(ma,ndim),
     &tres(ma)
      ierr=0
      rinc=5.d0

      do 150 j=1,mdim
         do 160 k=2,5
            save(k,j)=0.d0
 160     continue
 150  continue
c     find slowness at depth of event
      call finlay(ev(3),layev)
      uze=1.d0/(b(layev)+g(layev)*(ev(3)-z(layev)))
      nnn=natot
c***  this index because not always 23 stations reporting
c     create a matrix with no zero rows
      do 200 nn=1,mdim
         if (arti(nn).ne.0.d0) then
            nnn=nnn+1
            epdis=dsqrt((ev(1)-stn(1,nn))**2+
     +      (ev(2)-stn(2,nn))**2)
c           calculate travtime table for that station elev
            call table(ev(3),layev,stn(3,nn),levs(nn),rinc)
c           interpolate for correct epicentral distance
            call ginterp(ev(3),layev,stn(3,nn),levs(nn),
     +      epdis,pep,tep,interr)
            totwt=wt(nn)*sig(nn)
            save(2,nn)=(arti(nn)-ev(4)-tep-sc(nn))
            save(3,nn)=epdis
            save(4,nn)=pep
            save(5,nn)=datan2((ev(1)-stn(1,nn)),
     +      (ev(2)-stn(2,nn)))/pion180

c*** check that model has enough layers
            if(pep.gt.0.d0.and.pep.lt.u(nodes)) then
               write(45,170)nn,stn(3,nn),epdis,ev(3),pep
 170           format('ray out of model',i4,1x,4f15.8)
               save(1,nn)=4.0d0
               nnn=nnn-1
               ierr=2
               goto 190
            endif

            tres(nnn)=save(2,nn)/totwt
c***        calculate a(nnn,1),dt/devx
            dtdx=dabs(pep)*(ev(1)-stn(1,nn))/epdis
            a(nnn,1)=dtdx/totwt
c***        calculate a(nnn,2),dt/devy
            dtdy=dabs(pep)*(ev(2)-stn(2,nn))/epdis
            a(nnn,2)=dtdy/totwt
c***        calculate a(nnn,3),dt/devz
            dtdz=-dsqrt(uze**2-pep**2)
            dtdz=-dsign(dtdz,pep)
            a(nnn,3)=dtdz/totwt
c***        calculate a(nnn,4),dt/devt
            a(nnn,4)=1.d0/totwt
         endif
 190     continue
c     station loop
  200 continue
      natot=nnn
      return
      end

      subroutine retray(ev,part,sart,stn,levs,psc,ssc,
     &psig,ssig,pwt,swt,mdim,psave,ssave,
     &a,tres,ma,chip,chis,nparr,nsarr,ierr)
c     simply trace rays(11/7/90)
c     this version has pick weighting.(8/22/90)
c     psave(1,i)=weight
c          (2,i)=residual
c          (3,i)=epicentral distance
c          (4,i)=ray parameter
c          (5,i)=azimuth
c     version separates chip and chis (3/4/88)
c     ev(1-4) is event x,y,z,origin time
c
c     calls subroutines:
c         finlay       table      ginterp
c         svd          mult       eigsrt
c         hderiv       switchv
c
c     error conditions
c        ierr=1 event depth perturbed above ground
c        ierr=2 model not deep enough
c        ierr=3 location didn't converge in maxiter iterations
c        ierr=4 moho pick in this event
c        ierr=5 less than 4 arrivals
c        ierr=6 less than 4 arrivals above model limit

      implicit real*8 (a-h,o-z)
      parameter(mnod=25)
      common /mod/g(mnod),b(mnod),z(mnod),u(mnod),nodes
      common /psmod/psg(mnod,2),psb(mnod,2),psz(mnod,2),psu(mnod,2),nops
      common /travtable/ptab(200),xtab(200),ttab(200),maxpindex
      parameter(ndim=4,ldim=1,iwt=0)
      dimension stn(ndim,mdim),levs(mdim),ev(ndim),
     &part(mdim),sart(mdim),psc(mdim),psig(mdim),ssc(mdim),
     &ssig(mdim),pwt(mdim),swt(mdim),tres(ma),a(ma,ndim),
     &psave(5,mdim),ssave(5,mdim)
c     these are internal to subroutine
      ierr=0
      chilast=1.d0



c              zero arrays
               do 5070 j=1,ma
                  tres(j)=0.d0
 5070          continue
               chi=0.d0
               natot=0
               call switchv(1)
               call hderiv(ev,part,stn,levs,psig,pwt,psc,mdim,
     +         natot,a,tres,ma,psave,ierr)
               nparr=natot
               call switchv(2)
               call hderiv(ev,sart,stn,levs,ssig,swt,ssc,mdim,
     +         natot,a,tres,ma,ssave,ierr)
               nsarr=natot-nparr

               chip=0.d0
               chis=0.d0
               do 502 i=1,nparr
                   chip=chip+tres(i)*tres(i)
 502           continue
               do 503 i=nparr+1,natot
                   chis=chis+tres(i)*tres(i)
 503           continue
               chi=chip+chis

               chilast=chi
               return
 
            end
      subroutine switchv(ips)
c     switch from using the swave velocity model to the pwave
c     velocity model  (11/28/87 4:30)
c     called with ips=1  ==> change to p velocities
c     called with ips=2  ==> change to s velocities


      implicit real*8 (a-h,o-z)
      parameter (mnod=25)
      common /mod/g(mnod),b(mnod),z(mnod),u(mnod),nodes
      common /psmod/psg(mnod,2),psb(mnod,2),psz(mnod,2),psu(mnod,2),nops


      nodes=nops
      do 10 j=1,mnod
         g(j)=psg(j,ips)
         b(j)=psb(j,ips)
         z(j)=psz(j,ips)
         u(j)=psu(j,ips)
 10   continue

      return
      end
c****************************************************************
      subroutine unsw(ips)
c     after inversion put model back into p or s model storage(12/3 3)
c     called with ips=1  ==> change p velocities 
c     called with ips=2  ==> change s velocities


      implicit real*8 (a-h,o-z)
      parameter (mnod=25)
      common /mod/g(mnod),b(mnod),z(mnod),u(mnod),nodes
      common /psmod/psg(mnod,2),psb(mnod,2),psz(mnod,2),psu(mnod,2),nops


      nodes=nops
      do 10 j=1,mnod
         psg(j,ips)=g(j)
         psb(j,ips)=b(j)
         psz(j,ips)=z(j)
         psu(j,ips)=u(j)
 10   continue

      return
      end
       subroutine table(ze,jeq,zs,jst,ai)
c      calculates a table of t and x for p=sin(angle)/vmax
c      angle is incremented by ai
c     input parameters:
c     ze=depth of event       jeq=model layer containing ze
c     zs=depth of station     jst=model layer containing zs
c     output written to travtable
c     call readmod and finlay first to find jeq,jst
c     negative p's are direct rays

      implicit real*8 (a-h,o-z)
      parameter (mnod=25)
      common /mod/g(mnod),b(mnod),z(mnod),u(mnod),nodes
      common /travtable/ptab(200),xtab(200),ttab(200),maxpindex

       do 5 i=1,200
          ptab(i)=0.d0
          xtab(i)=0.d0
          ttab(i)=0.d0
 5     continue
c      remember reciprocity
       zlower=dmax1(ze,zs)
       zhigher=dmin1(ze,zs)
       call finlay(zlower,laylow)
       call finlay(zhigher,layhigh)
       pmax=1.d0/(b(laylow)+g(laylow)*(zlower-z(laylow)))
c      in case eq epicenter is in a lvz:
       do 6 k=layhigh,laylow
          if (pmax.gt.u(k)) pmax=u(k)
 6     continue
       i=0
c      direct ray part of table
       do 7 angle=ai,89.9,ai
         i=i+1
         ptab(i)=-dsin(angle*.0174532925)*pmax
         call time(ze,jeq,zs,jst,ptab(i),xtab(i),ttab(i),tau,ierr)
 7     continue
       i=i+1
       ptab(i)=-(pmax-1.d-17)
       call time(ze,jeq,zs,jst,ptab(i),xtab(i),ttab(i),tau,ierr)
       i=i+1
       ptab(i)=pmax-1.d-17
       call time(ze,jeq,zs,jst,ptab(i),xtab(i),ttab(i),tau,ierr)
c      turning ray part of table
       angle=89.9
 10    continue
         angle=angle-ai
         i=i+1
         ptab(i)=dsin(angle*.0174532925)*pmax
         call time(ze,jeq,zs,jst,ptab(i),xtab(i),ttab(i),tau,ierr)
       if (xtab(i).lt.6000.d0) goto 10
       if (i.gt.200) then
         print *,'too many values to fit in traveltime table'
       endif
       maxpindex=i
      
       return
       end
      subroutine time(ze,jeq,zs,jst,p,x,t,tau,ierr)
c     calculate epicentral distance,time,tau using a plane layer model
c     with constant gradients of velocity
c     input parameters:
c     ze=depth of event       jeq=model layer containing ze
c     zs=depth of station     jst=model layer containing zs
c     p=ray parameter (negative for direct ray)
c     output arguments:
c     x=epicentral distance   t=time
c     tau=delay time (t-p*x)  ierr=0 means no errors
c     call readmod and finlay first to find jeq,jst

      implicit real *8 (a-h,o-z)
      parameter (mnod=25)
      common /mod/g(mnod),b(mnod),z(mnod),u(mnod),nodes
      common /travtable/ptab(200),xtab(200),ttab(200),maxpindex

      ierr = 0  
      x = 0.d0
      t = 0.d0
      tau = 0.d0
      
c     flag for direct or turning ray
      turning=p
      p=dabs(p)
      uzp=p

      if (p.eq.0.d0) then
        ierr=-1
        return
      endif
      

c     find layer ray bottoms in
      do 30 j=1,nodes
        if (uzp.gt.u(j)) then
          jp=j-1
          goto 31
        endif
 30   continue
      jp=nodes
 31   continue

      


c     for the case when elevation of station is lower than
c     depth of earthquake, by reciprocity it's the same as
c     if the earthquake was at the depth of the station and
c     the station was at the depth of the quake.
      if (ze.lt.zs) then
        uze=1.d0/(b(jst)+g(jst)*(zs-z(jst)))
        uzs=1.d0/(b(jeq)+g(jeq)*(ze-z(jeq)))
        je=jst
        js=jeq
      else
        uzs=1.d0/(b(jst)+g(jst)*(zs-z(jst)))
        uze=1.d0/(b(jeq)+g(jeq)*(ze-z(jeq)))
        js=jst
        je=jeq
      endif

c     quit if uzp>uze or uzp>uzs
      if (uzp.gt.uzs) then
        ierr=-3
        p=turning
        return
      endif
      if (uzp.gt.uze) then
        ierr=-2
        p=turning
        return
      endif


c     calc time of upgoing ray
      t=tkern(uzs,p)/g(js)-tkern(uze,p)/g(je)
      x=xkern(uzs,p)/g(js)-xkern(uze,p)/g(je)
      do 50 m=js+1,je
        t=t+tkern(u(m),p)*(1.d0/g(m)-1.d0/g(m-1))
        x=x+xkern(u(m),p)*(1.d0/g(m)-1.d0/g(m-1))
 50   continue

      if (turning.gt.0d0) then
c       calc time of downgoing ray
        t=t+2.d0*( tkern(uze,p)/g(je) - tkern(uzp,p)/g(jp) )
        x=x+2.d0*( xkern(uze,p)/g(je) - xkern(uzp,p)/g(jp) )
        do 40 ii=je+1,jp
          t=t+2.d0*tkern(u(ii),p)*(1.d0/g(ii)-1.d0/g(ii-1))
          x=x+2.d0*xkern(u(ii),p)*(1.d0/g(ii)-1.d0/g(ii-1))
 40     continue
      endif

c     calc tau
      tau=t-p*x
      p=turning
      return
      end

      real*8 function tkern(u,p)
      implicit real *8 (a-h,o-z)
      tkern=dlog(u)+dlog(1.d0+dsqrt(1.d0-(p/u)*(p/u)))
c      tkern=dlog(u+dsqrt(u*u-p*p))
      return
      end

      real*8 function xkern(u,p)
      implicit real *8 (a-h,o-z)
      xkern=dsqrt(1.d0-(p/u)*(p/u))/p
c      xkern=dsqrt(u*u-p*p)/p/u
      return
      end

c $Id$ 
