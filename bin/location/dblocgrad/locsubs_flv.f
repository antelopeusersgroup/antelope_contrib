      subroutine covsub(esvd,fsvd,cov,cor,erdx,nvec,nlog,nit)
c     calculate resolution matrix too.
c     calculate covariance and errors and output them to unit nit
c     if nit=-1, no output

      implicit real*8 (a-h,o-z)
      dimension esvd(nvec,nvec),fsvd(nvec),cov(nvec,nvec),erdx(nvec)
      dimension cor(nvec,nvec)
      parameter(ldim=1)
      character *20 title
      data tol/5.d-14/

c     use space in cov matrix to calculate resolution matrix
      do 300 l=1,nlog
         do 305 k=1,nlog
            cov(k,l)=0.d0
            do 310 j=1,nlog
               cov(k,l)=cov(k,l)+esvd(l,j)*esvd(k,j)
 310        continue
 305     continue
 300  continue
      if(nit.ne.-1) then
      title='resolution matrix'
      call mfeout(cov,nvec,nlog,nlog,title,0,nit)
      endif


c***  calculate covariance matrix
      do 252 i=1,nlog
         do 253 j=1,nlog
            cov(j,i)=0.d0
            do 254 k=1,nlog
               if (fsvd(k).gt.tol) then
                  cov(j,i)=cov(j,i)+esvd(j,k)/fsvd(k)
     +              *esvd(i,k)/fsvd(k)
               endif
 254        continue
 253     continue
 252  continue

c***    calculate correlation matrix
        do 255 i=1,nlog
           if(cov(i,i).gt.tol) then
              do 256 j=1,nlog
                 if(cov(j,j).gt.tol) then
                    cor(j,i)=cov(j,i)/dsqrt(cov(j,j))/dsqrt(cov(i,i))
                  else
                    cor(j,i)=0.d0
                  endif
 256          continue
            else 
              do 258 j=1,nlog
                 cor(j,i)=0.d0
 258          continue
            endif
 255    continue

c***  calculate errors
      do 257 j=1,nlog
         erdx(j)=dsqrt(cov(j,j))
 257  continue
   
      if(nit.eq.-1) return

      title='covariance matrix'
      call mfeout(cov,nvec,nlog,nlog,title,0,nit)
      title='correlation matrix'
      call mfeout(cor,nvec,nlog,nlog,title,0,nit)
      title='errors'
      call mfeout(erdx,ldim,ldim,nlog,title,0,nit)
     
      return
      end      
      subroutine duppick(stn,stname,sig,levs,mdim,nex,nsta,ierr)
      implicit real*8 (a-h,o-z)
      character *7 c7
      dimension stn(4,mdim),sig(mdim),levs(mdim)
      character *6 stname(mdim)
      c7='       '

         nex=nex+1
         if(nex.gt.mdim) then
            print *,'too many repeated picks'
            ierr=-4
            nex=nex-10
            return
         endif
         do 20 i=1,4
            stn(i,nex)=stn(i,nsta)
 20      continue
         sig(nex)=sig(nsta)
         levs(nex)=levs(nsta)
         c7='d'//stname(nsta)
         stname(nex)=c7(1:6)
         nsta=nex
         return
         end
      subroutine jack(a,ma,ndim,narr,t,usvd,vsvd,qsvd,erx,
     +  atmp,ttmp,xtmp,xi,x1,x2,xst,nsize)
c     calculate only weighted jackknife estimate(3/22/90)
c     jackknife estimate of solution
c     nsize is the size of the sample set, nreal is the number of
      implicit real *8 (a-h,o-z)
      dimension a(ma,ndim),usvd(ma,ndim),qsvd(ndim),
     +  vsvd(ndim,ndim),t(ma),atmp(ma,ndim),erx(ndim),
     +  ttmp(ma),xtmp(ndim),xi(ndim),x1(ndim),x2(ndim),
     +  xst(ma,ndim)
      parameter (maxreal=882)
      dimension wmean(4),ws(maxreal)
    
c     jackknife estimate deleting ith arrival
      do 605 j=1,ndim
        x1(j)=0.d0
        x2(j)=0.d0
        wmean(j)=0.d0
 605  continue
      wtot=0.d0
      nreal=narr/nsize

      do 610 ireal=1,nreal

      if(nsize.eq.1) then
c       delete an arrival from matrix
        do 615 j=1,ndim
           ind=0
           do 620 i=1,ireal-1
              ind=ind+1
              atmp(ind,j)=a(i,j)
 620       continue
           do 625 i=ireal+1,narr
              ind=ind+1
              atmp(ind,j)=a(i,j)
 625       continue
 615    continue
           ind=0
           do 630 i=1,ireal-1
              ind=ind+1
              ttmp(ind)=t(i)
 630       continue
           do 635 i=ireal+1,narr
              ind=ind+1
              ttmp(ind)=t(i)
 635       continue
        njk=ind
      endif
         
        
c----------------------------------------------------------
c        invert matrix, get estimate xi
         call dinv(atmp,ttmp,ma,njk,ndim,xi,usvd,vsvd,qsvd,
     +   xtmp)

c-----------------------------------------------------------
c       calculate weight (weight is the determinant of matrix)
        ws(ireal)=1.d0
        do 160 i=1,ndim
           ws(ireal)=ws(ireal)*qsvd(i)*qsvd(i)
 160    continue
        wtot=wtot+ws(ireal)

        do 640 j=1,ndim
           wmean(j)=wmean(j)+ws(ireal)*xi(j)
           xst(ireal,j)=xi(j)
 640    continue

 610  continue

      if(wtot.eq.0.d0) then
            do 470 i=1,ndim
               do 475 j=1,ndim
                  vsvd(j,i)=999.999
 475           continue
               erx(i)=999.999
 470        continue
            return
      endif
           
      do 650 j=1,ndim
         x1(j)=wmean(j)/wtot
 650  continue

      scal1=dble(narr-nsize-ndim+1)/dble(nsize)
c     find weighted covariance matrix from bootstrap 
c     calculate b'*b
      do 400 k=1,ndim
         do 450 j=1,ndim
             vsvd(j,k)=0.d0
             do 500 i=1,nreal
                vsvd(j,k)=vsvd(j,k) + ws(i)*
     &            (xst(i,j)-x1(j))*(xst(i,k)-x1(k))
 500         continue
              vsvd(j,k)=scal1*vsvd(j,k)/wtot
 450     continue
 400  continue
      do 410 i=1,ndim
          erx(i)=dsqrt(vsvd(i,i))
 410  continue

       
      return
      end
      subroutine kickout(art,tres,mdim,narr,rwt,ibigout)
c
c  removes largest outlier if (tres(arrival)-rwt(1)) > rwt(2)
c  calculate residual weight vector (8/27/90)
c  input:
c	art	arrival times
c	tres	residual/std.dev weight
c	mdim	max stations array dimension
c	nparr	number of p arrivals
c	rwt	residual std dev cutoff
c  output:
c	ibigout	element of largest outlier which was removed

      implicit real*8 (a-h,o-z)
      dimension tres(mdim),rwt(3),art(mdim)

      if(rwt(2).eq.rwt(3)) then
c
c  step function weighting
c
         ibigout = 0
         bigout  = 0.d0
         nnn     = 0

         do 50 i = 1, mdim
            if (art(i) .ne. 0.d0) then
               nnn = nnn + 1
               dr  = dabs(tres(nnn) - rwt(1))
               if (dr .gt. rwt(2)) then
                  if (dr .gt. bigout) then
                     bigout  = dr
                     ibigout = i
                  endif
               endif
            endif
 50      continue

         if (ibigout .ne. 0) then
            art(ibigout) = 0.d0
         endif
      else
c        ramp to zero weighting not enabled yet
         print *,'ramp to zero weighting not enabled yet'
      endif

      return
      end

      subroutine leqs( ev, part, sart, stn, levs, psc, ssc,
     +                 psig, ssig, pwt, swt, rwt, mdim, psave, ssave, 
     +                 a, usvd, vsvd, qsvd, tres, ma, chip, chis, 
     +                 nparr, nsarr, ierr)
c
c     ev=input starting location
c     ev=output event coordinates
c     part=p arrival times
c     sart=s arrival times
c     stn=input station coordinates
c     levs=input slowness at elev of station and node index
c     psc=input p station corrections
c     ssc=input s station corrections
c     psig=input p station weight read in readsig
c     ssig=input s station weight
c     pwt=input weight of p pick
c     swt=input weight of s pick
c     rwt=input residual std dev cutoff 
c     mdim=input max stations array dimension
c     a=work space matrix of partial derivatives
c     usvd=output svd decomp of a, used for jackknife errors
c     vsvd=output svd decomp of a, used for jackknife errors
c     qsvd=output svd decomp of a, used for jackknife errors
c     tres=output residual/std.dev weight
c     ma=input max arrivals=2*mdim
c     chip=output chisquared misfit of p-wave data (<~30*nsta)
c     chis=output chisquared misfit of s-wave data (<~60*nsta)
c     nparr=ouput number of p arrivals
c     nsarr=ouput number of s arrivals
c     this version has pick weighting.(8/22/90)
c     this version makes depth perturbation half as big if 
c     event is going to be perturbed above ground
c     this version outputs station data in array psave(5,mdim)
c     and ssave(5,mdim) (11/30/87 6:00a)
c     psave(1,i)=weight
c          (2,i)=residual
c          (3,i)=epicentral distance
c          (4,i)=ray parameter
c          (5,i)=azimuth
c     version separates chip and chis (3/4/88)
c     this version can do weighted least squares (11/29/87 8:00)
c     by setting iwt=1
c     this version uses subroutine switchv and it locates
c     earthquakes with s and p times (11/28/87 6:30)
c     this version uses subroutine hderiv (11/28/87 5:45)
c     locate an earthquake
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
     &ssig(mdim),pwt(mdim),swt(mdim),a(ma,ndim),tres(ma),
     &usvd(ma,ndim),psave(5,mdim),ssave(5,mdim),
     &vsvd(ndim,ndim),qsvd(ndim),rwt(3,2)

c     these are internal to subroutine

      dimension tmp2(ndim),xres(ndim),awt(ndim),evsav(ndim)
      parameter(perchi=.02d0,perq=.01d0,maxiter=7,isvd=1)

      mq=5
      ierr=0
      chilast=1.d0

            do 40 i=1,ndim
               evsav(i)=ev(i)
 40         continue

c           start again here if there's an outlier
 5079       iter=0
            do 50 i=1,ndim
               ev(i)=evsav(i)
 50         continue
 5080       continue
               iter=iter+1
c***           next iteration for event location

c              zero arrays
               do 5030 j=1,ndim
                  do 5040 k=1,ma
                     a(k,j)=0.d0
 5040             continue
 5030          continue
               do 5070 j=1,ma
                  tres(j)=0.d0
 5070          continue
               chi=0.d0
               natot=0
c              load p wave model into gmod common
               call switchv(1)
c              raytrace using current location and calculate 
c              partial derivatives
c              and residuals for p wave data
               call hderiv(ev,part,stn,levs,psig,pwt,psc,mdim,
     +         natot,a,tres,ma,psave,ierr)
               nparr=natot
c              load s wave model into gmod common
               call switchv(2)
c              raytrace and calculate partial derivatives
c              and residuals for s wave data
               call hderiv(ev,sart,stn,levs,ssig,swt,ssc,mdim,
     +         natot,a,tres,ma,ssave,ierr)
               nsarr=natot-nparr
               chip=0.d0
               chis=0.d0

               if (natot.lt.4) then
                  ierr=6
                  return
               endif
            
               do 502 i=1,nparr
                   chip=chip+tres(i)*tres(i)
 502           continue
               do 503 i=nparr+1,natot
                   chis=chis+tres(i)*tres(i)
 503           continue
               chi=chip+chis

c***           calculate weights if weighted least squares is desired
               if (iwt.eq.1) then
                  do 505 k=1,ndim
                     awt(k)=0.d0
                     do 510 j=1,natot
                        awt(k)=awt(k)+a(j,k)**2
 510                 continue
                     awt(k)=1.d0/dsqrt(awt(k))
 505              continue
                  do 515 k=1,ndim
                     do 520 j=1,natot
                        a(j,k)=a(j,k)*awt(k)
 520                 continue
 515              continue
               endif
                        
c              invert the matrix (needed for error calc too)
c***           decompose a into usvd*qsvd*vtsvd
               call svd(ma,ndim,natot,ndim,a,ndim,usvd,
     +         vsvd,qsvd,isvd)
               call eigsrt(qsvd,vsvd,ndim,ndim,usvd,ma,natot)
c***           unweight matrix
               if (iwt.eq.1) then
                  do 525 k=1,ndim
                     do 530 j=1,ndim
                        vsvd(j,k)=vsvd(j,k)*awt(j)
 530                 continue
 525              continue
               endif

c***           want to continue?
               chidif=dabs(1.d0-(chi/chilast))
               if (chidif.lt.perchi.or.iter.eq.maxiter) then
c                 kickout sets traveltime of biggest outlier to
c                 zero and starts relocating over again
                  call kickout(part,tres,mdim,nparr,rwt,ibigout)
                  ioutlier=ibigout
                  call kickout(sart,tres(nparr+1),mdim,nsarr,
     &            rwt(1,2),ibigout)
                  ibigout=max(ibigout,ioutlier)
                  if(ibigout.ne.0) then
                     goto 5079
                  else if (iter.eq.maxiter) then
                     ierr=3
c                     write (45,*) nused
                     return
                  else
c                     write (45,*) nused
                     return
                  endif
               endif
 
c**            solve for xres the location perturbation
               call mult(tres,usvd,tmp2,ldim,ma,ndim,ldim,natot,ndim)
c              discard singular values smaller than perq
               do 560 j=2,ndim
                  if (qsvd(j)/qsvd(1).lt.perq) then
                     mq=j
                     goto 563
                  endif
 560           continue
 563           continue
               do 3500 k=1,mq-1
                     tmp2(k)=tmp2(k)/qsvd(k)
3500           continue
               do 3505 k=mq,ndim
                     tmp2(k)=0.d0
 3505          continue
               nused=mq-1
               call mult(vsvd,tmp2,xres,ndim,ndim,ldim,ndim,ndim,ldim)
c              catch some bugs(no events above the ground)
 590           continue

               if((ev(3)+xres(3)).lt.0.d0) then
c                 make depth perturbation half as big
                  xres(3)=xres(3)/2.0
                  goto 590
               endif
               if(xres(3).gt.10.d0) then
                  xres(3)=10.0
               endif
               do 600 j=1,ndim
                  ev(j)=ev(j)+xres(j)
 600           continue

               chilast=chi
c***        end of iteration for location for this event
            goto 5080
 
            end

c $Id$ 
