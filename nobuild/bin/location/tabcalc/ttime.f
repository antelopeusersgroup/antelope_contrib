      subroutine ttime(ip)
c-----------------------------------------------------------------------
c       this subroutine performs the major calculation in this program. 
c  it generates a travel time curve corresponding to the present source 
c  depth.  the section of the total velocity model of length nwindow
c  formed by ttable is used in an accumulative fashion to calculate the 
c  travel times of the upward rays.  downward rays are then calculated
c  by the simple difference from the surface focus travel time curve. 
c  branches are flagged as forward, revers, shadow,or upward.  arguments
c  passed are-
c         ip-index to enter surface focus table.
c     inputs coming through common- 
c         nwindow-length of window in velocity model for this increment 
c         vwork-velocity array giving velocity model window 
c         dwork-corresponding depths
c-----------------------------------------------------------------------
	include 'table.common' 
	include 'upward.common'
	include 'surface.common' 
	include 'control.common' 
	include 'window.common'
      character bnorm,revs,shad,up
      parameter(bnorm='d',revs='r',shad='s',up='u') 
      parameter(flaglvz=1.0e38) 
c--loop on rays.  find the accumulated travel time and distance to this 
c--depth. 
          do 100 iup=ip+1,np
          rayp=pup(iup) 
          call txint(rayp,vwork,dwork,nwindow,tt,xx)
          tup(iup)=tup(iup)+tt
  100     xup(iup)=xup(iup)+xx
c--find smallest p needed 
      kount=ip+1
  125 if(xup(kount).lt.xmin) go to 150
      kount=kount + 1 
      go to 125 
  150 continue
      lastp=kount 
c--do not do straight up case twice if kount=np 
      if(kount.eq.np) lastp=lastp-1 
c--calculate largest allowed ray index
c--pmax=1/vsource 
      ipmax=ip+1
c--calculate the reversed segment of upward rays. 
c--start with the straight up ray.
      ttab(1)=tup(np) 
      xtab(1)=0.
      ptab(1)=0.
      branch(1)=up
      irange = lastp-ipmax+2
c--when low velocity zones are present the pup array is not a 
c--continuously decreasing sequence.  when this occurs in the following 
c--loop, those points are eliminated by testing the t array.  
      itt=2 
          do 175 iup=2,irange 
          iuprev=lastp-iup+2
c--do not make test for small p.
          if(iuprev.gt.ns) go to 170
               if(t(iuprev).gt.flaglvz) go to 175 
  170     ttab(itt)=tup(iuprev) 
          xtab(itt)=xup(iuprev) 
          ptab(itt)=pup(iuprev) 
          branch(itt)=up
          itt=itt+1 
  175     continue
      itt=itt-1 
c--for sources outside low velocity zones shoot one extra ray 
c--straight out.
c--calculate travel times for rays starting out downward
c--first check to see if there are any.  if not exit gracefully.
      if(ipmax.ge.ns) then
          ntab=irange 
          return
      endif 
      if(t(ip+1).lt.flaglvz) then 
          itt=itt+1 
          vsource=vwork(nwindow)
          pmax=1./vsource 
c--be careful of coasely traced layered models for near surface 
c--sources. 
          if(ip.le.1) then
               tt = 0.0 
               xx = 0.0 
          else
               call txint(pmax,v,d,ip,tt,xx)
          endif 
c--be careful if the bottom of the window falls on a grid point.
c--(see ttable.)
            if(dwork(nwindow).eq.d(ip)) then
                 dtt=0.0
                 dxx=0.0
            else
                 call txint(pmax,vwork(nwindow-1),dwork(nwindow-1),2, 
     $                       dtt,dxx) 
            endif 
          ttab(itt)=tt+dtt
          xtab(itt)=xx+dxx
          ptab(itt)=pmax
          if(ttab(itt).lt.flaglvz) then 
               branch(itt)=up 
          else
               branch(itt)=shad 
          endif 
      endif 
          do 200 idown=ipmax,ns 
          itt=itt+1 
          if(t(idown).gt.flaglvz) then
c--lvz branch 
               ttab(itt)=t(idown) 
               xtab(itt)=x(idown) 
               ptab(itt)=p(idown) 
               branch(itt)=shad 
          else
c--normal branch
               ttab(itt)=t(idown)-tup(idown)
               xtab(itt)=x(idown)-xup(idown)
               ptab(itt)=p(idown) 
c--branch is always named by looking backward one point.
c--first point after a shadow zone is labeled as = shad 
               if(xtab(itt-1).gt.flaglvz) then
                    branch(itt) = shad
               elseif(xtab(itt).ge.xtab(itt-1))then 
                    branch(itt)=bnorm 
               else 
                    branch(itt)=revs
               endif  
          endif 
  200     continue
      ntab=itt
      return
      end 

c $Id$ 
