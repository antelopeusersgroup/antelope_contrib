      subroutine arrivalout(stnt,stname,stn,save,pjunk,
     +  ev,sig,mdim,jwt,ips,lun,org,lid,lddate)
c     outputs residual information to unit lun for one event (7/6/88)
c     ips=1 for p and ips=2 for s

      implicit real*8 (a-h,o-z)
      parameter (mnod=25)
      parameter (on180pi=180.d0/3.14159265358979d0)
      common /mod/g(mnod),b(mnod),z(mnod),u(mnod),nodes
      common /psmod/psg(mnod,2),psb(mnod,2),psz(mnod,2),psu(mnod,2),nops
      dimension stnt(mdim),ev(6),save(5,mdim),stn(4,mdim)
      dimension sig(mdim),jwt(mdim),org(4)
      character *6 stname(mdim),AUTH
      parameter (AUTH='UC1D')
      character *119 pjunk(mdim),tjunk
      character *9 lddate
c     save(1,i)=weight
c         (2,i)=residual
c         (3,i)=epicentral distance
c         (4,i)=ray parameter
c         (5,i)=azimuth from station to event


      sldep=ev(3)-org(4)
      call dll(evlong,evlat,ev(1),ev(2))
      call finlay(ev(3),layz)
      vs=psb(layz,ips)+psg(layz,ips)*(ev(3)-psz(layz,ips))
         do 506 j=1,mdim
            if (stnt(j).ne.0.d0) then
               stnt(j)=stnt(j)+ev(6)
               call finlay(stn(3,j),layr)
               vr=psb(layr,ips)+psg(layr,ips)*(stn(3,j)-psz(layr,ips))

c              ray angle at source 0 = up, 180 = down
               if(save(4,j).gt.0.d0) then
                  sang=180.d0-on180pi*dasin(save(4,j)*vs)
                  rang=180.d0-on180pi*dasin(save(4,j)*vr)
               else if(stn(3,j).le.ev(3)) then
                  sang=on180pi*dasin(-save(4,j)*vs)
                  rang=180.d0-on180pi*dasin(-save(4,j)*vr)
               else
c                 hypocenter above station
                  sang=180.d0-on180pi*dasin(-save(4,j)*vs)
                  rang=on180pi*dasin(-save(4,j)*vr)
               endif
               baz=datan2((ev(1)-stn(1,j)),
     +            (stn(2,j)-ev(2)))*on180pi
               tjunk=pjunk(j)
               write(lun,526)tjunk(1:38),stnt(j),tjunk(74:105),
     +         (save(k,j),k=2,5),baz,
     +         sang,rang,evlong,evlat,sldep,lid,lddate
 526           format(a38,1x,f15.3,a32,
     +         f9.5,1x,f7.2,1x,f9.6,1x,f6.1,1x,f6.1,1x,
     +         f6.1,f6.1,1x,f9.4,1x,f9.4,1x,f9.4,1x,i1,1x,a9)

c               write(lun,525)ievid,stnt(j),stname(j),jwt(j),
c     +         (save(k,j),k=2,5),evlong,evlat,sldep
c 525           format(i10,f17.4,1x,a6,1x,i2,1x,
c     +         f8.5,1x,f7.2,1x,f19.16,1x,f5.0,3f10.4)
            endif
 506     continue

      return
      end

      subroutine readast(stname,stn,psig,ssig,levs,ev,part,
     +sart,mdim,nparr,nsarr,lp,le,pjunk,ejunk,jwt,org,ierr)
c     modified to not use 4 or C picks
      implicit real*8 (a-h,o-z)
      parameter (ndim=4)
      dimension ev(6),jwt(mdim,2),part(mdim),sart(mdim),org(4),
     & nwt(10),stn(4,mdim),psig(mdim),ssig(mdim),levs(mdim)
      character ejunk*100,wtchar*10
      character *119 pjunk(mdim,2),tpjunk
      character *1 tph,twt
      character *6 stname(mdim),tsta
      save itemp,tsta,twfid,ttime,tph,twt,tpjunk
      data itemp/0/
      data wtchar/'01234AB  C'/,nwt/0,1,2,3,4,0,1,2,3,4/

c     ---------------------------------------------------
c     ierr=-1  =>  error in or end of arrival time  data file
c     ierr=-2  =>  error in or end of event data file
c     ierr=-3  =>  found arrival time with no location
c     ierr=-4  =>  more than 10 duplicate station picks

      ierr=0
      do 3 i=1,mdim
         part(i)=0.d0
         sart(i)=0.d0
 3    continue
      nparr=0
      nsarr=0
      nexdup=mdim-9
      read(le,5,end=60,err=59) ejunk
 5    format(a100)
      read(ejunk,6)ievid,etime,xlat,xlon,depth
 6    format(i8,22x,f15.3,1x,f9.4,1x,f9.4,1x,f9.4)

         call km(xlon,xlat,ev(1),ev(2))
         ev(3)=depth+org(4)
         ev(4)=10.d0
         ev(5)=0.d0
         ev(6)=etime-10.d0

c        process last line saved
         if(ievid.eq.itemp) then
            iwt=nwt(index(wtchar,twt))
            if(iwt.ne.4) then
               do 7 j=1,mdim
                  if(tsta.eq.stname(j)) then
                     if(tph.eq."P") then
                        if(part(j).ne.0.d0) then
                           call duppick(stn,stname,psig,levs,
     +                     mdim,nexdup,j,ierr)
                        endif
                        nparr=nparr+1
                        part(j)=ttime-ev(6)
                        jwt(j,1)=iwt
                        pjunk(j,1)=tpjunk
                     else if (tph.eq."S") then
                        if(sart(j).ne.0.d0) then
                           call duppick(stn,stname,ssig,levs,
     +                     mdim,nexdup,j,ierr)
                        endif
                        nsarr=nsarr+1
                        sart(j)=ttime-ev(6)
                        jwt(j,2)=iwt
                        pjunk(j,2)=tpjunk
                     else
                        print *,'unidentified phase, ievid=',
     +                  ievid,' station= ',tsta
                     endif
                     goto 8
                  endif
 7             continue
               print *,'station ',tsta,' not in station list'
            endif
 8          continue
c
         else if (itemp.ne.0) then
            print *,'no location for evid= ',itemp,
     +      ' or no arrivals for evid= ',ievid
            ierr=-3
            return
         endif
              
c        loop to read arrivals for matching evid
 10      continue
         read(lp,11,end=15,err=14) tpjunk
 11      format(a119)
         read(tpjunk,12) itemp,tsta,twfid,ttime,tph,twt
 12      format(i8,1x,a6,1x,i11,33x,f15.3,8x,a1,2x,a1)


         if(ievid.eq.itemp) then
            iwt=nwt(index(wtchar,twt))
            if(iwt.ne.4) then
               do 13 j=1,mdim
                  if(tsta.eq.stname(j)) then
                     if(tph.eq."P") then
                        if(part(j).ne.0.d0) then
                           call duppick(stn,stname,psig,levs,
     +                     mdim,nexdup,j,ierr)
                        endif
                        nparr=nparr+1
                        part(j)=ttime-ev(6)
                        jwt(j,1)=iwt
                        pjunk(j,1)=tpjunk
                     else if (tph.eq."S") then
                        if(sart(j).ne.0.d0) then
                           call duppick(stn,stname,ssig,levs,
     +                     mdim,nexdup,j,ierr)
                        endif
                        nsarr=nsarr+1
                        sart(j)=ttime-ev(6)
                        jwt(j,2)=iwt
                        pjunk(j,2)=tpjunk
                     else
                        print *,'unidentified phase, ievid=',
     +                  ievid,' station= ',tsta
                     endif
                     goto 120
                  endif
 13            continue
               print *,'station ',tsta,' not in station list'
            endif
 120        continue

         else
            return
         endif
         goto 10

 14   continue
      print *,'error reading p'
 15   continue
      ierr=-1
      return

 59   continue
      print *,'error reading event'
c     if hit end of e file before end of p file
 60   continue
      print *,'end of e file'
      ierr=-2
      read(lp,65,end=85,err=84) itemp
 65   format(i8)
      print *,'no location for evid= ',itemp
      return

 84   continue
      print *,'error reading p file'
 85   continue
      print *,'end of p file'
 90   continue
      return
      end


c*****************************************************************
      subroutine readctrl(xfac,tacc,stepl,org)
c     read the control file (5/2/88) add stepl (6/3/88)
c     line #  columns  format variable  explanation
c        1      1-6     f6.1   org1     decimal origin longitude
c        1      7-12    f6.1   org2     decimal origin latitude
c        1     13-18    f6.1   org3     angle (in degrees) of counter-
c                                       clockwise rotation of axes
c        2      1-8     f8.7   xfac     enhancement factor
c        2      9-16    f8.7   tacc     traveltime improvement factor
c        2      17-24   f8.7   stepl    maximum interval between deriv-
c                                       ative calculations
c-------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      dimension org(4)

      open(12,file='control')
      read(12,105) (org(i),i=1,4)
 105  format(4f6.1)
      read(12,115) xfac,tacc,stepl
 115  format(3f8.7)
      close(12)
      return
      end
      subroutine readone(stname,ev,part,sart,mdim,nparr,nsarr,jwt,
     +org,lunp,luns,ievid,ierr)
c     read in p picks and s picks from files pinvdata and sinvdata
c     one event at a time
c     if ievid = -1 then we've gotten to the end of the file

      implicit real*8 (a-h,o-z)
      parameter (mnod=25)
      common /mod/g(mnod),b(mnod),z(mnod),u(mnod),nodes
      common /travtable/ptab(200),xtab(200),ttab(200),maxpindex
      parameter (ndim=4,nread=8)
      dimension ev(ndim+2),nwt(10)
      dimension part(mdim),sart(mdim),org(4),jwt(mdim,2)
      dimension temp(nread),tems(nread)
      character *6 stname(mdim),stemp,stems
      character wtchar*10,twt*1,twts*1
      save itemp,temp,stemp,items,tems,stems,twt,twts
      data wtchar/'01234AB  C'/,nwt/0,1,2,3,4,0,1,2,3,4/
      ierr=0
      nparr=0
      nsarr=0
      nexdup=mdim-9

c     initialize arrays
      do 1 j=1,mdim
         part(j)=0.d0
         sart(j)=0.d0
 1    continue

c     process line of p pick data saved from last run of subroutine
      ievid=itemp
      call km(temp(1),temp(2),ev(1),ev(2))
      ev(3)=temp(3)
      ev(4)=10.d0
      ev(5)=0.d0
      ev(6)=temp(4)-10.d0
      iwt=nwt(index(wtchar,twt))
      
      do 5 j=1,mdim
         if (stemp.eq.stname(j)) then
           nparr=nparr+1
           part(j)=temp(6)-ev(6)
           jwt(j,1)=iwt
         endif
 5    continue

c     loop to read the rest of p picks for this event
10    continue
      read(lunp,*,end=15,err=15,iostat=ival)itemp,(temp(j),j=1,5),
     +stemp,temp(6),twt
      print *,'twt',twt
      if(itemp.eq.ievid) then
         iwt=nwt(index(wtchar,twt))
         do 25 j=1,mdim
            if (stemp.eq.stname(j)) then
              nparr=nparr+1
              if(part(j).ne.0.d0) then
                 call duppick(stn,stname,psig,levs,
     +           mdim,nexdup,j,ierr)
              endif
              part(j)=temp(6)-ev(6)
              jwt(j,1)=iwt
            endif
 25      continue
      else 
c       that's all the p picks for this event
        goto 20
      endif
      goto 10

 15   continue
      ierr=-1
 20   continue
c     process line of s pick data saved from last run of subroutine
      if (items.eq.ievid) then
            iwts=nwt(index(wtchar,twts))
            do 40 j=1,mdim
               if (stems.eq.stname(j)) then
                  nsarr=nsarr+1
                  sart(j)=tems(6)-ev(6)
                  jwt(j,2)=iwts
               endif
 40         continue
      else if (items.ne.0.and.items.ne.ievid) then
c        don't have s picks for this event
         return
      endif

c     loop to read the rest of s picks for this event
 35   continue
c        read from sinvdata
         read(luns,*,end=45,err=45,iostat=ival)items,(tems(j),j=1,5),
     +   stems,tems(6),twt
         if (items.eq.ievid) then
            iwts=nwt(index(wtchar,twts))
            do 30 j=1,mdim
               if (stems.eq.stname(j)) then
                  nsarr=nsarr+1
                  if(sart(j).ne.0.d0) then
                     call duppick(stn,stname,ssig,levs,
     +               mdim,nexdup,j,ierr)
                  endif
                  sart(j)=tems(6)-ev(6)
                  jwt(j,2)=iwts
               endif
 30         continue
         else if (items.ne.ievid) then
            return
         endif
      goto 35
 45   continue
      return
      end


c $Id$ 
