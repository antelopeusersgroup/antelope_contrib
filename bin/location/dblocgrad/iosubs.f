c     these routines prompt for user input
c     subroutine mfask(questl,fdef,m)
c     subroutine miask(questl,idef,m)
c     integer function iask(quest,idef)
c     real *8 function fask(quest,fdef)
c     character function cask(quest,def,nc)
c     character questl*160, quest*80, def*nc

      subroutine mfask(questl,fdef,m)
      implicit real*8 (a-h,o-z)
      character *160 questl
      character *160 ctemp
      character *12 fmt
      dimension fdef(m)

      iq=index(questl,'  ')
      ns=10*m
      last=iq+3+ns
      if(last.gt.160) then
         print *,'question too long'
         stop
      endif
      questl(iq+1:iq+1)='('
      write(fmt,5)m
 5    format('(',i3.3,'f10.4)')
      write(questl(iq+2:iq+ns+1),fmt)(fdef(j),j=1,m)
      questl(iq+ns+2:iq+ns+3)='):'
      write(fmt,20)last
 20   format('(a',i3.3,')')
      write(*,fmt) questl(1:last)
      read(*,'(a160)') ctemp
      if(ctemp.ne.'') then
        read(ctemp,*) (fdef(j),j=1,m)
      endif
      return
      end
   
      subroutine miask(questl,idef,m)
      character *160 questl
      character *160 ctemp
      character *12 fmt
      dimension idef(m)

      write(ctemp,*)(idef(j),j=1,m)
      iq=index(questl,'  ')
      lis=index(ctemp,'   ')
      write(fmt,20)lis
 20   format('(a',i3.3,')')
      nq=1+iq+lis+3
      if(nq.gt.160) then
         print *,'question too long'
         stop
      endif
      questl(iq+1:iq+1)='('
      write(questl(iq+2:iq+lis+1),fmt) ctemp
      questl(iq+lis+2:iq+lis+3)='):'
      ilast=iq+lis+3
      write(fmt,20)ilast
      write(*,fmt) questl(1:iq+lis+3)
      read(*,'(a160)') ctemp
      if(ctemp.ne.'') then
        read(ctemp,*) (idef(j),j=1,m)
      endif
      return
      end
   
      integer function iask(quest,idef)
      character *80 quest
      character *10 ctemp
      character *12 fmt
      print *,quest,idef
      iq=index(quest,'  ')
      if(iq+13.gt.80) then
         print *,'question too long'
         stop
      endif
      quest(iq+1:iq+1)='('
      write(quest(iq+2:iq+11),'(i10)') idef
      quest(iq+12:iq+13)='):'
      last=iq+13
      write(fmt,20)last
 20   format('(a',i3.3,')')
      read(*,'(a10)') ctemp
      if(ctemp.eq.'') then
        iask=idef
      else
        read(ctemp,*) iask
      endif
      return
      end


      real *8 function fask(quest,fdef)
      implicit real*8 (a-h,o-z)
      character *80 quest
      character *20 ctemp
      character *12 fmt
      iq=index(quest,'  ')
      if(iq+23.gt.80) then
         print *,'question too long'
         stop
      endif
      quest(iq+1:iq+1)='('
      write(quest(iq+2:iq+21),'(g20.6)') fdef
      quest(iq+22:iq+23)='):'
      last=iq+23
      write(fmt,20)last
 20   format('(a',i3.3,')')
      write(*,fmt) quest(1:last)
      read(*,'(a20)') ctemp
      if(ctemp.eq.'') then
        fask=fdef
      else
        read(ctemp,*) fask
      endif
      return
      end

      character function cask(quest,def,nc)
      character *80 quest
      character *(*) def
      character *12 fmt
      cask(1:80)=''

      iq=index(quest,'  ')
      id=index(def,'  ')
      if (id.eq.0) id=nc
      if (iq.eq.0) iq=80
      if(iq+id+5.gt.80) then
         print *,'question too long'
         stop
      endif


      quest(iq+1:iq+1)='('
      quest(iq+2:iq+id+1)=def
      quest(iq+id+2:iq+id+3)='):'
      last=iq+id+3
      write(fmt,20)last
 20   format('(a',i3.3,')')
      write(*,fmt) quest(1:last)
      write(fmt,20)nc
      read(*,fmt) cask(1:nc)
      if(cask(1:1).eq.'') cask(1:nc)=def
      return
      end

      subroutine readstn(stname,stn,ndim,mdim,levs)
c     this version of subroutines modified - (11/29/87 12:30)
c     to accomodate s-times
c     reads stnfile, calculates slownesses,finds layer containing
c     stations
c     output arguments:
c     stname=ndim*mdim array stores location of stations
c     levs=1*mdim array stores model layer station is in
c
      implicit real *8 (a-h,o-z)
      parameter (mnod=25)
      common /mod/g(mnod),b(mnod),z(mnod),u(mnod),nodes
      common /travtable/ptab(200),xtab(200),ttab(200),maxpindex
      dimension stn(ndim,mdim),levs(mdim)
      character *6 stname(mdim)
      data top/2655.d0/
c     stn(1,i)=x,(2,i)=y,(3,i)=z,(4,i)=u
 

      open(unit=12,file='stnfile')
c     do for each station
 30   continue
c        read stn data
         read (12,*,end=35) i,stname(i),dlat,dlon,elev
c        convert to kilometers
         call km(dlon,dlat,stn(1,i),stn(2,i))
         stn(3,i)=(top-elev)/1000.d0
c        find layer receiver is in
         call finlay(stn(3,i),levs(i))
         stn(4,i)=1.d0/(b(levs(i))+g(levs(i))*(stn(3,i)-z(levs(i))))
         goto 30
 35   continue

c     make some extra space to accomodate duplicate picks
      do 40 i=mdim-9,mdim
         stname(i)='dup'
         do 50 j=1,4
            stn(j,i)=u(1)
 50      continue
 40   continue
         

      close (12)
      return 
      end
c*********************************************************
      subroutine readsig(stname,mdim,psig,ssig,sfil)
c     reads from stnsig file  std. dev. of residuals
c     input arguments:mdim=# of stations
c     output arguments:sig=1*mdim  array storing std. dev.
c
      implicit real *8 (a-h,o-z)
      dimension psig(mdim),ssig(mdim)
      character *40 sfil
      character *6 name,stname(mdim)
 

      open(unit=12,file=sfil)
c     do for each station
 30   continue
c        read stn data
         read (12,*,end=35) name,tpsig,tssig
         do 20 i=1,mdim
            if(stname(i).eq.name) then
               psig(i)=tpsig
               ssig(i)=tssig
               goto 30
            endif
 20      continue
         goto 30
 35   continue

      close (12)
      return 
      end
c**********************************************************
      subroutine finlay(d,layd)
c     finds layer in model that depth d is in.
c     input: d=depth
c     output: layd=model layer

      implicit real*8 (a-h,o-z)
      parameter(mnod=25)
      common /mod/g(mnod),b(mnod),z(mnod),u(mnod),nodes
      
      j=nodes+1
 20   continue
        j=j-1
      if (d.lt.z(j)) goto 20
      layd=j

      return
      end
c****************************************************************************

      subroutine km(dlon,dlat,xkm,ykm)
c     calculate (x,y) coordinates given decimal lat and lon
c     origin is at xo,yo
c     input: dlon=decimal longitude ; dlat=decimal latitude
c     output: xkm=km in x direction from xo,yo
c             ykm=km in y direction from xo,yo

      implicit  real*8 (a-h,o-z)
      parameter (pion180=3.14159265358979d0/180.d0)
      parameter (xo=-116.7d0,yo=33.5d0)

      xkm=6371.d0*dcos(dlat*pion180)*(dlon-xo)*pion180
      ykm=6371.d0*(dlat-yo)*pion180
      return
      end
 
c****************************************************************************

      subroutine dll(dlon,dlat,xkm,ykm)
c     calculate  decimal lat and lon given (x,y) coordinates
c     origin is at xo,yo
c     input: xkm=km in x direction from xo,yo
c             ykm=km in y direction from xo,yo
c     output: dlon=decimal longitude ; dlat=decimal latitude

      implicit  real*8 (a-h,o-z)
      parameter (pion180=3.14159265358979d0/180.d0)
      parameter (xo=-116.7d0,yo=33.5d0)

      dlat=yo+ykm/pion180/6371.d0
      dlon=xo+xkm/dcos(dlat*pion180)/6371.d0/pion180
      return
      end

c******************************************************
      subroutine readsc(stname,mdim,psc,ssc,pfil,sfil)
c
      implicit real*8 (a-h,o-z)
      dimension psc(mdim),ssc(mdim)
      character *6 stname(mdim),name
      character *40 pfil,sfil
      data lun/12/

c     initialize station corrections to 0
      do 10 i=1,mdim
         psc(i)=0.d0
         ssc(i)=0.d0
 10   continue

c     read in station corrections
      open (unit=lun,file=pfil)
 60   continue
         read(lun,*,end=65,err=64) name,sc
            do 80 i=1,mdim
               if(name.eq.stname(i)) then
                   psc(i)=sc
                   goto 60
               endif
 80         continue
         goto 60
 64   print *,'error reading p station corrections'
 65   continue
      close(lun)


      open (unit=lun,file=sfil)
 70   continue
         read(lun,*,end=75,err=74) name,sc
            do 90 i=1,mdim
               if(name.eq.stname(i)) then
                   ssc(i)=sc
                   goto 70
               endif
 90         continue
         goto 70
 74   print *,'error reading p station corrections'
 75   continue

      close(lun)
      end
c******************************************************
      subroutine readwt(wtval,rwt)
      implicit real*8 (a-h,o-z)
      dimension wtval(0:4,2),rwt(3,2)
      open(unit=12,file='control')
      read(12,'(/)')
      do 75 i=0,4
         read(12,*) k,wtval(k,1),wtval(k,2)
 75   continue
      read(12,*) (rwt(i,1),i=1,3)
      read(12,*) (rwt(i,2),i=1,3)
      close (12)
      return
      end
      subroutine readmod(lunp,luns)
c     reads model from model file into common
c     calculates gradients,slownesses
c
      implicit real *8 (a-h,o-z)
      parameter (loutt=6,lintt=5)
      parameter (mnod=25)
      common /mod/g(mnod),b(mnod),z(mnod),u(mnod),nodes
      common /psmod/psg(mnod,2),psb(mnod,2),psz(mnod,2),psu(mnod,2),nops
      dimension ncheck(2)
 
      open(unit=10,file='gmodel')
      open(unit=11,file='gmodels')
      do 30 ips=1,2
        lun=9+ips
        read (lun,*) nops
        ncheck(ips)=nops
        read (lun,*) psb(1,ips),psz(1,ips)
        do 20 i=2,nops
        read (lun,*) psb(i,ips),psz(i,ips)
        psg(i-1,ips)=(psb(i,ips)-psb(i-1,ips))/(psz(i,ips)-psz(i-1,ips))
 20     continue
        read (lun,*) psg(nops,ips)
c       calc u's at all interfaces
        do 40 i=1,nops
 40     psu(i,ips)=1.d0/psb(i,ips)
        close(lun)
 30   continue
      if (ncheck(1).ne.ncheck(2)) then
      write (loutt,'(a)')'p and s models have different number of nodes'
      endif
c***    output initial model to model file
c      write (lunp,280) (psb(j,1),j=1,nops)
c      write (lunp,285) (psu(j,1),j=1,nops)
c      write (luns,280) (psb(j,2),j=1,nops)
c      write (luns,285) (psu(j,2),j=1,nops)
 280  format('initial velocity model:',/,4(f19.16,1x),/,4(f19.16,1x),/,
     +   4(f19.16,1x),/,4(f19.16,1x),/,4(f19.16,1x),/,4(f19.16,1x))
 285  format('initial slownesses:',/,4(f19.17,1x),/,4(f19.17,1x),/,
     +   4(f19.17,1x),/,4(f19.17,1x),/,4(f19.17,1x),/,4(f19.17,1x))
      call switchv(1)
      close(10)
      close(11)
      return
      end


c $Id$ 
