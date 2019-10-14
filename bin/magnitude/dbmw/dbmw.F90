parameter (NMAX = 100000, NMAX1 = 113616)
logical :: dir_e
logical :: exis
character*100 table
character *100 chanexpr
character *100 dbmasdir,stringS	
character*150 subset,subchan
character*64 dir
character*15 keyname,pro_fix
character*10 evento
character*4 Ptype,Stype
integer aphas,verb,runflag,chanflag,Mwflag
integer itest,nch,nch1,arid,icha,commid,aridts
real*8  pretime,postime,startime,diff 
!integer pf,retcode,d2npts,nptsN,strlen
integer retcode,d2npts,nptsN,strlen
integer*8 pf
integer*4 i
integer dbP1(4),dbP2(4)
real dati(NMAX),x(NMAX) ,noise(NMAX),noisS(NMAX),rappsr(NMAX),sismS(NMAX)
real sismoN(NMAX),sismoE(NMAX),sismtmp(NMAX)
real sismo(NMAX),sismoA(NMAX),sismoV(NMAX),sismoD(NMAX)
real sismorg(NMAX),xtim
real *8 t0_req, t1_req, t1o_req, t1,t2,samprate,calib
real *8 t3_req, t4_req, t4o_req, t3
real sampint
!----------------------------------------------
! 64 bit
!
integer*8 NULL
integer*8 ex
integer NMAX64
integer*8 nsamp
integer nptsM
real*8  otime,olat,olon,slat,slon,elat,elon,ml,stopti
integer db_o(4),db_or(4),dbresult(4),db_ar(4),db_ml(4)
integer db(4),db_re(4),db_re1(4),db_si(4),db_in(4),db_wf(4)
integer db_ca(4),db_caO(4),db_ca_(4)
integer db_as(4),db_s(4),db_i(4),db_w(4),dbmw(4),dbmwn(4),dbmagn(4),dbmag(4)
integer lastid(4)
integer n_w, n_ca, n_st, n_in, n_or, n_o, n_wf, n_net
integer n_ml, nn, nw, nwn, nn1
integer orid, evid, oridmag, oridmg
integer npts
!
!---------------------------------------------
real *8 spikv,wfinizio,wffine,tnoise
real spik,inc,first,last,Gampl
character*6 sta,sta_,stats
character *8 chan,chan_
character*128 string_t1,string_t2,string_t3,string_Pph,string_Sph
character*100 rfile,resfile,filename
character*100 filexy,filepr,filepr1,filspt,filelog,filepf,dirpr,home_dir
real zero,xmean
real*8 depth,arrP,arrS,arrP_,arrS_,res,resP,resS,resmin,sphasetime,pphasetime
real*8 odep
real*8 arrSo
integer iargc,npts_i,errfl,nptsSi
character*278 dbreco
character*155 dbreco1
character*182 dbremw,dbremag
character*136 dbremwn,dbremagn
character*100 label
character*30 filter
character*24 param(NMAX),ptest,mtest,smw(NMAX)
character*20 string_ot,string_ot1
character*15 auth
character*8 ntest,nmw(NMAX)
character*6 magtype, magtyts
character*7 labor,labev
character*4 labMw
character*1 segtype
character*1 idum
real*8 ff1, ff2, f1, f2
real fmin,fmax,finf,fsup
complex sism(NMAX),t(NMAX),xder(NMAX),nois(NMAX),tR(NMAX)
complex sismA(NMAX),sismV(NMAX),sismD(NMAX)
complex hsb1
complex hsb2
real sqbsum,abmax,Q
real dista,freqq(NMAX),ipdista
real PGA,PGV,PGD,PSA03,PSA10,PSA30,RMSA,energ,Te,Sarag,Mf,Arias,Housner,freq1,delhou
real dura
real*8 PERMIN,PERMAX,distmin,distmax1,distmax2,distmax3,distmax4,distrad
real*8 lddate,delta,delta1,az,seaz,esaz,magmin,mlsta,azN,azR
integer ispike,ihou,flagerr,usta,rsta,tsta
real*8 inizio,startwin,stopwin,startnoise,stopnoise
real*8 staMw(100),staM0(100),staeqR(100),staf0(100)
real*8 netMw,netM0,netf0,neteqR,sigmaMw
real*8 rho_pf,v_pf,k_pf,Q_f0,Q_f1,Q_f2
common/BUT/sqbsum,abmax,hsb1(NMAX),hsb2(NMAX)
COMMON/CPAZ/ t,phase(NMAX),absval(NMAX),freq(NMAX)
COMMON/RDATI/ff1,ff2,ifilt,np1,np2
COMMON/GAUSS1/XGAU(NMAX)
COMMON/DERI/xder
COMMON/ARA/ DMP(5),SD(5,1000),SA(5,1000),GA(NMAX1),PSSV(5,1000),ID
COMMON/BT2/PD(1000),SV(5,1000),IP                                           
COMMON/BT3/PD1(100),IP1
COMMON/FRANGE/PERMIN,PERMAX,IRANGE
COMMON/MWCOM/orid,evid,lddate,arrP,arrS,Ptype,Stype,auth,chan
COMMON/MWCOM01/staMw,staM0,staeqR,staf0,nmwdat
COMMON/MwCOM02/rho_pf,v_pf,k_pf,Q_f0,Q_f1,Q_f2

character*120 command

external fill

include "db.i"
include "coords.i"

!-------------------------------------------------------------------
! DATA
!
      data pi/3.141592654/
      data grav/981./
      runflag = 0
      lstflag = 0
!--------------------------------------------------------------------

if ( iargc() .gt. 2 .or. iargc() .lt. 1) then
  write ( 0, * ) "Usage: dbmw filepf orid" 
  stop 
endif

call getarg (1, filepf )
  if ( iargc() .eq. 2 ) then
   call getarg ( 2, evento )
   read(evento,'(i10)') orid
  endif

if (iargc() .eq. 2) runflag = 1

strlen = LEN(TRIM(ADJUSTL(filepf)))
filepf = filepf(1:strlen)
!--------------------------------------------------------------------
auth='dbmw'
Mwflag = 1

!--------------------------------------------------------------------
write(6,*) "******************************************"
write(6,*) "* Program dbmw version 6.10 Jun 2015     *"
write(6,*) "* Antelope release 5.4                   *"
write(6,*) "* Giovanni Costa                         *"
write(6,*) "******************************************"
write(6,*)  "- pf file: ",filepf(1:strlen)
!--------------------------------------------------------------------
! 	 READ PF file start
!
call pfread ( filepf, pf, retcode )
if ( retcode .ne. 0 ) call complain ( 0, "Error in parameter file:" )

    call pfcompile ( "inline from the program", pf, retcode )
    if ( retcode .ne. 0 ) call complain ( 0, "Failed in pfcompile" )
    call pfget_string ( pf, 'dbname',table)
    call pfget_string ( pf, 'home_dir',home_dir)
    call pfget_string ( pf, 'stringS',stringS )
    call pfget_int ( pf, 'verb', verb )
    call pfget_double( pf, 'pretime', pretime )
    call pfget_double( pf, 'postime', postime )
    call pfget_int ( pf, 'ispike', ispike )
    call pfget_int ( pf, 'nfin', nfin )
    call pfget_double( pf, 'spikv', spikv )
    call pfget_int ( pf, 'idetr', idetr )
    call pfget_int ( pf, 'iwin', iwin )
    call pfget_int ( pf, 'd2npts', d2npts )
    call pfget_int ( pf, 'ifilt', ifilt )
    call pfget_double( pf, 'f1', f1 )
    call pfget_double( pf, 'f2', f2 )
    call pfget_int ( pf, 'np1', np1 )
    call pfget_int ( pf, 'np2', np2 )
    call pfget_double ( pf, 'tnoise', tnoise )
    call pfget_int ( pf, 'aphas', aphas )
    call pfget_double ( pf, 'PERMIN', permin ) 
    call pfget_double ( pf, 'PERMAX', permax )
    call pfget_string ( pf, 'chanexpr', chanexpr )
    call pfget_double( pf, 'magmin', magmin )
    call pfget_double ( pf, 'distmin', distmin )
    call pfget_double ( pf, 'distmax1', distmax1 )
    call pfget_double ( pf, 'distmax2', distmax2 )
    call pfget_double ( pf, 'distmax3', distmax3 )
    call pfget_double ( pf, 'distmax4', distmax4 )
    call pfget_double ( pf, 'depth', depth )
    call pfget_double ( pf, 'Q_f0', Q_f0 )
    call pfget_double ( pf, 'Q_f1', Q_f1 )
    call pfget_double ( pf, 'Q_f2', Q_f2 )
    call pfget_double ( pf, 'rho_pf', rho_pf )
    call pfget_double ( pf, 'v_pf', v_pf )
    call pfget_double ( pf, 'k_pf', k_pf )
!--------------------------------------------------------------------	
        startime = str2epoch(stringS)
tnoise = tnoise +1.
!
call pffree ( pf )
!
! 	 READ PF file stop
!####################################################################
strlen = LEN(TRIM(ADJUSTL(home_dir)))
filelog = home_dir(1:strlen)//'/logs/dbmw.log'
dbmasdir = home_dir(1:strlen)//'/dbmaster'
dirpr = home_dir(1:strlen)//'/PARAM/'

print *, '- database: ',table

!--------------------------------------------------------------------
!
!        OPEN DATABASE
!       

if ( dbopen_database ( table, "r+", db ) .lt. 0 ) call die ( 0, "Can't open table" )

if ( db(3) .lt. 0 ) call dblookup(db_or, db, "", "origin", "", "")
  call dbquery (db_or,dbRECORD_COUNT, n_or)

        if(runflag.eq.0.and.startime.gt.0.0001) then
          write(subset,*) "time > ",startime
          call dbsubset(db_or,db_or,subset,"")
          call dbquery (db_or,dbRECORD_COUNT, n_or)
        endif
        call dblookup(db_w, db, "", "wfdisc", "", "")
        call dbquery (db_w,dbRECORD_COUNT, n_w)
        call dblookup(db_caO, db, "", "calibration", "", "")
        call dbquery (db_caO,dbRECORD_COUNT, n_ca)

if (runflag.eq. 1) n_or =1

itest = dbclose(db)
!---------------------------------------------------------------------
!   
!	  Starting MAIN loop over orid
!
!---------------------------------------------------------------------
do imain=0,n_or-1

lstflag = 0

itest = dbopen_database ( table, "r+", db )

if ( db(3) .lt. 0 ) call dblookup(db_or, db, "", "origin", "", "")

   if(runflag.eq.0.and.startime.gt.0.0001) then
     write(subset,*) "time > ",startime
     call dbsubset(db_or,db_or,subset,"")
     call dbquery (db_or,dbRECORD_COUNT, n_or)
   endif
   call dblookup(db_w, db, "", "wfdisc", "", "")
   call dbquery (db_w,dbRECORD_COUNT, n_w)
   call dblookup(db_caO, db, "", "calibration", "", "")
   call dbquery (db_caO,dbRECORD_COUNT, n_ca)

  nmwdat = 0
  tsta = 0
  rsta = 0
  usta = 0

  db_or(4) = imain
  if(runflag.lt.1) then
    if( dbgetv (db_or,"","orid",orid,"evid",evid,0).lt.0) call complain (0, "dbgetv error 1" )    
  endif
!
  write(subset,*) "orid == ",orid
  call dbsubset(db_o,db_or,subset,"")
  call dbquery (db_o,dbRECORD_COUNT, n_o)
  call dbjoin(db_wf,db_w,db_o,0,0,0,0,"")
  call dbquery (db_wf,dbRECORD_COUNT, n_wf)
!print *, 'DPC_1 n_w n_wf n_ca, n_w: ', n_wf, n_ca, n_w
!---------------------------------------------------------------------
!   
!	  MAIN if over waveforms
!
!---------------------------------------------------------------------
 if(n_wf.gt.0) then
!		
   do i=0, n_o-1
        db_o(4) = i
        if( dbgetv ( db_o,"",  &
        &   "evid",evid,"lat",olat,"lon",olon,"depth",odep,"time",otime,"ml",ml,0) &
        &   .lt. 0) call complain (0, "dbgetv error 2" )
   enddo
   pro_fix = 'fixed!'
   if(odep.gt.0.0) then
     depth = odep
     pro_fix = ''
   endif
!
   if(ml.lt.2.0) distrad=distmax1/111.195
   if(ml.ge.2.0.and.ml.lt.4.0) distrad=distmax2/111.195
   if(ml.ge.4.0.and.ml.lt.6.0) distrad=distmax3/111.195
   if(ml.ge.6.0) distrad=distmax4/111.195
           distmin = distmin/111.195
!		
      call epoch2str(string_ot,otime,"%D %k %M %S")

      write(subchan,*) "chan =~/"//chanexpr

      call dbsubset(dbresult,db_wf,subchan,"")
      call dbquery(dbresult,dbRECORD_COUNT, n_w)
      call dbjoin(db_ca,dbresult,db_caO,0,0,0,0,"")
      call dbquery(db_ca,dbRECORD_COUNT, n_ca)

if(ml.ge.magmin.and.n_ca.gt.0) then

print *,'ORID: ',orid
print *,'Origin time: ',string_ot
print *,'Lat.',olat,'Lon.',olon,'Dep.',fixdep, depth
print *,'Ml = ',ml
print *,' Event ACCEPTED' 
print *,' Number of stations: ', n_w
!---------------------------------------------------------------------
!   
!	  Starting loop over station
!
!---------------------------------------------------------------------
do ista=0,n_w-1

        arid = -1

print *, '==============================================================================='

        dbresult(4) = ista
        if( dbgetv ( dbresult,"","sta",sta,"chan",chan,0) &
        &      .lt. 0) call complain (0, "dbgetv error 3" )
        
        call dblookup(db_ar, db, "","arrival", "", "")
        call dblookup(db_as, db, "","assoc", "", "")
        call dblookup(db_si, db, "","site", "", "")
        call dblookup(db_s, db, "", "sensor", "", "")
        call dblookup(db_i, db, "", "instrument", "", "")

        call nchar(sta,nch)
        subset = 'sta == "'//sta(1:nch)//'"'
!print *, 'Mac_006 subset:', subset
        call dbsubset(db_si,db_si,subset,"")
        call dbjoin(db_re,db_o,db_si,0,0,0,0,"")
        call dbquery(db_re,dbRECORD_COUNT, n_st)
!----------------------------------------------------------------------
! response filename
!
    call dbjoin(db_in,db_si,db_s,0,0,0,0,"")
    call dbsubset(db_in,db_in,subchan,"")
    subset = 'chan == "'//chan(1:3)//'"'
    call dbsubset(db_in,db_in,subset,"")
    write(subset,*) "time < ",otime, " && endtime > ",otime
    call dbsubset(db_in,db_in,subset,"")
    call dbjoin(db_in,db_in,db_i,0,0,0,0,"")
    call dbquery(db_in,dbRECORD_COUNT, n_in)
!----------------------------------------------------------------------
! check responsefile 
!
 if(n_in.gt.0) then
      do i=0, n_in-1
        db_in(4) = i
        if( dbgetv ( db_in,"","dir",dir,"dfile",rfile,"ncalib",calib,0) &
        &      .lt. 0) call complain (0, "dbgetv error 4" )
        if(dbex_compile(db_in,'extfile()',ex,0).lt.0) call complain(0,'dbex_compile problem!')
        if(dbex_eval(db_in,ex,0,resfile).lt.0) call complain(0,'dbex_eval problem!')
      enddo
!----------------------------------------------------------------------
!
      do i=0, n_st-1
         db_re(4) = i
         if( dbgetv ( db_re,"","sta",sta,"lat",olat,"lon",olon,"site.lat",slat,"site.lon",slon,0) &
        &   .lt. 0) call complain (0, "dbgetv error 5" )
      enddo
!---------------------------------------------------------------------------  
       IF(olon.GE.0.0) olon=360.0-olon
       IF(olon.LT.0.0) olon=-olon
       IF(slon.GE.0.0) slon=360.0-slon
       IF(slon.LT.0.0) slon=-slon
       diff=olon+(360.-slon)
       IF(diff.GE.360.) diff=diff-360.
       elat = olat
       elon = olon

       CALL DISTAN(elat,elon,slat,slon,delta1,dista,az)

        write(6,*) 'Distance epic.:', dista
        ipdista=sqrt(dista**2+depth**2)
        write(6,*) 'Distance hypo.:', ipdista
        delta=dista/111.195

        IF(diff.GT.180.) az=360.-az
        azN=az
!----------------------------------------------------------------------
  if(delta.lt.distrad) then
   if(verb.ge.1) then 
      write(6,*) '=================='
      write(6,*) '= Station: ',sta,'='
      write(6,*) '=================='
      write(6,100) dista,az,distmin*111.195,distrad*111.195
      write(6,*) '   response file: ',resfile
      write(6,*) '   calib: ',calib
    endif
!----------------------------------------------------------------------
        mlsta=-999.0
        call dblookup(dbmag, db, "", "stamag", "", "")
        call dbquery(dbmag,dbRECORD_COUNT, n_net)
        call nchar(sta,nch)
        subset = 'sta == "'//sta(1:nch)//'" && magtype == "ml"'
        call dbsubset(db_ml,dbmag,subset,"")
        write(subset,*) "orid == ",orid
        call dbsubset(db_ml,db_ml,subset,"")
        call dbquery(db_ml,dbRECORD_COUNT, n_ml)
        if(n_ml.eq.1) then
          db_ml(4)=0
          if( dbgetv ( db_ml,"","magnitude",mlsta,0) &
          &      .lt. 0) call complain (0, "dbgetv error 6" )  
        endif
!----------------------------------------------------------------------
!  Calculate phases
! 
        arrP=pphasetime(delta1,depth)+otime
        arrS=sphasetime(delta1,depth)+otime
        Ptype='synt'
        Stype='synt'
        resP=0.0
        resS=0.0
!-----------------------------------------------------------------------
!  extract db phases
!
    if(aphas.lt.1) then
      call dbjoin(db_re,db_o,db_as,0,0,0,0,"")
      call dbsubset(db_re,db_re,subset,"")
      call dbjoin(db_re,db_re,db_ar,0,0,0,0,"")
!
! P phase
!
     call nchar(sta,nch)
	 subset= 'phase == "P" && sta == "'//sta(1:nch)//'"'

     call dbsubset(db_re1,db_re,subset,"")
     call dbquery(db_re1,dbRECORD_COUNT, n_w)
!print *,'Records in arrP ',n_w
	 resmin=9999.
     if(n_w.gt.0) then
       do i=0, n_w-1
           db_re1(4) = i
         if( dbgetv ( db_re1,"","arrival.time",arrP_,"timeres",res,"arid",arid,"seaz",seaz,"esaz",esaz,0) &
         &      .lt. 0) call complain (0, "dbgetv error 7" )
	     if(abs(res).lt.resmin) then 
	       resmin=abs(res)
		   arrP=arrP_
		   resP=res
	     endif
	   enddo
	   Ptype='db  '
     endif
!
! S phase
!
     call nchar(sta,nch)
	 subset= 'phase == "S" && sta == "'//sta(1:nch)//'"'

     call dbsubset(db_re1,db_re,subset,"")
     call dbquery(db_re1,dbRECORD_COUNT, n_w)
!print *,'Records in arrS ',n_w
	 resmin=9999.
     if(n_w.gt.0) then
       do i=0, n_w-1
           db_re1(4) = i
         if( dbgetv ( db_re1,"","arrival.time",arrS_,"timeres",res,0) &
         &      .lt. 0) call complain (0, "dbgetv error 8" )
	     if(abs(res).lt.resmin) then 
	       resmin=abs(res)
		   resS=res
		   arrS=arrS_
	     endif
	   enddo
	   Stype='db  '
	 endif
!
       endif
!
!print *, "Azimuth vari:", seaz,esaz,az

    arrSMw=ArrS-otime
!---------------------------------------------------------------------
! end extract db phases
!

     call strtime(string_Pph,arrP)
     call strtime(string_Sph,arrS)

     if(verb.ge.1) write(6,"('    P phase: ',a25,' res: ',f7.3,1x,a4,' S phase: ',a25,' res: ',f7.3,1x,a4)") &
    &   string_Pph,resP,Ptype,string_Sph,resS,Stype

!----------------------------------------------------------------------
     call nchar(sta,nch)
     call nchar(chan,nch1)
     subset = 'sta == "'//sta(1:nch)//'" && chan =="'//chan(1:nch1)//'"'
!print *, "Mac_008 n_ca n_ca_ subset:", n_ca , n_ca_, subset
     call dbsubset(db_ca_,db_ca,subset,"")
!print *, "Mac_009 n_ca n_ca_ subset:", n_ca , n_ca_, subset
     call dbquery(db_ca_,dbRECORD_COUNT, n_ca)
!----------------------------------------------------------------------
!   Check calibration row
!
     if(n_ca.ge.1) then
        do i=0, n_ca-1
          db_ca_(4) = i
          if( dbgetv ( db_ca_,"","samprate",samprate,"calib",calib,"segtype",segtype,0) &
             &      .lt. 0) call complain (0, "dbgetv error 9" )
        enddo
!----------------------------------------------------------------------
!   Check calib   
!
     if(calib.gt.10e-09) then
!----------------------------------------------------------------------
!   Check segtype
!
       write(6,*) '   segtype: ', segtype
       if(segtype.eq.'A'.or.segtype.eq.'V'.or.segtype.eq.'D') then
!----------------------------------------------------------------------
!   Define signal-noise start time and end time
!         
	 t0_req = arrP - pretime
         t1_req = arrP + postime
	 t1o_req = t1_req
	 t3_req = arrP - tnoise - 2.
         t4_req = arrP - 2.
	 t4o_req = t4_req
         t_diff = t1_req -t3_req
         npts_i = int(t_diff*samprate)
!----------------------------------------------------------------------
!   Start loop over channels
!
   chanflag=0
   do icha=1,3

     finf=0.0
     fsup=0.0
     fmin=0.0
     fmax=0.0
     ff1 = f1
     ff2 = f2

     nremw = 0
     npar = 0
     npar1 = 0

     if(icha.eq.1) then
       tsta = tsta+1
     endif
	       
     if(icha.eq.1) chan=chan(1:2)//'Z'
     if(icha.eq.2) chan=chan(1:2)//'N'
     if(icha.eq.3) chan=chan(1:2)//'E'
     write(6,*)
     write(6,*) ' Channel:',chan
     write(6,*) ' -----------'
!----------------------------------------------------------------------
!    Open output table wfparam
!
     call dblookup(dbP1, db, "", "wfparam", "", "")
     if(dbP1(2) .lt. 0) call die ( 0, "Can't open table" )
     call dbquery ( dbP1, dbRECORD_COUNT, nn )

     if(nn .gt. NMAX) then
	        print *, "Number of records in wfparam table:", nn
                call die ( 0, "Table wfparam number of record > NMAX" )
     endif

     write(ptest,'(a6,1x,a8,1x,i8)') sta,chan,orid

     npar = nn
     do i=0,nn-1
       dbP1(4) = i
       itest=dbget(dbP1,dbreco)
       param(i)=dbreco(1:24)
       if(ptest.eq.dbreco(1:24)) then
         itest=dbmark(dbP1)
         npar=npar-1
       endif
       dbreco=''
     enddo
     npar=npar+1
     itest=dbcrunch(dbP1)
!----------------------------------------------------------------------
!    Open output table wfdamage
!
     call dblookup(dbP2, db, "", "wfdamage", "", "")
     if(dbP2(2) .lt. 0) call die ( 0, "Can't open table" )
     call dbquery ( dbP2, dbRECORD_COUNT, nn1 )

     if(nn1 .gt. NMAX) then
                print *, "Number of records in wfdamage table:", nn1
                        call die ( 0, "Table wfdamage number of record > NMAX" )
     endif

     write(ptest,'(a6,1x,a8,1x,i8)') sta,chan,orid

     npar1 = nn1
     do i=0,nn1-1
       dbP2(4) = i
       itest=dbget(dbP2,dbreco1)
       param(i)=dbreco1(1:24)
       if(ptest.eq.dbreco1(1:24)) then
         itest=dbmark(dbP2)
         npar1=npar1-1
       endif
       dbreco=''
     enddo
     npar1=npar1+1
     itest=dbcrunch(dbP2)
!--------------------------------------------------------------------- 
     if(icha.eq.2.and.Mwflag.gt.0) then
!----------------------------------------------------------------------
!    Open output table stamag
!
!         call dblookup(dbmag, db, "", "stamag", "", "")
         if(dbmag(2) .lt. 0) call die ( 0, "Can't open stamag table" )
         call dbquery ( dbmag, dbRECORD_COUNT, nmg )
         nremag = nmg
         if(nmg.gt.0) then
           do i=0,nmg-1
             dbmag(4) = i
         if( dbgetv(dbmag,"","orid",oridmg,"sta",stats,"magtype",magtyts,0).lt.0) &
           & call complain (0, "dbgetv error 10" )
             if(oridmg.eq.orid.and.magtyts.eq.'mw'.and.stats.eq.sta) then
               itest=dbmark(dbmag)
               nremag=nremag-1
             endif
             dbremag=''
           enddo
           nremag=nremag+1
           itest=dbcrunch(dbmag)
         endif
         call dbquery ( dbmag, dbRECORD_COUNT, nmg )
!----------------------------------------------------------------------
!    Open output table stamw
!
         call dblookup(dbmw, db, "", "stamw", "", "")
         if(dbmw(2) .lt. 0) call die ( 0, "Can't open stamw table" )
         call dbquery ( dbmw, dbRECORD_COUNT, nw )

         if(nw .gt. NMAX) call die ( 0, "Table stamw number of record > NMAX" )

         write(mtest,'(a6,1x,a2,"T     ",1x,i8)') sta,chan(1:2),orid

          nremw = nw
          if(nw.gt.0) then
            do i=0,nw-1
              dbmw(4) = i
              itest=dbget(dbmw,dbremw)
              smw(i)=dbremw(1:24)
              if(mtest.eq.dbremw(1:24)) then
                itest=dbmark(dbmw)
                nremw=nremw-1
              endif
              dbremw=''
            enddo
            nremw=nremw+1
            itest=dbcrunch(dbmw) 
          endif
!----------------------------------------------------------------------
!    Open output table netmw
!
       call dblookup(dbmwn, db, "", "netmw", "", "")

       if(dbmwn(2) .lt. 0) call die ( 0, "Can't open netmw table" )

       call dbquery ( dbmwn, dbRECORD_COUNT, nwn )

       write(ntest,'(i8)') orid

       nremwn = nwn
       if(nwn.gt.0) then
         do i=0,nwn-1
           dbmwn(4) = i   
           itest=dbget(dbmwn,dbremwn)
           nmw(i)=dbremwn(1:8)
           if(ntest.eq.dbremwn(1:8)) then
             itest=dbmark(dbmwn)
             nremwn=nremwn-1
           endif
           dbremwn=''
         enddo
       endif
       nremwn=nremwn+1
       itest=dbcrunch(dbmwn) 

!----------------------------------------------------------------------
!    Open output table netmag
!
       call dblookup(dbmagn, db, "", "netmag", "", "")
       call dblookup(lastid, db, "", "lastid", "", "")

       if(dbmagn(2) .lt. 0) call die ( 0, "Can't open netmag table" )
       if(lastid(2) .lt. 0) call die ( 0, "Can't open lastid table" )

       call dbquery ( dbmagn, dbRECORD_COUNT, nmagn )

       write(ntest,'(i8)') orid

       nremagn = nmagn
       if(nmagn.gt.0) then
         do i=0,nmagn-1
           dbmagn(4) = i
           if( dbgetv (dbmagn,"","orid",oridmag,"magtype",magtype,0).lt.0) call complain (0, "dbgetv error 11" )
           if(oridmag.eq.orid.and.magtype.eq.'mw') then
             itest=dbmark(dbmagn)
             nremagn=nremagn-1
           endif
           dbremagn=''
         enddo
       endif
       nremagn=nremagn+1
       itest=dbcrunch(dbmagn)

     endif
!----------------------------------------------------------------------
! read waveforms
!
          npts=0
          dati=0.0

          if(verb.gt.1) then 
            call strtime(string_t1,t1_req)
            call strtime(string_t3,t3_req)
            write(6,*) '     Time request start: ',string_t3(1:24), ' stop: ',string_t1(1:24)
          endif


          nptsM = 0

          do i=0,n_wf-1
            db_wf(4) = i
            if(dbgetv(db_wf,"","sta",sta_,"chan",chan_,"time",wfinizio,"endtime",wffine,"nsamp",nsamp,0).lt.0) &
              &  call complain (0,"dbgetv error 12")
            if(sta_.eq.sta.and.chan_.eq.chan) then
              if(nsamp.lt.npts_i) then
                npts_i=nsamp
                t1_req = t3_req + (npts_i-1)/samprate + 1.
              endif
              if(t1_req.gt.wffine) t1_req=wffine 
              if(t3_req.lt.wfinizio) t3_req=wfinizio
              NMAX64 = NMAX            
              call trgetwf2 ( db_wf, 0, dati, NMAX64, t3_req, t1_req, &
                 &  t3, t1, nptsM, fill, 0 )
            endif
          enddo

          npts =nptsM

          if(verb.gt.1) then 
            call strtime(string_t3,wfinizio)
            call strtime(string_t1,wffine)
            write(6,*) '     From wfdisc table start: ',string_t3(1:24), ' stop: ',string_t1(1:24)
            call strtime(string_t3,t3)
            call strtime(string_t1,t1)
            write(6,*) '     From trgetwf2 start: ',string_t3(1:24), ' stop: ',string_t1(1:24)
          endif

          npts_t=(t1-t3)*samprate

!-------------------------------------------------------------------
!  Check npts
!
    if(npts.gt.0.and.npts.lt.NMAX) then
!-------------------------------------------------------------------
         if((t3_req-t3).lt.0.0.and.abs(t3_req-t3).gt.0.5) then 
           if(verb.ge.1) write(6,*) 'Pretime ',pretime+(t3_req-t3),' less then pf pretime ',pretime
        endif
        if((t1+0.5).lt.t1_req) write(6,*) '     Wform end time less then pf posttime ',postime 

!-----------------------------------------------------------------------
!  Signal pre-process
!
        npts=npts-1
        sismo=0.0
        flagerr = 0
!----------------------------------------------------------------------
!  	Test data missing (overlapping???)
! 

        do i2=1,npts
           sismo(i2)=dati(i2+1)
           sismorg(i2)=sismo(i2)
           if(i2.lt.npts-1) then
             if(abs(dati(i2+1)).gt.1.0e+38.and.abs(dati(i2+2)).gt.1.0e+38) flagerr = 1
           endif
        enddo

        if(flagerr.lt.1) then

!----------------------------------------------------------------------
!   Despike
!
        spikv = 1.0e+03
        spik = spikv 
        if(ispike.eq.1) call rspike(sismo,npts,spik,NMAX)
!----------------------------------------------------------------------
!   Remove mean
!
	     call RMEAN(sismo,npts,NMAX,xmean)
!----------------------------------------------------------------------
!   Compute inc
!
	     last = samprate 
             call CHPAR(npts,last,inc)
!----------------------------------------------------------------------
!   Detrend
!
             if(idetr.eq.1) call DETREN (inc,npts,sismo,NMAX)
!----------------------------------------------------------------------
!   D2
!								
             if(iwin.eq.1) call d2(sismo,npts,d2npts,NMAX)
!------------------------------------------------------------------------
! Read response file and calib costant
!
             call risposta(resfile)

!---------------------------------------------------------------------------
! Apply calib
!
             Gampl = calib

             sismotmp=0.0

             do i2=1,npts
                sismo(i2)=sismo(i2)*Gampl
                sismtmp(i2) = sismo(i2)
             enddo

!		  xtim=0.0 
!		  do i=1,npts
!		   xtim=xtim+1./samprate
!		   write(44,*) xtim,sismo(i),sismorg(i)
!		  enddo
!		  close(44)

!-------------------------------------------------------------------
!  Define signal, Mw and noise windows
!
         inizio = arrP - 1. - t3
         startwin = arrS - (inizio*0.1) - t3
         stopwin = arrS + dista/3. - t3
         startnoise = 0.0
         stopnoise = tnoise  - 1.

         if ( startwin.lt.0 ) call complain ( 0, "Error in Mw window definition!" )

         nstartS = int(inizio*samprate)+1
         nptsSi =  npts - nstartS
         
         nptsS = int(startwin*samprate)+1 
         nptsW = int((stopwin-startwin)*samprate)+1

         nptsN = int(tnoise * samprate)+1

!----------------------------------------------------------------------------
!  Check for errors in windows definition  
!
         flagerr = 0
         if(nptsSi.lt.1.or.nptsS.lt.1.or.nptsW.lt.1.or.nptsN.lt.1) flagerr  = 1
         if(nptsS.gt.nptsSi.or.nptsN.gt.nptsSi) flagerr = 1
         if(inizio.le.0.0) flagerr = 1
!
         if(flagerr.lt.1) then

!---------------------------------------------------------------------------
!   Prepare signals for computation
!
         call nchar(sta,nch)
!	           filename=sta(1:nch)//'Segnale.dat'
!	           open(45,file=filename)
!	           filename=sta(1:nch)//'Noise.dat'
!	           open(47,file=filename)
!	           filename=sta(1:nch)//'Finestra.dat'
!	           open(46,file=filename)	   
!
!	seismogram

		 sismo=0.0
		 idat=0
		 xtim =0.0

		 do i = nstartS,nstartS+nptsSi-1
		   idat=idat+1
		   sismo(idat) = sismtmp(i)
		   xtim=(1./samprate)*i
!  		   write(45,*) xtim,sismo(idat)
		 enddo
!		 close(45)

!
!   noise

		 noise=0.0
		 idat=0
		 do i = 1,nptsN
		   idat=idat+1
		   noise(idat) = sismtmp(i)
		   xtim=(1./samprate)*i
!  		   write(47,*) xtim,noise(idat)
		 enddo
		 nptsN=idat
!		 close(47)

!
!   Mw 
!
         if(icha.eq.2.and.Mwflag.gt.0) then
		   sismoN=0.0
		   idat=0
		   do i =nptsS,nptsS+nptsW-1
		     idat=idat+1
		     sismoN(idat) = sismtmp(i)
		     xtim=(1./samprate)*i
!  		     write(46,*) xtim,sismoN(idat)
		   enddo

		   nptsW=idat
!		   close(46)
		 endif

         if(icha.eq.3.and.Mwflag.gt.0) then
		   idat=0
		   sismoE=0.0
		   do i =nptsS,nptsS+nptsW-1
			 idat=idat+1
		     sismoE(idat) = sismtmp(i)
		   enddo
		 endif
!---------------------------------------------------------------------------
!  set npts to nptsSi (number of points of the signal window)
!

           call POTE(nptsSi,npts,0)

!---------------------------------------------------------------------------
!  set inc in frequency domain
!
             call CHPAR(npts,last,inc)

!---------------------------------------------------------------------------
!   Complex signal
!
              sism = cmplx(0.0,0.0)
			  nois = cmplx(0.0,0.0)
			  x = 0.0
              do i2=1,npts
                 sism(i2)=cmplx(sismo(i2),0.0)
				 nois(i2)=cmplx(noise(i2),0.0)
	             x(i2) = (1./samprate)*(i2-1)
              enddo
!---------------------------------------------------------------------------
!   Compute FFT
!
              call pfft(npts,sism,-1.,NMAX)
              call pfft(npts,nois,-1.,NMAX)
!
              sism(1) = cmplx(0.0,0.0)
			  nois(1) = cmplx(0.0,0.0)
			  			 
	      npts2 = npts/2

	      call SMOOTH(abs(sism),sismS,npts,3)
   	      call SMOOTH(abs(nois),noisS,npts,3)
              
!	      do i = 1,npts
!	        sismS(i) = abs(sism(i))
!		noisS(i) = abs(nois(i))
!	      enddo
			  
              sismS(1) = cmplx(0.0,0.0)
	      noisS(1) = cmplx(0.0,0.0)
!-----------------------------------------------------------------------------
! compute signal noise ratio
!
	      freq1=0.0
	      freqq(1)=0.0

	      do i2=2,npts2+1      
                freq1=freq1+inc
	        freqq(i2)=freq1
	        rappsr(i2) = sismS(i2)/noisS(i2)
	      enddo
!-----------------------------------------------------------------------------
! compute freq max and freq min
! 
              call F1F2(rappsr,freqq,npts2,fmin,fmax,tnoise)
       	      if(verb.gt.0) write(6,"(6x,'Noise test: freq min = ',f7.3,' freq max = ',f7.3)") fmin,fmax
	      if(verb.ge.3.and.icha.gt.1) then
	        freq1=0.0
	        call nchar(sta,nch)
	        if(icha.eq.2) filename=sta(1:nch)//'snrN.dat'
	        if(icha.eq.3) filename=sta(1:nch)//'snrE.dat'
!	        open(58,file=filename)
!	        do i=1,npts2+1
!                 freq1=freq1+inc
!	          write(58,*) freq1,sismS(i),noisS(i),rappsr(i)
!	        enddo
!	        close (58)
              endif
!---------------------------------------------------------------------------
! Define finf and fsup
!

              finf=fmin
	      fsup=fmax

              if(icha.gt.1) then
	        if(fmin.gt.finf) finf=fmin
	        if(fmax.lt.fsup) fsup=fmax
              endif

              if(ifilt.eq.3.or.ifilt.eq.4) then
                if(fmin.gt.ff1) ff1 = fmin
                if(fmax.lt.ff2) ff2 = fmax
	      endif

!-----------------------------------------------------------------------------
! if frequency range
!

              if(fsup.gt.finf.and.finf.lt.1.1) then 
!               if(ifilt.gt.2.or.ifilt.lt.3.and.ff1.ge.fmin.and.ff2.lt.fmax) then
               if(ff1.ge.fmin.and.ff2.le.fmax) then

!	      open(22,file='Filtered')
!	      open(23,file='Corrected')
!	      open(24,file='Row')
!	      open(26,file='Noise')

!---------------------------------------------------------------------------
! Compute response curve
!
              call PAZ(npts_fr,npts,first,last,inc,Gampl,segtype)
              if(icha.eq.2.or.icha.eq.3) tR=t
!-------------------------------------------------------------
!  apply filter
!
              if(ifilt.eq.2) write(filter,"('BW',2(1x,f5.1,1x,i2))") ff1,np1,ff2,np2
              if(ifilt.eq.1) write(filter,"('GA',2(1x,f5.1,1x,'0'))") ff1,ff2
              if(ifilt.eq.4) write(filter,"('Ba',2(1x,f5.1,1x,i2))") ff1,np1,ff2,np2
              if(ifilt.eq.3) write(filter,"('Ga',2(1x,f5.1,1x,'0'))") ff1,ff2
	      if(verb.ge.1) write(6,*) '     Apply filter: ', filter

!-------------------------------------------------------------
!  Signal processing
!
	          freq1 = 0.0
			  sismA=cmplx(0.0,0.0)
			  sismV=cmplx(0.0,0.0)
			  sismD=cmplx(0.0,0.0)

	          do i2=2,npts2+1      
                    freq1=freq1+inc
				
!	            write(24,*) freq1,abs(sism(i2)),abs(nois(i2)),abs(t(i2)),abs(hsb1(i2)),abs(hsb2(i2))
!                   write(26,*)	freq1,noisS(i2),abs(nois(i2)),sismS(i2),abs(sism(i2)),rappsr(i2)
!
!  Butterworth filter
!
	            if(ifilt.eq.2.or.ifilt.eq.4) then
	              if(np1.ne.0)sism(i2) = sism(i2)*hsb1(i2)
                      if(np2.ne.0)sism(i2) = sism(i2)*hsb2(i2)
	            endif

!		
!  Gaussian filter
!
	            if(ifilt.eq.1) then
	              sism(i2) = sism(i2)*xgau(i2)
	            endif
!
!	            write(22,*) freq1,abs(sism(i2)),abs(nois(i2)),abs(t(i2)),abs(hsb1(i2)),abs(hsb2(i2))

!----------------------------------------------------------
!  Deconvolve response 
!
                sism(i2) = (sism(i2)/t(i2))

!	        write(23,*) freq1,abs(sism(i2)),abs(nois(i2)),abs(t(i2)),abs(hsb1(i2)),abs(hsb2(i2))
!
                errfl=0
!
! Input in Acceleration
!
	           if(segtype.eq.'A') then
	             sismA(i2) = sism(i2)
	             sismV(i2) = sismA(i2)/xder(i2)
	             sismD(i2) = sismV(i2)/xder(i2)
		     errfl=1	                
		   endif
!
! Input in Velocity
!
	           if(segtype.eq.'V') then
	             sismA(i2) = sism(i2)*xder(i2)
	             sismV(i2) = sism(i2)
	             sismD(i2) = sismV(i2)/xder(i2)	                	                
		     errfl=1	                
		   endif					 
!
! Input in Displacement
!
	           if(segtype.eq.'D') then
	             sismD(i2) = sism(i2)
                     sismV(i2) = sismD(i2)*xder(i2)
                     sismA(i2) = sismV(i2)*xder(i2)
		     errfl=1	                
		   endif
!
              enddo       
!-------------------------------------------------------------------
!              close(23)
!              close(22)
!              close(24)
!	       close(26)
!-------------------------------------------------------------------
! 
!
              call nchar (sta,nch)
              call nchar (chan,nch1)
              if(verb.gt.2) then
                filexy=sta(1:nch)//"-"//chan(1:nch1)//".AVD"
	        open(3,file=filexy)
	      endif

 	      l2=1

              do i2 = npts,npts-npts2+2,-1
                l2 = l2+1
	        sism(i2) = conjg(sism(l2))
                sismA(i2) = conjg(sismA(l2))
	        sismV(i2) = conjg(sismV(l2))
	        sismD(i2) = conjg(sismD(l2))
	       enddo
!---------------------------------------------------------------------------
!   Compute inverse FFT
!
	      call pfft(npts,sism,1.,NMAX)
              call pfft(npts,sismA,1.,NMAX)
              call pfft(npts,sismV,1.,NMAX)
              call pfft(npts,sismD,1.,NMAX)

	      PGA=0.0
	      PGV=0.0
 	      PGD=0.0
              sismoA = 0.0
              sismoV = 0.0
              sismo = 0.0
	  
              do i2 = 2,nptsSi

                sismA(i2) = sismA(i2)/npts		  
	        sismV(i2) = sismV(i2)/npts
	        sismD(i2) = sismD(i2)/npts
                sismoA(i2) = real(sismA(i2))
                sismoV(i2) = real(sismV(i2))
                sismoD(i2) = real(sismD(i2))

	        if(abs(sismoA(i2)).gt.PGA) PGA=abs(sismoA(i2))
	        if(abs(sismoV(i2)).gt.PGV) PGV=abs(sismoV(i2))
	        if(abs(sismoD(i2)).gt.PGD) PGD=abs(sismoD(i2))
                if(verb.gt.2) write(3,*) x(i2),sismorg(i2),sismoA(i2),sismoV(i2),sismoD(i2)

              enddo
!
	      if(verb.gt.1) close(3)
!---------------------------------------------------------------------------
!       Compute response spectra
!
              sampint = 1.0/samprate
	          PSA03=0.0
	          PSA10=0.0
	          PSA30=0.0
              IRANGE=1
              call RESPEK(sampint,nptsSi,sismoA)
              label = home_dir(1:strlen)//"/respectra/"
              inquire(FILE=label,EXIST=exis)
              if(exis) then
!               write(*,*) "Directory ",TRIM(label)," already exists."
              else
                command = "mkdir "//label
                call system(TRIM(command))
                write(*,*) "Directory ",TRIM(label)," created."
              endif
              call epoch2str(string_ot1,otime,"%G")
              label=TRIM(label)//TRIM(string_ot1)
              inquire(FILE=label,EXIST=exis)
              if(exis) then
!               write(*,*) "Directory ",TRIM(label)," already exists."
              else
                command = "mkdir "//label
                call system(TRIM(command))
                write(*,*) "Directory ",TRIM(label)," created."
              endif
              call epoch2str(string_ot1,otime,"%H%M%S")
              label = TRIM(label)//"/"//TRIM(string_ot1)
              inquire(FILE=label,EXIST=exis)
              if(exis) then
!               write(*,*) "Directory ",TRIM(label)," already exists."
              else
                command = "mkdir "//label
                call system(TRIM(command))
                write(*,*) "Directory ",TRIM(label)," created."
              endif
              filspt = TRIM(label)//"/"//sta(1:nch)//"-"//chan(1:nch1)//".spt"
              open(30,file=filspt)
	      do i2=1,IP
!               if((1.0/PD(i2)).ge.ff1.and.(1.0/PD(i2)).le.ff2) then
!!	          write(30,"(4(1x,e13.6))") PD(i2),SA(1,i2)*1.0e-07,SV(1,i2)*1.0e-07,SD(1,i2)*1.0e-07
                  write(30,"(4(1x,e13.6))") PD(i2),SA(1,i2)*1.0e-07
!               endif
	      enddo
	      close(30)
              write(*,*) "     response spectrum file: ",filspt," created"
              write(*,*) "     min freq: ",ff1," max freq: ",ff2
!----------------------------------------------------------------------------
	          PSA03=0.0
	          PSA10=0.0
	          PSA30=0.0
              IRANGE=0
              call RESPEK(sampint,nptsSi,sismoA)
	          do i3=1,1
	            write(idum,'(i1)') i3
	            do i2=1,IP
                      if(i3.eq.1) then
	                if(abs(PD(i2)-0.3).lt..001.and.ff1.le.3.0) PSA03=SA(i3,i2)
	                if(abs(PD(i2)-1.0).lt..001.and.ff1.le.1.0) PSA10=SA(i3,i2)
	                if(abs(PD(i2)-3.0).lt..001.and.ff1.le.0.3) PSA30=SA(i3,i2)
		      endif 
	            enddo
	          enddo
!---------------------------------------------------------------------------
!    Compute Arias
!         
              call ENERGY(sismoA,sampint,nptsSi,energ,dura,te)
              RMSA = sqrt(energ/dura)
              Arias = energ * pi / (grav * 2.0)

!---------------------------------------------------------------------------
!    Housner intensity - Mar 2007 Giovanni
!
              IRANGE=1
!
              call RESPEK(sampint,nptsSi,sismoA)
!	              
	      delhou=abs(PD(1)-PD(2))
              Housner=0.0
              Housner1=0.0
              do ihou=1,200
!	        write(31,*) PD(ihou),SV(3,ihou),pssv(3,ihou)
                Housner1=Housner1+(SV(1,ihou)*delhou)
                if(PD(ihou).ge.0.1.and.PD(ihou).le.2.5) then
	          Housner=Housner+(SV(1,ihou)*delhou)
                endif
	      enddo
              write(*,*) "     Housner1, Housner:", Housner1, Housner
!---------------------------------------------------------------------------
!   write parameters file
!
              PGA =PGA * 1.0e-07
              PGV =PGV * 1.0e-07
              PGD =PGD * 1.0e-07
	      PSA03 = PSA03 * 1.0e-07
	      PSA10 = PSA10 * 1.0e-07
	      PSA30 = PSA30 * 1.0e-07
	      Housner = Housner * 1.0e-07
!---------------------------------------------------------------------------
! Compute saragoni Factor & Cosenza and Manfredi Damage Factor
!
              Sarag = Arias/(te*te)
              Mf = (2.*grav/pi)*Arias/(PGA*PGV)
!---------------------------------------------------------------------------
			  if(verb.ge.1) then
			    write(6,220) PGA,PGV,PGD
			    write(6,221) PSA03,PSA10,PSA30
			    write(6,222) Arias,Housner
			  endif 

 220 		  format(6x,'PGA   = ',f12.5,' PGV     = ',f12.5,' PGD   = ',f12.5)
 221 		  format(6x,'PSA03 = ',f12.5,' PSA10   = ',f12.5,' PSA30 = ',f12.5)
 222 		  format(6x,'Arias = ',f12.5,' Housner = ',f12.5)
              
			  iterOK=1
!---------------------------------------------------------------------------
!   add record to the db tables wfparam  and wfdamage
!
              lddate=now()

              if(PSA03.lt.1.0e-06) PSA03 = -999.
              if(PSA10.lt.1.0e-06) PSA10 = -999.
              if(PSA30.lt.1.0e-06) PSA30 = -999.
              if(PGA.lt.1.0e-06) PGA = -999.
              if(PGV.lt.1.0e-06) PGV = -999.
              if(PGD.lt.1.0e-06) PGD = -999.
              if(Arias.lt.1.0e-06) Arias = -999.
              if(Housner.lt.1.0e-06) Housner = -999.

              if(RMSA.lt.1.0e-06) RMSA = -999.
              if(Sarag.lt.1.0e-06) Sarag = -999.
              if(Te.lt.1.0e-06) Te = -999.
              if(Mf.lt.1.0e-06) Mf = -999.
              if(PGD.lt.1.0e-06) PGD = -999.

              if(PGA.gt.-999..or.PGV.gt.-999.) then
!--------------------------------------------------------------------
! write records in wfparam table
!
	        t2 = t3 + (nptsSi-1)*sampint
                write(dbreco,230) sta,chan,orid,filter,t3,t2,	   &
                &                 mlsta,dista,az,PGA,PGV,	   &
                &                 PSA03,PSA10,PSA30,Arias,Housner, &
                &                 arid,auth,lddate
 230            format(a6,1x,a8,1x,i8,1x,a30,2(1x,f17.5),3(1x,f7.2), &
                &      7(1x,f15.6),1x,i8,1x,a15,1x,f17.5)

	        istor=dbP1(4) 

	        dbP1(4)= npar
		itest = dbadd(dbP1,dbreco)
	        if(itest.lt.0) call die ( 0, "Can't write record in table wfparam" )
!--------------------------------------------------------------------
! write records in wfdamage table
!
                write(dbreco1,231) sta,chan,orid,PGD,dura,RMSA,Sarag,Te,Mf,auth,lddate
 231            format(a6,1x,a8,1x,i8,1x,6(1x,f15.6),1x,a15,1x,f17.5) 

                istor1=dbP2(4)

                dbP2(4)= npar1
                itest = dbadd(dbP2,dbreco1)
                if(itest.lt.0) call die ( 0, "Can't write record in table wfdamage" )
!--------------------------------------------------------------------
               endif
!-----------------------------------------------------------------------------
               if(icha.eq.2) chanflag=chanflag+1
               if(icha.eq.3) chanflag=chanflag+1
!-----------------------------------------------------------------------------
   else
    if(verb.ge.1) write(6,*) '     SNR test not passed ',sta,'_',chan,' REMOVED'
   endif
  else
   if(verb.ge.1) then 
     if(fsup.lt.finf) write(6,*) '     fsup lt finf ',sta,'_',chan,' REMOVED'
     if(fsup.lt.finf) write(6,*) '     finf lt 1.1 Hz ',sta,'_',chan,' REMOVED'
   endif
  endif
!
! end if frequency range
!--------------------------------------------------------------------
		    else 
		      if(verb.ge.1) write(6,*) '     Wrong windows definition ',sta,'_',chan,' REMOVED'
		    endif
!
! End check windows
!----------------------------------------------------------------------
          else
		      if(verb.ge.1) write(6,*) '     Wrong data (data missing?, overlapping?) ',npts,' - ',sta,'_',chan,' REMOVED'
		  endif
! 
!  	Test data missing (overlapping???)
!----------------------------------------------------------------------

          else
		      if(verb.ge.1) write(6,*) '     Wrong npts ',npts,' - ',sta,'_',chan,' REMOVED'
		  endif
!
! End check npts
!----------------------------------------------------------------------

         enddo
!
!   End loop over channels
!----------------------------------------------------------------------
! Mw computation
!
  if(delta.lt.distrad.and.delta.ge.distmin) then

   if(verb.ge.1) then
      write(6,*) '=================================='
      write(6,*) '- Mw computation  Start'
   endif

!----------------------------------------
   if(chanflag.eq.2.and.Mwflag.eq.1) then

      chanflag = 0
!     azR=270.-azN
      azR=azN-90.

      arrSo = arrS-otime

!----------------------------------------------------------------------
!   Enter in Mw_core for Mw computation
!

     call Mw_core(dbremw,sismoE,sismoN,tR,npts,nptsW,inc,azR,arrSo,dista,Q,finf,fsup,segtype,samprate,sta,verb)

!--------------------------------------------------------------------------------
! add row to stamw database 
!
         if(verb.ge.1) write(6,*) '  Add new record to table .stamw' 
print *, '  Add new record to table .stamw'
         istor=dbmw(4) 
         dbmw(4)=nremw
         itest=dbadd(dbmw,dbremw)
         if(itest.lt.0) call die ( 0, "Can't write record" )
!--------------------------------------------------------------------^M
! extract magid from lastid table                   
!
      if(lstflag.eq.0) then
        call dbquery (lastid,dbRECORD_COUNT,lstid)
        do ilst=0,lstid-1
          lastid(4) = ilst
          if( dbgetv (lastid,"","keyname",keyname,"keyvalue",keyvalue,0).lt.0) call complain (0, "dbgetv error 13")
          if(keyname.eq.'magid') then
            magid = keyvalue
            itest=dbmark(lastid)
            magid = magid + 1
            if(dbputv(lastid,"","keyname",keyname,"keyvalue",magid,"lddate",lddate,0).lt.0) call complain (0, "dbputv error 14" )
          endif
        enddo
        itest=dbcrunch(lastid)
        lstflag = 1
      endif
      call dbquery (lastid,dbRECORD_COUNT,lstid)
!--------------------------------------------------------------------------------
! add row to stamag database 
!
         call dbquery (dbmag,dbRECORD_COUNT,ndbmg)
         dbmag(4)= ndbmg
         itest = dbaddnull(dbmag)
         if(dbputv(dbmag,"","magid",magid,"sta",sta,"arid",arid, &
           & "orid",orid,"evid",evid,"magtype",'mw',"magnitude",staMw(nmwdat), &
           & "auth",auth,"lddate",lddate,0).lt.0) &
           &  call complain (0, "dbputv error" )
         call dbquery (dbmag,dbRECORD_COUNT,ndbmg)
!---------------------------------------------------------------------------------
!
   else
   if(verb.ge.1) then
      write(6,101) chanflag, Mwflag
101   format('  Number of channels(OK=2):',i2,' Mwflag(OK=1):',i2)
    endif

   endif
!-----------------------------------------------------------------------
  else

   if(verb.ge.1) then
      write(6,*) '=================================='
      write(6,*) '  Mw computation  station REMOVED'
      write(6,100) dista,az,distmin*111.195,distrad*111.195
100   format('    Distance:',f7.2,' Azimuth:',f7.2,' min distance:',f7.2,' max distance:',f7.2)
   endif
  endif
!----------------------------------------------------------------------
      else
        if(verb.ge.1) then
            write(6,*) '     Wrong segtype:',segtype,' station ',sta,'_',chan,' REMOVED'	   
	endif
      endif
!
!   End check segtype
!-----------------------------------------------------------------------
     else
       if(verb.ge.1) then
         write(6,*) '     Wrong calib:',calib,' station REMOVED - CHECK dbmaster!!!!'                 
       endif
     endif
!
!   End check calib
!-----------------------------------------------------------------------

     else
       if(verb.ge.1) write(6,*) '     No calibration row - n_ca=',n_ca,' station ',sta,'_',chan,' REMOVED'
	 endif
!
!   End check calibration row
!----------------------------------------------------------------------
  else
    if(verb.ge.1) write(6,*) ' Station: ',sta,'Distance:',dista,' Azimuth: ',az,' min distance: ',distrad*111.195,' REMOVED'
  endif
!
!   End check for general minimum distance
!----------------------------------------------------------------------

 else
   if(verb.ge.1) write(6,*) ' Station ',sta,' REMOVED - No response file found'
 endif
!
! end check responsefile 
!-----------------------------------------------------------------------
enddo
!---------------------------------------------------------------------
!   
!	  End loop over station
!
!---------------------------------------------------------------------
else
  if(verb.ge.1.and.n_ca.lt.1) then
    write(6,*) '    Orid: ',orid,'No waveforms to process ',sta,' REMOVED'
print *,'Origin time: ',string_ot,'Lat.',olat,'Lon.',olon,'Ml = ',ml,' ORID: ',orid,' REJECTED'	 
  endif
  if(verb.ge.1.and.ml.lt.magmin) then
    write(6,*) '    Orid: ',orid,'Ml:',ml,' ',sta,' REMOVED'
print *,'Origin time: ',string_ot,'Lat.',olat,'Lon.',olon,'Ml = ',ml,' ORID: ',orid,' REJECTED'	 
  endif
endif	 ! magnitudo min n_ca
!
! Main if
!
 else
   if(verb.ge.1) write(6,*) 'No waveforms for orid ',orid
   print *,'No waveforms for orid ',orid 
 endif
!---------------------------------------------------------------------
!      Moment magnitude network table
!

 if(nmwdat.gt.0) then
	
!-----------------------------------------------------------------------
       netMw=0.0
       netM0=0.0
       netf0=0.0
       neteqR=0.0
       sigmaMw=0.0

   do ii=1,nmwdat
     netMw=netMw+staMw(ii)
     netM0=netM0+staM0(ii)
     netf0=netf0+staf0(ii)
     neteqR=neteqR+staeqR(ii)
   enddo

   netMw=netMw/float(nmwdat)
   netM0=netM0/float(nmwdat)
   netf0=netf0/float(nmwdat)
   neteqR=neteqR/float(nmwdat)
   do ii=1,nmwdat
     sigmaMw=sigmaMw+(staMw(ii)-netMw)**2
   enddo
   sigmaMw = sqrt(sigmaMw**2/float(nmwdat))

!--------------------------------------------------------------------------------
! add row to database 
!
      usta=nmwdat
      rsta = tsta - usta
      dbremwn=''
      if(qual.lt.10.e-06) qual = -999.
      if(commid.eq.0) commid = -1

      write(dbremwn,110) orid,evid,netMw,ml,sigmamw,netM0,netf0,neteqR,usta,rsta,qual, &
      & auth,commid,lddate
 110  format(i8,1x,i8,2(1x,f7.2),1x,f7.2,1x,e9.3,2(1x,f7.2),2(1x,i8),1x,f7.2,1x,a15,1x,i8,1x,f17.5)
 
      istor=dbmwn(4) 
      dbmwn(4)=nremwn
      itestn = dbadd(dbmwn,dbremwn)
      if(itestn.lt.0) call die ( 0, "Can't write record" )

!--------------------------------------------------------------------^M
! write records in netmag table
!

      call dbquery (dbmagn,dbRECORD_COUNT,ndbmg)
      dbmagn(4)= ndbmg
print *, 'Scrivo netmag table, records presenti:', ndbmg
print *, magid, orid, evid, magtype, usta, netMw, signamw, auth, lddate
      itest = dbaddnull(dbmagn)
      if(dbputv(dbmagn,"","magid",magid,"orid",orid,"evid",evid, &
        & "magtype",'mw',"nsta",usta,"magnitude",netMw,"uncertainty",sigmamw, &
        & "auth",auth,"lddate",lddate,0).lt.0) &
        &  call complain (0, "dbputv error 15" )
       call dbquery (dbmagn,dbRECORD_COUNT,ndbmg)
!---------------------------------------------------------------------------------
!  End MW network table
!
 endif
!---------------------------------------------------------------------
!
!         Ending MAIN loop over orid
!
!---------------------------------------------------------------------
    itest = dbclose(db)

 400 format(a125)
enddo 
!--------------------------------------------------------------------------
!
stop
end
!===================================================================
        subroutine fill ( db, dati, i0, i1, imax, value )
        implicit none
        integer db 
        integer*8 i, i0, i1, imax
        real dati(imax)
        real value

!print *, "Mac_fill db, i0, i1, imax, value:", db, i0, i1, imax, value

        do i=i0+1, i1
            dati(i) = value
        end do
        end

        subroutine nchar(string,nch)
        implicit none
        character* (*) string
        integer i,nch
        do i=1,128
         if(string(i:i).eq." ") then
          nch=i-1
          go to 10
         endif
        enddo
 10     continue
        return
        end
!===================================================================
        subroutine risposta(resfile)
        implicit none
        integer nch,i,i1,iflag
        character*132 reco
        character*100 resfile
        real *8 norma,a1,a2,a3,a4
        complex *8 poli(20),zeri(20),poli_er(20),zeri_er(20)
    	real Campl,percal 
        integer np,nz,ip,iz
        COMMON/RPAZ/ poli,zeri,np,nz,Campl,percal

        iflag=0

        call nchar(resfile,nch)

        open(1,file=resfile(1:nch),status="OLD")
        do i=1,100
          if(iflag.eq.0) then
            read(1,'(a132)') reco
            do i1=1,129
              if(reco(1:1).ne.'#') then
                if(reco(i1:i1+2).eq.'paz'.or.reco(i1:i1+2).eq.'PAZ') then
                   iflag=1
                   read(1,*) norma
                   read(1,*) np
                   do ip=1,np
                     read(1,*) a1,a2,a3,a4
                     poli(ip) = cmplx(a1,a2)
                     poli_er(ip) = cmplx(a3,a4)
                   enddo
                   read(1,*) nz
                   do iz=1,nz
                     read(1,*) a1,a2,a3,a4
                     zeri(iz) = cmplx(a1,a2)
                     zeri_er(iz) = cmplx(a3,a4)
                   enddo
                endif
              endif
            enddo
          endif
        enddo
	Campl = norma

        if ( iflag .ne. 1 ) then
           write(6,*) "Wrong response file: ",resfile
        endif
        close(1)
        return
        end
!=================================================================
      SUBROUTINE POTE(idat,nn,iflag)
!=================================================================        
      idat1=idat
      if(iflag.eq.1) idat1=idat*2     
      DO I1=1,50
      IPOT=2**I1
      IF(IPOT.GE.(idat1)) then
      NN=IPOT
      go to 33
      endif
      ENDDO
   33 continue
!      write(*,100) idat,nn
  100 format(/,1x,'POTE, ndata: input=',i10,'   output=',i10)
      RETURN
      END  
!=================================================================	    
      SUBROUTINE POTE1(idat,nn,iflag)
!=================================================================        
      idat1=idat
      if(iflag.eq.1) idat1=idat*2     
      DO I1=1,50
      IPOT=2**I1
      IF(IPOT.GE.(idat1)) then
      NN=2**(I1-1)
      go to 33
      endif
      ENDDO
   33 continue
      write(*,100) idat,nn
  100 format(/,1x,'POTE1, ndata: input=',i10,'   output=',i10)
      RETURN
      END    
!------------------------------------------------------------------------
!   Subroutine rmean	 (Giovanni gennaio 2006)
!       
	SUBROUTINE RMEAN(sism,ndata,nmax,xmean)
	dimension sism(nmax)
 
	    xmean = 0.0

          do i=1,ndata
           xmean=xmean+sism(i)        
          enddo
  
           xmean=xmean/ndata
 
          do i=1,ndata
           sism(i)=sism(i)-xmean        
          enddo
 
!          write(*,100) xmean
 100      format(/,1x,'Mean:',e16.9,' - Mean removed')
!------------------------------------------------------------------------
	RETURN
	END
!------------------------------------------------------------------------
!   Subroutine rspike	 (Giovanni gennaio 2006)
!       
	SUBROUTINE rspike(sism,ndata,spikv,nmax)
	dimension sism(nmax)
!
!
        iflag =0
		ifin = 1000 
		spikv = 5          
		do ii=1,ndata

          valdif=1500.*abs(sism(ii-1)-sism(ii+1))

		  if(valdif.gt.1.0e-07) then

	       if(((abs(sism(ii))-abs(sism(ii+1)))+(abs(sism(ii))-abs(sism(ii-1)))/2.).gt.valdif) then

	         iflag =1
	         sismold=sism(ii)
	         sism(ii)=(sism(ii-1)+sism(ii+1))/2.0
	         write(*,100) ii,sismold,sism(ii)
  100        format(/,1x,'Rspike sostituito punto ',i6, ' vecchio: ',e16.9,' nuovo:',e16.9)
	       endif
		  endif
	    enddo
	    if(iflag.eq.1) write(*,120) 
  120   format(/,1x,'WARNING!!!! ispike=1 despike applied!!!!')

!------------------------------------------------------------------------
	RETURN 
	END
!-----------------------------------------------------------------------
      subroutine DETREN (par,ipar,y,nmax)
!-----------------------------------------------------------------------
      dimension y(nmax)
      real par
      integer ipar
        data q/1.0e+06/
!-----------------------------------------------------------------------
      bm=float(ipar*(ipar-1))*0.5/q
      am=(2*ipar-1)*bm/3.
      an=ipar/q
      c=0.0
      d=0.0
        do 1 i=1,ipar
               c=c+y(i)*(i-1)
 1             d=d+y(i)
      delta=(am*an-bm*bm)*q
      a=(c*an-d*bm)/delta
      b=(d*am-c*bm)/delta
        do 2 i=1,ipar
 2    y(i)=y(i)-a*(i-1)-b
      c =a/par
!      write(*,100)  c,b
      return
 100  format(/' Detrend is applied, slope =',e11.4,' units per sec,' &
     & ,' shift =',e11.4,' units')
      end                                  
! ----------------------------------------------------------------------
!   subroutine D2
!
      SUBROUTINE D2(yy,lung,tran,NMAX)
      real*4 yy(nmax), yy2(nmax), em, f, pi
      integer tran, inferiore, ii, jj, lung, invariati
      data pi/3.141592654/ 
!      write(*,100) tran      
  100 format(/,1x,'D2 window is applied with ',i5,' points')    
      inferiore = tran + 1
      invariati = lung-tran
      do ii=inferiore, invariati
         yy2(ii)=yy(ii)
      enddo
      em = pi/tran
      do ii=1, tran
         f=0.5*(1.-cos(float(ii-1)*em))
         yy2(ii)=yy(ii)*f
         jj=lung+1-ii
         yy2(jj)=yy(jj)*f
      enddo
      do ii=1, lung
         yy(ii)=yy2(ii)
      enddo  
      Return 
      end
!======================================================================
      SUBROUTINE CHPAR(npunti,last,inc)  
!======================================================================
      real inc,last
!	 
!       Sampling step in frequency domain is sampling interval
!       devided by number of samples:
      inc=last/(npunti-1)
!
!      write(*,100) npunti,last,inc
  100 format(/,1x,'Chpar - npunti, Max freq., freq. step: ',i8,2e13.6)  
      RETURN
      END
!=================================================================
      SUBROUTINE PAZ(idat,npunti,first,last,inc,Gampl1,labG)
!=================================================================
!       input:
!               npunti = number of samples in expanded time series
!               inc = samples/sec/npunti  if sz
!               Gampl1 = Ga calibration factor for acceleration. 
!               Gampl1 = Gd calibration factor for displacement.
!               labG = '2' or labG = '0'
! 
      parameter ( NMAX = 100000 )
      character*1 labG
      character*18 transfile
      real first,inc,last,omega,absval,amaxa,phase
      real*8 ff1,ff2,f1, f2
      complex t(NMAX),hrvres,NUM,DEN,D,awcal
      complex P(20),Z(20),xder(NMAX)
!      integer np1
!      integer np2
      complex hs1
      complex hs2
      complex hsb1
      complex hsb2
      real sqbsum,abmax
      common/BUT/sqbsum,abmax,hsb1(NMAX),hsb2(NMAX)
      COMMON/RDATI/f1,f2,ifilt,np1,np2
      COMMON/RPAZ/ P,Z,npoli,nzero,Campl,percal
      COMMON/CPAZ/ t,phase(NMAX),absval(NMAX),freq(NMAX)
      COMMON/GAUSS1/XGAU(NMAX)
      COMMON/DERI/xder
!-----------------------------------------------------------------------
      t=cmplx(0.0,0.0)
      xder=cmplx(0.0,0.0)
      hsb1=cmplx(0.0,0.0)
      hsb2=cmplx(0.0,0.0)	  
!-----------------------------------------------------------------------
!      write(*,1)Campl,Gampl1 
  1   format(/,1x,'PAZ - Campl, Gampl1:',e16.9,f9.3)   
!-----------------------------------------------------------------------
!
!       Add poles or subtract zeroes at (0.0,0.0) for output of ground 
!       displacement, velocity, or acceleration.
!       Dont use more than necessary zeroes and poles.
!
        imxdat=NMAX
        ndis=0
        nvel=0
        nacc=0
        if(labG.eq.'D') then
	    ndis=1
	  endif
        if(labG.eq.'V')then
	    nvel=1
	  endif
        if(labG.eq.'A')then 
	    nacc=1
	  endif
!-----------------------------------------------------------------------
!       For Butterworth filter

        hs1 = cmplx(1.0,0.0)
        hs2 = cmplx(1.0,0.0)
        print *,"Subroutine PAZ compute Butterworth filter for frequency range: ", f1, f2
!-----------------------------------------------------------------------
      twopi=6.28318530717959D0
!-----------------------------------------------------------------------
      do i=1,imxdat/2
        t(i)=cmplx(0.0e+00,0.0e+00)
        hsb1(i)=cmplx(0.0e+00,0.0e+00)
        hsb2(i)=cmplx(0.0e+00,0.0e+00)
      enddo
!
      phase(1)=0.0
      absval(1)=0.0
      freq(1)=0.0

      hsb1(1) = cmplx(0.0,0.0)
      hsb2(1) = cmplx(0.0,0.0)

!        
!     put the response at zero frequency to zero
!
      t(1)=cmplx(0.0,0.0)
!
!  check input for validity
!
      first=first*twopi
      last=last*twopi
      inc=inc*twopi
      if(first.gt.last) stop
!
!  set frequency to start value
!
      amp1=0.0e+00
      icon=0
      omega=first
        amaxa=0.0
!-----------------------------------------------------------------------
!  compute transfer function
!       it is really not necessary to calculate more than npunti/2 +1
!       complex values of t in frequency domain.
!-----------------------------------------------------------------------
  222 format(1x,'Dati maggiori di NMAX: ',i5)
      idat=1
      omega=0.0e00
      f = 0.0e00
      sqbsum = 0.0
      abmax=0.0

      do it=2,npunti/2+1
        f = f + inc/twopi
        omega=omega+inc
        idat=idat+1 
        if(idat.gt.NMAX) then
                write(*,222) idat
        stop
        endif   
!
        D = CMPLX(0.0,OMEGA)
!        
        xder(idat)=D
!        
!-----------------------------------------------------------------------
        NUM=CMPLX(1.0e+00,0.0e+00)
        if(nzero.gt.0) then
                do 400 i=1,nzero
                        NUM=NUM*(D-Z(i))
!                        write(*,*) i,num,d,z(i)
  400           continue
        endif
!	write(*,*)
        DEN=CMPLX(1.0e+00,0.0e+00)
        if(npoli.gt.0) then
                do 300 i=1,npoli
                        DEN=DEN*(D-P(i))   
!                        write(*,*)i,den,d,p(i)
  300           continue
        endif
!-----------------------------------------------------------------------
        awcal = NUM/DEN
        HRVRES = Campl*awcal
!========================================================================
        if(ifilt.eq.2.or.ifilt.eq.4) then
!
! Calculate Butterworth filter for equivalent frequencies

           if(f1.ne.0.0.and.np1.ne.0)call Bworth(f,f1,-np1,hs1)
           if(f2.ne.0.0.and.np2.ne.0)call Bworth(f,f2,np2,hs2)
           if(np1.ne.0.or.np2.ne.0)then
                        if(idat.eq.npunti/2+1)then
                        sqbsum = sqbsum+hs1*hs2*conjg(hs1)*conjg(hs2)
                        else
                        sqbsum = sqbsum+2.*hs1*hs2*conjg(hs1)*conjg(hs2)
                        endif
           endif
           if(np1.ne.0.or.np2.ne.0)then
                        a=real(hs1*hs2)
                        b=aimag(hs1*hs2)
                        c=sqrt(a*a+b*b)
                        abmax=amax1(abmax,c)
           endif
!---------------------------------------------------------------------
                hsb1(idat)=hs1
                hsb2(idat)=hs2
!---------------------------------------------------------------------
        endif

!=====================================================================      
        t(idat)=hrvres
!---------------------------------------------------------------------
!
! Get absolute value of complex number 
!
        absval(idat)=cabs(t(idat))
!       write(*,*) idat,Campl,Gampl1,t(idat),absval(idat)  
        if(absval(idat).gt.amaxa)amaxa=absval(idat)
!
! Angle is returned in radians -- convert to degrees
!
        phase(idat)=57.29578e+00*atan2(imag(t(idat)),real(t(idat)))
        freq(idat)=omega/twopi
      enddo
!
!--------------------------------------------------------
!      transfile = 'transfer.function'//labG
!      open(45, file=transfile)
!	do i=1,idat
!	 write(45,*) i*(inc/twopi),absval(i),freq(i)
!	enddo
!	close(45)  
!--------------------------------------------------------

      inc=inc/twopi
!      
      if(ifilt.eq.1) then
!
! Calculate Gaussian band-pass filter for equivalent frequencies
!
        call GAUSS(xgau,idat,inc,f2,f1,NMAX)
      endif
!
      RETURN
      END  
!========================================================================                                                                               
! SUBROUTINE GAUSS FOR THE COMPUTATION OF                                       
! A FREQUENCY-DOMAIN, GAUSSIAN ROLL-OFF FILTER. 
! THE FILTER RESPONSE, F, IS                                                               
!                                                                               
!                                                                               
!                               F(OMEGA)                                        
!                                   |                                           
!                ***************** 1+*******************                        
!
!               *                   |                   *                       
!                                   |                                           
!              *                    |                    *                      
!                                   |                                           
!                                   |                                           
!             *                     |                     *                     
!                                   |                                           
!                                   |                                           
!            *                      |                      *                    
!                                   |                                           
!                                   |                                           
!           *                       |                       *                   
!                                   |                                           
!          *                        |                        *                  
!         *                        A+                         *                 
!     *                             |                             *             
!                                   |                                           
! --------+------+------------------+------------------+------+----------- OMEGA
!     -XCUTOF -XPEAK                0                XPEAK  XCUTOF              
!                                                                               
!                                                                               
! WHERE XCUTOF IS THE CUTOFF FREQUENCY USED IN FORMING THE TIME SERIES TO BE    
! FILTERED, AND XPEAK AND A ARE THE ADDITIONAL PARAMETERS THAT SPECIFY THE      
! ROLL-OFF CHARACTERISTICS OF THE FILTER:                                       
!                 _                                                             
!                |   EXP( -Y(OMEGA+XPEAK)**2 ) , OMEGA < -XPEAK                 
!                |                                                              
!   F(OMEGA) =  <    1                         , -XPEAK < OMEGA < XPEAK         
!                |                                                              
!                |   EXP( -Y(OMEGA-XPEAK)**2 ) , OMEGA > XPEAK                  
!                 -                                                             
! WHERE                                                                         
!              Y = -LN(CUTAMP)/(XCUTOF-XPEAK)**2  .                             
!                                                                               
!-------------------------------------------------------------------------
!     Modified by Giovanni - February 1998      
!-------------------------------------------------------------------------
      SUBROUTINE GAUSS(XGAU,ICC,DELOMG,f1d,f2d,NMAX)
      DIMENSION XGAU(NMAX),XGAU1(NMAX),TEST(NMAX) 
	  real*8 f1d,f2d                                                     
      TWOPI=2.0d+00*3.141592654
      delomg=delomg*twopi
!
!
      write(*,110) ICC,DELOMG/twopi
  110 format(1x,'points, freq. step: ',i6,e11.4)    
!=====================================================================      
      do iter=1,2                                 
!      
!    iter=1 > low pass filter
!    iter=2 > high pass filter
!

       ifr=iter-1
       if(ifr.eq.0) XPEAK=f1d
       if(ifr.eq.1) XPEAK=f2d
!      
       if(ifr.eq.0) pfocf=1.1d+00
       if(ifr.eq.1) pfocf=0.9d+00
       cutamp=.1d-00
!
       XCUTOF=XPEAK*PFOCF
!
       if(ifr.eq.0) then
        write(*,120) XPEAK,XCUTOF,CUTAMP

  120   format(1x,' Low-pass  xpeak: ',f7.3,' xcutoff: ',f7.3,' cutamp: ',f7.3)
       endif   
       if(ifr.eq.1) then
        write(*,121) XPEAK,XCUTOF,CUTAMP
  121   format(1x,'High-pass  xpeak: ',f7.3,' xcutoff: ',f7.3,' cutamp: ',f7.3)
       endif   
!
       CZERO=0.0d+00
       CONE=1.0d+00
       XPEAK=XPEAK*TWOPI                                                     
       XCUTOF=XCUTOF*TWOPI
!                                                    
       if(ifr.eq.0) XGAU(1)=CONE
       if(ifr.eq.1) XGAU(1)=CZERO
!                                                                     
       Y=-LOG(CUTAMP)/(XCUTOF-XPEAK)**2 
       argmax=0.0                                        
       DO 530 I=2,ICC
!
        if(ifr.eq.0) J=I
        if(ifr.eq.1) J=ICC+2-I
!                                                                  
        OMEGA=DELOMG*(J-1)                                                          
        test(J)=omega/twopi
!           
        IF(OMEGA.GT.XPEAK.AND.IFR.EQ.0)  GO TO 50                                         
        IF(OMEGA.LT.XPEAK.AND.IFR.EQ.1)  GO TO 50 
!
        XGAU(J)=CONE                                                              
        GO TO 530                                                                 
   50   continue
        ARG=Y*(OMEGA-XPEAK)**2                                                     
        IF(ARG.LT.130.0d+00)  GO TO 51
        XGAU(J)=CZERO                                                             
        GO TO 530 
   51   continue
        if(abs(ARG).gt.argmax) argmax=abs(ARG)                                                                
        XGAU(J)=EXP(-ARG)                                                       
  530   CONTINUE
        if(iter.eq.1) then
          do i=1,icc
            XGAU1(I)=XGAU(I) 
          enddo
        endif              
!    
       ENDDO 
          do i=1,icc
            XGAU(I)=XGAU(I)*XGAU1(I)
          enddo
!
      testmax=0.0
!-------------------------------------------------------
      delomg=delomg/twopi
      RETURN                                                                    
      END
!============================================================================
        subroutine pfft(lx,cx,delta,NMAX)
!
        integer j,i,lx,m,istep,l
        complex cx(NMAX),cw,ctemp
        real signi,sc,delta,arg 
!
!       start execution
        j=1
        if(delta.lt.0)then
                signi = -1.
        else
                signi = 1.
        endif
        sc = abs(delta)
        do 630 i = 1,lx
                if (i.gt.j) goto 610
                ctemp = cx(j) * sc
                cx(j) = cx(i) * sc
                cx(i) = ctemp
 610            m = lx/2
 620            if (j.le.m) goto 630
                j = j - m
                m = m/2
                if (m.ge.1) goto 620
 630    j = j + m
        l = 1
 640  istep = 2 * l
        do 650 m = 1, l
                arg = 3.141592653*signi*(m-1)/l
                cw = cmplx(cos(arg),sin(arg))
                do 650 i = m, lx, istep
                        ctemp = cw * cx(i+l)
                        cx(i+l) = cx(i) - ctemp
 650                    cx(i) = cx(i) + ctemp
        l = istep
        if (l.lt.lx) goto 640
        return
        end
!======================================================================
        SUBROUTINE BWORTH(F,F0,NP,HS)                                    
!                                                                       
!     BWORTH CALCULATE THE RESPONSE OF A NP POLE BUTTERWORTH FILTER     
!     UP TO AS MANY POLES AS THE ARRAYS S AND T ARE DIMENSIONED         
!                                                                       
!     F:  FREQUENCY(HZ)                                                
!     F0: THE CORNER FREQUENCY OF THE FILTER                            
!     NP: THE NUMBER OF POLES, NEGATIVE FOR HIGH PASS                   
!     HS: COMPLEX RESPONSE OF THE FILTER                               
!                                                                       
!                                                                      
!                                                                      
!     THE FORMULA USED -- H(S)=1/(S-S1)(S-S2)...(S-SK)                  
!
!                         I*PI*(1/2+((2*K-1)/(2*NP)))                  
!     WHERE          SK=EXP                                             
!
!                    K=1,2,...,NP                                       
!
!                    S=I(F/F0)                                         
!                                                                       
!     REF THEORY AND APPLICATION OF DIGITAL SIGNAL PROCESSING           
!     RABINER AND GOLD PAGE 227 PRENTICE-HALL 1975                      
!                                                                       
!--------------------------------------------------------------------   
!                                                                       
      COMPLEX S(20),T(20),AS,BK,HS
	  real*8 F0
      HS=CMPLX(1.,0.0)                                                 
      N=IABS(NP)                                                       
      IF(NP.EQ.0)GO TO 6                                                
      IF(F.EQ.0.0.AND.NP.LT.0)HS=CMPLX(0.,0.)                           
      IF(F.EQ.0.0.AND.NP.LT.0)GO TO 6                                   
      DO 1 K=1,N                                                        
      AN=FLOAT(K)                                                       
      AK=3.141592654*(0.5+(((2.*AN)-1.)/(2.*FLOAT(N))))                
      BK=CMPLX(0.0,AK)                                                  
 1    S(K)=CEXP(BK)                                                    
      SS=F/F0                                                           
      AS=CMPLX(0.0,SS)                                                  
      IF(NP.LT.0)AS=1./AS                                              
      T(1)=AS-S(1)                                                      
      IF(N.EQ.1)GO TO 5                                                 
      DO 2 I=2,N                                                        
      T(I)=(AS-S(I))*T(I-1)
 100  format(2(1x,e13.6))
 2    continue
5     CONTINUE                                                          
      HS=1./T(N)                                                        
6     continue
      RETURN  
      END                                                  
!======================================================================
      SUBROUTINE xyout(npunti,x,y,npts,sta,chan,NMAX,lste)  
!======================================================================
      real x(NMAX),y(NMAX)
	character*10 sta,chan
	character*80 filexy
	character*2 lste
	
    call nchar (sta,nch)
	call nchar (chan,nch1)

	filexy =sta(1:nch)//"-"//chan(1:nch1)//lste//".xy"

	call nchar(filexy,nch)
	open(7,file=filexy(1:nch))
	 do i=1,npunti
	  write(7,*) x(i),y(i)
	 enddo
	close(7)

      write(*,100) filexy
  100 format(' File xy: ',a80)  
      RETURN
      END
!===================================================================
      SUBROUTINE RESPEK(DEL,NDATA,X1)          
!                                                                               
!     D.FAEH 8.9.93                                                             
!                                                                               
!                                                                               
!                                                                               
!  ELENCO DELLE VARIABILI PRINCIPALI                                            
!                                                                               
!  DEL    PASSO DI INTEGRAZIONE USATO DA CORRE                                  
!  DMP    VALORI DEI DAMPINGS DEGLI OSCILLATORI                                 
!  GA     ACCELERAZIONE IN INPUT                                                
!  IA     LUNGHEZZA DELL'AREA DI GA NECESSARIA ALL'INTEGRAZIONE PER             
!         TENER CONTO DELLE OSCILLAZIONI LIBERE                                 
!         NOTA : IA+NPUMAX=LENGHT(GA)                                           
!  ID     NUMERO DEI VALORI DEI DAMPINGS                                        
!  IP     NUMERO DEI VALORI DEI PERIODI DEGLI OSCILLATORI                       
!  LENGA  LUNGHEZZA DELL' AREA DI GA LENGA=NPUMAX+IA                            
!  NDATA  NUMERO DI PUNTI IN INPUT                                              
!  NPUMAX NUMERO MASSIMO DI PUNTI IN INPUT                                      
!  PD     VALORI DEI PERIODI DEGLI OSCILLATORI PER CUI E' CALCOLATA LA RISPOSTA 
!  PSSV   PSEUDO VELOCITA' DI RISPOSTA                                          
!  SA     PICCO MASSIMO DELL' ACCELERAZIONE DI RISPOSTA                         
!  SD     PICCO MASSIMO DELLO SPOSTAMENTO DI RISPOSTA                           
!  SV     PICCO MASSIMO DELLA VELOCITA' DI RISPOSTA                             
!                                                                               
!                                                                               
!      DIMENSIONI DELLE TABELLE                                                 
!                                                                               
!      DMP,SV02      : NUMERO DEI DAMPINGS                                      
!      SV,SD,SA,PSSV : NUMERO DEI DAMPINGS,NUMERO DELLE FREQUENZE A CUI SI      
!                      CALCOLANO GLI SPETTRI DI RISPOSTA                        
!      GA            : NUMERO MASSIMO DI PUNTI DELLA TIME HISTORY IN INPUT      
!                      (NPUMAX) PIU' "L' OSCILLATORE LIBERO" (IA)               
!      PD            : NUMERO DELLE FREQUENZE A CUI SI CALCOLANO GLI SPETTRI    
!                      DI RISPOSTA                                              
!                                                                               
!les      IMPLICIT REAL*8 (A-H,O-Z)
      parameter (NMAX = 100000, NMAX1=113616 )
      IMPLICIT REAL (A-H,O-Z)
	real*8 permin,permax
      COMMON/ARA/ DMP(5),SD(5,1000),SA(5,1000),GA(NMAX1),PSSV(5,1000),ID           
      COMMON/BT2/PD(1000),SV(5,1000),IP                                           
      COMMON/BT3/PD1(100),IP1
      COMMON/FRANGE/PERMIN,PERMAX,IRANGE
!                                                                               
      DIMENSION X1(NMAX)                                                       
!                                                                               
!      DEFINE THE VALID FREQUENCY RANGE                                         
!
! Modifica terremoto emilia:
!
      PERMIN1= 0.02
      PERMAX1= 3.5
!                                             	                                      	
      IF(IRANGE.EQ.1) THEN                                                      
         IP=200                                                                                                          
         DELPER=(PERMAX1-PERMIN1)/200.                                          
         DO 1 I=1,IP
           PD(I)=PERMIN1+(DELPER*(I-1))                                       
   1     CONTINUE                                                               
      ELSE                                                                      
         IP=IP1                                                                 
         DO 2 I=1,IP                                                            
         PD(I)=PD1(I)                                                           
   2     CONTINUE                                                               
      ENDIF                                                                     
!                                                                               
!     RESET THE QUANTITIES                                                      
!                                                                               
      DO 100 I=1,5                                                              
      DO 100 N=1,1000                                                            
      SA(I,N)=0.0                                                               
      SV(I,N)=0.0                                                               
      SD(I,N)=0.0                                                               
      PSSV(I,N)=0.0                                                             
  100 CONTINUE                                                                  
!                                                                               
!                                                                                                                                                                                                                                        
!      ID=5                                                                      
!      DMP(1)=0.00                                                               
!      DMP(2)=0.02                                                               
!      DMP(3)=0.05                                                               
!      DMP(4)=0.10                                                               
!      DMP(5)=0.20a
!
       ID=1
	 DMP(1)=0.05                                                               
!                                                                               
!      NPUMAX E' LA LUNGHEZZA DI GA - IL MAX IA                                 
!                                                                               
      IA=13616                                                                  
      NPUMAX=NMAX

      LENGA=NPUMAX+IA                                                           
      SF=1.0                                                                    
!                                                                               
!                                                                               
!                                                                               
      DO 10 N=1,NDATA                                                           
      GA(N)=X1(N)                                                               
  10  CONTINUE                                                                  
      DO 20 N=NDATA+1,LENGA                                                     
      GA(N)=0.0                                                                 
  20  CONTINUE                                                                  
!                                                                               
!                                                                               
!     CALCOLO DEGLI SPETTRI DI RISPOSTA                                         
!                                       	                                         
      CALL SPCTRA(NDATA,SF,DEL,LENGA)                                           
!                                                                               
!     DETERMINA I VALORI MASSIMI                                                
!                                                                               
      APER=0.0                                                                  
      VPER=0.0                                                                  
      DPER=0.0                                                                  
      SVMAX=0.                                                                  
      SDMAX=0.                                                                  
      SAMAX=0.                                                                  
      IP5=IP-5

      DO 11 I=1,IP                                                              
      IF(ABS(SA(1,I)).GT.SAMAX) THEN                                            
      SAMAX=AMAX1(SAMAX,ABS(SA(1,I)))                                           
      APER=PD(I)                                                                
      ENDIF                                                                     
      IF(ABS(SV(1,I)).GT.SVMAX) THEN                                            
      SVMAX=AMAX1(SVMAX,ABS(SV(1,I)))                                           
      VPER=PD(I)                                                                
      ENDIF                                                                     
      IF(ABS(SD(1,I)).GT.SDMAX) THEN                                            
      SDMAX=AMAX1(SDMAX,ABS(SD(1,I)))                                           
      DPER=PD(I)                                                                
      ENDIF                                                                     
   11 CONTINUE                                                                  
!                                                                               
!      FINE PROGRAMMA                                                           
!                                                                               
      RETURN
      END
 
      BLOCK DATA
!les  IMPLICIT REAL*8 (A-H,O-Z)                                                                      
      IMPLICIT REAL (A-H,O-Z)                                                                      
      COMMON/BT3/PD1(100),IP1                                                   
!                                                                               
!                                                                               
!      PERIODS FOR THE SPECTRA :                                                
!              STANDARD REVIEW PLANE 3.7.1                                      
!                                                                               
!      91 PERIODS BETWEEN 0.04 AND 15 SECONDS :                                 
!                                                                               
!       6 FROM  0.040  TO  0.050   (INC. 0.002)                                 
!      10 FROM  0.055  TO  0.100   (INC. 0.005)                                 
!      10 FROM  0.11   TO  0.20    (INC. 0.01 )                                 
!      15 FROM  0.22   TO  0.50    (INC. 0.02 )                                 
!      10 FROM  0.55   TO  1.00    (INC. 0.05 )                                 
!      10 FROM  1.10   TO  2.00    (INC. 0.10 )                                 
!      15 FROM  2.20   TO  5.00    (INC. 0.20 )                                 
!      10 FROM  5.50   TO 10.00    (INC. 0.50 )                                 
!       5 FROM 11.00   TO 15.00    (INC. 1.00 )                                 
!                                                                               
!      DATA IP1/91/                                                              
!      DATA PD1/0.040,0.042,0.044,0.046,0.048,0.050,                      ! 6    
!    *        0.055,0.060,0.065,0.070,0.075,0.080,0.085,0.090,0.095,0.1, !10    
!    *        0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19,0.20,         !10    
!    *        0.22,0.24,0.26,0.28,0.30,0.32,0.34,0.36,0.38,0.40,         !10    
!    *        0.42,0.44,0.46,0.48,0.50,                                  ! 5    
!    *        0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95,1.00,         !10    
!    *        1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0,                   !10    
!    *        2.2,2.4,2.6,2.8,3.0,3.2,3.4,3.6,3.8,4.0,4.2,4.4,4.6,4.8,5. !15    
!    *       ,5.5,6.0,6.5,7.0,7.5,8.0,8.5,9.0,9.5,10.,                   !10    
!    *        11.,12.,13.,14.,15.,9*0./                                  !14 
      DATA IP1/3/                                                              
      DATA PD1/0.3,1.0,3.0 &
     &         ,0.0,0.0,0.0,0.0,0.0,0.0,0.0	&
     &         ,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0	&
     &         ,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0	&
     &         ,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0	&
     &         ,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0	&
     &         ,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0	&
     &         ,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0	&
     &         ,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0 &
     &         ,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0	&
     &         ,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0/
   
      END                                                                       
      SUBROUTINE SPCTRA(NDATA,SF,DEL,LENGA)                                     
!                                                                               
!     THIS SUBROUTINE COMPUTES SA, SV, SD, (6.28/T)*SD SPECTRA                  
!     FOR 91 PERIODS BETWEEN 0.04 AND 15 SECONDS FOR THE INPUT ACCELERA-        
!     TION WITH EQUALLY SPACED DATA POINTS                                      
!                                                                               
!     THE OUTPUT CONSISTS OF SA, SV, SD AND (6.28/T)*SD SPECTRA.                
!                                                                               
!                                                                               
!     INPUT PARAMETERS:                                                         
!     NDATA=NO. OF DATA IN THE ACCELEROGRAM                                     
!     SF=SCALING FACTOR FOR INPUT DATA IN UNITS OF G                            
!     DEL=DT INTERVAL FOR INPUT DATA                                            
!                                                                               
!les      IMPLICIT REAL*8 (A-H,O-Z)
      parameter (NMAX = 100000, NMAX1 = 113616 )
      IMPLICIT REAL (A-H,O-Z)
      COMMON/ARA/DMP(5),SD(5,1000),SA(5,1000),GA(NMAX1),PSSV(5,1000),ID            
      COMMON/BT2/PD(1000),SV(5,1000),IP                                           
!                                                                               
!     COMPUTE SPECTRA                                                           
!                                                                               
!                                                                               
      CALL PCNO3(NDATA,DEL,SF,LENGA)                                            
!                                                                               
      DO 26 I=1,IP                                                              
      DO 26 J=1,ID                                                              
   26 PSSV(J,I)=6.2831853*SD(J,I)/PD(I)                                         
!                                                                               
      RETURN                                                                    
      END                                                                       
                                                                                                                                                                                                                                              
      SUBROUTINE PCNO3(N,DEL,SF,LENGA)                                          
!                                                                               
!les      IMPLICIT REAL*8 (A-H,O-Z)
      parameter (NMAX = 100000, NMAX1 = 113616 )
      IMPLICIT REAL (A-H,O-Z)
      INTEGER L,IA,I,J,M,K                                                      
!les      REAL*8 DW,W2,AMAX,VMAX,DMAX,X,XX,VERTL,BEB,BB7,SL,G,TY,TYD,TYDD             
!les      REAL*8 DELT,DELP,D,P,W,DTPLOT,TD,AMD,TDL                                    
!les      REAL*8 AAAA,AAA,AA,AE,BBBB,BBB,BB,BE,AMDL                                   
!les      REAL*8 A(2,2),B(2,2),SAVPT(750)                                             
      REAL DW,W2,AMAX,VMAX,DMAX,X,XX,VERTL,BEB,BB7,SL,G,TY,TYD,TYDD             
      REAL DELT,DELP,D,P,W,DTPLOT,TD,AMD,TDL                                    
      REAL AAAA,AAA,AA,AE,BBBB,BBB,BB,BE,AMDL                                   
      REAL A(2,2),B(2,2),SAVPT(750)                                             
!     DOUBLE PRECISION RTIME,TIMEP                                              
!                                                                               
      COMMON/ARA/DMP(5),SD(5,1000),SA(5,1000),GA(NMAX1),PSSV(5,1000),ID            
      COMMON/BT2/PD(1000),SV(5,1000),IP                                           
!                                                                               
      EQUIVALENCE (A(2,2),AAAA),(A(2,1),AAA),(A(1,2),AA),(A(1,1),AE)            
      EQUIVALENCE (B(2,2),BBBB),(B(2,1),BBB),(B(1,2),BB),(B(1,1),BE)            
!                                                                               
!      INIZIALIZZAZIONI                                                         
!                                                                               
      LLP=750                                                                   
      DTPLOT=0.2                                                                
!                                                                               
!      CAMBIA SCALA                                                             
!                                                                               
      DO 200 J=1,N                                                              
      GA(J)=GA(J)*SF                                                            
  200 CONTINUE                                                                  
!                                                                               
!      CICLO SUGLI SMORZAMENTI                                                  
!                                                                               
      DO 4 J=1,ID                                                               
      D=DMP(J)                                                                  
!                                                                               
!      CICLO SUI PERIODI DEGLI OSCILLATORI                                      
!                                                                               
      DO 5 K=1,IP                                                               
      P=PD(K)                                                                   
      W=6.283185/P                                                              
!                                                                               
!      CALCOLA L' INTERVALLO DI INTEGRAZIONE                                    
!                                                                               
      DELP=P/10.                                                                
      L=DEL/DELP+1.-1.E-05                                                      
      VERTL=1.0/L                                                               
      DELT=DEL*VERTL                                                            
!                                                                               
!                                                                               
!      RANGE DEI VALORI ASSUNTI DALLE VARIABILI PRINCIPALI                      
!                                                                               
!      PER DEL = .01  - PASSO DI INTEGRAZIONE -                                 
!                     0.04   =<  P   =<  15.0                                   
!                       3    >=  L   >=   1                                     
!                     0.0033 =< DELT =<   0.01                                  
!                      24    =<  IA  =<   3000                                  
!      PER DEL = .02  - PASSO DI INTEGRAZIONE -                                 
!                     0.04   =<  P   =<  15.0                                   
!                       6    >=  L   >=   1                                     
!                     0.0033 =< DELT =<   0.01                                  
!                      24    =<  IA  =<   3000                                  
!                                                                               
!      IL SIMBOLO >= SIGNIFICA MAGGIORE O UGUALE                                
!      IL SIMBOLO =< SIGNIFICA MINORE O UGUALE                                  
!                                                                               
!      NOTA DEL IA DIPENDONO SOLO DALLA FREQUENZA E                             
!      NON DAL PASSO DI CAMPIONAMENTO                                           
!                                                                               
!                                                                               
!      CALCOLA LE MATRICI A E B                                                 
!                               
                                                 
      CALL PCNO4(D,W,DELT,A,B)                                                  
!                                                                               
!      INIZIALIZZAZIONI                                                         
!                                                                               
      X=0.                                                                      
      XX=0.                                                                     
      RTIME=0.                                                                  
!                                                                               
      LP=1                                                                      
      TD=1.                                                                     
      AMD=0.                                                                    
      TIMEP=0.                                                                  
!                                                                               
      DMAX=0.                                                                   
      VMAX=0.                                                                   
      AMAX=0.                                                                   
!                                                                               
      I=1                                                                       
      DW=-2.0*W*D                                                               
      W2=-W*W                                                                   
!                                                                               
!      NOTE NEGATIVES                                                           
!                                                                               
      BEB=-(BE+BB)                                                              
      BB7=-(BBB+BBBB)                                                           
      IA=2.0*P/DELT+0.5                                                         
!                                                                               
!      CALCOLO DELLE RISPOSTE                                                   
!                                                                               
!      CICLO SUI PUNTI DI INTEGRAZIONE                                          
!                                                                               
   7  SL=(GA(I+1)-GA(I))*VERTL                                                  
      M=0                                                                       
!                                                                               
!      CICLO DI AFFINAMENTO DELL' INTEGRAZIONE                                  
!      ( FUNZIONE DEL PERIODO DELL' OSCILLATORE )                               
!                                                                               
   6  G=GA(I)+SL*M                                                              
!                                                                               
!      CALCOLA : TYDD = ACCELERAZIONE                                           
!                TYD  = VELOCITA'                                               
!                TY   = SPOSTAMENTO                                             
!                                                                               
      TY=AA*XX-SL*BB+AE*X+BEB*G                                                 
      TYD=AAAA*XX-SL*BBBB+AAA*X+BB7*G                                           
      TYDD=ABS(DW*TYD+W2*TY)                                                    
!                                                                               
!      MONITORING AND SAVING THE PEAK VALUES OF DISPLACEMENT                    
!                                                                               
!      SEI GIUNTO ALLA MAX DIMENSIONE DELLA TABELLA SAVPT

!                                                                               
      IF(LP.GT.LLP) GO TO 112                                                   
!                                                                               
!      NON C'E' L'INVERSIONE DI SEGNO DELLE VELOCITA'                           
!      => NON E' UN PICCO DI SPOSTAMENTO                                        
!                                                                               
      IF(TYD*XX.GT.0.) GO TO 112                                                
!                                                                               
      AMDL=AMD                                                                  
      TDL=TD                                                                    
      AMD=ABS(X)                                                                
      TD=RTIME                                                                  
!                                                                               
!      INTERPOLATION ONE POINT EVERY DTPLOT SECONDS                             
!      (SAVPT E' IL PICCO DI SPOSTAMENTO)                                       
!                                                                               
  111 IF (TIMEP.GE.RTIME) GO TO 112                                             
      SAVPT(LP)=AMDL+(AMD-AMDL)*(TIMEP-TDL)/(RTIME-TDL)                         
      LP=LP+1                                                                   
      TIMEP=TIMEP+DTPLOT                                                        
      IF(LP.LT.LLP) GO TO 111                                                   
!                                                                               
  112 CONTINUE                                                                  
!                                                                               
!      DETERMINA I VALORI MASSIMI E RIDEFINISCI IN                              
!       X  LO SPOSTAMENTO AL "PASSO PRECEDENTE"                                 
!       XX LA VELOCITA' AL "PASSO PRECEDENTE"                                   
!                                                                               
      DMAX=AMAX1(DMAX,ABS(TY))                                                  
      X=TY                                                                      
!                                                                               
      VMAX=AMAX1(VMAX,ABS(TYD))                                                 
      XX=TYD                                                                    
!                                                                               
      AMAX=AMAX1(AMAX,TYDD)                                                     
!                                                                               
      RTIME=RTIME+DELT                                                          
!                                                                               
!      TEST SUL CICLO DI AFFINAMENTO                                            
!      (NOTA IL RANGE DI L IN FUNZIONE DELLA FREQUENZA)                         
!                                                                               
      M=M+1                                                                     
      IF(M.LT.L) GO TO 6                                                        
!                                                                               
!                                                                               
!      TEST PER LA FINE DELL'INTEGRAZIONE                                       
!                                                                               
      I=I+1                                                                     
!                                                                               
!      TEST SUL NUMERO DI PUNTI DELL' ACCELEROGRAMMA                            
!                                                                               
      IF(I-N) 7,17,19                                                           
!                                                                               
!      TEST SUL NUMERO DI PUNTI PIU' LO SMORZAMENTO                             
!      (CONTRIBUTO OSCILLAZIONE LIBERA)                                         
!                                                                               
   19 IF(I-(N+IA)) 17,8,8                                                       
   17 CONTINUE
      IF(I.EQ.LENGA) GO TO 20                                                   
      GA(I+1)=0                                                                 
      GO TO 7                                                                   
!                                                                               
!      FINE CICLO DI INTEGRAZIONE                                               
!                                                                               
    8 CONTINUE                                                                  
!                                                                               
!       ONLY AT THE HIGHEST FREQUENCY                                           
!                                                                               
!      K = 1 : SMORZAMENTO PIU' PICCOLO                                         
!      J = 1 : PERIODO PIU' BASSO                                               
!                                                                               
      IF(K.EQ.1.AND.J.EQ.1) LLP=LP-1                                            
      IF((LP-1).LT.LLP) GO TO 17                                                
!                                                                               
!      MEMORIZZA LA RISPOSTA PER IL DATO OSCILLATORE ED IL DATO SMORZAMENTO     
!                                                                               
      SD(J,K)=DMAX                                                              
      SV(J,K)=VMAX                                                              
      SA(J,K)=AMAX

!                                                                               
!      SCARICA SUL FILE 10 I PRIMI 50 SPOSTAMENTI                               
!                                                                               
      IF(LLP.GT.50) LLP=50                                                      
      LLL=LLP/5                                                                 
      LLP=LLL*5                                                                 
!                                                                               
!      FINE DEL CICLO SUI PERIODI DEGLI OSCILLATORI                             
!                                                                               
    5 CONTINUE                                                                  
!                                                                               
!      FINE DEL CICLO SUI DAMPINGS                                              
!                                                                               
    4 CONTINUE                                                                  
!                                                                               
      RETURN                                                                    
!                                                                               
!      STOP PER TABELLE TROPPO PICCOLE                                          
!                                                                               
   20 print *, LENGA                                                       
      STOP                                                                      
!                                                                               
 1000 FORMAT(10X,'ERRORE NEL DIMENSIONAMENTO DELLE TABELLE',/ &                  
     &       ,10X,'LA TABELLA GA DEVE ESSERE PIU LUNGA DI ',I7 &                  
     &       ,' POSIZIONI')                                                   
!                                                                               
      END                                                                       
!
      SUBROUTINE PCNO4 (D,W,DELT,A,B)                                           
!                                                                               
!          SUBROUTINE FOR COMPUTATION OF MATRICES A AND B                       
!                         PCN04                                                 
!                                                                               
!les      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT REAL (A-H,O-Z)
      DIMENSION A(2,2),B(2,2)                                                   
!                                                                               
      DW=D*W                                                                    
      D2=D*D                                                                    
!                                                                               
      A0=EXP(-DW*DELT)                                                          
      A1=W*SQRT(1.-D2)                                                          
      AD1=A1*DELT                                                               
      A2=SIN(AD1)                                                               
      A3=COS(AD1)                                                               
      A7=1.0/(W*W)                                                              
      A4=(2.0*D2-1.0)*A7                                                        
      A5=D/W                                                                    
      A6=2.0*A5*A7                                                              
      A8=1.0/A1                                                                 
      A9=-(A1*A2+DW*A3)*A0                                                      
      A10=(A3-DW*A2*A8)*A0                                                      
      A11=A2*A8                                                                 
      A12=A11*A0                                                                
      A13=A0*A3                                                                 
      A14=A10*A4                                                                
      A15=A12*A4                                                                
      A16=A6*A13                                                                
      A17=A9*A6                                                                 
!                                                                               
      A(1,1)=A0*(DW*A11+A3)                                                     
      A(1,2)=A12                                                                
      A(2,1)=A10*DW+A9                                                          
      A(2,2)=A10                                                                
!                                                                               
      DINV=1.0/DELT                                                             
      B(1,1)=(-A15-A16+A6)*DINV-A12*A5-A7*A13                                   
      B(1,2)=(A15+A16-A6)*DINV+A7                                               
      B(2,1)=(-A14-A17-A7)*DINV-A10*A5-A9*A7                                    
      B(2,2)=(A14+A17+A7)*DINV                                                  
!                                                                               
      RETURN                                                                    
      END
!======================================================================
      SUBROUTINE ENERGY(z1,dt,nstop,w,dura,te)
      parameter (NMAX = 100000)
!
 	DIMENSION Z(NMAX),Z1(NMAX)
	W=0.0D+00
        dura = 0.0
	i5start=0
	i5stop=0
!
 	nstart=1
	wpcent0=5.
        wpcent1=95.
! compute amaxa
	amaxa=0.0D+00
	amina=0.0D+00
        icount = 0
! 	write(*,*)'Subroutine ENERGY: nstart,nstop',nstart,nstop
	do i=nstart,nstop
!------------------------------------------
	  z(i)=z1(i)*1.0e-07
!------------------------------------------
	  if(abs(z(i)).GT.amaxa) amaxa=abs(z(i))
	enddo
!--------------------------------------------
! compute total Energy
!
        zz=0.0
        zz1=0.0
        W=0.0
        DO I=nstart,nstop
          zz=z(i)
          zz1=z(i+1)
          W=W+DT*(ZZ*ZZ+ZZ1*ZZ1)/2.0D+00
          if(sign(1.0,z(i)).ne.sign(1.0,z(i+1))) icount =icount+1
        ENDDO
        write(*,*) "     Total energy: ",W," nstart nstop:, ",nstart,nstop
        if(w.lt.1.0e-09) write(*,*) "WARNING!!!! subroutine ENERGY: ",i5start,test,amaxa,wpcent,nstart,nstop
!
        test=amaxa*wpcent0/100.d+00
        test0=W*wpcent0/100.d+00
        test1=W*wpcent1/100.d+00
        write(*,*) "     ",wpcent0,'% energy:',test0,wpcent1,"% energy:",test1
!------------------------------------------
! i5start is the first sample for which the amplitude is gt 5% of the peak
! i5stop  is the last  sample for which the amplitude is gt 5% of the peak
!
	do i=nstart,nstop
	  if(abs(z(i)).gt.test) then
	    i5start=i
	    go to 1
          endif
        enddo
 1      continue
        j=nstop+1
	do i=nstart,nstop
          j=j-1
	  if(abs(z(j)).gt.test) then
            i5stop=j
            go to 2
	  endif
        enddo
 2      continue
	if(i5stop.ge.nstop) i5stop=nstop-1
        write(*,*) "i5start i5stop:, ",i5start,i5stop
!--------------------------------------------
! compute Energy
!
        zz=0.0
        zz1=0.0
        W=0.0
        Wtmp=0.0
        i6start=0
        i6stop=0
	DO I=nstart,nstop
	  zz=z(i)
	  zz1=z(i+1)
	  Wtmp=Wtmp+DT*(ZZ*ZZ+ZZ1*ZZ1)/2.0D+00
          if(Wtmp.ge.test0.and.Wtmp.le.test1) then
            if(i6start.eq.0) i6start=i
            W=W+DT*(ZZ*ZZ+ZZ1*ZZ1)/2.0D+00
            if(sign(1.0,z(i)).ne.sign(1.0,z(i+1))) icount =icount+1
          else
            if(i6stop.eq.0.and.Wtmp.gt.test1) i6stop=i
          endif  
        ENDDO
        write(*,*) "      Duration energy: ",W," i6start i6stop:, ",i6start,i6stop
!-------------------------------------------------
! compute duration
!
        dura1 = (i5stop - i5start) * DT
        dura = (i6stop - i6start) * DT
        write(*,*) dura,dura1
!------------------------------------------------------------------------
! compute intensity of zero crossing

        te = float(icount)/dura
!
        write(*,100) w,dura, icount, te
  100   format(/,1x,'     Energy:',e9.4,' duration:',e9.4,' icount:',i8,' Te:',e9.4)
        RETURN
        END
!----------------------------------------------------------------------
      SUBROUTINE DISTAN(ELAT,ELON,SLAT,SLON,DELTA,DIST,Z)
!
!   CALCOLO DISTANZA STAZIONE-EPICENTRO NOTE LE COORDINATE
!   SLAT=LAT.STAZIONE     ELAT=LAT.EPICENTRO
!   SLON=LON.STAZIONE     ELON=LON.EPICENTRO
!   CALCOLO AZIMUT STAZIONE-EPICENTRO ANGOLO DA NORD VERSO EST
!
!
!   VEDI IN : K.E. BULLEN 'AN INTRODUCTION TO THE THEORY OF SEISMOLGY'
!
!   PAG 151: 'CALCULATION OF THE EPICENTRAL DISTANCE.....'
!
!      ESEMPIO  DI IMPUT TAPE5 :
!
! 2
!00.000  000.000      <<<-  LATITUDINE , LONGITUDINE EPICENTRO
!00.000  045.000      <<<-  LATITUDINE , LONGITUDINE STAZIONE
!45.000  000.000      <<<-  LATITUDINE , LONGITUDINE STAZIONE
!00.000  000.000      <<<-  LATITUDINE , LONGITUDINE STAZIONE
!
!
!
!
      real*8 ELAT,ELON,SLAT,SLON,DELTA,Z
      real dist
      REAL H1,H2
      SLA=SLAT
      SLO=SLON
!---------------------------------------------
      R1=6378.137
      R2=6356.75
      PI=3.141592654
      RD=5.729577951E+01
      FL=1./297.
      ECC=2.*FL-FL**2.
!--- CONVERSIONE GRADI-RADIANTI  -------------
      SLA=SLA/RD
      SLO=SLO/RD
      ELAT=ELAT/RD
      ELON=ELON/RD
!-----------------------------------------------------------
!
!     CORREZIONE LATITUDINE PER UNA TERRA ELLISSOIDICA
!
!     PER L'EPICENTRO :
!
      ELAT=(1.-ECC)*TAN(ELAT)                                           
      ELAT=ATAN(ELAT)                                                  
!
!
!
!    PER LA STAZIONE :
!
      SLA=(1.-ECC)*TAN(SLA)                                             
      SLA=ATAN(SLA)                                                     
!
!
!------------------------------------------------------------
!
      A1=COS(ELAT)*COS(ELON)
      B1=COS(ELAT)*SIN(ELON)
      C1=SIN(ELAT)
      D1=SIN(ELON)
      E1=-COS(ELON)
      F1=-COS(ELAT)
      G1=SIN(ELAT)*COS(ELON)
      H1=SIN(ELAT)*SIN(ELON)
      A2=COS(SLA)*COS(SLO)
      B2=COS(SLA)*SIN(SLO)
      C2=SIN(SLA)
      D2=SIN(SLO)
      E2=-COS(SLO)
      F2=-COS(SLA)
      G2=SIN(SLA)*COS(SLO)
      H2=SIN(SLA)*SIN(SLO)
      DS=A1*A2+B1*B2+C1*C2
      DELTA=ACOS(DS)
      Z=(A2-G1)**2+(B2-H1)**2+(C2-F1)**2
      if(abs(delta).lt.1.0e-07) delta=1.0e-07
      TMP=(Z-2)/(2.*SIN(DELTA))
      if(tmp.gt.1.0) tmp=1.0
      if(tmp.lt.-1.0) tmp=-1.0
      Z=ACOS(TMP)*RD
!---------- CALCOLO RAGGIO MEDIO STAZIONE-EPICENTRO --------
      ALAT=(ELAT+SLA)/2.
      RAGMED=R1*COS(ALAT)**2+R2*SIN(ALAT)**2
      AR=RAGMED*PI/180.
!-----------------------------------------------------------
      DELTA=DELTA*RD
      DIST=DELTA*AR
!-----------------------------------------------------------
!write(6,*) 'SUB DISTAN: ',ELAT,ELON,SLAT,SLON,DELTA,DIST,Z
      RETURN
      END
!
!----------------------------------------------------------------------
      SUBROUTINE SMOOTH(dati,dati1,npts,spts)
      parameter (NMAX = 100000)
!
      DIMENSION dati(NMAX),dati1(NMAX)
      integer npts, spts, sptsd2, fpts

      sptsd2 = spts/2

      dati1 = 0.0
      do i=2,npts
        fpts=0
        do ii=2,spts
          iii=i+(ii-sptsd2)-1
          if(iii.ge.1.and.iii.le.npts) then
            dati1(i) = dati1(i)+dati(iii)
            fpts=fpts+1
          endif 
        enddo
        dati1(i) = dati1(i)/fpts
      enddo
      return
      end
!----------------------------------------------------------------------
      SUBROUTINE F1F2(rappsr,freq,npts,fmin,fmax,tnoise)
      parameter (NMAX = 100000)
      real*8 tnoise
      real freq(NMAX),rappsr(NMAX),fmin,fmax,freqmin
      integer sup,inf

!
! F min
!

 freqmin=1./tnoise
 fmin=freqmin

 do i=2,npts
	iflag=1
	if(freq(i).gt.freqmin) then
     do j=i,i+50
	   if(j.lt.npts) then
	    if (rappsr(j).lt.2.8) iflag=0
	   endif
	 enddo
			
	 if (iflag.eq.1) then
		inf=i 
	    fmin=freq(i)
		goto 100
	 endif
   endif
  enddo	

100 continue	

   if (iflag.eq.0) then
     print *,'no freqmin found'
   endif

!----------------------------------------------------------
! Fmax
!

 do l=npts,2,-1
 
  iflag1=1
	
	do m=l-9,l
	  if(m.gt.0) then
	    if (rappsr(m).lt.5)	iflag1=0
	  endif
	enddo
			
	if (iflag1.eq.1) then			
	  sup=l 
	  fmax=freq(l)
	  goto 101
	endif
		
 enddo	

101 continue	
	if (iflag.eq.0) then
	  print*,'no freqmax found'		
	endif
!-----------------------------------------------------------
return
end
!
!
!----------------------------------------------------------------------
  SUBROUTINE rotasig(dataE,dataN,dataT,npts,azR)
  parameter (NMAX = 100000)
  real dataN(NMAX),dataE(NMAX),dataT(NMAX)
  real *8 azR
  integer npts

    do i=1,npts
      dataT(i)=(dataN(i)*cos(azR))-(dataE(i)*sin(azR))        
    enddo
	 
  return
  end 
!
!
!----------------------------------------------------------------------
SUBROUTINE Mw_core(dbremw,sismoEc,sismoNc,t,npts,nptsW,inc,azR,arrSo,dista,Q,finf,fsup,segtype,samprate,sta,verb)
  parameter (NMAX = 100000)
  real sismoN(NMAX),sismoE(NMAX),sismoT(NMAX),spet(NMAX),spet1(NMAX),sismoNc(NMAX),sismoEc(NMAX)
  complex spetD(NMAX),spetV(NMAX)
  real spetrD(NMAX),spetrV(NMAX),spetrDq(NMAX),spetrVq(NMAX),spetrDqd(NMAX),spetrVqd(NMAX)
  real inc,finf,fsup,Q,dista
  complex spetT(NMAX),t(NMAX),xder(NMAX)
  real *8 azR,samprate,arrSo
  real *8 qual
  integer evid, orid,commid
  integer npts,nptsW,verb
!  integer NMAX,e,i,ii,nch,npts2,nptse ,test,test1,inf,sup
  integer e,i,ii,nch,npts2,nptse ,test,test1,inf,sup
  real pi,freq1
  real *8 arrP,arrS,lddate
  real *8 omega,SD2,SV2,stressdrop,rho,v,k
  real V2(NMAX),S2(NMAX)
  real*8 staMw(100),staM0(100),staeqR(100),staf0(100)
  real*8 Mw,M0,f0
  real*8 rho_pf,v_pf,k_pf,Q_f0,Q_f1,Q_f2
  character*1 segtype
  character*4 Ptype,Stype
  character*8 chamw,chan
  character*7 fildeb
  character*6 sta
  character*15 auth
  character*20 filename
  character*182 dbremw
  COMMON/DERI/xder
  COMMON/MWCOM/orid,evid,lddate,arrP,arrS,Ptype,Stype,auth,chan
  COMMON/MWCOM01/staMw,staM0,staeqR,staf0,nmwdat
  COMMON/MwCOM02/rho_pf,v_pf,k_pf,Q_f0,Q_f1,Q_f2
   
  pi=3.141592653589793
  chamw=chan(1:2)//'T'
  commid = -1
  qual = -999.0

  nmwdat = nmwdat + 1
  !if(fsup.gt.20.0) fsup = 20.0

!------------------------------------------------------------------------

  
 sismoE=0.0
 sismoN=0.0
 sismoT=0.0

 do i=1,nptsW
   sismoE(i)=sismoEc(i)*1.0E-09
   sismoN(i)=sismoNc(i)*1.0E-09
 enddo

 if(verb.ge.1) write(6,*) '     Signal-noise rotation, azimuth = ',azR

      call rotasig(sismoE,sismoN,sismoT,nptsW,azR)

 if(verb.eq.3) then
   call nchar(sta,nch)
   filename=sta(1:nch)//'MWindow.dat'
   open(57,file=filename)
   filename=sta(1:nch)//'MWspettri.dat'
   open(56,file=filename)

   x=0.0
   do i=1,nptsW
     x=x+(1.0/samprate)
     write(57,*) x,sismoE(i),sismoN(i),sismoT(i)
   enddo

   close (57)
 endif

 spet=0.0
 spet1=0.0
 spetrD=0.0
 spetrV=0.0

 spetT=cmplx(0.0,0.0)
 spetD=cmplx(0.0,0.0)
 spetV=cmplx(0.0,0.0)
 spetT(1)=cmplx(0.0,0.0)
 spetD(1)=cmplx(0.0,0.0)
 spetV(1)=cmplx(0.0,0.0)
!---------------------------------------------------------------------------
!   D2
!	
!print *,'Entro in D2 2',nptsW
 call d2(sismoT,nptsW,20,NMAX)

!---------------------------------------------------------------------------
 do i=2,npts
   spetT(i)=cmplx(sismoT(i),0.0)
   x=x+(1.0/samprate)
 enddo

!---------------------------------------------------------------------------
!   Compute FFT
!
 call pfft(npts,spetT,-1.,NMAX)

 freq1=0.0
 npts2 = npts/2
!----------------------------------------------------------
!  Deconvolve response 
!

 do i=2,npts2+1
   freq1=freq1+inc
   spetT(i) = (1.0/samprate)*(spetT(i)/t(i))

!
! Input in Acceleration
!

   if(segtype.eq.'A') then
     spetV(i) = spetT(i)/xder(i)
     spetD(i) = spetV(i)/xder(i)
   endif

!
! Input in Velocity
!

   if(segtype.eq.'V') then
     spetV(i) = spetT(i)
     spetD(i) = spetV(i)/xder(i)                              
   endif
!
! Input in Displacement
!
   if(segtype.eq.'D') then
     spetD(i) = spetT(i)
     spetV(i) = spetD(i)*xder(i)
   endif
   spetrD(i)=abs(spetD(i))
   spetrV(i)=abs(spetV(i))
   
 enddo 

!
!
!--------------------------------------------------------------
 call SMOOTH(spetrD,spet,npts2+1,3)
 call SMOOTH(spetrV,spet1,npts2+1,3)

 freq1=0.0

 do i=2,npts2+1
   spetrD(i)=spet(i)
   spetrV(i)=spet1(i)
   freq1=freq1+inc

!
!  Q correction  
!
!  Q=400.*(freq1/1.0)**0.55
   Q=80.*(freq1/1.0)**1.1
         

   spetrDq(i)= spetrD(i)*exp((pi*arrSo*freq1)/Q)
   spetrVq(i)= spetrV(i)*exp((pi*arrSo*freq1)/Q)
!
!  Distance correction 
!
   spetrDqd(i)=spetrDq(i)*dista*1000.0
   spetrVqd(i)=spetrVq(i)*dista*1000.0

   if(verb.eq.3) write(56,*) freq1,abs(spetT(i)),spetrD(i),spetrV(i),spetrDq(i),spetrVq(i),spetrDqd(i),spetrVqd(i)
!  if(verb.eq.3) write(56,*) spetrDqd(i),spetrVqd(i)

 enddo

 if(verb.eq.3) close(56)
!------------------------------------------------------------
! Andrews - Antonella 2008
!------------------------------------------------------------
 rho = rho_pf
 v = v_pf   
 k = k_pf
!------------------------------------------------------------	  
 freq1=0.0
 SD2= 0.0
 SV2= 0.0

 test=0
 test1=0

 e = 0
 do ii=2,npts2+1
   freq1=freq1+inc
   if(freq1.ge.finf.and.freq1.le.fsup) then
     e=e+1
     if(test.eq.0) then
       inf=ii
       test=1
     endif
     S2(e)=spetrDqd(ii)**2
     V2(e)=spetrVqd(ii)**2
     sup=ii
   endif
 enddo

 nptse=e

 do i=1,nptse-1
   SD2=SD2+((S2(i)+S2(i+1))*inc/2.)
 enddo

 SD2=2.*SD2

 do i=1,nptse-1
   SV2=SV2+((V2(i)+V2(i+1))*inc/2.)
 enddo

 SV2=2.*SV2
 omega=sqrt(4.*(SD2**(3./2.))*(SV2**(-1./2.)))
 f0=(1/(2.*pi))*sqrt(SV2/SD2)
 M0=omega*4*pi*k_pf*rho_pf*(v_pf**3.)
 Mw=(2./3.)*log10(M0)-6.1

! Madariaga

 stressdrop=7./16.*(2.*pi/2.34/v)**3.*M0*f0**3.
 eqray= 2.34*v/(2.0*pi*f0*1000.)
 if(verb.ge.1) write(6,100) Mw,M0,f0,stressdrop
   100  format(6x,'Mw = ',f5.2,' M0 = ',e13.6,' f0 = ',f7.2,' Stressdrop = ',e13.6)
!--------------------------------------------------------------------------------
! add row to database 
!
  dbremw=''
  write(dbremw,110) sta,chamw,orid,evid,Mw,M0,f0,eqray,dista,azR,qual, &
    & arrP,Ptype,arrS,Stype,segtype,auth,commid,lddate
 110  format(a6,1x,a8,1x,i8,1x,i8,1x,f7.2,1x,e9.3,5(1x,f7.2),2(1x,f17.5,1x,a4),1x,a1,1x,a15,1x,i8,1x,f17.5)
!--------------------------------------------------------------------------------  

 staMw(nmwdat)=Mw
 staM0(nmwdat)=M0
 staeqR(nmwdat)=eqray
 staf0(nmwdat)=f0

!--------------------------------------------------------------------------------
  return
  end
