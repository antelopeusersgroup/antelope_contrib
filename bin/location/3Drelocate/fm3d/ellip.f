c	ELLIPTICITY CORRECTIONS - Polynomial interpolation
c
      subroutine ellip()
      real sc0
      real sc1
      real sc2
c SQUARE ROOT OF 3. TIMES .5
      real s3
c EPICENTAL CO-LATITUDE - RADIANS
      real ecolat
c EPICENTRAL DISTANCE - RADIANS
      real edist
c AZIMUTH FROM EPICENTER TO RECEIVER - RADIANS
      real az
c EPICENTRAL DEPTH (km)
      real edepth
c ELLIPTICITY CORRECTION -OUTPUT
      real tcor
      character*(*) phase
c TAU's
      real t0, t1, t2
c	CONSTANTS FOR POLYNOMIAL
      real t0con(8,10), t1con(8,10), t2con(8,10)
      integer ii, j
      real adepth
      data (t0con(1,j),j=1,10) /-0.01711,-1.7791,0.,0.,0.,-0.9630,-13.
     &2326,13.7390,0.,0./
      data (t0con(2,j),j=1,10) /-0.08291,-2.1455,2.4538,-0.7907,0.,2.
     &0258,-12.9357,2.1287,5.2668,-0.9229/
      data (t0con(3,j),j=1,10) /-1.5022,-0.0943,1.9655,-1.1661,.1393,3.
     &4920,-9.9051,-0.3875,5.3581,-0.0686/
      data (t0con(4,j),j=1,10) /2.9971,-2.9549,0.4082,0.,0.,28.1650,9.
     &2160,-17.9030,-5.2995,3.2029/
      data (t0con(5,j),j=1,10) /3.6775,-2.2221,0.,0.,0.,-1.3127,-6.2476,
     &1.6684,0.,0./
      data (t0con(6,j),j=1,10) /-10.6238,15.4993,-7.4840,1.0673,0.,3.
     &2763,-6.4596,-0.4923,0.,0./
      data (t0con(7,j),j=1,10) /-0.01332,-3.2777,-1.2243,7.5246,0.,-3.
     &4856,-10.3187,43.4834,-70.5341,-50.2287/
      data (t0con(8,j),j=1,10) /-0.07859,-4.0924,4.6116, -1.4760,0.,2.
     &9104,-17.8661, 4.6262,7.1486,-1.9154/
      data (t1con(1,j),j=1,10) /.0040,-0.7841,6.0441,-17.5535,0.,-0.
     &2549,2.0519,-19.0605,-37.8235,54.5110/
      data (t1con(2,j),j=1,10) /-.0048, .0839,-2.2705,2.4137,-0.5957,-2.
     &4241,-4.2792,1.9728,3.5644,-0.5285/
      data (t1con(3,j),j=1,10) /.0033,-1.3485,0.1735,1.1583,-0.4162,-0.
     &1096,0.2576,-0.5978,0.1888,0.1600/
      data (t1con(4,j),j=1,10) /2.6249,-.0025,-0.2086,-0.0184,0.,-1.
     &5077,0.9904,0.3513,0.,0./
      data (t1con(5,j),j=1,10) /3.4213,-0.9359,0.,0.,0.,0.,0.,0.,0.,0./
      data (t1con(6,j),j=1,10) /-8.0633,8.0238,-1.7407,0.,0.,0.,0.,0.,0.
     &,0./
      data (t1con(7,j),j=1,10) /0.0109,-1.2300,8.9145,-27.5847,0.,-0.
     &6951,5.6201,-33.0908,-83.8233,102.4333/
      data (t1con(8,j),j=1,10) /-0.0311,0.1896,-4.0694,4.2599,-1.0387,-
     &3.9368,-8.4379,2.6814,6.9535,-0.6086/
      data (t2con(1,j),j=1,10) /0.0107,0.0275,-0.6912,0.0347,0.1157,-0.
     &1836,0.,0.0296,0.,0./
      data (t2con(2,j),j=1,10) /0.0107,0.0275,-0.6912,0.0347,0.1157,-0.
     &1836,0.,0.0296,0.,0./
      data (t2con(3,j),j=1,10) /0.0005,-0.01231,-1.0156,0.4396,0.,0.,0.,
     &0.,0.,0./
      data (t2con(4,j),j=1,10) /-3.5838,2.1474,-0.3548,0.,0.,-1.3369,-5.
     &4889,0.6809,1.5096,-0.0763/
      data (t2con(5,j),j=1,10) /-2.9912,1.0313,0.,0.,0.,0.,0.,0.,0.,0./
      data (t2con(6,j),j=1,10) /3.2814,-7.1224,3.5418,-0.5115,0.,0.,0.,
     &0.,0.,0./
      data (t2con(7,j),j=1,10) /0.00025,0.1685,-2.2435,3.3433,0.,-0.
     &0503,0.5353,1.5362,-14.3118,-3.2938/
      data (t2con(8,j),j=1,10) /0.0843,-0.2917,-0.6767,-0.2934,0.2779,-
     &0.4336,0.0306,0.07113,0.,0./
      data s3 /.8660254/
      save sc0, sc1, sc2
c
c	INITIAL CALL TO SET UP CONSTANTS
      entry ellref(ecolat)
c
      sc0 = .25*(1.+3.*cos(2.*ecolat))
      sc1 = s3 * sin(2.*ecolat)
      sc2 = s3 * sin(ecolat)*sin(ecolat)
      return
c
c	CALLED ONCE FOR EACH STATION - RETURNS ELLIPTICITY CORRECTION IN tcor
      entry ellcor(edist, azim, edepth, phase,tcor)
      adepth = edepth/6371.
c DETERMINE INDEX FOR POLYNOMIAL
      if(.not.(phase .eq. 'P'))goto 23000
         if(.not.(edist .lt. (15.*(3.14159265/180.))))goto 23002
            ii = 1
            goto 23003
c        else
23002       continue
            ii = 2
23003    continue
         goto 23001
c     else
23000    continue
         if(.not.(phase .eq. 'PcP'))goto 23004
            ii = 3
            goto 23005
c        else
23004       continue
            if(.not.(phase .eq. 'PKPab'))goto 23006
               ii = 4
               goto 23007
c           else
23006          continue
               if(.not.(phase .eq. 'PKPbc'))goto 23008
                  ii = 5
                  goto 23009
c              else
23008             continue
                  if(.not.(phase .eq. 'PKIKP'))goto 23010
                     ii = 6
                     goto 23011
c                 else
23010                continue
                     if(.not.(phase .eq. 'S'))goto 23012
                        if(.not.(edist .lt. (15.*(3.14159265/180.))))
     &                    goto 23014
                           ii = 7
                           goto 23015
c                       else
23014                      continue
                           ii = 8
23015                   continue
                        goto 23013
c                    else
23012                   continue
                        tcor = 0.
                        return
23013                continue
23011             continue
23009          continue
23007       continue
23005    continue
23001 continue
c	COMPUTE TAU's
c      write(6,*) ii
      t0 = t0con(ii,1) + edist*(t0con(ii,2) + edist*(t0con(ii,3) + 
     &edist*(t0con(ii,4) + edist*t0con(ii,5)))) +adepth*(t0con(ii,6) + 
     &adepth*t0con(ii,7)) + adepth*edist*(t0con(ii,8) + t0con(ii,9)*
     &adepth + t0con(ii,10)*edist)
      t1 = t1con(ii,1) + edist*(t1con(ii,2) + edist*(t1con(ii,3) + 
     &edist*(t1con(ii,4) + edist*t1con(ii,5)))) +adepth*(t1con(ii,6) + 
     &adepth*t1con(ii,7)) + adepth*edist*(t1con(ii,8) + t1con(ii,9)*
     &adepth + t1con(ii,10)*edist)
      t2 = t2con(ii,1) + edist*(t2con(ii,2) + edist*(t2con(ii,3) + 
     &edist*(t2con(ii,4) + edist*t2con(ii,5)))) +adepth*(t2con(ii,6) + 
     &adepth*t2con(ii,7)) + adepth*edist*(t2con(ii,8) + t2con(ii,9)*
     &adepth + t2con(ii,10)*edist)
      tcor = sc0 * t0 + sc1 * cos(azim) * t1 + sc2 * cos(2.*azim) * t2
      return
      end


C ELLIPTICITY CORRECTIONS FOR AK135 MODEL (full set of phases)
C
       SUBROUTINE kellip()
C==========================================================================
C                                                                         
C    Ellipticity correction for any given phase using
C    Dziewonski & Gilbert representation
C                                                   
C      The ellipticity corrections are found by linear interpolation       
C    in terms of values calculated for the ak135 model for a wide 
C    range of phases to match the output of the iasp software 
C
C     first call:  ellref(ecolat) 
C                        - to set up source dependent constants
C     2nd call  :  ellcor(phase,edist,depth,ecolat,azim,tcor,abrt) 
C                        - to calculate correction for a station                                                                                     C                                                                         
C    Parameters: 
C    character  
C          phase : a  string specifying the PHASE,   -e.g P, ScP etc.  
C                                                        
C    real 
C          edist  :  epicentral distance to station (in degrees)     
C          edepth :  depth of event         
C          ecolat :  epicentral co-latitude of source (in radians) 
C          azim   :  azimuth from source to station (in radians)
C                                
C          tcor   :  time correction for path to allow for ellipticity
C 
C    logical 
C          abrt   :  a logical variable -usally set to .FALSE.  
C                    which is set to .TRUE. if a phase for      
C                    which no data is available is chosen       
C                                                                         
C==========================================================================
C   B.L.N. Kennett RSES,ANU        May 1995, August 1996                 
C   (based on earlier routine by D.J. Brown)
C   with input from W. Spakman, Utrecht
C=========================================================================
      character *(*) phase
      character*8 phcod(57)
      integer phind(57),phspn(57),phnch(57)
      real edist,edepth,ecolat,azim,
     ^     sc0,sc1,sc2,s3,tcor,
     ^     tau0, a0,b0,h0,d0,e0,f0,g0,
     ^     tau1, a1,b1,h1,d1,e1,f1,g1,
     ^     tau2, a2,b2,h2,d2,e2,f2,g2
      real dpth(6),delta(50)
      real t0(50,6),t1(50,6),t2(50,6)
      integer Ne,Nd
      logical abrt
      data phcod/
     & "Pup   ","P     ","Pdiff ","PKPab ","PKPbc ","PKPdf ",
     & "PKiKP ","pP    ","pPKPab","pPKPbc","pPKPdf","pPKiKP",
     & "sP    ","sPKPab","sPKPbc","sPKPdf","sPKiKP","PcP   ",
     & "ScP   ","SKPab ","SKPbc ","SKPdf ","SKiKP ","PKKPab",
     & "PKKPbc","PKKPdf","SKKPab","SKKPbc","SKKPdf","PP    ",
     & "P'P'  ","Sup   ","S     ","Sdiff ","SKSac ","SKSdf ",
     & "pS    ","pSKSac","pSKSdf","sS    ","sSKSac","sSKSdf",
     & "ScS   ","PcS   ","PKSab ","PKSbc ","PKSdf ","PKKSab",
     & "PKKSbc","PKKSdf","SKKSac","SKKSdf","SS    ","S'S'  ",
     & "SP    ","PS    ","PnS   "/
      data phind/
     &        1,      14,      91,     136,     165,     178,
     &      235,     364,     433,     462,     475,     532,
     &      661,     742,     771,     784,     841,     970,
     &     1047,    1100,    1113,    1134,    1195,    1316,
     &     1337,    1382,    1507,    1516,    1573,    1702,
     &     1827,    1932,    1945,    2022,    2067,    2132,
     &     2197,    2234,    2295,    2356,    2425,    2490,
     &     2551,    2628,    2681,    2694,    2711,    2772,
     &     2781,    2838,    2967,    3140,    3273,    3398,
     &     3587,    3656,    3697/
      data phspn/
     &        3,      19,      11,       7,       3,      14,
     &       32,      17,       7,       3,      14,      32,
     &       20,       7,       3,      14,      32,      19,
     &       13,       3,       5,      15,      30,       5,
     &       11,      31,       2,      14,      32,      31,
     &       26,       3,      19,      11,      16,      16,
     &        9,      15,      15,      17,      16,      15,
     &       19,      13,       3,       4,      15,       2,
     &       14,      32,      43,      33,      31,      47,
     &       17,      10,       6/ 
      data phnch/
     &        3,       1,       5,       5,       5,       5,
     &        5,       2,       6,       6,       6,       6,
     &        2,       6,       6,       6,       6,       3,
     &        3,       5,       5,       5,       5,       6,
     &        6,       6,       6,       6,       6,       2,
     &        4,       3,       1,       5,       5,       5,
     &        2,       6,       6,       2,       6,       6,
     &        3,       3,       5,       5,       5,       6,
     &        6,       6,       6,       6,       2,       4,
     &        2,       2,       3/ 
      data dpth/ 0.0, 100.0, 200.0, 300.0, 500.0, 700.0 /
      save sc0,sc1,sc2
c...
c     In addition to the phase names listed above a number of phase aliases
c     are available in the routine phase_alias, e.g. Pn --> P etc
c     The input phase code is first checked against the phcod array
c     and next against the phase aliases.
c<sc>
c	                     initial call to set up source dependent constants
      entry kellref(ecolat)
c                                            
      s3 = sqrt(3.0)/2.0
      sc0 = 0.25*(1.0+3.0*cos(2.0*ecolat))
      sc1 = s3*sin(2.0*ecolat)
      sc2 = s3*sin(ecolat)*sin(ecolat)
      return
c<sc>
c<ec>                                           phase identification
      entry kellcor(phase,edist,edepth,ecolat,azim,tcor,abrt)
*      write(6,*) "phase,edist,edepth,ecolat,azim"
*      write(6,*)  phase,edist,edepth,ecolat,azim
      Nd = 6
      NUMPH = 57
      deldst = 5.0
      abrt = .FALSE.
c                                             check on the length of phase
      l=len(phase)
      if(l.lt.8) then
       stop 
     >    'character variable `phase` should have at least length 8'
      endif

c                                             select phase
      ip = -1
      nc=min(lnblk(phase),8)
      do 10 i=1,NUMPH
        if(nc.ne.phnch(i)) goto 10
        if (phase(1:nc) .eq. phcod(i)(1:nc)) then
          ip = i
          go to 11
        endif
 10   continue
 11   continue

      if(ip.eq.-1) then
c                                             check phase aliases
        call phase_alias(phase,edist,ip)
      endif
      Ne = phspn(ip)
*      write(6,*) "ip:",ip
c                                              phase not found
      if(ip.lt.0) then
        write(6,*) phase,'  is not available'
        abrt = .true.
        return
      endif
c                                               special case of upgoing waves
*
c
c                                                acquire phase information
       nr = phind(ip)
*       write(6,*) "nrec:",nr
       read(21,61,rec=nr) phcod(ip),np,d1,d2
*       write(6,*) "phcode,np,d1,d2: ", phcod(ip),np,d1,d2
       nr = nr+1
       if(np.ne.Ne) write(6,*) "HELP! - index wrong"
       do 15 i=1,np
         read(21,62,rec=nr) delta(i)
         nr = nr+1
         read(21,63,rec=nr) (t0(i,m),m=1,6)
         nr = nr+1
         read(21,63,rec=nr) (t1(i,m),m=1,6)
         nr = nr+1
         read(21,63,rec=nr) (t2(i,m),m=1,6)
         nr = nr+1
 15    continue         
 61    format(a8,i10,2f10.0)
 62    format(f10.0)
 63    format(6f10.4)
c                                  distance index
       idist = 1 + int( (edist-d1)/ deldst )
       if(edist.lt.d1) idist =1
       if(edist.gt.d2) idist= np-1
c                                  depth index
       do 25 j = 1,Nd-1
         if ((dpth(j).le.edepth).and.(dpth(j+1).ge.edepth))then
            jdepth = j
            goto 26
         endif
 25    continue
 26    continue
*       write(6,*) "idist, jdepth;",idist,jdepth
c
*                      need to allow for zero entries (where phase
*                      description strongly depth dependent)
c tau0
         a0 = t0(idist,jdepth)
         b0 = t0(idist,jdepth+1)
         h0 = t0(idist+1,jdepth+1)
         d0 = t0(idist+1,jdepth)
         e0 = a0 + 
     ^       (d0-a0)*(edist-delta(idist))/(delta(idist+1)-delta(idist))
         f0 = b0 + 
     ^       (h0-b0)*(edist-delta(idist))/(delta(idist+1)-delta(idist))
         g0 = e0 + 
     ^       (f0-e0)*(edepth-dpth(jdepth))/(dpth(jdepth+1)-dpth(jdepth))
         tau0 = g0
c tau1
         a1 = t1(idist,jdepth)
         b1 = t1(idist,jdepth+1)
         h1 = t1(idist+1,jdepth+1)
         d1 = t1(idist+1,jdepth)
         e1 = a1 + 
     ^       (d1-a1)*(edist-delta(idist))/(delta(idist+1)-delta(idist))
         f1 = b1 + 
     ^       (h1-b1)*(edist-delta(idist))/(delta(idist+1)-delta(idist))
         g1 = e1 + 
     ^       (f1-e1)*(edepth-dpth(jdepth))/(dpth(jdepth+1)-dpth(jdepth))
         tau1 = g1
c tau2
         a2 = t2(idist,jdepth)
         b2 = t2(idist,jdepth+1)
         h2 = t2(idist+1,jdepth+1)
         d2 = t2(idist+1,jdepth)
         e2 = a2 + 
     ^       (d2-a2)*(edist-delta(idist))/(delta(idist+1)-delta(idist))
         f2 = b2 + 
     ^       (h2-b2)*(edist-delta(idist))/(delta(idist+1)-delta(idist))
         g2 = e2 + 
     ^       (f2-e2)*(edepth-dpth(jdepth))/(dpth(jdepth+1)-dpth(jdepth))
         tau2 = g2
c
*         write(6,*) "tau0,tau1,tau2:",tau0,tau1,tau2
         caz = cos(azim)
         cbz = cos(2.0*azim)
*         write(6,*) "azim,caz,cbz",azim,caz,cbz    
c
         tcor = sc0*tau0 + sc1*cos(azim)*tau1 + sc2*cos(2.0*azim)*tau2
c
      return
c<ec>
      end
      subroutine phase_alias(phase,delta,ip)

c-    check for alternative phase names
c     input phase, delta
c     output ip (index of phcod)

      character*(*) phase
      if(phase(1:3).eq.'Pg ') then
c       phase='P       '
        ip=2
      else if(phase(1:3).eq.'Sg ') then
c       phase='S       '
        ip=33
      else if(phase(1:4).eq.'pPg ') then
c       phase='pP      '
        ip=8
      else if(phase(1:4).eq.'sPg ') then
c       phase='sP      '
        ip=13
      else if(phase(1:4).eq.'pSg ') then
c       phase='pS      '
        ip=37
      else if(phase(1:4).eq.'sSg ') then
c       phase='sS      '
        ip=40
c
      elseif(phase(1:3).eq.'Pb ') then
c       phase='P       '
        ip=2
      else if(phase(1:3).eq.'Sb ') then
c       phase='S       '
        ip=33
      else if(phase(1:4).eq.'pPb ') then
c       phase='pP      '
        ip=8
      else if(phase(1:4).eq.'sPb ') then
c       phase='sP      '
        ip=13
      else if(phase(1:4).eq.'pSb ') then
c       phase='pS      '
        ip=37
      else if(phase(1:4).eq.'sSb ') then
c       phase='sS      '
c
      elseif(phase(1:3).eq.'Pn ') then
c       phase='P       '
        ip=2
      else if(phase(1:3).eq.'Sn ') then
c       phase='S       '
        ip=33
      else if(phase(1:4).eq.'pPn ') then
c       phase='pP      '
        ip=8
      else if(phase(1:4).eq.'sPn ') then
c       phase='sP      '
        ip=13
      else if(phase(1:4).eq.'pSn ') then
c       phase='pS      '
        ip=37
      else if(phase(1:4).eq.'sSn ') then
c       phase='sS      '
        ip=40
      else if(phase(1:4).eq.'SPn ') then
c       phase='SP      '
        ip=55
      else if(phase(1:4).eq.'SPb ') then
c       phase='SP      '
        ip=55
      else if(phase(1:4).eq.'SPg ') then
c       phase='SP      '
        ip=55
      else if(phase(1:4).eq.'SnP ') then
c       phase='SP      '
        ip=55
      else if(phase(1:4).eq.'PSn ') then
c       phase='PS      '
        ip=56
      else if(phase(1:5).eq.'PnPn ') then
c       phase='PP      '
        ip=30
      else if(phase(1:5).eq.'SnSn ') then
c       phase='SS      '
        ip=53
c                                       upgoing P, S
      else if(phase(1:2).eq.'p ') then
c       phase='Pup     '
        ip=1  
      else if(phase(1:2).eq.'s ') then
c       phase='Sup     '
        ip=32 
c                                        
      else if(delta.le.100.0.and.phase.eq.'pPdiff  ') then
c       phase='pP      '
        ip=8
      else if(delta.le.100.0.and.phase.eq.'sPdiff  ') then
c       phase='sP      '
        ip=13
      else if(delta.le.100.0.and.phase.eq.'pSdiff  ') then
c       phase='pS      '
        ip=37
      else if(delta.le.100.0.and.phase.eq.'sSdiff  ') then
c       phase='sS      '
        ip=40
      else if(delta.ge.100.0.and.phase.eq.'pPdiff  ') then
c       phase='Pdiff   '
        ip=8
      else if(delta.ge.100.0.and.phase.eq.'sPdiff  ') then
c       phase='Pdiff   '
        ip=13
      else if(delta.ge.100.0.and.phase.eq.'pSdiff  ') then
c       phase='Sdiff   '
        ip=37
      else if(delta.ge.100.0.and.phase.eq.'sSdiff  ') then
c       phase='Sdiff    '
        ip=40
c            
      else if(delta.le.165.0.and.phase.eq.'PKPdiff ') then
c       phase='PKPbc '
        ip=5
      else if(delta.le.165.0.and.phase.eq.'pPKPdiff') then
c       phase='pPKPbc '
        ip=10
      else if(delta.le.165.0.and.phase.eq.'sPKPdiff') then
c       phase='sPKPbc '
        ip=15
c                             
      else if(phase(1:4).eq."P'P'") then
c       phase="P'P'    "
        ip =31
      else if(phase(1:4).eq."S'S'") then
c       phase="P'P'    "
        ip =54
c                            diffractions (approx)
      else if(delta.gt.100.0.and.phase.eq.'pPdiff  ') then
c       phase='Pdiff   '
        ip=8
      else if(delta.gt.100.0.and.phase.eq.'sPdiff  ') then
c       phase='Pdiff   '
        ip=13
      else if(delta.gt.100.0.and.phase.eq.'pSdiff  ') then
c       phase='Sdiff   '
        ip=37
      else if(delta.gt.100.0.and.phase.eq.'sSdiff  ') then
c       phase='Sdiff    '
c
      else
        ip=-1
      endif
      return
      end
c-----
      integer function lnblk(s)
      character *(*) s
      l = len(s)
      do i=l,1,-1
        if (s(i:i).gt.' ') then
          lnblk=i
          return
        endif
      end do   
      lnblk=0
      return
      end
