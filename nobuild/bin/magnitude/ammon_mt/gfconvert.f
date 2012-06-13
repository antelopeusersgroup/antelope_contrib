*
*      program to convert g randall's mij responses into langston
*         and helmberger fundamental fault responses
*
*      author:  Chuck Ammon, UC Santa Cruz
*      version:  1.1, August 1994
*
*      usage: gfconvert r_0300_0.0 outfile 10
*
*      (or give no command line arguments and the code will prompt
*        for the needed information
*
*      on output, z is positive up, r is poistive away from source
*        and t is positive to the right when looking in r direction
*        this is the opposite of the mijkennett coordinate system
*
*      if you want to keep the original coordinate system, use a 
*        negative scale_factor
*
***********************************************************************
*
       program gfconvert
*
       integer maxpts, mxsrc
       real deg_to_rad, rad_to_deg
       parameter(maxpts=2048, mxsrc=6)
       parameter(deg_to_rad = 0.017453292, rad_to_deg = 57.29577951)
       real vert(maxpts), radial(maxpts),tangential(maxpts)
       real mt_z(maxpts,6), mt_r(maxpts,6), mt_t(maxpts,6)
       real btime, dt
       real strike, dip, rake, sta_az, rstrike
       real comp_az,scale_factor
       real mtensor(6), distance, evdp, omarker, user(3)
       real stks(5),dps(5),rks(5),sazs(5)
       integer i, nerr
       integer ixx,ixy,ixz,iyy,iyz,izz
       integer nlen,lnblnk,npts,icouple
       integer stdin,stdout,ounit
       character*256 gftn_name, outfile, infile
       character*10 gr_suf(mxsrc), ctemp

       common /header/ btime,dt,evdp,distance,omarker,user
*       
       write(stdout,*)'GFCONVERT: Version 1.1'
*
       stdin = 5
       stdout = 6
       ounit = 10
*
*      ordering of the moment tensor
*
       ixx = 1
       ixy = 2
       ixz = 3
       iyy = 4
       iyz = 5
       izz = 6
*
       gr_suf(ixx) = '_mxx '
       gr_suf(ixy) = '_mxy '
       gr_suf(ixz) = '_mxz '
       gr_suf(iyy) = '_myy '
       gr_suf(iyz) = '_myz '
       gr_suf(izz) = '_mzz '
*     
*      set up the default settings for the fundamental faults
*
*      VSS: P-SV       
       stks(1) =  0
       dps(1)  = 90 
       rks(1)  =  0
       sazs(1) = 45
*
*      VDS: P-SV
       stks(2) =  0
       dps(2)  = 90 
       rks(2)  = 90
       sazs(2) = 90
*
*      CLV: P-SV
       stks(3) =  0
       dps(3)  = 45 
       rks(3)  = 90
       sazs(3) = 45
*
*      VSS: SH
       stks(4) =  0
       dps(4)  = 90 
       rks(4)  =  0
       sazs(4) =  0
*
*      VDS: SH
       stks(5) =  0
       dps(5)  = 90 
       rks(5)  = 90
       sazs(5) =  0
*
*      Terminal or command line IO
*   
       nargs = iargc()

       if(nargs .eq. 0) then
*
         write(stdout,*)
     &     'Enter the mijkennett basename for any component.'
*
         write(stdout,*) 'e.g. r_0300_0.0'
         read(stdin,*) gftn_name         
*
         write(stdout,*) 'What is the output base filename?'
         read(stdin,*) outfile
*       
         write(stdout,*)'Enter seismogram scale factor.'
         read(stdin,*) scale_factor
*
       else if(nargs .eq. 3) then
*
         call getarg(1,gftn_name)
         call getarg(2,outfile)
         call getarg(3,ctemp)
         read(ctemp,*) scale_factor
*
       else
*
         write(stdout,*)'e.g. usage: gfconvert r_0300_0.0 outfile 10'
         stop
*
       end if
*
*      END of user input ***********************************************
*      
*      change the sign of the scale factor to flip the polarity of the
*         seismograms coming out of GR's reflectivity code.
*         on output, z is positive up, r is poistive away from source
*         and t is positive to the right when looking in r direction
*
*         if you want to keep the original coordinate system, use a 
*          negative scale_factor
*
       scale_factor = -scale_factor
*
*
*      READ IN THE moment tensor green's functions
*
       nlen = lnblnk(gftn_name)
*
*      get the station azimuth used in the compuation of the mij
*        responses
*
       infile(1:nlen+4) = 'z'//gftn_name(2:nlen)//gr_suf(1)      
       call rsac1(infile,mt_z,npts,btime,dt,10,nerr)
       call getfhv('USER4',comp_az,nerr)
       call getfhv('DIST',distance,nerr)
       call getfhv('EVDP',evdp,nerr)
       call getfhv('O',omarker,nerr)
       call getfhv('USER1',user(1),nerr)
       call getfhv('USER2',user(2),nerr)
       call getfhv('USER3',user(3),nerr)
*
       write(stdout,*) 
     &     'Mij responses were computed for az = ',comp_az
*
*
*      READ in the Green's Functions
*      
       do 1500 icouple = 1, 6
       
         infile(1:nlen+4) = 'z'//gftn_name(2:nlen)//gr_suf(icouple)      
         call rsac1(infile,mt_z(1,icouple),npts,btime,dt,maxpts,nerr)
              
         infile(1:nlen+4) = 'r'//gftn_name(2:nlen)//gr_suf(icouple)      
         call rsac1(infile,mt_r(1,icouple),npts,btime,dt,maxpts,nerr)
       
         infile(1:nlen+4) = 't'//gftn_name(2:nlen)//gr_suf(icouple)      
         call rsac1(infile,mt_t(1,icouple),npts,btime,dt,maxpts,nerr)
       
1500   continue
*
*       some info for the user
*
        write(stdout,*)
     &   'The output files have +Z = up, +R = away from src'
        write(stdout,*)
     &   '+T = to the right, when looking in the +R direction.'
        write(stdout,*)
     &   'This is the opposite of the mijkennett convention.'
*
        nlen = lnblnk(outfile)
*     
*       LOOP OVER FUNDAMENTAL FAULTS
*
       do 5000 ifault = 1,5
*
         do 1540 i = 1,npts
            radial(i) = 0
            vert(i) = 0
            tangential(i) = 0
1540     continue
*
*        set up the strike, dip, rake, and sta_az   
*          for the given fundamental fault
*    
         strike = stks(ifault)
         dip = dps(ifault)
         rake = rks(ifault)
         sta_az = sazs(ifault)
*
         rstrike = comp_az - (sta_az - strike)
*
         call dislocation_to_mtensor(rstrike,dip,rake,sta_az,mtensor)
*
*        sum the green's functions with the appropriate weights
*  
         do 1600 icouple = 1, 6     
           do 1550 i = 1,npts
            vert(i) =     vert(i) +  mtensor(icouple)*mt_z(i,icouple)
            radial(i) = radial(i) +  mtensor(icouple)*mt_r(i,icouple)
            tangential(i) = 
     &              tangential(i) +  mtensor(icouple)*mt_t(i,icouple)
1550       continue
1600     continue
*
*       Scale the seismograms
*
           do 1750 i = 1,npts
               radial(i)     = radial(i)*scale_factor
               vert(i)       = vert(i)*scale_factor
               tangential(i) = tangential(i)*scale_factor
1750       continue
*
            if(ifault .eq. 3)then
               do 1751 i = 1,npts
                 radial(i) = radial(i)*2
                 vert(i)   = vert(i)*2
1751       continue
           end if
*
*      OUTPUT
*       
       call write_gf(ifault,outfile,nlen,vert,radial,tangential,npts)
*      
5000   continue
*      end loop over fundamental faults
*
*
*
       stop
       end
*
       subroutine write_gf(ifault,outfile,nlen,v,r,t,npts)
*
       integer ifault,nlen,npts
       real v(npts), r(npts), t(npts)
       real omarker,user(3),xdummy, evdp, distance
       character*(*) outfile
       character*256 gftn

       common /header/ btime,dt,evdp,distance,omarker,user

*      WRITE OUT the Green's Functions
*       
       call newhdr
       call setfhv('DELTA',dt,nerr)
       call setfhv('B',btime,nerr)
       call setlhv('LEVEN',.TRUE.,nerr)
       call setihv('IFTYPE','ITIME',nerr)
       call setnhv('NPTS',npts,nerr)
       call setfhv('DIST',distance,nerr)
       call setfhv('EVDP',evdp,nerr)
       call setfhv('O',omarker,nerr)
       call setfhv('USER1',user(1),nerr)
       call setfhv('USER2',user(2),nerr)
       call setfhv('USER3',user(3),nerr)
*
       if(ifault .eq. 1) then
         gftn(1:nlen+7) = outfile(1:nlen)//"_pz.vss"      
         gftn(nlen+8:) = ''
         call wsac0(gftn,xdummy,v,nerr)
         gftn(1:nlen+7) = outfile(1:nlen)//"_sr.vss"      
         gftn(nlen+8:) = ''
         call wsac0(gftn,xdummy,r,nerr)

       else if(ifault .eq. 2)then
         gftn(1:nlen+7) = outfile(1:nlen)//"_pz.vds"      
         gftn(nlen+8:) = ''
         call wsac0(gftn,xdummy,v,nerr)
         gftn(1:nlen+7) = outfile(1:nlen)//"_sr.vds"      
         gftn(nlen+8:) = ''
         call wsac0(gftn,xdummy,r,nerr)

       else if(ifault .eq. 3)then
         gftn(1:nlen+7) = outfile(1:nlen)//"_pz.clv"      
         gftn(nlen+8:) = ''
         call wsac0(gftn,xdummy,v,nerr)
         gftn(1:nlen+7) = outfile(1:nlen)//"_sr.clv"      
         gftn(nlen+8:) = ''
         call wsac0(gftn,xdummy,r,nerr)

       else if(ifault .eq. 4)then
         gftn(1:nlen+7) = outfile(1:nlen)//"_st.vss"      
         gftn(nlen+8:) = ''
         call wsac0(gftn,xdummy,t,nerr)

       else if(ifault .eq. 5)then
         gftn(1:nlen+7) = outfile(1:nlen)//"_st.vds"      
         gftn(nlen+8:) = ''
         call wsac0(gftn,xdummy,t,nerr)
       end if
       
       return
       end
*
       subroutine dislocation_to_mtensor(strike,dip,rake,sta_az,m)
       real strike,dip,rake,sta_az,m(6)
       real deg_to_rad, rad_to_deg
       parameter(deg_to_rad = 0.017453292, rad_to_deg = 57.29577951)
       real sd,s2d,cd,c2d,sr,cr,ss,s2s,cs,c2s
       integer izz,ixy,ixz,ixx,iyz,iyy
*
*      input the angles in degrees
*
*      ordering of the moment tensor
*
       ixx = 1
       ixy = 2
       ixz = 3
       iyy = 4
       iyz = 5
       izz = 6
*
*      compute some constants
*        	    
       sd = sin(deg_to_rad*dip)
       s2d = sin(deg_to_rad*2.0*dip)
       cd = cos(deg_to_rad*dip)
       c2d = cos(deg_to_rad*2.0*dip)
*       
       sr = sin(deg_to_rad*rake)
       cr = cos(deg_to_rad*rake)
*       
       ss = sin(deg_to_rad*strike)
       s2s = sin(deg_to_rad*2.0*strike)
       cs = cos(deg_to_rad*strike)
       c2s = cos(deg_to_rad*2.0*strike)
       
       m(ixx) = -(sd*cr*s2s + s2d*sr*ss*ss)
       m(ixy) =  (sd*cr*c2s + 0.5*s2d*sr*s2s)
       m(ixz) = -(cd*cr*cs + c2d*sr*ss)
       m(iyy) =  (sd*cr*s2s - s2d*sr*cs*cs)
       m(iyz) = -(cd*cr*ss - c2d*sr*cs)
       m(izz) =   s2d*sr
              
       return
       end
