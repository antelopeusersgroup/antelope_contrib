*
*      program to sum fundamental responses to produce a dislocation response
*
*      author:  Chuck Ammon, UC Santa Cruz
*      version:  1.1, February 1993
*
*      Note that langston's moment tensor has sign flips for all elements
*           relative to aki and richard's.  This results from a direction
*           of the fault normal opposite of aki and richards.
*
*      I use aki and richard notation, so if you are using langston type
*           fundamental responses and a moment tensor defined in aki and
*           richards coordinates, you should flip the sign of every element
*           in the moment tensor.
*           Use the aki and richards coordinates of the moment tensor for
*           randall green's functions.
*
       program gfcombine
*
       integer maxpts, mxsrc
       real deg_to_rad, rad_to_deg
       parameter(maxpts=2048, mxsrc=6)
       parameter(deg_to_rad = 0.017453292, rad_to_deg = 57.29577951)
       real vss_t(maxpts), vds_t(maxpts)
       real vss_r(maxpts), vds_r(maxpts), ffds_r(maxpts)
       real vss_z(maxpts), vds_z(maxpts), ffds_z(maxpts)
       real vert(maxpts), radial(maxpts),tangential(maxpts)
       real mt_z(maxpts), mt_r(maxpts), mt_t(maxpts)
       real btime, dt
       real strike, dip, rake, sta_az, rstrike
       real sd,cd,s2d,c2d,ss,s2s,cs,c2s,sr,cr
       real cstaz,c2staz,sstaz,s2staz,comp_az,scale_factor
       real a1,a2,a3,a4,a5, mtensor(6), idislocation,imtensor
       integer i, nerr, iwave,p_sv_only,sh_only,all,gf_type
       integer ilangston, irandall,ixx,ixy,ixz,iyy,iyz,izz
       integer isrc_type, nlen,lnblnk,npts, icouple
       integer stdin,stdout,ounit
       character*256 gftn_name, outfile, infile, ofile
       character*10 gr_suf(mxsrc)
*       
       stdin = 5
       stdout = 6
       ounit = 10
*       
       p_sv_only = 1
       sh_only = 2
       all = 3
*
       iLANGSTON = 1
       iRANDALL = 2
       
       idislocation = 1
       imtensor = 2
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
*      Terminal IO
*   
       write(stdout,*) 'What kind of GFs, Langston (1), Randall (2)?'
       read(stdin,*) gf_type
       if(gf_type .eq. iRANDALL) then
         write(stdout,*) 'Summation of moment tensor components.'
         write(stdout,*) 'I assume you used a station az of zero.'
         write(stdout,*) 'Enter the basename for any component.'
         write(stdout,*) 'e.g. r_0300_0.0'
       else if(gf_type .eq. iLANGSTON)then
         write(stdout,*) 'Summation of fundamental fault responses.'
         write(stdout,*) 'What is the GFtn base filename?'
       end if
       read(stdin,*) gftn_name
*
       write(stdout,*) 'What is the output base filename?'
       read(stdin,*) outfile
*
       write(stdout,*) 'Dislocation (1) or moment tensor (2) response?'
       read(stdin,*) isrc_type
*
       if(isrc_type .eq. idislocation)then
        write(stdout,*) 'Enter the strike, dip, rake (in degrees).'
        read(stdin,*) strike, dip, rake
       else if (isrc_type .eq. imtensor)then
        if(gf_type .eq. IRANDALL) then
         write(stdout,*) 'Enter Moment tensor:'        
         write(stdout,*) 'mxx,mxy,mxz,myy,myz,mzz.'
         read(stdin,*) mtensor(ixx),mtensor(ixy),mtensor(ixz),
     &                 mtensor(iyy),mtensor(iyz),mtensor(izz)
        else if (gf_type .eq. ILANGSTON) then
         write(stdout,*) 'Enter Deviatoric Moment tensor:'        
         write(stdout,*) 'mxx,mxy,mxz,myy,myz.'
         read(stdin,*) mtensor(ixx),mtensor(ixy),mtensor(ixz),
     &                 mtensor(iyy),mtensor(iyz)
         mtensor(izz) = -(mtensor(ixx) + mtensor(iyy))
        end if
       end if
*
       write(stdout,*)'Enter the station azimuth (in degrees).'
       read(stdin,*) sta_az
       
       write(stdout,*)'Enter seismogram scale factor.'
       read(stdin,*) scale_factor
*
       write(stdout,*) 'What wave types (1) P-SV, (2) SH, (3) ALL?'
       read(stdin,*) iwave
*
*      END of terminal input
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
       rstrike = sta_az - strike
       if(rstrike .lt. 0.0) rstrike = rstrike + 360.0
       ss = sin(deg_to_rad*rstrike)
       s2s = sin(deg_to_rad*2.0*rstrike)
       cs = cos(deg_to_rad*rstrike)
       c2s = cos(deg_to_rad*2.0*rstrike)
*      
       if(gf_type .eq. iLANGSTON) then 
*
*      READ in the Green's Functions
*
       nlen = lnblnk(gftn_name)
*       
       if(iwave .ne. sh_only) then
         infile(1:nlen+7) = gftn_name(1:nlen)//"_pz.clv"      
         call rsac1(infile,ffds_z,npts,btime,dt,maxpts,nerr)
              
         infile(1:nlen+7) = gftn_name(1:nlen)//"_pz.vss"      
         call rsac1(infile,vss_z,npts,btime,dt,maxpts,nerr)
       
         infile(1:nlen+7) = gftn_name(1:nlen)//"_pz.vds"      
         call rsac1(infile,vds_z,npts,btime,dt,maxpts,nerr)
       
         infile(1:nlen+7) = gftn_name(1:nlen)//"_sr.vss"      
         call rsac1(infile,vss_r,npts,btime,dt,maxpts,nerr)
       
         infile(1:nlen+7) = gftn_name(1:nlen)//"_sr.vds"      
         call rsac1(infile,vds_r,npts,btime,dt,maxpts,nerr)
       
         infile(1:nlen+7) = gftn_name(1:nlen)//"_sr.clv"      
         call rsac1(infile,ffds_r,npts,btime,dt,maxpts,nerr)
       end if 
       
       if(iwave .ne. p_sv_only) then      
         infile(1:nlen+7) = gftn_name(1:nlen)//"_st.vss"      
         call rsac1(infile,vss_t,npts,btime,dt,maxpts,nerr)
       
         infile(1:nlen+7) = gftn_name(1:nlen)//"_st.vds"      
         call rsac1(infile,vds_t,npts,btime,dt,maxpts,nerr)
       end if
       
       if(isrc_type .eq. idislocation)then
*
*      compute the coefficients from Langston and Helmberger (1975, GJRAS)
*    	
         a1 = s2s*cr*sd + 0.5*c2s*sr*s2d
         a2 = cs*cr*cd - ss*sr*c2d
         a3 = 0.5*sr*s2d
*              
         a4 = c2s*cr*sd - 0.5*s2s*sr*s2d
         a5 = -ss*cr*cd - cs*sr*c2d
       
       else if (isrc_type .eq. imtensor) then
*
*      compute the coefficients from Langston et al. (1982, PEPI)
*    	
         cstaz = cos(sta_az * deg_to_rad)
         c2staz = cos(2*sta_az * deg_to_rad)
         sstaz = sin(sta_az * deg_to_rad)
         s2staz = sin(2*sta_az * deg_to_rad)
         
         a1 = 0.5*(mtensor(iyy)-mtensor(ixx))*c2staz
     &        - mtensor(ixy)*s2staz
         a2 = mtensor(ixz)*cstaz + mtensor(iyz)*sstaz
         a3 = 0.5*(mtensor(ixx) + mtensor(iyy))
         a4 = 0.5*(mtensor(ixx) - mtensor(iyy))*s2staz
     &        - mtensor(ixy)*c2staz
         a5 = mtensor(iyz) * cstaz - mtensor(ixz) * sstaz
       end if
*
*      sum the green's functions with the appropriate weights
*       
       do 1000 i = 1,npts
*       
        if(iwave .ne. p_sv_only) then      
          tangential(i) = a4 * vss_t(i) + a5 * vds_t(i)
        end if
*
        if(iwave .ne. sh_only) then
          radial(i) = a1*vss_r(i) + a2*vds_r(i) + a3*ffds_r(i)
          vert(i) = a1*vss_z(i) + a2*vds_z(i) + a3*ffds_z(i)
        end if
*
1000   continue
*
       end if
*      end of summation of Langston style green's functions
*
*
       if(gf_type .eq. iRANDALL) then 
*
       nlen = lnblnk(gftn_name)
*
*      get the station azimuth used in the compuation of the mij
*        responses
*
       infile(1:nlen+4) = 'z'//gftn_name(2:nlen)//gr_suf(1)      
       call rsac1(infile,mt_z,npts,btime,dt,10,nerr)
       call getfhv('USER4',comp_az,nerr)
       write(stdout,*) 'Mij responses for az = ',comp_az
*
       if(isrc_type .eq. idislocation)then
        rstrike = comp_az - (sta_az - strike)
        call dislocation_to_mtensor(rstrike,dip,rake,sta_az,mtensor)
       end if
*
*      READ in the Green's Functions
*      
       do 1500 icouple = 1, 6
       
       if(iwave .ne. sh_only) then
         infile(1:nlen+4) = 'z'//gftn_name(2:nlen)//gr_suf(icouple)      
         call rsac1(infile,mt_z,npts,btime,dt,maxpts,nerr)
              
         infile(1:nlen+4) = 'r'//gftn_name(2:nlen)//gr_suf(icouple)      
         call rsac1(infile,mt_r,npts,btime,dt,maxpts,nerr)
       
         infile(1:nlen+4) = 't'//gftn_name(2:nlen)//gr_suf(icouple)      
         call rsac1(infile,mt_t,npts,btime,dt,maxpts,nerr)
       
       end if 
*
*      sum the green's functions with the appropriate weights
*       
       do 1250 i = 1,npts
*       
        if(iwave .ne. p_sv_only) then      
          tangential(i) = tangential(i) + mtensor(icouple)*mt_t(i)
        end if
*
        if(iwave .ne. sh_only) then
          radial(i) = radial(i) + mtensor(icouple)*mt_r(i)
          vert(i) = vert(i) + mtensor(icouple)*mt_z(i)
        end if

*
1250   continue
*
1500     continue
*       end of loop over moment tensor couples
       end if
*      end of summation of RANDALL style green's functions (mij's)
*
*
*       Scale the seismograms
*
       do 1750 i = 1,npts
*       
        if(iwave .ne. p_sv_only) then      
          tangential(i) = tangential(i)*scale_factor
        end if
*
        if(iwave .ne. sh_only) then
          radial(i) = radial(i)*scale_factor
          vert(i) = vert(i)*scale_factor
        end if
1750   continue
*
*
*      OUTPUT
*
       nlen = lnblnk(outfile)
*       
       if(iwave .ne. p_sv_only) then      
         ofile(1:nlen+2) = outfile(1:nlen)//".t"      
         call wsac1(ofile,tangential,npts,btime,dt,nerr)
       end if
*
       if(iwave .ne. sh_only) then
         ofile(1:nlen+2) = outfile(1:nlen)//".r"      
         call wsac1(ofile,radial,npts,btime,dt,nerr)
         ofile(1:nlen+2) = outfile(1:nlen)//".z"      
         call wsac1(ofile,vert,npts,btime,dt,nerr)
       end if      
*
       stop
       end
*
*
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
