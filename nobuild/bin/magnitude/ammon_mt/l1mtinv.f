*
*      deviatoric moment tensor inversion using L1 norm
*
*      Time function is fixed, so problem is linear
*
*      Author:  Chuck Ammon, UC Santa Cruz, Saint Louis University
*      Version: 2.1 September 1994
*               parts by George Randall, U South Carolina
*
*      Notation from Langston (1981)
*        x is north, y is east, z is down
*        all signs are opposite Aki and Richards due to a different
*        choice of the direction for the fault normal
*
       program l1mtinv
*

       real deg_to_rad, rad_to_deg
       parameter(maxpts=0512, nprm = 5, mxwvs = 30, nmax = mxwvs*maxpts)
       parameter(deg_to_rad = 0.017453292, rad_to_deg = 57.29577951)
*
       common /gftns/ vss(maxpts),vds(maxpts),ffds(maxpts),
     &                response(maxpts)
       common /observations/ obs(maxpts)
*
      real pred(maxpts)
      real a(nmax,nprm), asave(nmax,nprm)
      real data(nmax), datasv(nmax)
      real mtensor(6)
      real btime, dt, m0
      real az(mxwvs), wt(mxwvs)
      real sum_sq_amp(mxwvs)
      real obswt(mxwvs),err
*      
      real q(nmax+2,nprm+2), res(nmax), x(nprm+2)
      real cu(2,nmax+nprm)
      real toler,error,total_sum_sq, total_sum_sq_wt
      integer iu(2,nmax+nprm),s(nmax),itotal_pts
      integer i,k,l,m,n,klmd,klm2d,nklmd,n2d,iter
*
      integer ixx,iyy,ixy,ixz,iyz, wvtype(mxwvs), iPz, iPr, iSH
      integer nerr, etype,  ibegin, npts(mxwvs)
      integer slen, lnblnk
      integer stdin,stdout,ounit,inunit
      character*256 obs_name(mxwvs), gftn_name(mxwvs), infile, ofile
      character*258 wvfile
      character*32 ascfmt
*
*     scale factor to allow L1 routine to converge
*
      real scale_for_l1
*
       stdin = 5
       stdout = 6
       ounit = 10
       inunit = 11
*
       ixx = 1
       iyy = 2
       ixy = 3
       ixz = 4
       iyz = 5
*
       vwt = 1
       rwt = 1
       twt = 1
       ims_amp_wt = 0
*
       iPz = 1
       iSH = 2
       iPr = 3
*
       ibegin = 1
*
       total_sum_sq = 0
       total_sum_sq_wt = 0
*
       itotal_pts = 0
*
       write(stdout,*)'What is the input file name?'
       read(stdin,*) infile
*
       write(stdout,*)'What is the output file name?'
       read(stdin,*) ofile
*
       write(stdout,*)'What is the scale_for_l1?'
       read(stdin,*) scale_for_l1
*
       open(unit = ounit, file = ofile)
       write(ounit,*)'*********** L1 INVERSION **********************'
*
       open(unit = inunit, file = infile)
       read(inunit,*) nwaves, m0
       do 5 i = 1, nwaves
         read(inunit,*)wvtype(i),obs_name(i),gftn_name(i),
     &                 az(i),wt(i)
5      continue
       close(inunit)
*
************************************************************************
*      Output some event and inversion 
*           information (using the first data file)
*     
       call getevent_data(obs_name(1),ounit)
*
************************************************************************
*
*      Now read in the data, GFs, and set up the matrix equations
*       
       do 125 i = 1, nwaves
*
         call getwaveforms(gftn_name(i),obs_name(i),wvtype(i),
     &                    npts(i),dt,btime,ounit)      
*
*        compute the norm of the data
*
         etype = 2
         call getnorm(obs,npts(i),etype,sum_sq_amp(i))
*
*        SET UP WEIGHTING
*
*        if the input wt is > 0, then the 
*           wt = (wt / sum square amplitude of the data)
*           this will normalize the data to a "unit variance" before weighting
*           ie, if wt(i) = 1.0, then the seis is weighted by 1/sum_sq_amp
*
*        if the input wt <= 0, 
*           then the weight is simply abs(wt(i)) (no sum_sq_amp
*           factor is included
*
         if(wt(i) .gt. 0.0) then
           if(sum_sq_amp(i) .eq. 0.0) then
             write(stdout,*) 'ERROR: Sum Sq Amp = 0.0, file = ',obs_name(i)
             stop
           end if
           obswt(i) = wt(i)/sum_sq_amp(i)
         else if(wt(i) .le. 0.0) then
           obswt(i) = abs(wt(i))
         end if
*
         write(ounit,*) 'Sum of Square Amplitudes: ',sum_sq_amp(i)
         write(ounit,*) 'Weight:  ',obswt(i)
         write(ounit,*) ' '
*
*        keep track of the total sum of square amplitudes to provide 
*           "normalized"
*         error measurements
*
         total_sum_sq = total_sum_sq + sum_sq_amp(i)
         total_sum_sq_wt = total_sum_sq_wt + (sum_sq_amp(i) * obswt(i))
*
*
*        SET-UP THE INVERSION
*
*        compute the response for individual mt elements
*        and store in the system matrix a
*
         if(ibegin .gt. nmax) then
           write(stdout,*) 'ERROR: Overflowing the a matrix'
           write(stdout,*) 'ibegin = ',ibegin
           write(stdout,*) 'max allowed =', nmax
           stop
         end if
*
*        a copy of the "a matrix" is saved  (before weighting)
*           for later computation of errors and predicted fits
*           a quick alternative is to just read in the seismograms
*           and build the A matrix again later
*
         st_az = az(i) * deg_to_rad
         do 100 m = 1, 5
            call getmtresp(m,wvtype(i),st_az,npts(i))
            call appendvector(a(1,m),nmax,ibegin,
     &                        response,npts(i))
            call appendvector(asave(1,m),nmax,ibegin,
     &                        response,npts(i))
100    	continue
*
*        next set up the data vector (save a copy here as well)
* 
         call appendvector(data,nmax,ibegin,obs,npts(i))
         call appendvector(datasv,nmax,ibegin,obs,npts(i))
*
*        apply the weighting
*
         if(obswt(i) .ne. 1.0) then
           call apply_weights(a,nmax,ibegin,npts(i),1,5,data,obswt(i))
         end if
*
*        keep track of the number of rows in a matrix
*
         ibegin = ibegin + npts(i)
*        ibegin points to the next unused row of the a matrix
*
*
125   continue
*     end loop over waveforms
*
      do 127 i = 1, nwaves
         itotal_pts = itotal_pts + npts(i)
127   continue
*
      write(ounit,*) ' '
      write(ounit,*)
     &    'Using a total of ',itotal_pts, ' points in inversion.'
      write(ounit,*) ' '
*
*     END of SETUP *****************************************************
*
*
*     SOLVE FOR THE MOMENT TENSOR USING AN L1 NORM
*
*
*
      k = ibegin - 1
      l = 0
      m = 0
      n = 5
      klmd = nmax
      klm2d = klmd + 2
      nklmd = nmax + nprm
      n2d = nprm + 2
      kcode = 0
      toler = 1e-04
      iter = 10 * klmd
*     write(stdout,*)k,klmd
*
      do 130 i = 1, k
         do 128 j = 1, 5
           q(i,j) = a(i,j)*scale_for_l1
128      continue
         q(i,6) = data(i)*scale_for_l1
130   continue
*
      call cl1(k, l, m, n, klmd, klm2d, nklmd, n2d, q,
     * kcode, toler, iter, x, res, error, cu, iu, s)
     
      if(kcode .eq. 0)then
          do 135 i = 1,5
             mtensor(i) = x(i)
135       continue
      else if(kcode .eq. 1)then
        write(stdout,*)'No feasible solution to L1 fit found.'
        stop
      else if(kcode .eq. 2)then
        write(stdout,*)'Precision problems in L1 fit.'
        stop
      else if(kcode .eq. 3)then
        write(stdout,*)'No solution found in maximum iterations.'
        write(stdout,*)'Tried ',iter, ' iterations.'
        stop
      end if
      
*
      mtensor(6) = -(mtensor(1) + mtensor(2))
*
      call gmoment(mtensor,m0,ounit)
*
*      COMPUTE THE PREDICTED WAVEFORMS
* 
       total_err = 0.0
       total_wtd = 0.0
       ibegin = 1
       n = 0
*
*      write(ounit,*) ' '
       write(ounit,*)
     &  'file,weight,sum_square_error,fractional_error,ls_scale_factor'
*
*
       do 350 i = 1, nwaves
*
         do 280 j = 1, npts(i)
           pred(j) = 0
280      continue
*
         do 300 m = 1, 5
           do 290 j = 1, npts(i)
             k = ibegin + j - 1
             pred(j) = pred(j) + mtensor(m)*asave(k,m)
290        continue
300    	continue
*
*        write out the prediction as a sac file
*
         wvfile = ' '
         slen = lnblnk(obs_name(i))
         wvfile(1:slen+4) = 'l1p_'//obs_name(i)(1:slen)     
         call wsac1(wvfile,pred,npts(i),btime,dt,nerr)
*
*        COMPUTE THE ERRORS (scale factor = 1.0, l2 norm)
*
         err = 0.0
         call sum_err(datasv(ibegin),pred,1.0,npts(i),2,err)
         if(obswt(i) .ne. 0.0) then
           err = err * npts(i)
           total_err = total_err + err
           total_wtd = total_wtd + err * obswt(i)
           n = n + npts(i)
         end if
*
*        compute a scale factor for the seismogram fit
*          not used, just a statistic
*
         call scale_fit(datasv(ibegin),pred,npts(i),factor)
*
*        some nonsense for neat output
*
         slen = lnblnk(obs_name(i))
*
         if(slen .lt. 10) then
            write(ascfmt,'(a2,i1,a16)') '(a' , slen, ',1x,4(e16.9,1x))'
         else
            write(ascfmt,'(a2,i2,a16)') '(a' , slen, ',1x,4(e16.9,1x))'
         end if
*
         write(ounit,ascfmt)
     &     obs_name(i),obswt(i),err,err/sum_sq_amp(i),factor
* 
         ibegin = ibegin + npts(i)
*
350    continue
*      end loop over waveforms
*
*      OUTPUT the error for all waveforms
*
       tmpsig = 1.0 / float(n-6)
*
       write(ounit,*) ' '
       write(ounit,*)
     & 'Unweighted SSE,Weighted SSE,Unweighted RMS,Weighted RMS'
       write(ounit,*) total_err*tmpsig, total_wtd*tmpsig,
     &                 sqrt(total_err*tmpsig), sqrt(total_wtd*tmpsig)
*
*
*      Output sum of square errors divided by the sum of the square amplitudes.
*
       write(ounit,*)
     & 'Total Error divided by sum of all seismogram amplitudes'
       write(ounit,*)
     & 'Unweighted, weighted'
       write(ounit,*) total_err/total_sum_sq, total_wtd/total_sum_sq_wt
*       
       close(ounit)
*
       stop
       end
*
*      END of MAIN PROGRAM
*
*******************************************************************
*
*      find a simple LS scale factor for two vectors
*
*******************************************************************
       subroutine scale_fit(obs,pred,npts,scale)
     
       real obs(npts), pred(npts)
       real scale
       integer npts
       real numerator, denominator
       integer i

       numerator = 0.0
       denominator = 0.0
              
       do 10 i = 1, npts
         numerator = numerator + obs(i)*pred(i)
         denominator = denominator + pred(i)*pred(i)
10     continue

       scale = numerator / denominator
       
       return
       end
       
*******************************************************************
       
       subroutine sum_err(obs,pred,s,npts,etype,err)
*
*      oz,or,ot - three-components observed
*      cz, cr,ct - three-components model
*      s = scale factor for the model waveforms
*      npts = number of points
*      etype = 1 L2 norm; etype = 2 L1 norm
*      err = output errors
*
       real obs(npts), pred(npts)
       real s, res
       real err
       integer npts, etype
       
       ismp_l2 = 2
       ismp_l1 = 1
       
       err = 0
       
*
*     Simple L2 norm
*       
       if(etype .eq. ismp_l2)then
         do 10 i = 1, npts
          res = obs(i) - s*pred(i)
          err = err + res*res
 10      continue
 
         err = err/float(npts)
         
       end if
*
*      Simple L1 norm
*      
       if(etype .eq. ismp_l1)then
       
         do 20 i = 1, npts
          res = obs(i) - s*pred(i)
          err = err + abs(res)
 20    continue
 
         err = err/float(npts)
         
       end if
       
       return
 
       end

*******************************************************************
         
      subroutine getnorm(x,n,ntype,norm)
      real x(n), norm
      integer n, ntype
      
      integer l1,l2,i
      
      l1 = 1
      l2 = 2
      norm = 0
      
      if(ntype .eq. l1) then
        do 1 i = 1, n
          norm = norm + abs(x(i))
1       continue
      else if(ntype .eq. l2)then
       do 2 i = 1, n
         norm = norm + x(i)*x(i)
2      continue
      end if
       
       return
       end

******************************************************************************
*
*      routine that reads in and stores the green functions and observations
*           into the common blocks
*
******************************************************************************

       subroutine getwaveforms(gftn_name,obs_name,
     &                         iwvtype,npts,dt,btime,ounit)            
       integer maxpts, nprm, nmax, mxwvs, iwvtype, ounit
       real deg_to_rad, rad_to_deg
       parameter(maxpts=0512, nprm = 5, mxwvs = 30, nmax = mxwvs*maxpts)
       parameter(deg_to_rad = 0.017453292, rad_to_deg = 57.29577951)
       
       integer stdin,stdout,lnblnk,slen
       integer ipz,ipr,ish
       integer nptsdat, nptsgrn
       character*256 obs_name, gftn_name, infile
       character*16 ascfmt
       common /gftns/ vss(maxpts), vds(maxpts), ffds(maxpts),
     &                response(maxpts)

       common /observations/ obs(maxpts)
      
       iPz = 1
       iPr = 2
       iSH = 3
       
       stdin = 5
       stdout = 6
*
*      read in the fundamental fault responses
*
*        _pz.??? => vertical component
*        _sr.??? => radial component
*        _st.??? => transverse component
*       
       slen = lnblnk(gftn_name)
*
       infile = 'empty'
       if(iwvtype .eq. iPr .or. iwvtype .eq. iPz) then
         infile(1:slen+4) = gftn_name(1:slen)//'.vss'     
         call rsac1(infile,vss,nptsgrn,btime,dt,maxpts,nerr)
         infile(1:slen+4) = gftn_name(1:slen)//'.vds'     
         call rsac1(infile,vds,nptsgrn,btime,dt,maxpts,nerr)
         infile(1:slen+4) = gftn_name(1:slen)//'.clv'     
         call rsac1(infile,ffds,nptsgrn,btime,dt,maxpts,nerr)
*
        else if(iwvtype .eq. iSH) then
*      
         infile(1:slen+4) = gftn_name(1:slen)//'.vss'     
         call rsac1(infile,vss,nptsgrn,btime,dt,maxpts,nerr)
         infile(1:slen+4) = gftn_name(1:slen)//'.vds'     
         call rsac1(infile,vds,nptsgrn,btime,dt,maxpts,nerr)
       end if
*
       call getfhv('EVDP',depth,nerr)

       write(ounit,*) ' '
       write(ounit,*) 'Greens Function summary:'
       if(slen .lt. 10) then
          write(ascfmt,'(a6,i1,a1)')'(a13,a',slen,')'
       else
          write(ascfmt,'(a5,i2,a1)')'(a13,a',slen,')'
       end if
       write(ounit,ascfmt) '  basename = ',gftn_name
       write(ounit,*) ' npts = ',nptsgrn, ' dt = ',dt,' depth = ',depth
*
*      read in the data
*             
       slen = lnblnk(obs_name)
       infile = 'empty'
       infile(1:slen) = obs_name(1:slen)     
       call rsac1(infile,obs,nptsdat,btime,dt2,maxpts,nerr) 
*
       write(ounit,*) 'Data summary:'
       if(slen .lt. 10) then
          write(ascfmt,'(a6,i1,a1)')'(a13,a',slen,')'
       else
          write(ascfmt,'(a6,i2,a1)')'(a13,a',slen,')'
       end if
       write(ounit,ascfmt) '  basename = ',obs_name
       write(ounit,*) ' npts = ',nptsdat, ' dt2 = ',dt
*
*      error check
*
       if(dt .ne. dt2) then
          write(stdout,ascfmt) 'ERROR: dt not equal for ',obs_name
          stop
       end if
*
*      set shortest npts for length
*
       if(nptsdat .ne. nptsgrn) then
          write(ascfmt,'(a6,i2,a1)')'(a30,a',slen,')'
          write(ounit,ascfmt) '  WARNING: npts not equal for ',obs_name
          npts = min0( nptsgrn, nptsdat )
          write(ounit,*) ' Truncating to ',npts,' points.'
       else
         npts = nptsdat
       end if
*
       return
       end

******************************************************************************
*
*     routine that combines the fundamental fault responses of Langston and
*         Helmberger into moment tensor responses
*
******************************************************************************

      subroutine getmtresp(i,iwvtype,az,npts)
      integer maxpts, nprm, nmax, mxwvs, iwvtype
      real deg_to_rad, rad_to_deg
      parameter(maxpts=0512, nprm = 5, mxwvs = 30, nmax = mxwvs*maxpts)
      parameter(deg_to_rad = 0.017453292, rad_to_deg = 57.29577951)
       
      integer i, npts, ixx,iyy,ixy,ixz,izz,ipz,ipr,ish
      real az
      common /gftns/ vss(maxpts), vds(maxpts), ffds(maxpts),
     &                response(maxpts)
           
              
      ixx = 1
      iyy = 2
      ixy = 3
      ixz = 4
      iyz = 5
      
      iPz = 1
      iPr = 2
      iSH = 3
      
      c2a = cos(2 * az)
      s2a = sin(2 * az)
      ca = cos(az)
      sa = sin(az)
      
      if(iwvtype .eq. iPz .or. iwvtype .eq. iPr)then
      
         if(i .eq. ixx)then
            do 10 j = 1, npts
              response(j) = 0.5*(ffds(j) - c2a * vss(j))
10          continue
         else if(i .eq. iyy)then
            do 11 j = 1, npts
              response(j) = 0.5*(c2a*vss(j) + ffds(j))
11          continue
         else if(i .eq. ixy)then
            do 12 j = 1, npts
              response(j) = -s2a * vss(j)
12          continue
         else if(i .eq. ixz)then
            do 13 j = 1, npts
              response(j) = ca * vds(j)
13          continue
         else if(i .eq. iyz)then
            do 14 j = 1, npts
              response(j) = sa * vds(j)
14          continue
          end if

      else if(iwvtype .eq. iSH) then
      
         if(i .eq. ixx)then
            do 20 j = 1, npts
              response(j) = 0.5*s2a*vss(j)
20          continue
         else if(i .eq. iyy)then
            do 21 j = 1, npts
              response(j) = -0.5*s2a*vss(j)
21          continue
         else if(i .eq. ixy)then
            do 22 j = 1, npts
              response(j) = -c2a*vss(j)
22          continue
         else if(i .eq. ixz)then
            do 23 j = 1, npts
              response(j) = -sa * vds(j)
23          continue
         else if(i .eq. iyz)then
            do 24 j = 1, npts
              response(j) = ca * vds(j)
24          continue
          end if
       
       end if

       return
       end
       
*******************************************************************
       
      subroutine appendvector(outv,mxpts,ibegin,inv,npts)
      real inv(npts), outv(mxpts)
      integer mxpts,ibegin,npts, i, k
      
      do 1 i = 1, npts
        k = i + ibegin - 1
        outv(k) = inv(i)
1     continue

      return
      end
      
       
******************************************************************************
*
*     routine to scale a set of equations Ax = b by scaling the elements of
*          a and b
*
*     nr = number of rows to scale, nc = ncols
*     ir = first row to scale, ic = first col to scale
*
*     nrmax = maximum dimension of a and b in main program
*
******************************************************************************

      subroutine apply_weights(a,nrmax,ir,nr,ic,nc,b,wt)
      integer nrmax,nr,nc,ic,ir
      real a(nrmax,nc),b(nrmax),wt
      
      integer i,j
      
      n = ir + nr
      
      do 2 i = ir, n
        b(i) = b(i) * wt
        do 1 j = ic, nc
          a(i,j) = a(i,j) * wt
1       continue
2     continue

      return
      end

***********************************************************************
*
*     routine to output some event data using first file
*
*
      subroutine getevent_data(fname,ounit)
      character*256 fname
      integer ounit
      
      real evla,evlo,origin_time,obs
      integer nz(6),slen1,slen2
      character*32 ref_date, ref_time,ascfmt
      character*24 todays_date

*
*     read in the data header
*
      call rsac1(fname,obs,nptsdat,btime,dt2,1,nerr) 
*
      call getfhv('EVLA',evla,nerr)
      call getfhv('EVLO',evlo,nerr)
      call getfhv('O',origin_time,nerr)
      
      call getnhv('NZYEAR',nz(1),nerr)
      call getnhv('NZJDAY',nz(2),nerr)
      call getnhv('NZHOUR',nz(3),nerr)
      call getnhv('NZMIN',nz(4),nerr)
      call getnhv('NZSEC',nz(5),nerr)
      call getnhv('NZMSEC',nz(6),nerr)
      
      ihr = origin_time / 3600
      imin = (origin_time - 3600*ihr) / 60
      isec = (origin_time - 3600*ihr - 60*imin)
      imsec = 1000* (origin_time - 3600*ihr - 60*imin - isec)
      
      call kadate(nz(1),nz(2),32,ref_date,nerr)
      call katime(nz(3),nz(4),nz(5),nz(6),32,ref_time,nerr)
      
       write(ounit,*) 'Moment Tensor Inversion using l1mtinv:'
       call fdate(todays_date)
       write(ounit,*) 'Inversion run on: ',todays_date
       write(ounit,*)' '

       write(ounit,*)' '
       write(ounit,*)'Information from header of first data file: '
       
       slen1 = lnblnk(ref_date)
       slen2 = lnblnk(ref_time)
       
       write(ascfmt,'(a6,i2,a5,i2,a1)')
     &     '(a17,a'  , slen1 ,  ',4x,a' ,  slen2,   ')'
    
       write(ounit,ascfmt)'Reference Time: ',ref_date, ref_time
       write(ounit,*)'Origin time (s) relative to reference: ',origin_time
       write(ounit,'(a16,f7.3,2x,f8.3)')' Event Lat,Lon: ',evla, evlo
       write(ounit,*)' '
      
      return
      end
