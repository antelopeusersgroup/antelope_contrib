*
*      deviatoric moment tensor inversion
*
*      Time function is fixed, so problem is linear
*
*      Author:  Chuck Ammon, UC Santa Cruz, Saint Louis University
*               parts by George Randall, U South Carolina
*
*      Version: 2.1 September, 1994
*
*      Notation from Langston (1981)
*        x is north, y is east, z is down
*        all signs are opposite Aki and Richards due to a different
*        choice of the direction for the fault normal
*
       program mtinv
*
       integer maxpts, nprm, nmax, mxwvs
       real deg_to_rad, rad_to_deg
       parameter(maxpts=1024, nprm = 5, mxwvs = 30, nmax = mxwvs*maxpts)
       parameter(deg_to_rad = 0.017453292, rad_to_deg = 57.29577951)
*
       common /gftns/ vss(maxpts),vds(maxpts),ffds(maxpts),
     &                response(maxpts)
       common /observations/ obs(maxpts)
*
       real pred(maxpts)
       real a(nmax,nprm), ata(nprm,nprm), asave(nmax,nprm)
       real data(nmax), datasv(nmax), atb(nprm)
       real mtensor(6),  sval(nprm), v(nprm,nprm), tsval(nprm)
       real covmatrix(nprm,nprm)
       real btime, dt, m0
       real az(mxwvs), wt(mxwvs)
       real sum_sq_amp(mxwvs)
       real obswt(mxwvs),err, total_sum_sq, total_sum_sq_wt
       integer ixx,iyy,ixy,ixz,iyz, wvtype(mxwvs), iPz, iPr, iSH
       integer i, nerr, etype,  nr,nc, ibegin, npts(mxwvs)
       integer slen, lnblnk, itotal_pts
       integer stdin,stdout,ounit,inunit
       character*256 obs_name(mxwvs), gftn_name(mxwvs), infile, ofile
       character*258 wvfile
       character*32 ascfmt
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
       open(unit = ounit, file = ofile)
       write(ounit,*)'*********** L2 INVERSION **********************'
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
         write(ounit,*) ' Sum of Square Amplitudes: ',sum_sq_amp(i)
         write(ounit,*) ' Weight:  ',obswt(i)
         write(ounit,*) ' '
*
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
*     FORM THE NORMAL EQUATIONS
*
*     set up the number of rows in the a matrix
*
      nr = ibegin - 1
*
*     for now, only 5 elements of moment tensor are estimated
*
      nc = 5
*
*     set up the normal equations
*  
      call atrans_a(a,nr,nc,nmax,ata)
      call atrans_b(a,nr,nc,nmax,data,atb)
*
*     solve the normal equations using an SVD
*
      call svdcmp(ata,nprm,nprm,nprm,nprm,sval,v)
*
      write(ounit,*)' '     
      write(ounit,*)' Singular Values of A**t A:'     
      do 150 i = 1,5
         write(ounit,*) sval(i)
150   continue
*
      write(ounit,*)' '     
      write(ounit,*)' Singular Values of A:'     
      do 151 i = 1,5
	 tsval(i) = sqrt(sval(i))
         write(ounit,*) tsval(i)
151   continue
*
*     solve the AtAx = Atb system
*
      call svbksb(ata,sval,v,nprm,nprm,nprm,nprm,atb,mtensor)
*
      mtensor(6) = -(mtensor(1) + mtensor(2))
*
      call gmoment(mtensor,m0,ounit)
*
*   Compute the Covariance Matrix
*
       call svdvar(v,nprm,nprm,tsval,covmatrix,nprm)
       call print_cm(covmatrix,nprm,m0,ounit)
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
         wvfile(1:slen+2) = 'p_'//obs_name(i)(1:slen)     
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
*        the value of sum_sq_amp(i) != 0, this is checked when first computed
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
*      output the sum of the square errors divided by the number of traces
*        minus the number of unknowns (free parameters)
*
       tmpsig = 1.0 / float(n-6)
*
       write(ounit,*) ' '
       write(ounit,*)
     & 'Unweighted SSE,Weighted SSE,Unweighted RMS,Weighted RMS'
       write(ounit,*) total_err*tmpsig, total_wtd*tmpsig,
     &                 sqrt(total_err*tmpsig), sqrt(total_wtd*tmpsig)
*
*      Output sum of square errors divided by the sum of the square amplitudes.
*
       write(ounit,*)
     & 'Total Error divided by sum of all seismogram amplitudes'
       write(ounit,*)
     & 'Unweighted, weighted'
       write(ounit,*) total_err/total_sum_sq, total_wtd/total_sum_sq_wt
*
*      Perform the Hext analysis of HJP in preparation for plotting the
*      error ellipses on focal sphere about the P/T/B axes
*
       call hjp_hext(mtensor,covmatrix,nprm,total_err*tmpsig,ounit)
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
       parameter(maxpts=1024, nprm = 5, mxwvs = 30, nmax = mxwvs*maxpts)
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
       
       call getfhv('EVDP',depth,nerr)
*
       write(ounit,*) ' '
       write(ounit,*) 'Greens Function summary:'
       if(slen .lt. 10) then
          write(ascfmt,'(a6,i1,a1)')'(a13,a',slen,')'
       else
c KGLfix  write(ascfmt,'(a5,i2,a1)')'(a13,a',slen,')'
          write(ascfmt,'(a6,i2,a1)')'(a13,a',slen,')'
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
      parameter(maxpts=1024, nprm = 5, mxwvs = 30, nmax = mxwvs*maxpts)
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
      
*******************************************************************

      subroutine atrans_a(a,nr,nc,nmax,ata)
      real a(nmax,nc), ata(nc,nc)
      integer nr, nc, r, i, j
      
      do 3 i = 1, nc
        do 2 j = 1, nc
           ata(i,j) = 0
           do 1 r = 1, nr
             ata(i,j) = ata(i,j) + a(r,i)*a(r,j)
1          continue
2       continue
3     continue

      return
      end
      
*******************************************************************    
  
      subroutine atrans_b(a,nr,nc,nmax,b,atb)
      real a(nmax,nc), atb(nc),b(nr)
      integer nr, nc, r, i
      
      do 2 i = 1, nc
           atb(i) = 0
           do 1 r = 1, nr
             atb(i) = atb(i) + a(r,i)*b(r)
1          continue
2     continue

      return
      end
      
*******************************************************************

      subroutine truncate(singval,nmodparam,minfraction,
     &           ntruncated,maxmp)
      real singval(maxmp), minfraction
      integer nmodparam, ntruncated

      ntruncated = 0
      smax = 0.0
      do 1 i = 1, nmodparam
	if(singval(i) .gt. smax) smax = singval(i)
1     continue
      
      smin = smax * minfraction
      do 2 i = 1, nmodparam
	if(singval(i) .lt. smin) then
	   singval(i) = 0.0
	   ntruncated = ntruncated + 1
        end if
2     continue
	
      return
      end

*******************************************************************

      subroutine print_cm(cm,n,m0,ounit)
      real cm(n,n),m0
      integer n, ounit
      
      integer i,j,stdin,stdout
      
      stdin = 5
      stdout = 6
      
c     write(ounit,90) m0
      write(ounit,90) 
      write(ounit,91)
            
      do 10 i = 1,n
c       write(ounit,100) (cm(i,j)/m0, j = 1,i)
        write(ounit,100) (cm(i,j), j = 1,i)
10    continue

c90   format(/,'Covariance Matrix',' ( x ',e10.3,' )')
 90   format(/,'Covariance Matrix' )
 91   format('   xx          yy          xy         xz         yz')

100   format(5(e10.3,1x))

      write(ounit,*)

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

*******************************************************************
*
*     take the Mij and Cov(Mij,Mkl) tensors and perform the Hext (1963)
*     computations, being careful to reorder the input tensors into the
*     order used by Howard Patton's implementation of the Hext 
*
*******************************************************************

      subroutine hjp_hext(mij,cm,n,sigsq,ounit)
      real mij(n),cm(n,n),sigsq
      integer n, ounit
      
      integer i,j,stdin,stdout
      integer junit,munit
      integer ixx,iyy,ixy,ixz,iyz
      integer hjpxx,hjpyy,hjpxy,hjpxz,hjpyz,hjpzz
      integer hextxx,hextyy,hextxy,hextxz,hextyz,hextzz
      real dyncm, degrad, arg
      real smt(6), e(3,3), delm(3), ev(9), esave(3,3)
      real wroot(2,3), windx(2,3), wegnv(2,2,3)
      real amajor(3), bminor(3), thknot(3), mcov(6,6)
      equivalence (e(1,1),ev(1))
              
*
*     Using 1-N 2-E 3-D
*
      ixx = 1
      iyy = 2
      ixy = 3
      ixz = 4
      iyz = 5
      
*
*     Using 1-E 2-N 3-U
*     Re-arrange as
*     Mxx <-> Myy ;  -Mxz -> Myz ; -Myz -> Mxz ;  for the transformation
*
      hjpxx = 1
      hjpxy = 2
      hjpyy = 3
      hjpxz = 4
      hjpyz = 5
      hjpzz = 6
      
      hextxx = 1
      hextyy = 2
      hextzz = 3
      hextyz = 4
      hextxz = 5
      hextxy = 6
      
      stdin = 5
      stdout = 6

      junit = 7
      munit = 8

      degrad = 0.01745329252
      dyncm = 1.0

*
*     Scale the unit covariance by the noise (misfit) variance)
*
      do 10 i = 1,n
      do 10 j = 1,n
        cm(i,j) = cm(i,j) * sigsq
10    continue
      
      write(ounit,90) 
      write(ounit,91)
            
      do 11 i = 1,n
        write(ounit,99) (cm(i,j), j = 1,i)
11    continue

 90   format(/,'Covariance Matrix' )
 91   format('   xx          yy          xy         xz         yz')

 99   format(5(e10.3,1x))

      write(ounit,*)


*
*     Copy the Mij into Howard's order, adding Mzz = -(Mxx+Myy)
*
      smt( hjpxx ) = mij( iyy )
      smt( hjpxy ) = mij( ixy )
      smt( hjpyy ) = mij( ixx )
      smt( hjpxz ) = -mij( iyz )
      smt( hjpyz ) = -mij( ixz )
      smt( hjpzz ) = -( mij( ixx ) + mij( iyy ) )

*
*     Copy the scaled Coavriance matrix into upper diagonal in Howard's order
*     and calculate Cov(Mzz,Mij) = -( Cov(Mxx,Mij) + Cov(Myy,Mij) )
*     with Cov(Mzz,Mzz) = Cov(-(Mxx+Myy),-(Mxx+Myy) ) 
*     Also need to account for the different Mij coordinate systems as above.
*
      mcov( hextxx, hextxx ) = cm( iyy, iyy )
      mcov( hextxx, hextyy ) = cm( iyy, ixx )
      mcov( hextxx, hextzz ) = -( cm( iyy, iyy ) + cm( iyy, ixx) )
      mcov( hextxx, hextyz ) = -cm( iyy, ixz )
      mcov( hextxx, hextxz ) = -cm( iyy, iyz )
      mcov( hextxx, hextxy ) = cm( iyy, ixy )

      mcov( hextyy, hextyy ) = cm( ixx, ixx )
      mcov( hextyy, hextzz ) = -( cm( ixx, ixx ) + cm( ixx, iyy) )
      mcov( hextyy, hextyz ) = -cm( ixx, ixz )
      mcov( hextyy, hextxz ) = -cm( ixx, iyz )
      mcov( hextyy, hextxy ) = cm( ixx, ixy )
      
      mcov( hextzz, hextzz ) = cm( ixx, ixx ) + cm( iyy, iyy ) 
     1                         + 2.* cm( ixx, iyy)
      mcov( hextzz, hextyz ) = -( -cm( ixx, ixz ) - cm( iyy, ixz ) )
      mcov( hextzz, hextxz ) = -( -cm( ixx, iyz ) - cm( iyy, iyz ) )
      mcov( hextzz, hextxy ) = -( cm( ixx, ixy ) + cm( iyy, ixy ) )

      mcov( hextyz, hextyz ) = cm( ixz, ixz )
      mcov( hextyz, hextxz ) = cm( ixz, iyz )
      mcov( hextyz, hextxy ) = -cm( ixz, ixy )

      mcov( hextxz, hextxz ) = cm( iyz, iyz )
      mcov( hextxz, hextxy ) = -cm( iyz, ixy )

      mcov( hextxy, hextxy ) = cm( ixy, ixy )

*
*     Fill in the mcov by symmetry
*
      do 15 i = 1,5
      do 15 j = i+1,6
        mcov(j,i) = mcov(i,j)
15    continue

      call eigens(smt,ev,3,0)
c
      call hext(smt,e,mcov,delm,wroot,wegnv,windx)
c
      smt(1)=smt(1)*dyncm
      smt(2)=smt(3)*dyncm
      smt(3)=smt(6)*dyncm
c
      do 40 i=1,3
40    delm(i)=delm(i)*dyncm
c
      do 305 i=1,3
      amajor(i)=2.79*sqrt(wroot(1,i))
      bminor(i)=2.79*sqrt(wroot(2,i))
      arg=wegnv(2,1,i)/wegnv(1,1,i)
      thknot(i)=atan(arg)/degrad
c
      do 310 j=1,3
310   esave(j,i)=e(j,i)
c
305   continue
c
      do 55 i=1,3
      call azdip(e(1,i),az,dip)
      e(1,i)=az
55    e(2,i)=dip
c
      write(junit,60)
      write(junit,75) (i,smt(i),delm(i),(e(ii,i),ii=1,2),i=1,3)
60    format(/10x,'deviatoric tensor in diagonalized form:')
   75 format(/10x,'lambda',i1,':',1p1e10.2,' +/- ',e8.2,1x,
     * 'dyn-cm,',1x,'az=',0p1f7.1,1x,'deg e. of north,',1x,'dip=',
     * f5.1,1x,'deg')
c
c     write out eigenvector orientations to file
c
      write(munit,100) depth,((e(ii,i),ii=1,2),i=1,3)
100   format('depth=',f5.1,', t: az=',f7.1,', dip=',f5.1,
     *                     ', b: az=',f7.1,', dip=',f5.1,
     *                     ', p: az=',f7.1,', dip=',f5.1)
      write(munit,300) ((esave(j,i),j=1,3),amajor(i),bminor(i),
     * thknot(i),i=1,3)
300   format(5(1x,f7.4),1x,f7.1)


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
      
       write(ounit,*) 'Moment Tensor Inversion using mtinv:'
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
