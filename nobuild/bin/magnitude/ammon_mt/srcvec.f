c====================================================================================
c
c  Synthetic Seismogram code for complete synthetics using spectral technique 
c    for Moment Tensor and Point Force sources in flat layered media
c
c    Funded by Treaty Verification Program, Lawrence Livermore National Lab
c
c    George Randall, Dept. of Geological Sciences, University of South Carolina
c           ger@tigger.seis.scarolina.edu
c    Steve Taylor, Los Alamos National Lab
c           taylor@beta.lanl.gov
c
c        based on 
c       "Seismic Wave Propagation in Stratified Media" B.L.N. Kennett, 
c        Cambridge University Press, 1983
c
c    Many useful discussions with and testing by
c    Chuck Ammon, Harley Benz, and Bill Walter
c    are gratefully acknowledged.
c
c
c====================================================================================
c
c  Version 1.0   February  1  1993
c
      subroutine srcvec( m, p, isrc  )
         integer m, isrc
         complex p
c
c     compute the upward and downward p and s waves from a
c     symmetric moment tensor source, for integer angular order m
c     and slowness p, for source isrc, (4.64) kennett
c
c     fix sign of +1 and -1 terms give results consistent with crfl and others
c
      include 'kennet.inc'
c
c     su,sv,sw,sp,ss,st are the components of the jump in
c     the displacement - stress vector
c     from kennett (1983) (4.59 - 4.60)
c
      complex su,sv,sw,sp,ss,st,i,zero
      data i,zero/(0.,1.),(0.,0.)/
c
c     calculate up and downgoing P,SV,SH at source (4.64) kennett
c
      if ( m .eq. 0 ) then
         su = mzz(isrc)/(rhos*alfas*alfas)
         ss = p*( .5*(mxx(isrc)+myy(isrc))
     *         - mzz(isrc)*(1. - 2.*betas*betas/(alfas*alfas)))
         pup0(isrc) = i*(mds21*ss - nds11*su)
         svup0(isrc) = i*(mds22*ss - nds12*su)
         pdn0(isrc) = i*(mus21*ss - nus11*su)
         svdn0(isrc) = i*(mus22*ss - nus12*su)
         shup0(isrc) = zero
         shdn0(isrc) = zero
       endif
      if ( m .eq. 1 ) then
         sv = -( mxz(isrc) - i*myz(isrc) )/(2.*rhos*betas*betas)
         sw = -(-myz(isrc) - i*mxz(isrc) )/(2.*rhos*betas*betas)
         pupp1(isrc) =-i*nds21*sv
         svupp1(isrc) =-i*nds22*sv
         pdnp1(isrc) =-i*nus21*sv
         svdnp1(isrc) =-i*nus22*sv
         shupp1(isrc) =-i*ndssh*sw
         shdnp1(isrc) =-i*nussh*sw
       endif
      if ( m .eq. -1 ) then
         sv = -(-mxz(isrc) - i*myz(isrc) )/(2.*rhos*betas*betas)
         sw = -( myz(isrc) - i*mxz(isrc) )/(2.*rhos*betas*betas)
         pupm1(isrc) = -i*nds21*sv
         svupm1(isrc) = -i*nds22*sv
         pdnm1(isrc) = -i*nus21*sv
         svdnm1(isrc) = -i*nus22*sv
         shupm1(isrc) = -i*ndssh*sw
         shdnm1(isrc) = -i*nussh*sw
       endif
      if ( m .eq. 2 ) then
         ss = .25*p*( myy(isrc) - mxx(isrc) + 2.*i*mxy(isrc) )
         st = .25*p*( i*(mxx(isrc) - myy(isrc)) + 2.*mxy(isrc) )
         pupp2(isrc) = i*mds21*ss
         svupp2(isrc) = i*mds22*ss
         pdnp2(isrc) = i*mus21*ss
         svdnp2(isrc) = i*mus22*ss
         shupp2(isrc) = i*mdssh*st
         shdnp2(isrc) = i*mussh*st
       endif
      if ( m .eq. -2 ) then
         ss = .25*p*( myy(isrc) - mxx(isrc) - 2.*i*mxy(isrc) )
         st = .25*p*( -i*(mxx(isrc) - myy(isrc)) + 2.*mxy(isrc) )
         pupm2(isrc) = i*mds21*ss
         svupm2(isrc) = i*mds22*ss
         pdnm2(isrc) = i*mus21*ss
         svdnm2(isrc) = i*mus22*ss
         shupm2(isrc) = i*mdssh*st
         shdnm2(isrc) = i*mussh*st
       endif
      return
      end
