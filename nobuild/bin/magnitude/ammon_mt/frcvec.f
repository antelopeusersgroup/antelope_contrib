      subroutine frcvec( m, p, isrc  )
         integer m, isrc
         complex p
c
c     compute the upward and downward p and s waves from a
c     point force source, for integer angular order m
c     and slowness p, for source isrc, (4.64) kennett
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
c     point force 
c
c     calculate up and downgoing P,SV,SH at source (4.64) kennett
c
      if ( m .eq. 0 ) then
	 sp = fz(isrc)
	 fpup0(isrc) = i*mds11*sp
	 fsvup0(isrc) = i*mds12*sp
	 fpdn0(isrc) = i*mus11*sp
	 fsvdn0(isrc) = i*mus12*sp
       endif
      if ( m .eq. 1 ) then
         ss = ( -fx(isrc) + i*fy(isrc) )*0.5
         st = ( i*fx(isrc) + fy(isrc) )*0.5
         fpupp1(isrc) = i*mds21*ss
         fsvupp1(isrc) = i*mds22*ss
         fpdnp1(isrc) = i*mus21*ss
         fsvdnp1(isrc) = i*mus22*ss
         fshupp1(isrc) = i*mdssh*st
         fshdnp1(isrc) = i*mussh*st
       endif
      if ( m .eq. -1 ) then
         ss = (  fx(isrc) + i*fy(isrc) )*0.5
         st = ( i*fx(isrc) - fy(isrc) )*0.5
         fpupm1(isrc) = i*mds21*ss
         fsvupm1(isrc) = i*mds22*ss
         fpdnm1(isrc) = i*mus21*ss
         fsvdnm1(isrc) = i*mus22*ss
         fshupm1(isrc) = i*mdssh*st
         fshdnm1(isrc) = i*mussh*st
       endif
      return
      end
