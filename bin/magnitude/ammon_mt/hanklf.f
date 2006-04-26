      subroutine hanklf( order, hnklfd, argarr, n )
        integer n
        real order, argarr(n)
        complex hnklfd(n)
c
c       compute the hankel function, multiplied by exp( + i arg )
c       for use with the generalized filon integration of
c       frazer and gettrust ( gjras v76, pp. 461-481 )
c       note the different fourier transform sign conventions....
c       computes for n arguments passed in array argarr
c
        intrinsic atan, cmplx, exp, sqrt, abs
        real argsqi, atan, az, bz, mu, pi, sqrt, arg
        real a1, a2, a3, b1, b2, b3, b4
        complex cmplx
        integer i
         pi = 4. * atan(1.)
         mu = 4. * order * order
         a1 = ( mu - 1. ) / 16.
         a2 =  ( mu - 13. )/8.
         a3 = (mu*mu -53.*mu +412.)/48.
         b1 =  ( mu - 1. )/8.
         b2 =  (mu - 25.)/48.
         b3 =  ( mu*mu -114.*mu + 1073.)/640.
         b4 = - pi*( order + 0.5 )/2.
         do 10 i = 1, n
            arg = argarr(i)
            if ( arg .lt. 1.5+ abs( order ) ) then
	      if ( arg .ne. 0. ) then
		hnklfd(i) = sqrt(2./(pi*arg)) * exp( cmplx( 0., b4 ) )
	      else
		hnklfd(i) = ( 0., 0. )
	      endif
             else
              argsqi = 1./ ( arg * arg )
              az = a1*argsqi*( 1. + ( a2 + a3*argsqi)*argsqi )
c
c             bz = arg + (b1/arg)*( 1. + ( b2 + b3*argsqi)*argsqi) + b4
c             modified for generalized filon, multiply by exp( + i arg )
c
              bz = (b1/arg)*( 1. + ( b2 + b3*argsqi)*argsqi) + b4
              hnklfd(i) = sqrt(2./(pi*arg)) * exp(cmplx(az,-bz))
             endif
 10      continue
         return
      end
