      complex function vslow(v,p,f)
      intrinsic csqrt,aimag,real,sqrt,abs
      complex v,p,f
      real t,eps
      parameter (eps = 0.001)
         vslow = csqrt( (1.,0.)/(v*v) - p*p )
         t = abs(real(vslow)) + abs(aimag(vslow))
         if ( t .lt. eps ) vslow = csqrt(eps*(-2.,-2.)/v)
         if ( aimag( f*vslow ) .gt. 0. ) vslow = -vslow
         return
      end
      complex function cphs( arg )
         complex arg
         intrinsic real, cexp
         real rmin
      parameter ( rmin = -20. )
         if ( real(arg) .lt. rmin ) then
            cphs = (0.,0.)
          else
            cphs = cexp(arg)
          endif
         return
      end
