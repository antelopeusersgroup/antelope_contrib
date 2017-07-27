       module trigd

	  implicit none

	  contains

	    function sind(x)
	      real*4 sind, x, pi180_sp
              pi180_sp=2.0e0 * asin(1.0e0) / 180.0e0
	      sind = sin(pi180_sp * x)
	    end function sind

	    function dsind(x)
	      real*8 dsind, x, pi180_dp
              pi180_dp=2.0d0 * asin(1.0d0) / 180.0d0
	      dsind = sin(pi180_dp * x)
	    end function dsind

	    function dasind(x)
	      real*8 dasind, x, pi180_dp
              pi180_dp=2.0d0 * asin(1.0d0) / 180.0d0
	      dasind = asin(x) / pi180_dp
	    end function dasind

	    function cosd(x)
	      real*4 cosd, x, pi180_sp
              pi180_sp=2.0e0 * asin(1.0e0) / 180.0e0
              print *,pi180_sp, x
	      cosd = cos(pi180_sp * x)
	    end function cosd

	    function dcosd(x)
	      real*8 dcosd, x, pi180_dp
              pi180_dp=2.0d0 * asin(1.0d0) / 180.0d0
	      dcosd = cos(pi180_dp * x)
	    end function dcosd

	    function dacosd(x)
	      real*8 dacosd, x, pi180_dp
              pi180_dp=2.0d0 * asin(1.0d0) / 180.0d0
	      dacosd = acos(x) / pi180_dp
	    end function dacosd

	    function atan2d(x,y)
	      real*4 atan2d, x, y, pi180_sp
              pi180_sp=2.0e0 * asin(1.0e0) / 180.0e0
	      atan2d = atan2(x,y) / pi180_sp
	    end function atan2d

	    function datan2d(x,y)
	      real*8 datan2d, x, y, pi180_dp
              pi180_dp=2.0d0 * asin(1.0d0) / 180.0d0
	      datan2d = atan2(x,y) / pi180_dp
	    end function datan2d

	end module trigd
