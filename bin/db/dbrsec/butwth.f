

c
c*******************************************************************************
c
c      Subroutine SETBFL
c
c      Created by Danny Harvey on May 23, 1989
c
c*******************************************************************************c
      SUBROUTINE SETBFL (FL, OL, FU, OU, DT)
c
      REAL*4 FL
      INTEGER*4 OL
      REAL*4 FU
      INTEGER*4 OU
      REAL*4 DT
c
c    Subroutine SETBFL will compute the recursion coefficients for a bandpass
c    minimum phase butterworth filter using a pre-warped bilinear s to z
c    transform. These coefficients are then stored in a common block and are
c    used by subroutine FILREC which applies the recursive filter to a sampled
c    data array.
c
c    Inputs  -	FL	= The lower cutoff frequency in hz. If this is zero,
c			  then the filter will be pure low pass.
c		OL	= The order of the lower cutoff sideband. If this zero,
c			  then the filter will be pure low pass.
c    		FU	= The upper cutoff frequency in hz. If this is zero,
c			  then the filter will be pure high pass.
c		OU	= The order of the upper cutoff sideband. If this zero,
c			  then the filter will be pure high pass.
c		DT	= The sampling increment in seconds of the data.
c
c    Parameters are defined as follows:
c		NSMAX	= The maximum total number of filter stages.
c
      PARAMETER  (NSMAX  =  100)
c
      COMMON /BUTFLC/ NS, IS(NSMAX), B(NSMAX), C(NSMAX), GAIN,
     1                    F(NSMAX), FM1(NSMAX), FM2(NSMAX),
     2                              RM1(NSMAX), RM2(NSMAX),
     3                              XIM1(NSMAX), XIM2(NSMAX),
     4                              XOM1(NSMAX), XOM2(NSMAX)
c
c    Get the filter stages in the s-domain.
c
      ns = 0
      IF (OL+OU .GT. NSMAX) RETURN
      IF (FU .EQ. 0.0 .OR. OU .EQ. 0) THEN
	ns = 0
	gain = 1.0
      ELSE
        CALL BUTSTA (OU, NSOUT, IS, B, C)
        CALL L2L (FU, NSOUT, IS, B, C, GN)
        NS = NSOUT
	GAIN = GN
      END IF
      IF (FL .EQ. 0.0 .OR. OL .EQ. 0) THEN
      ELSE
        CALL BUTSTA (OL, NSOUT, IS(NS+1), B(NS+1), C(NS+1))
        CALL L2H (FL, NSOUT, IS(NS+1), B(NS+1), C(NS+1), GN)
        NS = NS + NSOUT
	GAIN = GAIN * GN
      END IF
c
c    Compute the recursion coefficients.
c
      CALL RECOEF (DT, 0, 1)
c
c    Normal Exit
c
      RETURN
      END
c
c*******************************************************************************
c
c    Subroutine BUTSTA
c
c    Created by Danny Harvey on July 12, 1986
c
c*******************************************************************************
c
      SUBROUTINE BUTSTA (NORDER, NSOUT, IS, B, C)
c
c    Subroutine BUTSTA will compute a set of simple filter stages
c    from an nth order Butterworth low pass filter. This is for
c    normalized frequency with the cutoff frequency at one rad/sec.
c
c    Inputs  - NORDER = Order of the filter.
c
c    Outputs - NSOUT  = The number of stages required to describe the filter.
c              IS(NSOUT) = A stage type flag for each stage.
c                          = 0 - Stage is 1./(S + C(I))
c                          = 1 - Stage is 1./(S**2 + B(I)*S + C(I))
c                          = 2 - Stage is S/(S + C(I))
c                          = 3 - Stage is S**2/(S**2 + B(I)*S + C(I))
c              B(NSOUT),C(NSOUT) = The coefficients for each stage.
c
      DIMENSION  IS(NORDER), B(NORDER), C(NORDER)
c
      DATA  PI / 3.141592653589793 /
c
      NSOUT = 0
      IF (NORDER .LT. 1) THEN
        RETURN
      END IF
      IF (NORDER .EQ. 2*(NORDER/2)) THEN
        NODD = 0
      ELSE
        NODD = 1
      END IF
      IF (NODD .EQ. 1) THEN
        NSOUT = 1
        IS(1) = 0
        C(1) = 1.
        IF (NORDER .EQ. 1) RETURN
        M = (NORDER-1)/2
        XK = 1.
      ELSE
        M = NORDER/2
        XK = 0.5
      END IF
      XN = PI/FLOAT(NORDER)
      DO 10  I = 1,M
        NSOUT = NSOUT + 1
        IS(NSOUT) = 1
        C(NSOUT) = 1.
        B(NSOUT) = 2.0*COS(XK*XN)
        XK = XK + 1.
   10 CONTINUE
C
      RETURN
C
      END
c
c*******************************************************************************
c
c    Subroutine L2L
c
c    Created by Danny Harvey on July 12, 1986
c
c*******************************************************************************
c
      SUBROUTINE L2L (FTRAN, NS, IS, B, C, GAIN)
c
c    Subroutine L2L will perform a lowpass to lowpass frequency band
c    transformation on a normalized cutoff low pass filter.
c
c    Inputs  - FTRAN  = The desired cutoff frequency in Hz.
c              NS     = The number of filter stages.
c              IS(NS) = A stage type flag for each stage.
c                       = 0 - Stage is 1./(S + C(I))
c                       = 1 - Stage is 1./(S**2 + B(I)*S + C(I))
c                       = 2 - Stage is S/(S + C(I))
c                       = 3 - Stage is S**2/(S**2 + B(I)*S + C(I))
c              B(NS),C(NS) = The coefficients for each stage.
c
c    Outputs - B(NS),C(NS) = The transformed stage coefficients
c              GAIN  = A total filter gain function which will normalize
c                      the filter response at zero frequency.
c
      DIMENSION  IS(NS), B(NS), C(NS)
c
      DATA  PI / 3.141592653589793 /
C
      GAIN = 1.
      W = FTRAN*2.*PI
      W2 = W*W
      DO 10  I = 1, NS
        IF (IS(I) .EQ. 0) THEN
          GAIN = GAIN*W
          C(I) = C(I)*W
        ELSE IF (IS(I) .EQ. 1) THEN
          GAIN = GAIN*W2
          B(I) = B(I)*W
          C(I) = C(I)*W2
        ELSE IF (IS(I) .EQ. 2) THEN
          C(I) = C(I)*W
        ELSE
          B(I) = B(I)*W
          C(I) = C(I)*W2
        END IF
   10 CONTINUE
C
      RETURN
C
      END
c
c*******************************************************************************
c
c    Subroutine L2H
c
c    Created by Danny Harvey on July 12, 1986
c
c*******************************************************************************
c
      SUBROUTINE L2H (FTRAN, NS, IS, B, C, GAIN)
c
c    Subroutine L2H will perform a lowpass to highpass frequency band
c    transformation on a normalized cutoff low pass filter.
c
c    Inputs  - FTRAN  = The desired cutoff frequency in Hz.
c              NS     = The number of filter stages.
c              IS(NS) = A stage type flag for each stage.
c                       = 0 - Stage is 1./(S + C(I))
c                       = 1 - Stage is 1./(S**2 + B(I)*S + C(I))
c                       = 2 - Stage is S/(S + C(I))
c                       = 3 - Stage is S**2/(S**2 + B(I)*S + C(I))
c              B(NS),C(NS) = The coefficients for each stage.
c
c    Outputs - IS(NS)= The transformed stage types.
c              B(NS),C(NS) = The transformed stage coefficients.
c              GAIN  = A total filter gain function which will normalize
c                      the filter response at infinite frequency.
c
      DIMENSION  IS(NS), B(NS), C(NS)
c
      DATA  PI / 3.141592653589793 /
C
      GAIN = 1.
      W = FTRAN*2.*PI
      W2 = W*W
      DO 10  I = 1, NS
        IF (IS(I) .EQ. 0) THEN
          GAIN = GAIN/C(I)
          IS(I) = 2
          C(I) = W/C(I)
        ELSE IF (IS(I) .EQ. 1) THEN
          IS(I) = 3
          GAIN = GAIN/C(I)
          B(I) = B(I)*W/C(I)
          C(I) = W2/C(I)
        ELSE IF (IS(I) .EQ. 2) THEN
          IS(I) = 0
          GAIN = GAIN*W/C(I)
          C(I) = W/C(I)
        ELSE
          IS(I) = 1
          GAIN = GAIN*W2/C(I)
          B(I) = B(I)*W/C(I)
          C(I) = W2/C(I)
        END IF
   10 CONTINUE
C
      RETURN
C
      END
c
c******************************************************************************
c
c    Subroutine RECOEF
c
c    Created by Danny Harvey on July 25, 1986
c
c******************************************************************************
c
      SUBROUTINE RECOEF (DT, ITRAN, IWARP)
c
c    Subroutine RECOEF will compute digital filter recursion coefficients.
c
c    Inputs  - DT     = Sampling increment in seconds.
c              ITRAN  = Transform type flag.
c                       = 0 - Bilinear s to z transform.
c              IWARP  = Pre-warping flag (used only for ITRAN = 0).
c                       = 0 - Do not pre-warp.
c                       = 1 - pre-warp.
c
c    Parameters are defined as follows:
c        NSMAX  = Maximum number of filter stages.
c
      PARAMETER  (NSMAX  =  100)
c
      COMMON /BUTFLC/ NS, IS(NSMAX), B(NSMAX), C(NSMAX), GAIN,
     1                    F(NSMAX), FM1(NSMAX), FM2(NSMAX),
     2                              RM1(NSMAX), RM2(NSMAX),
     3                              XIM1(NSMAX), XIM2(NSMAX),
     4                              XOM1(NSMAX), XOM2(NSMAX)
c
      DATA  PI  / 3.141592653589793 /
c
      do 100  i = 1, ns
        if (is(i) .eq. 0) then
          omega = c(i)
          if (iwarp .eq. 1) then
            x = omega*dt
            if (x .ge. pi) x = 0.999*pi
            omega = 2.*tan(0.5*x)/dt
          end if
          den = 2. + omega*dt
          f(i) = omega*dt/den
          fm1(i) = f(i)
          fm2(i) = 0.
          rm1(i) = (2. - omega*dt)/den
          rm2(i) = 0.
        else if (is(i) .eq. 1) then
          omega = sqrt(c(i))
          z = 0.5*b(i)/omega
          if (iwarp .eq. 1) then
            x = omega*dt
            if (x .ge. pi) x = 0.999*pi
            omega = 2.*tan(0.5*x)/dt
          end if
          cc = omega*omega
          bb = 2.*z*omega
          den = 4. + 2.*dt*bb + cc*dt*dt
          f(i) = cc*dt*dt/den
          fm1(i) = 2.*f(i)
          fm2(i) = f(i)
          rm1(i) = (8. - 2.*cc*dt*dt)/den
          rm2(i) = (2.*dt*bb - 4. - cc*dt*dt)/den
        else if (is(i) .eq. 2) then
          omega = c(i)
          if (iwarp .eq. 1) then
            x = omega*dt
            if (x .ge. pi) x = 0.999*pi
            omega = 2.*tan(0.5*x)/dt
          end if
          den = 2. + omega*dt
          f(i) = 2./den
          fm1(i) = -f(i)
          fm2(i) = 0.
          rm1(i) = (2. - omega*dt)/den
          rm2(i) = 0.
        else
          omega = sqrt(c(i))
          z = 0.5*b(i)/omega
          if (iwarp .eq. 1) then
            x = omega*dt
            if (x .ge. pi) x = 0.999*pi
            omega = 2.*tan(0.5*x)/dt
          end if
          cc = omega*omega
          bb = 2.*z*omega
          den = 4. + 2.*dt*bb + cc*dt*dt
          f(i) = 4./den
          fm1(i) = -2.*f(i)
          fm2(i) = f(i)
          rm1(i) = (8. - 2.*cc*dt*dt)/den
          rm2(i) = (2.*dt*bb - 4. - cc*dt*dt)/den
        end if
  100 continue
c
c    Normal exit
c
      return
c
c    End of Subroutine RECOEF
c
      end
c
c******************************************************************************
c
c    Subroutine INIFIL
c
c******************************************************************************
c
      SUBROUTINE INIFIL (XIN)
c
      REAL*4             XIN
c
c    Subroutine INIFIL will initialize the recursion previous values to
c    be consistent with a constant input value. This will minimize startup
c    transients.
c
c    Input -	XIN	= The constant input value.
c
c    Parameters are defined as follows:
c        NSMAX  = Maximum number of filter stages.
c
      parameter  (nsmax  =  100)
c
      COMMON /BUTFLC/ NS, IS(NSMAX), B(NSMAX), C(NSMAX), GAIN,
     1                    F(NSMAX), FM1(NSMAX), FM2(NSMAX),
     2                              RM1(NSMAX), RM2(NSMAX),
     3                              XIM1(NSMAX), XIM2(NSMAX),
     4                              XOM1(NSMAX), XOM2(NSMAX)
c
      yin = xin
      do 10  i = 1, ns
	yout = yin*(f(i) + fm1(i) + fm2(i))/(1.0 - rm1(i) - rm2(i))
	xim1(i) = yin
	xim2(i) = yin
	xom1(i) = yout
	xom2(i) = yout
	yin = yout
   10 continue
c
      RETURN
      END
c
c******************************************************************************
c
c    Subroutine FILREC
c
c    Created by Danny Harvey on May 24, 1989
c
c******************************************************************************
c
      SUBROUTINE FILREC (NPTS, XIN, ISAVE, XOUT)
c
      REAL*4 XIN(npts)
      REAL*4 XOUT(npts)
c
c    Subroutine FILREC will filter an input sample array using
c    a recursive digital filter.
c
c    Inputs - 	NPTS		= The number of points in the input array.
c		XIN(NPTS)	= Input sample array.
c		ISAVE		= Save output flag.
c				  If this is 0, then the output array, XOUT,
c				  is not used. This allows the recursion
c				  previous values to be stablized by running
c				  the filter with an input time pad without
c				  saving the results.
c				  If this is not 0, then the output filtered
c				  values are saved in XOUT.
c
c    Output -	XOUT(NPTS)	= Filtered output samples.
c
c    Parameters are defined as follows:
c        NSMAX  = Maximum number of filter stages.
c
      parameter  (nsmax  =  100)
c
      COMMON /BUTFLC/ NS, IS(NSMAX), B(NSMAX), C(NSMAX), GAIN,
     1                    F(NSMAX), FM1(NSMAX), FM2(NSMAX),
     2                              RM1(NSMAX), RM2(NSMAX),
     3                              XIM1(NSMAX), XIM2(NSMAX),
     4                              XOM1(NSMAX), XOM2(NSMAX)
c
c    Loop over the input samples and apply the filter.
c
      do 50  j = 1, npts
        yin = xin(j)
        do 100  i = 1, ns
          yout = yin*f(i) + xim1(i)*fm1(i) + xim2(i)*fm2(i)
     1                    + xom1(i)*rm1(i) + xom2(i)*rm2(i)
          xom2(i) = xom1(i)
          xom1(i) = yout
          xim2(i) = xim1(i)
          xim1(i) = yin
          yin = yout
  100   continue
        if (isave .ne. 0) xout(j) = yout
   50 continue
c
c    Normal exit
c
      return
c
c    End of Subroutine FILREC
c
      end

c $Id$ 
