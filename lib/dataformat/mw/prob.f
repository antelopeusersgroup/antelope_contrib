      subroutine mdfd (rx, lm, ln, prob, ier)
C
C          Probabilities in several variance distributions
C
C  References:
C     Egon Dorrer, "Algorithm 322: F-Distribution [S14]",
C         Communications of the ACM, Vol 11(2), 1968, p116-117.
C     J.B.F. Field, "Certification of Algorithm 322",
C         Communications of the ACM, Vol 12(1), 1969, p39.
C     Hubert Tolman, "Remark on Algorithm 322 [S14]",
C         Communications of the ACM, Vol 14(2), 1979, p117.
C
C  Probabilities are returned as the integral of P(q)dq for q
C  in the range zero to X (F-ratio and chi-square) or from -X to +X
C  (Student's t and Normal).
C
C  For F-ratios:
C     M = numerator degrees of freedom
C     N = denominator degrees of freedom
C     X = F-ratio
C  For Student's t (two tailed):
C     M = 1
C     N = Degrees of freedom
C     X = The square of t
C  For normal deviates (two tailed):
C     M = 1
C     N = 5000
C     X = The square of the deviate
C  For chi-square:
C     M = Degrees of freedom
C     N = 5000
C     X = Chi-square/M
C
C  The returned probability will be in the range 0 to 1, unless an error
C  occurred, in which case -1.0 is returned.
C  Recoded by LW, 28-Mar-94.
C
      INTEGER M,N,LM,LN,A,B,I,J
      REAL*8    X,W,Y,Z,D,P,ZK
      REAL	RX, PROB
C
      ier = 0 
      X = RX
      M  = MIN(LM,300)
      N  = MIN(LN,5000)
      IF(MIN(M,N) .LT. 1) THEN
	  ier = 129 
          PROB = -1.0
          RETURN
      ENDIF
      if (rx .lt. 0.0) then
	ier = 130
	prob = -1.0
	return 
      endif
C
      A = 2*(M/2)-M+2
      B = 2*(N/2)-N+2
      W = X*FLOAT(M)/FLOAT(N)
      Z = 1.0/(1.0+W)
      IF(A .EQ. 1) THEN
          IF(B .EQ. 1)THEN
              P=SQRT(W)
              Y=0.3183098862
              D=Y*Z/P
              P=2.0*Y*ATAN(P)
          ELSE
              P=SQRT(W*Z)
              D=P*Z/(2.0*W)
          ENDIF
      ELSE
          IF(B .EQ. 1)THEN
              P=SQRT(Z)
              D=Z*P/2.0
              P=1.0-P
          ELSE
              D=Z*Z
              P=W*Z
          ENDIF
      ENDIF
      Y = 2.0*W/Z
      IF(A .EQ. 1) THEN
          DO 10 J=B+2,N,2
              D = (1.0 + FLOAT(A)/FLOAT(J-2)) * D * Z
              P = P + D * Y/FLOAT(J-1)
10        CONTINUE
      ELSE
          ZK = Z**((N-1)/2)
          D  = D * ZK * N/B
          P  = P * ZK + W * Z * (ZK-1.0)/(Z-1.0)
      ENDIF
      Y = W * Z
      Z = 2.0/Z
      B = N-2
      DO 20 I=A+2,M,2
          J = I + B
          D = (Y*D*FLOAT(J))/FLOAT(I-2)
20        P = P - Z * D/FLOAT(J)
      PROB = AMIN1(1.0,SNGL(P))
      PROB = AMAX1(0.0,PROB)
      RETURN
      END


c $Id$ 
