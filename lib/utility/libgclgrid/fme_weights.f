c------------------------------------------------------------
c  This is the primary function here that does interpolation 
c  by computing and applying a Jacobian transformation to warp
c  a set of 8 points into a standard set of coordinates (-1 to 1
c  in all three coordinate directions.  The routine computes 
c  a vector valued function evaluated at each point given
c
c Arguments:
c xx - 3 vector defining point in space to evaluate result at
c coord = array holding actual coordinates of 8 corner points
c fun = return vector of 8 weights to interpolate result for
c      point x using actual values at corners defined by coord.
c
c  Author:  Kagan Tuncay with modifications by G Pavlis
c  Written:  Oct-DEc 2000
c  Modified:  December 2002 Original subroutine found in older
c  libgclgrid interpolated a function based on values passed
c  in.  This function returns the weights so that multiple
c  calls with no change in position can be recomputed faster.
c  Modification:  June 2008
c  Found serious problems with convergence of the loop here
c  when a point was located just outside a distorted box.  
c  Repeated attempts to improve the lookup method to make this
c  go away failed.  The main thing this changed is that this
c  subroutine now returns and error code ierr.  0 means all is
c  fine while nonzero means the iteration in this routine
c  did not converge.  This is now used downstream in a recovery
c  using a different interpolation formula.
c------------------------------------------------------------

      subroutine fmeweights(xx,coord,fun,ierr)
      implicit none
      integer*4 ierr
      integer i,itno,j
      double precision x(3),xx(3),xdum
      double precision coord(8,3)
      double precision der(3,8),fun(8),jac(3,3)
      double precision residual(3),solution(3),er,det,jac1(3,3)
      integer MAXIT
      double precision CONVTEST
      parameter(MAXIT=20)
      parameter(CONVTEST=0.001)
      double precision scale

c  error flag set 0 as default
      ierr=0

c  We compute this scale in a bombproof way to make the code
c  independent of scale.  This is used in the convergence loop
c  below.  It is a little inefficient to do this, but safer
      scale = 0.0
      do j=2,8
            do i=1,3
                  scale = max(scale,abs(coord(j,i)-coord(j-1,i)))
            enddo
      enddo

      do i=1,3
         x(i)=0.
      enddo
      itno = 0

  100 continue
c
c     given x, derivatives of the interpolation functions with
c     respect to k1,k2,k3
c     der(i,j) -> i=1,3 j=1,8
c     
      call fmlin3(der,3,fun,x)
c
c     matmul -> der x coord = jac
c
      CALL MATMUL(DER,3,COORD,8,JAC,3,3,8,3)   
c
c     x=coord^T*F
c
      do i=1,3
         xdum=0.
         do j=1,8
            xdum=xdum+coord(j,i)*fun(j)
         enddo
         residual(i)=xdum
      enddo
c
c
c
      er=0.
      do i=1,3
         residual(i)=-(residual(i)-xx(i))
         er=er+residual(i)*residual(i)
      enddo
c
c     treex3 -> Jac^-1=Jac1
c
      CALL TREEX3(JAC,3,JAC1,3,DET)     
c
c     mvmult -> matrix x vector
c     jac*residual = solution
c
      call MVMULT(JAC1,3,residual,3,3,solution)

      do i=1,3
         x(i)=x(i)+solution(i)
      enddo

      er = er/scale
      itno = itno + 1
c	write(*,*) itno, er
      if( (itno.lt.MAXIT) .and. (er.gt.CONVTEST)) goto 100
c  Set ierr nonzero if the loop above didn't converge
      if(itno.ge.MAXIT) ierr=1

      return
      end

      SUBROUTINE GEO(IP,XCOORD,GG,COORD,ICOORD,NELEM,IEL,
     +NF,INF)
C
C     STEERING VECTOR AND COORDINATES
C 
      IMPLICIT NONE
      INTEGER INF,ICOORD,IEL
      DOUBLE PRECISION XCOORD(INF,*),COORD(ICOORD,*)
      INTEGER GG(24),NELEM(IEL,*),NF(*) 
      INTEGER INC,I,J,IP

      INC=0
      DO 1 I=1,8
      INC=INC+1
      GG(INC)=NF(NELEM(IP,I))
 1    CONTINUE
      DO 2 I=1,8
      DO 2 J=1,3
      COORD(I,J)=XCOORD(NELEM(IP,I),J)
 2    CONTINUE
 5    FORMAT(24I3)
      RETURN
      END

      SUBROUTINE FMLIN3(DER,IDER,FUN,xsi1)
C
C      FORMS THE SHAPE FUNCTIONS AND THEIR
C      DERIVATIVES FOR 8-NODED BRICK ELEMENTS
C
      IMPLICIT NONE
      INTEGER IDER
      DOUBLE PRECISION DER(IDER,8),FUN(8)
      DOUBLE PRECISION ETA,XI,ZETA
      DOUBLE PRECISION ETAM,XIM,ZETAM
      DOUBLE PRECISION ETAP,XIP,ZETAP,xsi1(3)
      XI=xsi1(1)
      ETA=xsi1(2)
      ZETA=xsi1(3)
      ETAM=1.-ETA
      XIM=1.-XI
      ZETAM=1.-ZETA
      ETAP=ETA+1.
      XIP=XI+1.
      ZETAP=ZETA+1.
      FUN(1)=.125*XIM*ETAM*ZETAM
      FUN(2)=.125*XIM*ETAM*ZETAP
      FUN(3)=.125*XIP*ETAM*ZETAP
      FUN(4)=.125*XIP*ETAM*ZETAM
      FUN(5)=.125*XIM*ETAP*ZETAM
      FUN(6)=.125*XIM*ETAP*ZETAP
      FUN(7)=.125*XIP*ETAP*ZETAP
      FUN(8)=.125*XIP*ETAP*ZETAM
      DER(1,1)=-.125*ETAM*ZETAM
      DER(1,2)=-.125*ETAM*ZETAP
      DER(1,3)=.125*ETAM*ZETAP
      DER(1,4)=.125*ETAM*ZETAM
      DER(1,5)=-.125*ETAP*ZETAM
      DER(1,6)=-.125*ETAP*ZETAP
      DER(1,7)=.125*ETAP*ZETAP
      DER(1,8)=.125*ETAP*ZETAM
      DER(2,1)=-.125*XIM*ZETAM
      DER(2,2)=-.125*XIM*ZETAP
      DER(2,3)=-.125*XIP*ZETAP
      DER(2,4)=-.125*XIP*ZETAM
      DER(2,5)=.125*XIM*ZETAM
      DER(2,6)=.125*XIM*ZETAP
      DER(2,7)=.125*XIP*ZETAP
      DER(2,8)=.125*XIP*ZETAM
      DER(3,1)=-.125*XIM*ETAM
      DER(3,2)=.125*XIM*ETAM
      DER(3,3)=.125*XIP*ETAM
      DER(3,4)=-.125*XIP*ETAM
      DER(3,5)=-.125*XIM*ETAP
      DER(3,6)=.125*XIM*ETAP
      DER(3,7)=.125*XIP*ETAP
      DER(3,8)=-.125*XIP*ETAP
      RETURN
      END

      SUBROUTINE TREEX3(JAC,IJAC,JAC1,IJAC1,DET)
C
C      FORMS THE INVERSE OF A 3 BY 3 MATRIX
C
      IMPLICIT NONE
      INTEGER IJAC,IJAC1
      DOUBLE PRECISION JAC(IJAC,*),JAC1(IJAC1,*)
      INTEGER K,L
      DOUBLE PRECISION DET

      DET=JAC(1,1)*(JAC(2,2)*JAC(3,3)-JAC(3,2)*JAC(2,3))
      DET=DET-JAC(1,2)*(JAC(2,1)*JAC(3,3)-JAC(3,1)*JAC(2,3))
      DET=DET+JAC(1,3)*(JAC(2,1)*JAC(3,2)-JAC(3,1)*JAC(2,2))
      JAC1(1,1)=JAC(2,2)*JAC(3,3)-JAC(3,2)*JAC(2,3)
      JAC1(2,1)=-JAC(2,1)*JAC(3,3)+JAC(3,1)*JAC(2,3)
      JAC1(3,1)=JAC(2,1)*JAC(3,2)-JAC(3,1)*JAC(2,2)
      JAC1(1,2)=-JAC(1,2)*JAC(3,3)+JAC(3,2)*JAC(1,3)
      JAC1(2,2)=JAC(1,1)*JAC(3,3)-JAC(3,1)*JAC(1,3)
      JAC1(3,2)=-JAC(1,1)*JAC(3,2)+JAC(3,1)*JAC(1,2)
      JAC1(1,3)=JAC(1,2)*JAC(2,3)-JAC(2,2)*JAC(1,3)
      JAC1(2,3)=-JAC(1,1)*JAC(2,3)+JAC(2,1)*JAC(1,3)
      JAC1(3,3)=JAC(1,1)*JAC(2,2)-JAC(2,1)*JAC(1,2)
      DO 1 K=1,3
      DO 1 L=1,3
      JAC1(K,L)=JAC1(K,L)/DET
    1 CONTINUE
      RETURN
      END      

      SUBROUTINE READELR(NX,NY,NZ,iel,NELEM)
C
C     FOR A DOMAIN WITH NX, NY, AND NZ IN X Y AND Z DIR.
C
      IMPLICIT NONE
      INTEGER IEL
      INTEGER N(8),NELEM(IEL,*)
      INTEGER NX,NY,NZ,nnn
      INTEGER II,I,K,J

      DO K=1,NZ
         DO J=1,NY
            DO  I=1,NX
               N(1)=(K-1)*(NX+1)*(NY+1)+(J-1)*(NX+1)+I
               N(2)=N(1)+(NX+1)*(NY+1)
               N(3)=N(2)+1
               N(4)=N(1)+1
               N(5)=N(1)+(NX+1)
               N(6)=N(5)+(NX+1)*(NY+1)
               N(7)=N(6)+1
               N(8)=N(5)+1
               NNN=NNN+1
               DO II=1,8
                  NELEM(NNN,II)=N(II)
               enddo
            enddo
         enddo
      enddo

      RETURN
      END

      SUBROUTINE MATMUL(A,IA,B,IB,C,IC,L,M,N)
C
C      FORMS THE PRODUCT OF TWO MATRICES
C
      IMPLICIT NONE
      INTEGER IA,IB,IC
      DOUBLE PRECISION  A(IA,*),B(IB,*),C(IC,*)
      INTEGER L,M,N,I,J,K
      DOUBLE PRECISION X

      DO 1 I=1,L
      DO 1 J=1,N
      X=0.0
      DO 2 K=1,M
    2 X=X+A(I,K)*B(K,J)
      C(I,J)=X
    1 CONTINUE
      RETURN
      END

      SUBROUTINE MATRAN(A,IA,B,IB,M,N)
C
C      FORMS THE TRANSPOSE OF A MATRIX
C
      IMPLICIT NONE
      INTEGER IA,IB
      DOUBLE PRECISION A(IA,*),B(IB,*)
      INTEGER M,N,I,J

      DO 1 I=1,M
      DO 1 J=1,N
    1 A(J,I)=B(I,J)
      RETURN
      END

      SUBROUTINE MVMULT(M,IM,V,K,L,Y)
C
C      MULTIPLIES A MATRIX BY A VECTOR
C
      IMPLICIT NONE
      INTEGER IM
      DOUBLE PRECISION M(IM,*),V(*),Y(*)
      INTEGER K,L,I,J
      DOUBLE PRECISION X

      DO 1 I=1,K
      X=0.
      DO 2 J=1,L
    2 X=X+M(I,J)*V(J)
      Y(I)=X
    1 CONTINUE
      RETURN
      END
