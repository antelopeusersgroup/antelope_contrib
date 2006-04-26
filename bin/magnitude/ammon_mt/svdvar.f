      SUBROUTINE SVDVAR(V,MA,NP,W,CVM,NCVM)
      PARAMETER (MMAX=20)
      DIMENSION V(NP,NP),W(NP),CVM(NCVM,NCVM),WTI(MMAX)
      DO 11 I=1,MA
        WTI(I)=0.
        IF(W(I).NE.0.) WTI(I)=1./(W(I)*W(I))
11    CONTINUE
      DO 14 I=1,MA
        DO 13 J=1,I
          SUM=0.
          DO 12 K=1,MA
            SUM=SUM+V(I,K)*V(J,K)*WTI(K)
12        CONTINUE
          CVM(I,J)=SUM
          CVM(J,I)=SUM
13      CONTINUE
14    CONTINUE
      RETURN
      END
