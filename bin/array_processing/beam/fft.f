      subroutine fft(lx,cx,signi)
C                                                                               
C ********************* FFT ************************************                
C                                                                               
C   COMPLEX FOURIER TRANSFORM.                                                  
C                                                                               
C                      LX          SIGNI*2*PI*I*(J-1)*(K-1)/LX                  
C   CX(K)  =  SCALE * SUM CX(J) * E                                             
C                     J=1             FOR K=1,2,...,LX=2**INTEGER               
C                                                                               
      COMPLEX CX(LX),CARG,CEXP,CW,CTEMP                                             
      IF (SIGNI.EQ.-1.0) THEN
        SCALE = 1./LX 
      ELSE
        SCALE = 1.
      ENDIF
      DO 5 I=1,LX                                                              
    5 CX(I)=CX(I)*SCALE                                                         
      J=1                                                                       
      DO 30 I=1,LX                                                              
      IF (I.GT.J) GO TO 10                                                       
      CTEMP=CX(J)                                                               
      CX(J)=CX(I)                                                               
      CX(I)=CTEMP                                                               
   10 M=LX/2                                                                    
   20 IF (J.LE.M) GO TO 30                                                       
      J=J-M                                                                     
      M=M/2                                                                     
      IF (M.GE.1) GO TO 20                                                       
   30 J=J+M                                                                     
      L=1                                                                       
   40 ISTEP=2*L                                                                 
      DO 50 M=1,L                                                               
      CARG=(3.14159265*SIGNI*(M-1))/L*(0.,1.)
      CW=CEXP(CARG)
      DO 50 I=M,LX,ISTEP                                                        
      CTEMP=CW*CX(I+L)                                                          
      CX(I+L)=CX(I)-CTEMP                                                       
   50 CX(I)=CX(I)+CTEMP                                                         
      L=ISTEP                                                                   
      IF (L.LT.LX) GO TO 40                                                      
      RETURN                                                                    
      END                                                                       
