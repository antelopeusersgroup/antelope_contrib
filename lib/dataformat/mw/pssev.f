      subroutine pssev(ndim,ndata,nefn,w,rtol,atol,efn,evalu,evalu1)
c
c  computes eigenvalues for the discrete prolate spheroidal sequences
c  in efn by integration of the corresponding squared discrete prolate
c  spheroidal wavefunctions over the inner domain
c
c  ndim   - leading dimension of efn
c  ndata  - actual number of time points in each column of efn
c  nefn   - number of dpss (= number of columns in efn)
c  w      - the bandwidth (= time-bandwidth product/ndata)
c  rtol   - relative error tolerance for the integration. this is
c           roughly equal to the number of significant figures in the
c           result.
c  atol   - absolute error tolerance for the integration. this should
c           be set to 10**-n, where n is the number of significant figures
c           that can be be represented on the machine.
c  efn    - array containing the dpss
c  evalu  - vector of length nefn to contain the eigenvalues
c  evalu1 - vector of length nefn to contain (1-eigenvalues)
c
      implicit double precision (a-h,o-z)
      dimension efn(ndim,nefn),evalu(nefn),evalu1(nefn),result(8)
      external pssevf
c
c  calls dgqif,d1mach
c
      small=1000.*d1mach(4)
      do 20 k=1,nefn
        call dgqif(0.d0,w,result,kk,rtol,atol,7,icheck,pssevf,
     $             efn(1,k),ndata)
        if (icheck.ne.0) then
          write(*,*) ' dgqif did not converge for evalu(k) k =',k
        endif
        evalu(k)=2*result(kk)
c
c  calculate 1-eigenvalues, explicitly if necessary
c
        if((1.0d0-evalu(k)).le.small)then
c
c  break up 1-evalu calculation into reasonable pieces so dgqif can converge
c
          sum=0.d0
          do 10 rstep=w,.5d0,w
            rlow=rstep
            rup=min(rstep+w,.5d0)
            call dgqif(rlow,rup,result,kk,rtol,atol,7,icheck,pssevf,
     $                 efn(1,k),ndata)
            sum=sum+result(kk)
            if(icheck.ne.0)then
               write(*,*)' dgqif did not converge for 1-evalu(k) k =',k
            endif
   10     continue
          evalu1(k)=2.*sum
        else
          evalu1(k)=1.0d0-evalu(k)
        endif
   20 continue
      return
      end

c $Id$ 
