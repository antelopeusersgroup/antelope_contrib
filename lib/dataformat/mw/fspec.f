      real function fspec(x,evalu,rpar,kspec)
      double precision evalu
      dimension evalu(kspec),rpar(2*kspec)
      sum=0.
      do 10 i=1,kspec
   10   sum=sum+evalu(i)*(x-rpar(kspec+i))/(evalu(i)*x+rpar(i))**2
      fspec=sum
      return
      end

c $Id$ 
