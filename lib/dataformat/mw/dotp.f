      subroutine dotp(i,n,ans,data)
      dimension data(1)
      ans=0.
      if(i.lt.0)return
      k=n-i
      do 10 j=1,k
   10   ans=ans+data(j+i)*data(j)
      return
      end

c $Id$ 
