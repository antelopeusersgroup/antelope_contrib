      subroutine arfilt(npts,nar,data,work,ar)
c
c  computes ar filter with nar terms to fit data, replaces data
c  with npts-nar residuals and returns filter in ar
c
c  inputs:
c
c  npts  - number of data points in data
c  nar   - number of filter terms less leading 1
c  data  - real vector of data
c  work  - real work vector of length nar+1
c
c  outputs:
c
c  ar    - vector of nar+1 ar filter terms (includes leading 1)
c
c  calls ywar
c
      double precision sum
      dimension data(npts),ar(nar+1),work(1)
      call ywar(npts,nar,data,work,ar)
      do 10 i=nar,1,-1
   10   ar(i+1)=-ar(i)
      ar(1)=1.
      do 30 i=nar+1,npts
        sum=data(i)
        do 20 j=2,nar+1
   20     sum=sum+ar(j)*data(i-j+1)
   30   data(i-nar)=sum
      npts=npts-nar
      nar=nar+1
      return
      end

c $Id$ 
