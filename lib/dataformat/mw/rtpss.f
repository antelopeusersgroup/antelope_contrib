      subroutine rtpss(nndim,ndata,nefn,w,efn,theta,ierr,work)
c
c  computes discrete prolate spheroidal sequences using the symmetric
c  tridiagonal form given in d.slepian, bell syst. tech. j. 57 (1978),
c  p. 1376, equation 14. there is a further reduction into even and
c  odd symmetric functions to minimize the size of the eigenvalue
c  problem, see eispack sections 2.1.17 and 2.2.8.
c  adapted by a.d. chave, lanl, jan 1986 from code written by
c  d.j. thomson, at&t bell labs.
c  argument list:
c    nndim-leading dimension of efn
c    ndata-number of data points (e.g., time) to return in efn,
c          this corresponds to n in slepian
c    nefn-number of dpss to compute, lowest order ones are returned
c         first
c    w-the bandwidth (=time-bandwidth product/ndata)
c    efn-array to contain the dpss, first dimension corresponds to
c        time and second to the order of the sequence
c    theta-vector of length nefn containing the differential eigenvalues
c    ierr-error flag, 0 for normal return, 1 for error in tridib,
c         2 for error in tinvit
c    work-work space of dimension at least 9*ndata/2+3*nefn/2+
c         ndata*nefn/4+11
c
c  calls rtpss1
c
      implicit double precision (a-h,o-z)
      dimension efn(nndim,nefn),theta(nefn),work(1)
      logical flag
      parameter (pi=3.141592653589793d0)
      ndim   = nndim
      tw     = 2.*w
      ctpw   = cos(pi*tw)
      nodefn = nefn/2
      nevefn = nefn - nodefn
      m      = ndata/2
      mdim   = ndata - m
c  allocate storage in work
      lefnh  = 1
      levh   = mdim*nevefn + lefnh
      lfv1   = nevefn + levh
      lfv2   = mdim + lfv1
      lfv3   = mdim + lfv2
      lw     = mdim + lfv3

c  compute even eigenfunctions
      sym=1.
      flag=.false.
      call rtpss1(efn,ndim,ndata,mdim,mdim,nevefn,ctpw,theta,flag,
     $             sym,work(lefnh),work(levh),work(lfv1),work(lfv2),
     $             work(lfv3),ierr,work(lw))
c  compute odd eigenfunctions
      sym=-1.
      flag=m.ne.mdim
      if(nodefn.gt.0)call rtpss1(efn(1,2),ndim,ndata,m,mdim,nodefn,
     $                            ctpw,theta(2),flag,sym,work(lefnh),
     $                            work(levh),work(lfv1),work(lfv2),
     $                            work(lfv3),ierr,work(lw))
      return
      end

c $Id$ 
