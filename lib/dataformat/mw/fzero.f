      subroutine fzero(f,b,c,r,re,ae,iflag,dpar,rpar,ipar)
c***begin prologue  fzero
c***date written   700901   (yymmdd)
c***revision date  820801   (yymmdd)
c***category no.  f1b
c***keywords  bisection,nonlinear,roots,zeros
c***author  shampine,l.f.,snla
c           watts,h.a.,snla
c***purpose  fzero searches for a zero of a function f(x) in a given
c            interval (b,c).  it is designed primarily for problems
c            where f(b) and f(c) have opposite signs.
c***description
c
c     based on a method by t j dekker
c     written by l f shampine and h a watts
c
c            fzero searches for a zero of a function f(x) between
c            the given values b and c until the width of the interval
c            (b,c) has collapsed to within a tolerance specified by
c            the stopping criterion, abs(b-c) .le. 2.*(rw*abs(b)+ae).
c            the method used is an efficient combination of bisection
c            and the secant rule.
c
c     description of arguments
c
c     f,b,c,r,re and ae are input parameters
c     b,c and iflag are output parameters (flagged by an * below)
c
c        f     - name of the real valued external function.  this name
c                must be in an external statement in the calling
c                program.  f must be a function of one real argument.
c
c       *b     - one end of the interval (b,c).  the value returned for
c                b usually is the better approximation to a zero of f.
c
c       *c     - the other end of the interval (b,c)
c
c        r     - a (better) guess of a zero of f which could help in
c                speeding up convergence.  if f(b) and f(r) have
c                opposite signs, a root will be found in the interval
c                (b,r); if not, but f(r) and f(c) have opposite
c                signs, a root will be found in the interval (r,c);
c                otherwise, the interval (b,c) will be searched for a
c                possible root.  when no better guess is known, it is
c                recommended that r be set to b or c; because if r is
c                not interior to the interval (b,c), it will be ignored.
c
c        re    - relative error used for rw in the stopping criterion.
c                if the requested re is less than machine precision,
c                then rw is set to approximately machine precision.
c
c        ae    - absolute error used in the stopping criterion.  if the
c                given interval (b,c) contains the origin, then a
c                nonzero value should be chosen for ae.
c
c       *iflag - a status code.  user must check iflag after each call.
c                control returns to the user from fzero in all cases.
c                xerror does not process diagnostics in these cases.
c
c                1  b is within the requested tolerance of a zero.
c                   the interval (b,c) collapsed to the requested
c                   tolerance, the function changes sign in (b,c), and
c                   f(x) decreased in magnitude as (b,c) collapsed.
c
c                2  f(b) = 0.  however, the interval (b,c) may not have
c                   collapsed to the requested tolerance.
c
c                3  b may be near a singular point of f(x).
c                   the interval (b,c) collapsed to the requested tol-
c                   erance and the function changes sign in (b,c), but
c                   f(x) increased in magnitude as (b,c) collapsed,i.e.
c                     abs(f(b out)) .gt. max(abs(f(b in)),abs(f(c in)))
c
c                4  no change in sign of f(x) was found although the
c                   interval (b,c) collapsed to the requested tolerance.
c                   the user must examine this case and decide whether
c                   b is near a local minimum of f(x), or b is near a
c                   zero of even multiplicity, or neither of these.
c
c                5  too many (.gt. 500) function evaluations used.
c      *dpar     vector of double precision parameters to pass to f
c      *rpar     vector of single precision parameters to pass to f
c      *ipar     vector of integer parameters to pass to f
c***references  l. f. shampine and h. a. watts, *fzero, a root-solving
c                 code*, sc-tm-70-631, september 1970.
c               t. j. dekker, *finding a zero by means of successive
c                 linear interpolation*, 'constructive aspects of the
c                 fundamental theorem of algebra', edited by b. dejon
c                 p. henrici, 1969.
c***routines called  r1mach
c***end prologue  fzero
c
      double precision dpar
      dimension dpar(1),rpar(1),ipar(1)
      external f
c
c     er is two times the computer unit roundoff value which is
c     defined here by the function r1mach.

c***first executable statement  fzero
      er = 2.0 * r1mach(4)
c
c     initialize
c
      z=r
      if(r.le.min(b,c).or.r.ge.max(b,c)) z=c
      rw=max(re,er)
      aw=max(ae,0.)
      ic=0
      t=z
      fz=f(t,dpar,rpar,ipar)
      t=b
      fb=f(t,dpar,rpar,ipar)
      kount=2
      if(sign(1.0,fz).eq.sign(1.0,fb)) go to 1
      c=z
      fc=fz
      go to 2
    1 if(z.eq.c) go to 2
      t=c
      fc=f(t,dpar,rpar,ipar)
      kount=3
      if(sign(1.0,fz).eq.sign(1.0,fc)) go to 2
      b=z
      fb=fz
    2 a=c
      fa=fc
      acbs=abs(b-c)
      fx=max(abs(fb),abs(fc))
c
    3 if (abs(fc) .ge. abs(fb)) go to 4
c     perform interchange
      a=b
      fa=fb
      b=c
      fb=fc
      c=a
      fc=fa
c
    4 cmb=0.5*(c-b)
      acmb=abs(cmb)
      tol=rw*abs(b)+aw
c
c     test stopping criterion and function count
c
      if (acmb .le. tol) go to 10
      if(fb.eq.0.) go to 11
      if(kount.ge.500) go to 14
c
c     calculate new iterate implicitly as b+p/q
c     where we arrange p .ge. 0.
c     the implicit form is used to prevent overflow.
c
      p=(b-a)*fb
      q=fa-fb
      if (p .ge. 0.) go to 5
      p=-p
      q=-q
c
c     update a and check for satisfactory reduction
c     in the size of the bracketing interval.
c     if not, perform bisection.
c
    5 a=b
      fa=fb
      ic=ic+1
      if (ic .lt. 4) go to 6
      if (8.*acmb .ge. acbs) go to 8
      ic=0
      acbs=acmb
c
c     test for too small a change
c
    6 if (p .gt. abs(q)*tol) go to 7
c
c     increment by tolerance
c
      b=b+sign(tol,cmb)
      go to 9
c
c     root ought to be between b and (c+b)/2.
c
    7 if (p .ge. cmb*q) go to 8
c
c     use secant rule
c
      b=b+p/q
      go to 9
c
c     use bisection
c
    8 b=0.5*(c+b)
c
c     have completed computation for new iterate b
c
    9 t=b
      fb=f(t,dpar,rpar,ipar)
      kount=kount+1
c
c     decide whether next step is interpolation or extrapolation
c
      if (sign(1.0,fb) .ne. sign(1.0,fc)) go to 3
      c=a
      fc=fa
      go to 3
c
c
c     finished. process results for proper setting of iflag
c
   10 if (sign(1.0,fb) .eq. sign(1.0,fc)) go to 13
      if (abs(fb) .gt. fx) go to 12
      iflag = 1
      return
   11 iflag = 2
      return
   12 iflag = 3
      return
   13 iflag = 4
      return
   14 iflag = 5
      return
      end

c $Id$ 
