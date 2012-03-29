      function qabm(w,t1,t2,qm)
      real*8 qabm,qm,c,arg,w,t1,t2
      intrinsic datan
      arg=(w*(t1-t2))/(1.0+w*w*t1*t2)
c     c=2/(pi*qm)
      c=0.6366198/qm
      qabm=c*datan(arg)
      if(qabm.eq.0.d0) qabm=1.0d-5
      qabm=1.0/qabm
      return
      end
      function vabm(w,t1,t2,qm)
c     vabm calculates dispersion due to anelasticity
      real*8 vabm,qm,c,arg,arg1,w,w12,t1,t2,w2,t12,t22
      intrinsic dlog
c     c=2/(pi*qm)
      c=0.6366198/qm
      c=c/4.0
      w2=w*w
      t12=t1*t1
      t22=t2*t2
      arg=(1.0+w2*t12)/(1.0+w2*t22)
c     normalize to 1 hz (w12 = (2*pi*1)**2
      w12=39.478418
      arg1=(1.0+w12*t12)/(1.0+w12*t22)
      vabm=(1.0 + c*dlog(arg))/(1.0 + c*dlog(arg1))
      return
      end
