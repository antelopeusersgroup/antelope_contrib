      SUBROUTINE svdcmp(a,m,n,mp,np,w,v)
        implicit none
        INTEGER, PARAMETER     :: dp = SELECTED_REAL_KIND(15,307)
      integer, PARAMETER  :: NMAX=500
      INTEGER  :: m,mp,n,np
      real(kind=dp) :: a(mp,np),v(np,np),w(np)
      INTEGER  :: i,its,j,jj,k,l,nm
      real(kind=dp) :: anorm,c,f,g,h,s,scale,x,y,z,rv1(NMAX),pythag

      g=0.0_dp
      scale=0.0_dp
      anorm=0.0_dp
      do i=1,n
        l=i+1
        rv1(i)=scale*g
        g=0.0_dp
        s=0.0_dp
        scale=0.0_dp
        if(i.le.m)then
          do k=i,m
            scale=scale+abs(a(k,i))
          end do
          if(scale.ne.0.0_dp)then
            do k=i,m
              a(k,i)=a(k,i)/scale
              s=s+a(k,i)*a(k,i)
            end do
            f=a(i,i)
            g=-sign(sqrt(s),f)
            h=f*g-s
            a(i,i)=f-g
            do j=l,n
              s=0.0_dp
              do k=i,m
                s=s+a(k,i)*a(k,j)
              end do
              f=s/h
              do k=i,m
                a(k,j)=a(k,j)+f*a(k,i)
              end do
            end do
            do k=i,m
              a(k,i)=scale*a(k,i)
            end do
          endif
        endif
        w(i)=scale *g
        g=0.0_dp
        s=0.0_dp
        scale=0.0_dp
        if((i.le.m).and.(i.ne.n))then
          do k=l,n
            scale=scale+abs(a(i,k))
          end do
          if(scale.ne.0.0)then
            do k=l,n
              a(i,k)=a(i,k)/scale
              s=s+a(i,k)*a(i,k)
            end do
            f=a(i,l)
            g=-sign(sqrt(s),f)
            h=f*g-s
            a(i,l)=f-g
            do k=l,n
              rv1(k)=a(i,k)/h
            end do
            do j=l,m
              s=0.0
              do k=l,n
                s=s+a(j,k)*a(i,k)
              end do
              do k=l,n
                a(j,k)=a(j,k)+s*rv1(k)
              end do
            end do
            do k=l,n
              a(i,k)=scale*a(i,k)
            end do
          endif
        endif
        anorm=max(anorm,(abs(w(i))+abs(rv1(i))))
      end do
      do i=n,1,-1
        if(i.lt.n)then
          if(g.ne.0.0_dp)then
            do j=l,n
              v(j,i)=(a(i,j)/a(i,l))/g
            end do
            do j=l,n
              s=0.0_dp
              do k=l,n
                s=s+a(i,k)*v(k,j)
              end do
              do k=l,n
                v(k,j)=v(k,j)+s*v(k,i)
              end do
            end do
          endif
          do j=l,n
            v(i,j)=0.0_dp
            v(j,i)=0.0_dp
          end do
        endif
        v(i,i)=1.0
        g=rv1(i)
        l=i
      end do
      do  i=min(m,n),1,-1
        l=i+1
        g=w(i)
        do j=l,n
          a(i,j)=0.0_dp
        end do
        if(g.ne.0.0_dp)then
          g=1.0_dp/g
          do j=l,n
            s=0.0_dp
            do k=l,m
              s=s+a(k,i)*a(k,j)
            end do
            f=(s/a(i,i))*g
            do k=i,m
              a(k,j)=a(k,j)+f*a(k,i)
            end do
          end do
          do j=i,m
            a(j,i)=a(j,i)*g
          end do
        else
          do j= i,m
            a(j,i)=0.0_dp
          end do
        endif
        a(i,i)=a(i,i)+1.0_dp
      end do
      do k=n,1,-1
        do its=1,30
          do l=k,1,-1
            nm=l-1
            if((abs(rv1(l))+anorm).eq.anorm)  goto 2
            if((abs(w(nm))+anorm).eq.anorm)  goto 1
          end do                        
1         c=0.0_dp
          s=1.0_dp
          do i=l,k
            f=s*rv1(i)
            rv1(i)=c*rv1(i)
            if((abs(f)+anorm).eq.anorm) goto 2
            g=w(i)
            h=pythag(f,g)
            w(i)=h
            h=1._dp/h
            c= (g*h)
            s=-(f*h)
            do j=1,m
              y=a(j,nm)
              z=a(j,i)
              a(j,nm)=(y*c)+(z*s)
              a(j,i)=-(y*s)+(z*c)
            end do
          end do
2         z=w(k)
          if(l.eq.k)then
            if(z.lt.0.0_dp)then
              w(k)=-z
              do j=1,n
                v(j,k)=-v(j,k)
              end do
            endif
            goto 3
          endif
          if(its.eq.50) stop 'no convergence in svdcmp'
          x=w(l)
          nm=k-1
          y=w(nm)
          g=rv1(nm)
          h=rv1(k)
          f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0*h*y)
          g=pythag(f,1.0_dp)
          f=((x-z)*(x+z)+h*((y/(f+sign(g,f)))-h))/x
          c=1.0_dp
          s=1.0_dp
          do j=l,nm
            i=j+1
            g=rv1(i)
            y=w(i)
            h=s*g
            g=c*g
            z=pythag(f,h)
            rv1(j)=z
            c=f/z
            s=h/z
            f= (x*c)+(g*s)
            g=-(x*s)+(g*c)
            h=y*s
            y=y*c
            do jj=1,n
              x=v(jj,j)
              z=v(jj,i)
              v(jj,j)= (x*c)+(z*s)
              v(jj,i)=-(x*s)+(z*c)
            end do
            z=pythag(f,h)
            w(j)=z
            if(z.ne.0.0_dp)then
              z=1.0_dp/z
              c=f*z
              s=h*z
            endif
            f= (c*g)+(s*y)
            x=-(s*g)+(c*y)
            do jj=1,m
              y=a(jj,j)
              z=a(jj,i)
              a(jj,j)= (y*c)+(z*s)
              a(jj,i)=-(y*s)+(z*c)
            end do
          end do
          rv1(l)=0.0_dp
          rv1(k)=f
          w(k)=x
        end do

3       continue

      end do
      return
      end subroutine 

      SUBROUTINE svbksb(u,w,v,m,n,mp,np,b,x)
        INTEGER, PARAMETER     :: dp = SELECTED_REAL_KIND(15,307)
      integer, PARAMETER :: NMAX=500
      INTEGER   :: m,mp,n,np
      real(kind=dp) :: b(mp),u(mp,np),v(np,np),w(np),x(np)
      INTEGER   :: i,j,jj
      real(kind=dp) :: s,tmp(NMAX)
      do j=1,n
        s=0.
        if(w(j).ne.0.)then
          do i=1,m
            s=s+u(i,j)*b(i)
          end do
          s=s/w(j)
        endif
        tmp(j)=s
      end do
      do j=1,n
        s=0.
        do jj=1,n
          s=s+v(j,jj)*tmp(jj)
        end do
        x(j)=s
      end do
      return
      END

      FUNCTION pythag(a,b)
        INTEGER, PARAMETER     :: dp = SELECTED_REAL_KIND(15,307)
      real(kind=dp) :: pythag
      real(kind=dp) :: a,b
      real(kind=dp) :: absa,absb
      absa=abs(a)
      absb=abs(b)
      if(absa.gt.absb)then
        pythag=absa*sqrt(1._dp+(absb/absa)**2)
      else
        if(absb.eq.0.)then
          pythag=0.
        else
          pythag=absb*sqrt(1._dp+(absa/absb)**2)
        endif
      endif
      return
      END

