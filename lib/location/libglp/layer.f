      subroutine layer(vz,zz,nz)
c-----------------------------------------------------------------------
c       layer takes a randomly digitized model and converts it to a 
c  that is uniformly spaced in depth.  it does this by digitizing the 
c  input model passed through common assuming points in that model
c  are connected by linear gradients. 
c 
c  arguements-
c     vz - array to hold velocities of regularly spaced model.
c     zz - array to hold depths of regularly spaced model.
c     nz - length of make vz and zz.
c 
c  note   this code once upon a time contained two while loops that 
c         were converted to if - goto construction.  my apologies.
c-----------------------------------------------------------------------
      common/blk1/nvel,vel(200),dep(200)
      dimension vz(nz),zz(nz) 
      zz(1)=dep(1)
      vz(1)=vel(1)
c--fill the depth array 
      dz=dep(nvel)/float(nz-1)
          do 100 i=2,nz 
  100     zz(i)=float(i-1)*dz 
c--initialize 
c--id is the pointer in the random model  
c--iz is the pointer in the uniform model 
      iz=2  
      id=2  
  110 if((id.gt.nvel).or.(iz.gt.nz)) go to 140
          if(dep(id).ge.zz(iz)) then
               vgrad=(vel(id)-vel(id-1))/(dep(id)-dep(id-1))
  120          if(dep(id).lt.zz(iz)) go to 130
                    vz(iz)=vel(id-1)+(zz(iz)-dep(id-1))*vgrad 
                    iz=iz+1 
                    if(iz.gt.nz) return 
                    go to 120 
  130          continue 
               id=id+1
          else
               id=id+1
          endif 
          go to 110 
  140 continue
      return
      end 

c $Id$ 
