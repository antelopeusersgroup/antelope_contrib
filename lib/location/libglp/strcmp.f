      logical function strcmp(s1,s2)
c-------------------------------------------------------------------- 
c          compares character strings in s1 and s2.  if they are
c  the same strcmp is set to .true.  if they are different it 
c  returns .false.
c  note   s1 and s2 do not have to be the same length.  if they are 
c         different lengths (length accessed via "len" function that is)
c         only the first len(shortstring) characters are directly 
c         compared.  in addition, when this happens the remainder 
c         of the longer string must be blank filled or the function 
c         will return a value of .false.  
c 
c  author   gary l. pavlis
c  written   june 1983
c---------------------------------------------------------------------- 
      character*(*) s1
      character*(*) s2
      integer len1,len2,i 
      len1=len(s1)
      len2=len(s2)
      do 100 i=1,min(len1,len2) 
           if(s1(i:i).ne.s2(i:i)) then
                strcmp = .false.
                return
           endif
  100 continue
      if(len1.eq.len2) then 
           strcmp = .true.
      else  
           if(len1.gt.len2) then
                do 150 i=len2+1,len1
                     if(s1(i:i).ne.' ') then
                          strcmp = .false.
                     endif
  150           continue
           else 
                do 200 i=len1+1,len2
                     if(s2(i:i).ne.' ') then
                          strcmp = .false.
                          return
                     endif
  200           continue
           endif
      endif 
      strcmp = .true. 
      return
      end 

c $Id$ 
