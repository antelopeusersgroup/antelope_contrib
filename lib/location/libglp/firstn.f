      integer function firstn(string) 
c---------------------------------------------------------------------
c        returns indexing position of first numeric character in
c  character string "string".  returns 0 if no numeric characters are 
c  found. 
c---------------------------------------------------------------------
      character*(*) string
      integer i,n 
      n = len(string) 
      do 100 i=1,n
           if((string(i:i).ge.'0').and.(string(i:i).le.'9')) then 
                firstn = i
                return
           endif
  100 continue
      firstn = 0
      return
      end 

c $Id$ 
