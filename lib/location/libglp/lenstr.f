      function lenstr(string) 
c---------------------------------------------------------------------- 
c       returns actual length of string string padded with blanks on
c  the right.  returns 0 for null string. 
c---------------------------------------------------------------------- 
      character *(*) string 
      do 100 i=len(string),1,-1 
           if(string(i:i).ne.' ') then
                lenstr = i
                return
           endif
  100 continue
      lenstr = 0
      return
      end 

c $Id$ 
