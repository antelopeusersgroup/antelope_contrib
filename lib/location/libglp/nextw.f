      character*(*) function nextw(string)
c---------------------------------------------------------------------- 
c      returns next word from string "string".  a word is defined here  
c  as bounded by anything that is not a alphanumeric character. 
c  returns a blank filled string if no alphanumeric characters are
c  found. 
c---------------------------------------------------------------------- 
      character*(*) string
      integer start,stop,i
      start=1 
      stop=len(string)
      limit=len(nextw)
      call clrstr(nextw)
c--skip until an alphanumeric character is found
      i = 1 
  100 if((('0'.le.string(i:i)).and.(string(i:i).le.'9')).or.
     $         (('a'.le.string(i:i)).and.(string(i:i).le.'z')).or.
     $         (('a'.le.string(i:i)).and.(string(i:i).le.'z'))) 
     $                                                   go to 150
            i = i + 1 
c--exit for case when no alphanumeric characters are found in the 
c--"string".  note this returns "nextw" blank filled. 
            if(i.gt.stop) return
            go to 100 
  150 continue
      start = i 
      j = 1 
  200 if(.not.((('0'.le.string(i:i)).and.(string(i:i).le.'9')).or.
     $         (('a'.le.string(i:i)).and.(string(i:i).le.'z')).or.
     $         (('a'.le.string(i:i)).and.(string(i:i).le.'z'))))
     $                                                       go to 250  
            i = i + 1 
            j = j + 1 
            if((j.gt.limit).or.(i.gt.stop)) go to 250 
            go to 200 
  250 continue
      stop = i - 1
      nextw=string(start:stop)
      return
      end 

c $Id$ 
