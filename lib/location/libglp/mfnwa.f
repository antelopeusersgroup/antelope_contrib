      character*(*) function mfnwa(name,append)
c-----------------------------------------------------------------------
c        makes file name with an appendage.  (mnemonic is for 
c  first character of each major word of this phrase.  this is wierd
c  i admit but anything else i could think of was potentially confusing.) 
c  that is, mfnwa takes the string in "name" and concatentates it with  
c  the string in "append" (blanks are removed). 
c  if the resultant string is too long for a legal file name  
c  (system dependent) "name" is truncated on the right so that the
c  resultant of the concatentation of the truncated string and "append" 
c  fits.  if "append" is too long, mfnwa is returned blank filled.
c  note:  this routine assumes "name" and "append" are both 
c         left justified tokens (i.e. no blanks). 
c 
c  author: gary l. pavlis, dept. of geol., indiana univ., bloomington,
c          indiana 47405
c  written: oct. 2, l983
c-------------------------------------------------------------------- 
      character*(*) name
      character*(*) append
c--this parameter gives th longest allowed file name.
c--that is, obviously, system dependent and should be checked for 
c--consistency with system being used.
      integer maxfnl  
      parameter(maxfnl=7) 
      integer lenn,lena,endn
      call strclr(mfnwa)
      lenn = lenstr(name) 
      lena = lenstr(append)
      endn = min(lenn,maxfnl-lena)
      if(endn.le.0) return
      mfnwa = name(1:endn)//append
      return
      end

c $Id$ 
