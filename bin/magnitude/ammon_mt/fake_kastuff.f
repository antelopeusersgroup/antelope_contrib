      subroutine kadate(yr,jday,thirtytwo,ref_date,nerr)
      integer yr,jday
      integer thirtytwo
      character*32 ref_date
      integer nerr

      ref_date = 'UNKNOWN'
      nerr = 0

      return 
      end

      subroutine katime(hr,min,sec,msec,thirtytwo,ref_time,nerr)
      integer hr,min,sec,msec
      integer thirtytwo
      character*32 ref_time
      integer nerr

      ref_time = 'UNKNOWN'
      nerr = 0

      return 
      end
