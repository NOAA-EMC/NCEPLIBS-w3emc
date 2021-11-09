!> @file
!> @brief Format a date and time into characters
!> @author Mark Iredell @date 1998-01-05

!> This subprogram forms various character strings useful
!> in describing an NCEP absolute date and time.
!>
!> ### Program History Log:
!> Date | Programmer | Comment
!> -----|------------|--------
!> 1998-01-05 | Mark Iredell | Initial.
!>
!> @param[in] IDAT NCEP absolute date and time (year, month, day, time zone,
!> hour, minute, second, millisecond)
!> @param[out] CDAT Strings describing date and time:
!> - CDAT(1) is the name of the day of the week;
!> - CDAT(2) is the name of the month;
!> - CDAT(3) is the day of month, year;
!> - CDAT(4) is the date in yyyy-mm-dd format;
!> - CDAT(5) is the date in yyyy.doy format;
!> - CDAT(6) is the time in hh:mm:ss format;
!> - CDAT(7) is the milliseconds in .xxx format;
!> - CDAT(8) is the time zone.
!>
!> @author Mark Iredell @date 1998-01-05
      subroutine w3pradat(idat,cdat)
      integer idat(8)
      character*(*) cdat(8)
      character*10 ctmp(8)
      character*10 cmon(12)
      data cmon/'January   ','February  ','March     ',
     &          'April     ','May       ','June      ',
     &          'July      ','August    ','September ',
     &          'October   ','November  ','December  '/
      character*10 cdow(7)
      data cdow/'Sunday    ','Monday    ','Tuesday   ',
     &          'Wednesday ','Thursday  ','Friday    ',
     &          'Saturday  '/
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  get day of week and day of year, convert day of week and month
!  to english names, write other formats of date and time, and
!  write time zone differential in one of three ways.
      jldayn=iw3jdn(idat(1),idat(2),idat(3))
      call w3fs26(jldayn,jy,jm,jd,jdow,jdoy)
      ctmp(1)=cdow(jdow)
      ctmp(2)='********'
      if(idat(2).ge.1.and.idat(2).le.12) ctmp(2)=cmon(idat(2))
      write(ctmp(3),'(i2,", ",i4)') idat(3),idat(1)
      write(ctmp(4),'(i4,"-",i2.2,"-",i2.2)') idat(1),idat(2),idat(3)
      write(ctmp(5),'(i4,".",i3.3)') idat(1),jdoy
      write(ctmp(6),'(i2.2,":",i2.2,":",i2.2)') idat(5),idat(6),idat(7)
      write(ctmp(7),'(".",i3.3)') idat(8)
      if(idat(4).eq.0) then
        write(ctmp(8),'("UTC")')
      elseif(mod(idat(4),100).eq.0) then
        kh=idat(4)/100
        write(ctmp(8),'("UTC",sp,i3.2,"h")') kh
      else
        kh=idat(4)/100
        km=abs(mod(idat(4),100))
        write(ctmp(8),'("UTC",sp,i3.2,"h",ss,i2.2,"m")') kh,km
      endif
      cdat=ctmp
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end
