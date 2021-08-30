!> @file
!> @brief Converts an ncep absolute date and time to another time zone.
!> @author Mark Iredell @date 1998-01-05

!> THis subprogram converts an ncep absolute date and time
!> to another time zone.
!>
!> Program history log:
!> - Mark Iredell 1998-01-05
!>
!> @param[in] NTZ Integer new time zone differential from utc in signed hh
!> or hhmm format (if ntz is invalid, no change is made.)
!> @param[in] IDAT Integer (8) ncep absolute date and time
!> (year, month, day, time zone, hour, minute, second, millisecond)
!> @param[out] JDAT Integer (8) ncep absolute date and time
!> (year, month, day, time zone, hour, minute, second, millisecond)
!>
!> @author Mark Iredell @date 1998-01-05

      subroutine w3ctzdat(ntz,idat,jdat)
      integer idat(8),jdat(8)
      real rinc1(5),rinc2(5)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  determine if the input time zone is in valid hh or hhmm format
      if(ntz.gt.-24.and.ntz.lt.24) then
        itz=ntz*100
      elseif(ntz.eq.mod(ntz/100,24)*100+mod(mod(ntz,100),60)/30*30) then
        itz=ntz
      else
        itz=idat(4)
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  determine new time of day, putting into reduced form
!  and possibly adjust the date as well
      rinc1(1)=0
      rinc1(2)=idat(5)+itz/100-idat(4)/100
      rinc1(3)=idat(6)+mod(itz,100)-mod(idat(4),100)
      rinc1(4)=idat(7)
      rinc1(5)=idat(8)
      call w3reddat(-1,rinc1,rinc2)
      jldayn=iw3jdn(idat(1),idat(2),idat(3))+nint(rinc2(1))
      call w3fs26(jldayn,jdat(1),jdat(2),jdat(3),jdow,jdoy)
      jdat(4)=itz
      jdat(5:8)=nint(rinc2(2:5))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end
