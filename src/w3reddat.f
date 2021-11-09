!> @file
!> @brief Reduce a time interval to a canonical form.
!> @author Mark Iredell @date 1998-01-05

!> This subprogram reduces an ncep relative time interval into one of seven
!> canonical forms, depending on the input it value.
!>
!> First reduced format type (IT=-1):
!>      RINC(1) is an arbitrary integer.
!>      RINC(2) is an integer between 00 and 23, inclusive.
!>      RINC(3) is an integer between 00 and 59, inclusive.
!>      RINC(4) is an integer between 00 and 59, inclusive.
!>      RINC(5) is an integer between 000 and 999, inclusive.
!>    If RINC(1) is negative, then the time interval is negative.
!>
!> Second reduced format type (IT=0):
!>    If the time interval is not negative, then the format is:
!>      RINC(1) is zero or a positive integer.
!>      RINC(2) is an integer between 00 and 23, inclusive.
!>      RINC(3) is an integer between 00 and 59, inclusive.
!>      RINC(4) is an integer between 00 and 59, inclusive.
!>      RINC(5) is an integer between 000 and 999, inclusive.
!>    Otherwise if the time interval is negative, then the format is:
!>      RINC(1) is zero or a negative integer.
!>      RINC(2) is an integer between 00 and -23, inclusive.
!>      RINC(3) is an integer between 00 and -59, inclusive.
!>      RINC(4) is an integer between 00 and -59, inclusive.
!>      RINC(5) is an integer between 000 and -999, inclusive.
!>
!> Days format type (IT=1):
!>      RINC(1) is arbitrary.
!>      RINC(2) is zero.
!>      RINC(3) is zero.
!>      RINC(4) is zero.
!>      RINC(5) is zero.
!>
!> Hours format type (IT=2):
!>      RINC(1) is zero.
!>      RINC(2) is arbitrary.
!>      RINC(3) is zero.
!>      RINC(4) is zero.
!>      RINC(5) is zero.
!>    (This format should not express time intervals longer than 300 years.)
!>
!> Minutes format type (IT=3):
!>      RINC(1) is zero.
!>      RINC(2) is zero.
!>      RINC(3) is arbitrary.
!>      RINC(4) is zero.
!>      RINC(5) is zero.
!>    (This format should not express time intervals longer than five years.)
!>
!> Seconds format type (IT=4):
!>      RINC(1) is zero.
!>      RINC(2) is zero.
!>      RINC(3) is zero.
!>      RINC(4) is arbitrary.
!>      RINC(5) is zero.
!>    (This format should not express time intervals longer than one month.)
!>
!> Milliseconds format type (IT=5):
!>      RINC(1) is zero.
!>      RINC(2) is zero.
!>      RINC(3) is zero.
!>      RINC(4) is zero.
!>      RINC(5) is arbitrary.
!>   (This format should not express time intervals longer than one hour.)
!>
!> ### Program History Log:
!> Date | Programmer | Comment
!> -----|------------|--------
!> 1998-01-05 | Mark Iredell | Initial.
!>
!> @param[in] IT Relative time interval format type
!> - (-1 for first reduced type (hours always positive),
!> - 0 for second reduced type (hours can be negative),
!> - 1 for days only, 2 for hours only, 3 for minutes only,
!> - 4 for seconds only, 5 for milliseconds only)
!> @param[in] RINC NCEP relative time interval (days, hours, minutes, seconds,
!> milliseconds)
!> @param[out] DINC NCEP relative time interval (days, hours, minutes,
!> seconds, milliseconds)
!>
!> @author Mark Iredell @date 1998-01-05
      subroutine w3reddat(it,rinc,dinc)
      real rinc(5),dinc(5)
!  parameters for number of units in a day
!  and number of milliseconds in a unit
!  and number of next smaller units in a unit, respectively
      integer,dimension(5),parameter:: itd=(/1,24,1440,86400,86400000/),
     &                                 itm=itd(5)/itd
      integer,dimension(4),parameter:: itn=itd(2:5)/itd(1:4)
      integer,parameter:: np=16
      integer iinc(4),jinc(5),kinc(5)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  first reduce to the first reduced form
      iinc=floor(rinc(1:4))
!  convert all positive fractional parts to milliseconds
!  and determine canonical milliseconds
      jinc(5)=nint(dot_product(rinc(1:4)-iinc,real(itm(1:4)))+rinc(5))
      kinc(5)=modulo(jinc(5),itn(4))
!  convert remainder to seconds and determine canonical seconds
      jinc(4)=iinc(4)+(jinc(5)-kinc(5))/itn(4)
      kinc(4)=modulo(jinc(4),itn(3))
!  convert remainder to minutes and determine canonical minutes
      jinc(3)=iinc(3)+(jinc(4)-kinc(4))/itn(3)
      kinc(3)=modulo(jinc(3),itn(2))
!  convert remainder to hours and determine canonical hours
      jinc(2)=iinc(2)+(jinc(3)-kinc(3))/itn(2)
      kinc(2)=modulo(jinc(2),itn(1))
!  convert remainder to days and compute milliseconds of the day
      kinc(1)=iinc(1)+(jinc(2)-kinc(2))/itn(1)
      ms=dot_product(kinc(2:5),itm(2:5))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  next reduce to either single value canonical form
!  or to one of the two reduced forms
      if(it.ge.1.and.it.le.5) then
!  ensure that exact multiples of 1./np are expressed exactly
!  (other fractions may have precision errors)
        rp=(np*ms)/itm(it)+mod(np*ms,itm(it))/real(itm(it))
        dinc=0
        dinc(it)=real(kinc(1))*itd(it)+rp/np
      else
!  the reduced form is done except the second reduced form is modified
!  for negative time intervals with fractional days
        dinc=kinc
        if(it.eq.0.and.kinc(1).lt.0.and.ms.gt.0) then
          dinc(1)=dinc(1)+1
          dinc(2:5)=mod(ms-itm(1),itm(1:4))/itm(2:5)
        endif
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end
