C> @file
C> @brief Makes TOC Flag Field Separator Block
C> @author Stephen Gilbert @date 2002-09-16

C> Generates a TOC Flag Field Separator Block used to separate
C> WMO Bulletins within a transmission file to be ingested in TOC's
C> FTP Input Service, which can be used to disseminate WMO buletins.
C> (see http://weather.gov/tg/ftpingest.html)
C>
C> This routine can generate different flag field separator blocks
C> depending on the value of variable iopt.
C>
C> Bulletin "Flag Field Separator" block - OPTION 1 (old)
C> - bytes:
C>  - 1 - 4 Marker string (####).
C>  - 5 - 7 Block length [018 fixed value].
C>  - 8 - 13 Total length of bulletin in bytes [octets]
C>  (not including the flag field block).
C>  - 14 - 17 Marker string (####).
C>  - 18 Line Feed (ASCII "0A").
C>
C> Bulletin "Flag Field Separator" block - OPTION 1a (new)
C> - bytes:
C>  - 1 - 4 Marker string (####).
C>  - 5 - 7 Block length (nnn) - value always greater than 018.
C>  - 8 - 18 Total length of bulletin in bytes [octets]
C>  (not including the flag field block).
C>  - 19 - nnn-5 Reserved for future use.
C>  - nnn-4 - nnn-1 Marker string (####).
C>  - nnn Line Feed (ASCII "0A").
C>
C> Bulletin "Flag Field Separator" block - OPTION 2 (limited)
C> - bytes:
C>  - 1 - 4 Marker string (****).
C>  - 5 - 14 Total length of bulletin in bytes [octets]
C>  (not including the flag field block).
C>  - 15 - 18 Marker string (****).
C>  - 19 Line Feed (ASCII "0A").
C>
C>
C> Program history log:
C> - Stephen Gilbert 2002-09-16
C>
C> @param[in] iopt Flag Field Separator block option:
C> = 1: Separator block for use with alphanumeric bulletins.
C> if lenin <= 18 and lenbull <= 999999, OPTION 1 block will be generated.
C> if lenin > 18 or lenbull > 999999, OPTION 1a block will be generated.
C> = 2: Separator block for use with GRIB/BUFR bulletins.
C> @param[in] lenin Desired length of the flag field separator block.
C> ignored, if iopt=2.
C> @param[in] lenbull Integer length of the bulletin (in bytes) that will follow
C> this separator block.
C> @param[out] csep*(*) Character array containing the flag field separator.
C> @param[out] lenout Integer length of the flag field separator block.
C>
C> @author Stephen Gilbert @date 2002-09-16
      subroutine mkfldsep(csep,iopt,lenin,lenbull,lenout)
C
      character*(*),intent(out) :: csep
      integer,intent(in) :: iopt,lenin,lenbull
      integer,intent(out) :: lenout
C
      character(len=4),parameter :: cstar='****',clb='####'
C
      if (iopt.eq.1) then
         if ( lenin .le. 18 .and. lenbull .le. 999999 ) then
                                      ! Create OPTION 1 separator block
            csep(1:4)=clb
            csep(5:7)='018'
            write(csep(8:13),fmt='(I6.6)') lenbull
            csep(14:17)=clb
            csep(18:18)=char(10)
            lenout=18
         else                         ! Create OPTION 1a separator block
            nnn=lenin
            if ( nnn.lt.23 ) nnn=23
            csep(1:4)=clb
            write(csep(5:7),fmt='(I3.3)') nnn
            write(csep(8:18),fmt='(I11.11)') lenbull
            csep(19:nnn-5)='0'
            csep(nnn-4:nnn-1)=clb
            csep(nnn:nnn)=char(10)
            lenout=nnn
         endif
      elseif (iopt.eq.2) then         !  Create OPTION 2 separator block
         csep(1:4)=cstar
         write(csep(5:14),fmt='(I10.10)') lenbull
         csep(15:18)=cstar
         csep(19:19)=char(10)
         lenout=19
      else
         print *,"mkfldsep: Option ",iopt," not recognized."
         csep(1:lenin)=' '
      endif
C
      return
      end
