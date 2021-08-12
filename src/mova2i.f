C> @file
C> @brief This Function copies a bit string from a Character*1 variable
C> to an integer variable.
C> @author Stephen Gilbert @date 1998-12-15

C> This Function copies a bit string from a Character*1 variable
C> to an integer variable.  It is intended to replace the Fortran Intrinsic
C> Function ICHAR, which only supports 0 <= ICHAR(a) <= 127 on the
C> IBM SP.  If "a" is greater than 127 in the collating sequence,
C> ICHAR(a) does not return the expected bit value when the -qhot
C> ( and therefore -qsmp) option is used when compiling.
C> This function can be used for all values 0 <= ICHAR(a) <= 255 and
C> will work with or without the -qhot compiler option.
C>
C> Program history log:
C> - Stephen Gilbert 1998-12-15
C> - Stephen Gilbert 2001-06-11 Added a step to fill an 8-byte character
C> array with the same value so that the f90 transfer function is more
C> predictable. All bytes will now contain the desired value.
C>
C> @param[in] a Character*1 variable that holds the bitstring to extract.
C> @return mova2i() Integer value of the bitstring in character a.
C>
      Integer Function mova2i(a)
C
       integer    mold
       character(len=1) a
       character(len=1) ctemp(8)

       ctemp(1:8)=a
c       mova2i=ishft(transfer(ctemp,mold),8-bit_size(mold))
       mova2i=iand(transfer(ctemp,mold),255)

       return
       end
