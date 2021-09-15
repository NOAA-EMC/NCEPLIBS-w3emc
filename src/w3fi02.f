C> @file
C> @brief Transfers array from 16 to 64 bit words.
C> @author Dennis Keyser @date 1992-06-29

C> Transfers an array of numbers from 16 bit (ibm integer*2)
C> IBM half-words to default integers.
C>
C> Program history log:
C> - Dennis Keyser 1992-06-29
C> - Stephen Gilbert 1998-11-17 Removed Cray references.
C>
C> @param[in] IN Starting address for array of 16 bit IBM half-words.
C> @param[in] NUM Number of numbers in 'IN' to transfer.
C> @param[out] IDEST Starting address for array of output integers.
C>
C> @note This is the inverse of library routine w3fi03.
C>
C> @author Dennis Keyser @date 1992-06-29
      SUBROUTINE W3FI02(IN,IDEST,NUM)
C
      INTEGER(2)  IN(*)
      INTEGER  IDEST(*)
C
      SAVE
C
C      CALL USICTC(IN,1,IDEST,NUM,2)
       IDEST(1:NUM)=IN(1:NUM)
C
      RETURN
      END
