C> @file
C> @brief Transfers default integers to 16 bit ints.
C> @author Dennis Keyser @date 1992-06-29

C> Transfers an array of numbers from default integer
C> words to 16 bit (IBM integer*2) IBM half-words.
C>
C> Program history log:
C> - Dennis Keyser 1992-06-29
C> - Stephen Gilbert 1998-11-17 Removed Cray references.
C>
C> @param[in] IN Starting address for array of default integers
C> @param[in] NUM Number of numbers in 'IN' to transfer.
C> @param[out] IDEST Starting address for array of 16 bit IBM half-words
C> @param[out] IER Error return code as follows:
C> IER = 0 - Transfer successful, all numbers
C> - Transferred without overflow.
C> IER = 1 - The transfer of one or more numbers
C> - Resulted in an overflow.
C>
C> @note This is the inverse of library routine w3fi02().
C>
C> @author Dennis Keyser @date 1992-06-29
      SUBROUTINE W3FI03(IN,IDEST,NUM,IER)
C
      INTEGER(2)  IDEST(*)
      INTEGER  IN(*)
C
      SAVE
C
C      CALL USICTI(IN,IDEST,1,NUM,2,IER)
      IDEST(1:NUM)=IN(1:NUM)
C
      RETURN
      END
