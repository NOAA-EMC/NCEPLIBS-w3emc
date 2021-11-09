C> @file
C> @brief Assembler language to move data
C> @author Unknown @date Unknown

C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> Unkonwn | Unknonw | Initial.
C>
C> @param[out] OUT
C> @param[in] IN
C> @param IBYTES
C> This subroutine may not be needed, its was in
C> assembler language to move data, it ran about three
C> times faster than a fortan do loop, it was used to
C> make sure the data to be unpacked was on a word boundary,
C> this may not be needed on some brands of computers.
C>
C> @author Unknown @date Unknown
       SUBROUTINE XMOVEX(OUT,IN,IBYTES)
       CHARACTER*1 OUT(*)
       CHARACTER*1 IN(*)
C
       INTEGER     IBYTES
C
       DO 100 I = 1,IBYTES
         OUT(I) = IN(I)
  100  CONTINUE
C
       RETURN
       END
