C> @file
C> @brief Get parm field from command-line.
C> @author David Shimomura @date 1995-05-23

C> To get the one command-line argument which starts with
C> "parm="; returning the parm field (without the keyword "parm=")
C> as a null-terminated string in the character string:cparm.
C>
C> Program history log:
C> - David Shimomura 1995-05-23
C> - Boi Vuong 1998-03-10 Remove the cdir$ integer=64 directive
C>
C> @param[out] NCH_PARM No. of characters in the parm field
C> @param[out] CPARM C*(*) cparm -- the destination for the parmfield
C> obtained from the command line; user should define the character string for
C> a size .le. 101-bytes, which would be big enough for the 100-char ibm
C> limit plus one extra byte for my null-terminator.
C> @param[out] iret_parm - Return code
C>  - = 0; Normal return
C>  - = -1; Abnormal exit. the user has failed
C> to define the cparm destination as a character string.
C>
C>  - = +1; A Warning:
C> the given arg in the command line was
C> too long to fit in the destination: cparm,
C> so i have truncated it.
C>
C>  - = +2; A warning:  no args at all on command line,
C> so i could not fetch the parm field.
C>
C>  - = +3; A warning:  no "parm="-argument exists
C> among the args on the command line,
C> so i could not fetch the parm field.
C>
C> - OKL:
C>  - FT06F001 - Some checkout printout
C>
C> @note To emulate the ibm parm field, the user should key_in on the
C> command line:
C> - parm='in between the single_quotes is the parm field'
C> what is returned from w3as00() from the parm= arg is
C> the parm field: which starts with the location beyond the
C> equal_sign of the keyword "parm=", and includes everything
C> which was within the bounds of the single-quote signs.
C> But the quote signs themselves will disappear; and a null-
C> terminator will be added.
C> The starting "parm=" is a key word for the parms, and should
C> not be used to start any other argument.
C>
C> @note I have changed the call sequence by adding a return code.
C>
C> @author David Shimomura @date 1995-05-23
      subroutine W3AS00(nch_parm,cparm,iret_parm)
C
       integer    kbytpwrd
       parameter (kbytpwrd=8)
       integer    maxnbyt
       parameter (maxnbyt=112)
C      ... WHERE 112 CHARACTERS IS SIZE OF CWORK FOR 100 CHARACTERS
C      ...   WITHIN QUOTES + 'PARM=' + BACKSLASHES + LINEFEEDS

       integer    maxnwrds
       parameter (maxnwrds=maxnbyt/kbytpwrd)

C      ... call seq. args ...
       INTEGER       NCH_PARM
       CHARACTER*(*) CPARM
       integer       iret_parm

C
C      ... FUNCTIONS ...
       external  lastch
       integer   lastch
       external  notrail
       integer   notrail
C      -------------------------------------------------------------
       integer        jwork(maxnwrds)
       character*112  cwork
       equivalence   (jwork,cwork)

       integer(4)     nargsinline,iargc,iar
       integer        nchars
       integer        lmt_txt
       integer        non_parm

       LOGICAL        LPARMQQ
       character*1    KLF
       character*1    NULLCHR
       character*1    lonech

C      . . . . . . . .   S T A R T   . . . . . . . . . . . . . . . .

       NULLCHR = char(0)
       KLF     = char(10)
C
       iret_parm = 0
       non_parm = 0

       LPARMQQ = .FALSE.
       NCH_PARM = 0

       lmt_dest = len(cparm)
       write(6,103)lmt_dest
  103  format(1h ,'W3AS00: dimensioned size (in bytes) of dest strng=',
     1             I11)
       if(lmt_dest .le. 0) then
         write(6,105)
  105    format(1h ,'W3AS00:FAILED on undefined destination ',
     1              'character string: CPARM')
         iret_parm = -1
         nch_parm = 0
         go to 999
       else if (lmt_dest .gt. 101) then
         lmt_dest = 101
       endif
       lmt_txt = lmt_dest - 1

       cparm(1:lmt_dest) = ' '

       narg_got = 0
C
       nargsinline = iargc()

       write(6,115) nargsinline
  115  format(1h ,'W3AS00: count of args found in command line =', I3)

       if(nargsinline .gt. 0) then
C        ... to scan every argument, looking only for the Arg which
C        ...    starts with "PARM="
         do  iar = 1,nargsinline
           LPARMQQ = .FALSE.

           cwork(1:) = ' '

           call getarg(iar,cwork)

           narg_got = narg_got + 1
           nchars = lastch(cwork)

           if(nchars .le. 0) then
             write(6,125)iar
  125        format(1h ,'W3AS00:getarg() returned an empty arg for',
     A                  ' no.',I3 )
           else
C            ... SOME TEXT EXISTS IN THIS ARG ...
C            ...   DOES IT START WITH "PARM=" ???
             if((cwork(1:5) .EQ. 'PARM=') .OR.
     1          (cwork(1:5) .EQ. 'parm=') ) then
               LPARMQQ = .TRUE.
C              ... this arg is special case of PARM=
C              ... which can include blanks, so cannot lastch() it ...
               nchars = notrail(cwork)
             endif
C ...             iwdss = ((nchars-1)/kbytpwrd) + 1
C            ... where iwdss points to last word so I could hex dump
C            ...    that last word, to see if NULL is there
C            ... There was no NULL; only blank fill.
             IF(LPARMQQ) THEN
C              ... FILTER OUT ANY BACKSLASH or LINE_FEED ...
               ioutc = 0
               do  inc = 6,nchars
                 if(ioutc .LT. lmt_txt) then
                   lonech = cwork(inc:inc)
                   if((lonech .EQ. '\\') .OR.
     1                (lonech .EQ. KLF)) then
                   else
                     ioutc = ioutc + 1
                     cparm(ioutc:ioutc) = lonech
                   endif
                 else
C                  ... comes here if ioutc .GE. lmt_txt,
C                  ... so I cannot increment ioutc for this inc char
C                  ... so truncate the string at (1:ioutc)
C                  ... a warning be return-coded ...
                   iret_parm = +1
                   go to 155
                 endif
               enddo
  155          continue
               nch_parm = ioutc
               np1 = nchars+1
               cparm(np1:np1) = NULLCHR
               go to 999
C              ... jump out of DO when PARM has been processed ...
             else
C              ... this is .not. a PARM field, do nothing w/ those,
               non_parm = non_parm + 1
             endif

           endif
         enddo
C        ... IF IT FALLS THRU BOTTOM OF DO, THEN IT DID NOT FIND
C        ...    THE PARM FIELD AMONG THE EXISTING ARGS
         iret_parm = 3
         nch_parm = 0

       ELSE
C        ... COMES HERE IF nargsinline = 0, so there were no args at all
         iret_parm = 2
         nch_parm = 0
       endif
       go to 999

  999  continue
       return
       end
        integer function lastch(str)
C       ... lastch() ... to point to the last character of a character
C       ...              string
C       ...        String terminators are first BLANK or NULL character
C       ...        encountered.
C       ... Caution:  I will limit scan on LEN(str)
C                     so you must give me a character string.
C

        character*(*) str

        character*1  NULLCHR
        character*1  BLANK
C
        integer    i
        integer    limit
C
        NULLCHR = char(0)
        BLANK = ' '
        limit = len(str)
        i = 0
        do while(i .LT. limit .AND. str(i+1:i+1) .NE. NULLCHR
     1                        .AND. str(i+1:i+1) .NE. BLANK)
           i = i + 1
        enddo

        lastch = i
        return
        end
        integer function notrail(str)
C       ...       mods for CRAY version               8-Dec-1994/dss
C
C       ... notrail() ... to point to the last non-blank character of a
C       ...               character string (which can have leading
C                         blanks and intermediate blanks); but after
C                         ignoring all trailing blank characters.
C       ...        String terminators are last BLANK or first NULL
C       ...        character encountered.
C
C       ...        This differs from LASTCH() which stops on first
C       ...        BLANK encountered when scanning from the start;
C       ...        NOTRAIL() will scan backwards from the end of the
C       ...        string, skipping over trailing blanks, until the
C       ...        last non-blank character is hit.
C       ...
C       ... Caution:  I will limit scan on LEN(str)
C                     so you must give me a character string.
C

        character*(*) str

        character*1  BLANK
        parameter   (BLANK = ' ')
C
        integer    i
        integer    limit
        integer    limitnl
        character*1  NULLCHR
C
        NULLCHR = char(0)
        i = 0
        limitnl = 0
        limit = len(str)
        if(limit .le. 0) go to 999
C       ... otherwise, at least one char len string ...
        limitnl = index(str(1:limit),NULLCHR)
        if(limitnl .le. 0) then
C         ... no NULLCHR exists in str(1:limit) ...
C         ... so go scan from limit
          go to 300

        else if(limitnl .eq. 1) then
          go to 999
C         ... which jumped out w/ pointer=0 if NULL in first position
        else
C         ... a NULLCHR existed within str(1:limit); so
C         ...   I want to scan backwards from before that NULLCHR
C         ...   which is located at limitnl
          limit = limitnl - 1
        endif
        if(limit .le. 0) go to 999
  300   continue
C       ... otherwise, we have a string of at least one char to look at
C       ... which has no NULLCHR in interval (1:limit)
        i = limit
        do while((i .GT. 0) .AND. (str(i:i) .EQ. BLANK))
           i = i - 1
        enddo

  999   continue
        notrail = i
        return
        end
