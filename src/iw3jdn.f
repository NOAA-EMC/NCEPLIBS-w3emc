C> @file
C> @brief Computes julian day number from year (4 digits), month, and day.
C> @author Ralph Jones @date 1987-03-29

C> Computes julian day number from year (4 digits), month,
C> and day. iw3jdn is valid for years 1583 a.d. to 3300 a.d.
C> Julian day number can be used to compute day of week, day of
C> year, record numbers in an archive, replace day of century,
C> find the number of days between two dates.
C>
C> Program history log:
C> - Ralph Jones 1987-03-29
C> - Ralph Jones 1989-10-25 Convert to cray cft77 fortran.
C>
C> @param[in] IYEAR Integer year (4 Digits)
C> @param[in] MONTH Integer month of year (1 - 12)
C> @param[in] IDAY Integer day of month (1 - 31)
C> @return IW3JDN Integer Julian day number
C> - Jan 1, 1960 is Julian day number 2436935
C> - Jan 1, 1987 is Julian day number 2446797
C>
C> @note Julian period was devised by joseph scaliger in 1582.
C> Julian day number #1 started on Jan. 1,4713 B.C. Three major
C> chronological cycles begin on the same day. A 28-year solar
C> cycle, a 19-year luner cycle, a 15-year indiction cycle, used
C> in ancient rome to regulate taxes. It will take 7980 years
C> to complete the period, the product of 28, 19, and 15.
C> scaliger named the period, date, and number after his father
C> Julius (not after the julian calendar). This seems to have
C> caused a lot of confusion in text books. Scaliger name is
C> spelled three different ways. Julian date and Julian day
C> number are interchanged. A Julian date is used by astronomers
C> to compute accurate time, it has a fraction. When truncated to
C> an integer it is called an Julian day number. This function
C> was in a letter to the editor of the communications of the acm
C> volume 11 / number 10 / october 1968. The Julian day number
C> can be converted to a year, month, day, day of week, day of
C> year by calling subroutine w3fs26.
C>
C> @author Ralph Jones @date 1987-03-29
      FUNCTION IW3JDN(IYEAR,MONTH,IDAY)
C
       IW3JDN  =    IDAY - 32075
     &            + 1461 * (IYEAR + 4800 + (MONTH - 14) / 12) / 4
     &            + 367 * (MONTH - 2 - (MONTH -14) / 12 * 12) / 12
     &            - 3 * ((IYEAR + 4900 + (MONTH - 14) / 12) / 100) / 4
       RETURN
       END
