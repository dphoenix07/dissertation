FUNCTION TIME_TO_DATE, time

;+
;NAME:
;		TIME_TO_DATE
;PURPOSE:
;		This converts a JTIME structure to a CDATE structure or 
;		a JTIME_NO_LEAP structure to a CDATE_NO_LEAP structure.
;CATEGORY:
;		Date and time calculations.
;CALLING SEQUENCE:
;		date = TIME_TO_DATE(time)
;INPUT:
;		time : {JTIME} or {JTIME_NO_LEAP} structure.
;OUTPUT:
;		{CDATE} or {CDATE_NO_LEAP} structure.
;KEYWORDS:
;     None.
;MODIFICATION HISTORY:
;     Kenneth Bowman, 1999-04.
;     Modified to handle NO_LEAP date and time structures, Kenneth Bowman, 2001-03.
;		Cameron Homeyer, 2011-12. Vectorized.
;-

COMPILE_OPT IDL2

IF((TOTAL(time.seconds LT 0) GE 1) OR (TOTAL(time.seconds GE 86400) GE 1)) THEN $;Check seconds
   MESSAGE, 'Seconds out of range in TIME_TO_DATE'

CASE TAG_NAMES(time[0], /STRUCTURE_NAME) OF
   'JTIME'         : BEGIN
       CALDAT, time.jday, month, day, year													;Compute year, month, and day
       no_leap = 0
   END

   'JTIME_NO_LEAP' : BEGIN
       CALDAT_NO_LEAP, time.jday, month, day, year											;Compute pseudo-year, month, and day
       no_leap = 1
   END

   ELSE            : MESSAGE, 'Date structure must be type JTIME or JTIME_NO_LEAP'
ENDCASE

hour    = time.seconds/3600																		;Compute hour
minute  = (time.seconds - 3600*hour)/60														;Compute minute
second  = time.seconds - 3600*hour - 60*minute												;Compute second

RETURN, MAKE_DATE(year, month, day, hour, minute, second, NO_LEAP = no_leap)

END
