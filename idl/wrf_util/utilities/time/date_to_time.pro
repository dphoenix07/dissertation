FUNCTION DATE_TO_TIME, date

;+
;NAME:
;     DATE_TO_TIME
;PURPOSE:
;     This converts a CDATE structure to a JTIME structure or 
;     a CDATE_NO_LEAP structure to a JTIME_NO_LEAP structure.
;CATEGORY:
;     Date and time calculations.
;CALLING SEQUENCE:
;     time = DATE_TO_TIME(date)
;INPUT:
;     date : {CDATE} or {CDATE_NO_LEAP} structure.
;OUTPUT:
;		{JTIME} or {JTIME_NO_LEAP} structure.
;KEYWORDS:
;     None.
;MODIFICATION HISTORY:
;     KPB, April, 1999.
;     Modified to handle NO_LEAP date and time structures, KPB, March, 2001.
;		Cameron Homeyer, 2011-12. Vectorized.
;-

COMPILE_OPT IDL2

CASE TAG_NAMES(date[0], /STRUCTURE_NAME) OF
   'CDATE'         : no_leap = 0
   'CDATE_NO_LEAP' : no_leap = 1
   ELSE            : MESSAGE, 'Date structure must be type CDATE or CDATE_NO_LEAP'
ENDCASE

RETURN, MAKE_TIME(date.year, date.month,  date.day, $
                  date.hour, date.minute, date.second, NO_LEAP = no_leap)

END
