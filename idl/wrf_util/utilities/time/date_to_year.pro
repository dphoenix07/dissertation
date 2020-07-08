FUNCTION DATE_TO_YEAR, date

;+
;NAME:
;     DATE_TO_YEAR
;PURPOSE:
;     This converts a CDATE structure to a time in years (year plus decimal fraction).
;CATEGORY:
;     Date and time calculations.
;CALLING SEQUENCE:
;     year = DATE_TO_YEAR(date)
;INPUT:
;     date : {CDATE} or {CDATE_NO_LEAP} structure.
;OUTPUT:
;		date in years (FLOAT).
;KEYWORDS:
;     None.
;MODIFICATION HISTORY:
;     KPB, April, 1999.
;     Modified to handle NO_LEAP date and time structures, KPB, March, 2001.
;-

COMPILE_OPT IDL2

CASE TAG_NAMES(date, /STRUCTURE_NAME) OF
   'CDATE'         : no_leap = 0
   'CDATE_NO_LEAP' : no_leap = 1
   ELSE            : MESSAGE, 'Date structure must be type CDATE or CDATE_NO_LEAP'
ENDCASE

RETURN, date.year + DOUBLE(TIME_DIFF(date,                   MAKE_DATE(date.year))) / $
						  DOUBLE(TIME_DIFF(MAKE_DATE(date.year+1), MAKE_DATE(date.year)))

END
