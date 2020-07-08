PRO CALDAY_TO_DATE, year, calday, month, day

;+
;NAME:
;		CALDAY_TO_DATE
;PURPOSE:
;		This functions returns the date for a given year and day number.  The
;     day number is the integral number of days between t and January 1 of
;     the same year.  The day number of January 1 is 1.
;CATEGORY:
;		Date and time calculations.
;CALLING SEQUENCE:
;		CALDAY_TO_DATE, year, calday, month, day
;INPUT:
;     year   : year
;		calday : calendar day number
;OUTPUT:
;		date  : CDATE or JTIME structure containing a valid date or time
;KEYWORDS:
;		None.
;;MODIFICATION HISTORY:
;     K. Bowman, 2006-08-24.
;-

COMPILE_OPT IDL2																			;Set compile options

CALDAT, JULDAY(1, 1, year) + calday - 1, month, day, year					;Find Julian day and convert to date

END
