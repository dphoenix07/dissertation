PRO CALDAT_NO_LEAP, jday, month, day, year

;+
;NAME:
;		CALDAT_NO_LEAP
;PURPOSE:
;		This procedure computes the pseudo-date for a calendar without leap days given 
;		a pseudo-Julian day.  The reference date is taken to be 2001-01-01.
;		The inverse of this procedure is JULDAY_NO_LEAP.
;CATEGORY:
;		Date and time calculations.
;CALLING SEQUENCE:
;		CALDAT_NO_LEAP, jday, month, day, year
;INPUT:
;		jday   : pseudo-Julian day for a calendar with no leap years.
;OUTPUT:
;		month  : month (1 - 12)
;		day    : day of the month (1 - 28, 30, or 31)
;		year   : calendar year
;KEYWORDS:
;		None.
;PROCEDURE:
;		This function computes the pseudo-date for a 365-day year using
;		the built in function CALDAT.  The pseudo-date is computed assuming
;		that every year has exactly 365 days.  The reference year must be a non-leap year.
;MODIFICATION HISTORY:
;		Kenneth Bowman, 2001-03.
;-

COMPILE_OPT IDL2

year = 2001 + FLOOR((jday - JULDAY(1, 1, 2001) + 0.5)/365)
CALDAT, jday - 365*(year - 2001), month, day, ryear

END
