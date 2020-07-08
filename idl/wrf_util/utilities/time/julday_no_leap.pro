FUNCTION JULDAY_NO_LEAP, month, day, year

;+
;NAME:
;		JULDAY_NO_LEAP
;PURPOSE:
;		This function computes a pseudo-Julian day for a calendar without leap days given a 
;		pseudo-date (month, day, and year).  The reference date is taken to be 1 January 2001.  
;		The inverse of this procedure is CALDAT_NO_LEAP.
;CATEGORY:
;		Date and time calculations.
;CALLING SEQUENCE:
;		jday = JULDAY_NO_LEAP(month, day, year)
;INPUT:
;		month  : month (1 - 12)
;		day    : day of the month (1 - 28, 30, or 31)
;		year   : calendar year
;OUTPUT:
;		LONG variable containing the psuedo-Julian day for a calendar without leap days.
;KEYWORDS:
;		None.
;PROCEDURE:
;		This function computes the day number from 1 January of a 365-day reference year using
;		the built in function JULDAY.  The psuedo-Julian day is then computed assuming
;		that every year has exactly 365 days.  The reference year must be a non-leap year.
;		Arguments are checked to ensure that the date requested is not February 29, that
;		month lies in the range (1-12), and that day lies in the range (1-31).
;MODIFICATION HISTORY:
;		Kenneth Bowman, 2001-03.
;		Cameron Homeyer, 2011-12. Vectorized.
;-

COMPILE_OPT IDL2

ileapday = WHERE((month EQ 2) AND (day EQ 29), nleapday)														;Check for leap day
IF(nleapday GT 0) THEN MESSAGE, 'February 29 not permitted in JULDAY_NO_LEAP'							;Check for leap day

IF((TOTAL(month  LT  1) GE 1) OR $
	(TOTAL(month  GT 12) GE 1)) THEN MESSAGE, 'Month out of range in JULDAY_NO_LEAP.'				;Check month range
IF((TOTAL(day    LT  1) GE 1) OR $
	(TOTAL(day    GT 31) GE 1)) THEN MESSAGE, 'Day out of range in JULDAY_NO_LEAP.'					;Check day range

RETURN, JULDAY(month, day, 2001) + 365*(year - 2001)

END
