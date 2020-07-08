FUNCTION PREVIOUS_MONTH, date

;+
;NAME:
;		PREVIOUS_MONTH
;PURPOSE:
;		This functions returns the year and month of the month prior to
;     the input argument date.  The output day is set to 1; and hour, minute, and
;		second are set to 0.
;CATEGORY:
;		Date and time calculations.
;CALLING SEQUENCE:
;		next = PREVIOUS_MONTH(date)
;INPUT:
;		date : CDATE or JTIME structure
;OUTPUT:
;     CDATE or CDATE_NO_LEAP structure containing the date of the first 
;     day of the month prior to the input date.
;KEYWORDS:
;		None.
;PROCEDURE:
;		If month is equal to 12, increment the year.  Increment the month modulo 12.
;MODIFICATION HISTORY:
;     K. Bowman, 2004-03-30.
;     K. Bowman, 2005-03-25.  Added NO_LEAP support.
;		C. Homeyer, 2011-09-28. Updated from NEXT_MONTH. 
;-

COMPILE_OPT IDL2																			;Set compile options

IF ((TAG_NAMES(date, /STRUCTURE_NAME) EQ 'JTIME')	OR $						;Convert to date structure if necessary
	 (TAG_NAMES(date, /STRUCTURE_NAME) EQ 'JTIME_NO_LEAP'))	$
	 		THEN date0 = TIME_TO_DATE(date) $
			ELSE date0 = date

IF (TAG_NAMES(date0, /STRUCTURE_NAME) EQ 'CDATE_NO_LEAP') $					;Check for NO_LEAP date
		THEN no_leap = 1 $
		ELSE no_leap = 0

IF (date0.month EQ 1) THEN year = date0.year -1 $
							 ELSE year = date0.year

RETURN, MAKE_DATE(year, ((date0.month + 10) MOD 12) + 1, NO_LEAP = no_leap)

END
