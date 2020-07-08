FUNCTION NEXT_YEAR, date

;+
;NAME:
;		NEXT_YEAR
;PURPOSE:
;		This functions returns the year following the input argument date.  
;		The output month and day are set to 1, and hour, minute, and
;		second are set to 0.
;CATEGORY:
;		Date and time calculations.
;CALLING SEQUENCE:
;		next = NEXT_YEAR(date)
;INPUT:
;		date : CDATE or JTIME structure
;OUTPUT:
;     CDATE or CDATE_NO_LEAP structure containing the date of the first 
;     day of the month following the input date.
;KEYWORDS:
;		None.
;PROCEDURE:
;		Increment the year.
;MODIFICATION HISTORY:
;     K. Bowman, 2005-12-03.
;-

COMPILE_OPT IDL2																	;Set compile options

IF ((TAG_NAMES(date, /STRUCTURE_NAME) EQ 'JTIME')	OR $				;Convert to date structure if necessary
	 (TAG_NAMES(date, /STRUCTURE_NAME) EQ 'JTIME_NO_LEAP'))	$
	 		THEN date0 = TIME_TO_DATE(date) $
			ELSE date0 = date

IF (TAG_NAMES(date0, /STRUCTURE_NAME) EQ 'CDATE_NO_LEAP') $			;Check for NO_LEAP date
		THEN no_leap = 1 $
		ELSE no_leap = 0

RETURN, MAKE_DATE(date0.year + 1, NO_LEAP = no_leap)					;Compute next month

END
