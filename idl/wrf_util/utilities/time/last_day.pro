FUNCTION LAST_DAY, date

;+
;NAME:
;		LAST_DAY
;PURPOSE:
;		This function returns the date for the last the day of the month
;     specified by the input argument date.  The year and month are not 
;     change.  The hour, minute, and second are set to 0.
;CATEGORY:
;		Date and time calculations.
;CALLING SEQUENCE:
;		last = LAST_DAY(date)
;INPUT:
;		date  : CDATE or JTIME structure
;OUTPUT:
;     CDATE structure containing the date of the last day of the specified month.
;KEYWORDS:
;		None.
;PROCEDURE:
;		Subtract one day from the first day of the subsequent month.
;MODIFICATION HISTORY:
;     K. Bowman, 2004-03-30.
;-

COMPILE_OPT IDL2															;Set compile options

IF ((TAG_NAMES(date, /STRUCTURE_NAME) EQ 'JTIME')	OR $
	 (TAG_NAMES(date, /STRUCTURE_NAME) EQ 'JTIME_NO_LEAP'))	THEN date0 = TIME_TO_DATE(date) $
																				ELSE date0 = date

RETURN, TIME_INC(NEXT_MONTH(date0), - !TauDSolar)				;Return day preceding first day of next month

END
