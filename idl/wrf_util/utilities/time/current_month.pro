FUNCTION CURRENT_MONTH, date

;+
;NAME:
;		CURRENT_MONTH
;PURPOSE:
;		This functions returns the date of the first day of the current month, with day set to 1 and
;		hour, minute, and second set to 0.
;CATEGORY:
;		Date and time calculations.
;CALLING SEQUENCE:
;		current = CURRENT_MONTH(date)
;INPUT:
;		date  : CDATE or JTIME structure
;OUTPUT:
;     CDATE structure containing the current date with the hour, minute, and second set to 0.
;KEYWORDS:
;		None.
;MODIFICATION HISTORY:
;     K. Bowman, 2011-01-31.
;-

COMPILE_OPT IDL2																	;Set compile options

IF ((TAG_NAMES(date, /STRUCTURE_NAME) EQ 'JTIME')	OR $				;Covert to date if necessary
	 (TAG_NAMES(date, /STRUCTURE_NAME) EQ 'JTIME_NO_LEAP'))	$
	 		THEN date0 = TIME_TO_DATE(date) $
			ELSE date0 = date

current        = date															;Copy day
current.day    = 1																;Set day to 1
current.hour   = 0																;Set hour to zero
current.minute = 0																;Set minute to zero
current.second = 0																;Set second to zero

RETURN, current																	;Return current month

END
