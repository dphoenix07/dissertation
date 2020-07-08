FUNCTION CURRENT_DAY, date

;+
;NAME:
;		CURRENT_DAY
;PURPOSE:
;		This functions returns the date of the current day, with hour, minute, and second set to 0.
;CATEGORY:
;		Date and time calculations.
;CALLING SEQUENCE:
;		current = CURRENT_DAY(date)
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
current.hour   = 0																;Set hour to zero
current.minute = 0																;Set minute to zero
current.second = 0																;Set second to zero

RETURN, current																	;Return next day

END
