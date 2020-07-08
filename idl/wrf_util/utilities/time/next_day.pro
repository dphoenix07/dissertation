FUNCTION NEXT_DAY, date

;+
;NAME:
;		NEXT_DAY
;PURPOSE:
;		This functions returns the date of the day following the input date.
;CATEGORY:
;		Date and time calculations.
;CALLING SEQUENCE:
;		next = NEXT_DAY(date)
;INPUT:
;		date  : CDATE or JTIME structure
;OUTPUT:
;     CDATE structure containing the date of the first day of the month
;     following date.  Hour, minute, and second are set to 0.
;KEYWORDS:
;		None.
;MODIFICATION HISTORY:
;     K. Bowman, 2004-03-30.
;-

COMPILE_OPT IDL2																	;Set compile options

IF ((TAG_NAMES(date, /STRUCTURE_NAME) EQ 'JTIME')	OR $				;Covert to date if necessary
	 (TAG_NAMES(date, /STRUCTURE_NAME) EQ 'JTIME_NO_LEAP'))	$
	 		THEN date0 = TIME_TO_DATE(date) $
			ELSE date0 = date

next        = TIME_INC(date0, !TauDSolar)									;Increment by 1 solar day
next.hour   = 0																	;Set hour to zero
next.minute = 0																	;Set minute to zero
next.second = 0																	;Set second to zero

RETURN, next																		;Return next day

END
