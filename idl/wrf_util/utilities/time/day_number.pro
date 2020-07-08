FUNCTION DAY_NUMBER, t

;+
;NAME:
;		DAY_NUMBER
;PURPOSE:
;		This functions returns the day number for a given date or time, t.  The
;     day number is the integral number of days between t and January 1 of
;     the same year.  The day number of January 1 is 1.
;CATEGORY:
;		Date and time calculations.
;CALLING SEQUENCE:
;		daynum = DAY_NUMBER(t)
;INPUT:
;		t  : CDATE or JTIME structure containing a valid date or time
;OUTPUT:
;     Integer containing the day number.
;KEYWORDS:
;		None.
;;MODIFICATION HISTORY:
;     K. Bowman, 2004-03-30.
;-

COMPILE_OPT IDL2																			;Set compile options

IF ((TAG_NAMES(t, /STRUCTURE_NAME) EQ 'CDATE')	OR $
	 (TAG_NAMES(t, /STRUCTURE_NAME) EQ 'CDATE_NO_LEAP'))	THEN 	BEGIN
	time  = DATE_TO_TIME(t)																;Convert t to JTIME
	jday0 = JULDAY(1, 1, t.year)														;Find Julian day of January 1
	RETURN, time.jday - jday0 + 1														;Return day number
ENDIF ELSE BEGIN
	date  = TIME_TO_DATE(t)																;Convert t to CDATE
	jday0 = JULDAY(1, 1, date.year)													;Find Julian day of January 1
	RETURN, t.jday - jday0 + 1															;Return day number
ENDELSE

END
