FUNCTION DATE_SET_PRECISION, date, precision

;+
;NAME:
;		DATE_SET_PRECISION
;PURPOSE:
;		This functions sets the precision of an existing date structure.
;CATEGORY:
;		Date and time calculations.
;CALLING SEQUENCE:
;		date = DATE_SET_PRECISION(date, precision)
;INPUT:
;		date      : CDATE structure
;		precision : string variable containing 'YEAR', 'MONTH', 'DAY', 'HOUR', or 'MINUTE'
;OUTPUT:
;     CDATE structure with values of components less significant than precision set to 0 or 1.
;		Examples:
;			DATE_SET_PRECISION(MAKE_DATE(2001,6,15,12,30,49), 'DAY') returns 2001-06-15 00:00:00.
;			DATE_SET_PRECISION(MAKE_DATE(2001,6,15,12,30,49), 'HOUR') returns 2001-06-15 12:00:00.
;			DATE_SET_PRECISION(MAKE_DATE(2001,6,15,12,30,49), 'YEAR') returns 2001-01-01 00:00:00.
;KEYWORDS:
;		None.
;MODIFICATION HISTORY:
;     K. Bowman, 2010-12-02.
;-

COMPILE_OPT IDL2																	;Set compile options

date0 = date																		;Make copy of date

SWITCH STRUPCASE(precision) OF
	'YEAR'   : date0.month  = 1
	'MONTH'  : date0.day    = 1
	'DAY'    : date0.hour   = 0
	'HOUR'   : date0.minute = 0
	'MINUTE' : date0.second = 0
	'SECOND' :
ENDSWITCH

RETURN, date0																		;Return next day

END
