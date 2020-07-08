FUNCTION MAKE_ISO_DATE_STRING, date, PRECISION = precision, COMPACT = compact, UTC = utc

; NAME:
;   MAKE_ISO_DATE_STRING
; PURPOSE:
;	 This function converts a cdate structure to an ISO 8601 string representation.
; CATEGORY:
;	 Date and time calculations.
; CALLING SEQUENCE:
;	 date_string = MAKE_ISO_DATE_STRING(date)
; INPUT:
;	 date      : a {CDATE}, {CDATE_NO_LEAP}, {JTIME}, or {JTIME_NO_LEAP} structure.
; OUTPUT:
;	 String containing the date as yyyy-mm-dd hh:mm:ss or other ISO variants.
;   Examples:  2001-01-01 12:33:17
;              2001-01-01 12Z			(PRECISION = 'hour', /UTC)
;              20010101T123317Z		(/COMPACT, /UTC)
; KEYWORDS:
;	 precision : Optional keyword to set the desired precision.  Acceptable values are
;               'year', 'month', 'day', 'hour', 'minute', 'second'.  Other values are
;               ignored.  Not case sensitive.
;	 compact   : If set, use compact notation.  (Omit date and time separators, use 'T' to separate
;               date from time
;	 UTC       : If set, 'Z' is added after the time to indicate that the time is UTC.
; COMMON BLOCKS:
;	 None.
; RESTRICTIONS:
;	 None.
; MODIFICATION HISTORY:
;   Kenneth Bowman, 2001-09-10.
;	 Cameron Homeyer, 2011-12. Vectorized.
;-

COMPILE_OPT IDL2

IF ((TAG_NAMES(date[0], /STRUCTURE_NAME) EQ 'JTIME')	OR $
	 (TAG_NAMES(date[0], /STRUCTURE_NAME) EQ 'JTIME_NO_LEAP'))	THEN date0 = TIME_TO_DATE(date) $
																					ELSE date0 = date

year		= STRING(date0.year,   FORMAT = "(I4.4)")
month		= STRING(date0.month,  FORMAT = "(I2.2)")
day		= STRING(date0.day,    FORMAT = "(I2.2)")
hour		= STRING(date0.hour,   FORMAT = "(I2.2)")
minute	= STRING(date0.minute, FORMAT = "(I2.2)")
second	= STRING(date0.second, FORMAT = "(I2.2)")

IF KEYWORD_SET(compact) THEN BEGIN															;Define separators for compact notation
	date_sep      = ''
	time_sep      = ''
	date_time_sep = 'T'
ENDIF ELSE BEGIN																					;Define separators for standard notation
	date_sep      = '-'
	time_sep      = ':'
	date_time_sep = ' '
ENDELSE

IF KEYWORD_SET(utc) THEN suffix = 'Z' ELSE suffix = ''

IF KEYWORD_SET(precision) THEN BEGIN
	CASE STRUPCASE(precision) OF
		'YEAR'   : RETURN, year
		'MONTH'  : RETURN, year + date_sep + month
		'DAY'    : RETURN, year + date_sep + month  + date_sep + day
		'HOUR'   : RETURN, year + date_sep + month  + date_sep + day + date_time_sep + hour + suffix
		'MINUTE' : RETURN, year + date_sep + month  + date_sep + day + date_time_sep + hour + time_sep + minute + suffix
		'SECOND' : RETURN, year + date_sep + month  + date_sep + day + date_time_sep + hour + time_sep + minute + time_sep + second + suffix
		ELSE     : RETURN, year + date_sep + month  + date_sep + day + date_time_sep + hour + time_sep + minute + time_sep + second + suffix
	ENDCASE
ENDIF ELSE BEGIN
	RETURN, year + date_sep + month  + date_sep + day + date_time_sep + hour + time_sep + minute + time_sep + second + suffix
ENDELSE

END
