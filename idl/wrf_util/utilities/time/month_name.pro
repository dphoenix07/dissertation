FUNCTION MONTH_NAME, date, SHORT = short

;+
; Name:
;      MONTH_NAME
; Purpose:
;      Return the name of a month given a date or month index. 
; Calling sequence:date
;      month_name = MONTH_NAME(date)
; Inputs:
;      date : CDATE structure or month index (1 - 12)
; Output:
;      String containing the name of the month.
; Keywords:
;      SHORT : if set, return 3-letter abbreviation of month name
; Author and history:
;      Kenneth P. Bowman.
;-

COMPILE_OPT IDL2																	;Set compile options

month_name = ['January', 'February', 'March',     $					;Long month names
              'April',   'May',      'June',      $
              'July',    'August',   'September', $
              'October', 'November', 'December']

short_name = ['JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', $			;Short month names
              'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC']

IF (SIZE(date, /TNAME) EQ 'STRUCT') THEN month = date.month $		;Find month index
												ELSE month = LONG(date)

IF ((month LT 1) OR (month GT 12)) THEN BEGIN							;Check for valid month
	MESSAGE, 'Month must be in the range 1 to 12', /CONTINUE			;Warning message
	RETURN, ''																		;Return empty string
ENDIF

IF KEYWORD_SET(short) THEN RETURN, short_name[month - 1] $			;Return month name
							 ELSE RETURN, month_name[month - 1]

END
