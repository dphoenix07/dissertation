FUNCTION MONTH_NAME_KPB, date, SHORT = short

;+
; Name:
;      MONTH_NAME_KPB
; Purpose:
;      Return the name of a month. 
; Calling sequence:
;      month_name = MONTH_NAME_KPB(month)
; Inputs:
;      date : date can be a CDATE structure or the actual month index (1 - 12)
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

IF (SIZE(date, /TNAME) EQ 'STRUCT') THEN month = date.month $		;If CDATE, extract month
												ELSE month = date					;Date is month index

IF ((LONG(month) LT 1) OR (LONG(month) GT 12)) THEN BEGIN			;Check for valid month
	MESSAGE, 'month must be in the range 1 to 12', /CONTINUE			;Warning message
	RETURN, ''																		;Return empty string
ENDIF

IF KEYWORD_SET(short) THEN RETURN, short_name[month-1] $				;Return function value
							 ELSE RETURN, month_name[month-1]

END
