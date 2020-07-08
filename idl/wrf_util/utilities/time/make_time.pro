FUNCTION MAKE_TIME, year, month, day, hour, minute, second, $
   JULDAY = jday, SECONDS = seconds, NO_LEAP = no_leap

;+
;NAME:
;		MAKE_TIME
;PURPOSE:
;		This makes a Julian day plus seconds from a date and time.
;CATEGORY:
;		Date and time calculations.
;CALLING SEQUENCE:
;		date = MAKE_TIME([year, [month, [day, [hour, [minute, [second]]]]]])
;INPUT:
;		year   : Optional calendar year
;		month  : Optional month (1 to 12)
;		day    : Optional day of the month (1 to 28, 30, or 31)
;		hour   : Optional hour (0 to 23)
;		minute : Optional minute (0 to 59)
;		second : Optional second (0 to 59)
;OUTPUT:
;     t    : structure {jtime, jday: 0L, seconds: 0L} 
;            containing the date as Julian day and seconds from midnight
;KEYWORDS:
;     None.
;MODIFICATION HISTORY:
;     KPB, April, 1999.
;     Updated to include NO_LEAP option, KPB, March, 2001.
;		Cameron Homeyer, 2011-12. Vectorized.
;-

COMPILE_OPT IDL2

nt = (N_ELEMENTS(jday) > N_ELEMENTS(year) > 1)															;Set number of times

IF (N_ELEMENTS(jday) GT 1) THEN nt = SIZE(jday, /DIMENSIONS) ELSE $
IF (N_ELEMENTS(year) GT 1) THEN nt = SIZE(year, /DIMENSIONS)

IF KEYWORD_SET(no_leap) THEN time = MAKE_ARRAY(nt, VALUE = {JTIME_NO_LEAP}) $					;Create time structure (no leap days)
								ELSE time = MAKE_ARRAY(nt, VALUE = {JTIME}        )					;Create time structure (standard calendar)
           
IF (N_ELEMENTS(jday) GT 0) THEN BEGIN

	ibad = WHERE((seconds LT 0) OR (seconds GE 86400), nbad)											;Check seconds
   IF (nbad GT 0) THEN MESSAGE, 'Seconds out of range in MAKE_TIME'

   time.jday    = jday																							;Assign Julian day
   time.seconds = seconds																						;Assign seconds

ENDIF ELSE BEGIN

	IF (N_PARAMS() LT 6) THEN second = MAKE_ARRAY(nt, VALUE = 0)									;Default value for second
	IF (N_PARAMS() LT 5) THEN minute = MAKE_ARRAY(nt, VALUE = 0)									;Default value for minute
	IF (N_PARAMS() LT 4) THEN hour   = MAKE_ARRAY(nt, VALUE = 0)									;Default value for hour
	IF (N_PARAMS() LT 3) THEN day    = MAKE_ARRAY(nt, VALUE = 1)									;Default value for day
	IF (N_PARAMS() LT 2) THEN month  = MAKE_ARRAY(nt, VALUE = 1)									;Default value for month
	IF (N_PARAMS() LT 1) THEN year   = MAKE_ARRAY(nt, VALUE = 1)									;Default value for year

	IF((TOTAL(month  LT  1) GE 1) OR $
		(TOTAL(month  GT 12) GE 1)) THEN MESSAGE, 'Month out of range in MAKE_TIME.'			;Check month range
	IF((TOTAL(day    LT  1) GE 1) OR $
		(TOTAL(day    GT 31) GE 1)) THEN MESSAGE, 'Day out of range in MAKE_TIME.'				;Check day range
	IF((TOTAL(hour   LT  0) GE 1) OR $
		(TOTAL(hour   GT 23) GE 1)) THEN MESSAGE, 'Hour out of range in MAKE_TIME.'			;Check hour range
	IF((TOTAL(minute LT  0) GE 1) OR $
		(TOTAL(minute GT 59) GE 1)) THEN MESSAGE, 'Minute out of range in MAKE_TIME.'			;Check minute range
	IF((TOTAL(second LT  0) GE 1) OR $
		(TOTAL(second GT 59) GE 1)) THEN MESSAGE, 'Second out of range in MAKE_TIME.'			;Check second range

   IF KEYWORD_SET(no_leap) THEN time.jday = JULDAY_NO_LEAP(month, day, year) $				;Compute Julian day
   								ELSE time.jday = JULDAY(month, day, year)								;Compute NO_LEAP Julian day

   time.seconds = 3600*hour + 60*minute + second														;Compute seconds

ENDELSE

IF (N_ELEMENTS(time) EQ 1) THEN time = time[0]															;Convert to scalar

RETURN, time

END
