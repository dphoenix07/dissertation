FUNCTION TIME_INC, t0, dt, CHECK = check

;+
;NAME:
;     TIME_INC
;PURPOSE:
;     This computes the time given a start time and an increment.
;CATEGORY:
;     Date and time calculations.
;CALLING SEQUENCE:
;     time = TIME_INC(t, dt)
;INPUT:
;     t0   : structure {jtime, jday: 0L, seconds: 0L} 
;            containing the date as Julian day and seconds from midnight
;     dt   : time increment in seconds (positive or negative)
;OUTPUT:
;     t1   : structure {jtime, jday: 0L, seconds: 0L} 
;            containing the date as Julian day and seconds from midnight
;KEYWORDS:
;     check: If set, input parameter range checking is turned on.
;            Check can also be set to a two-element array containing
;            the lower and upper limits on the Julian day.
;PROCEDURE:
;     This function does some simple range checking on the input parameters
;     and then computes the time difference.
;MODIFICATION HISTORY:
;     Kenneth Bowman, 1995-08.
;     Kenneth Bowman, 2001-09-12.  Updated to handle CDATE structures as well as JTIME structures.
;		Cameron Homeyer, 2011-12. Vectorized.
;-

COMPILE_OPT IDL2

nt = (N_ELEMENTS(year) > 1)																		;Set number of times

IF ((TAG_NAMES(t0[0], /STRUCTURE_NAME) EQ 'CDATE')	OR $
	 (TAG_NAMES(t0[0], /STRUCTURE_NAME) EQ 'CDATE_NO_LEAP'))	THEN t0_temp = DATE_TO_TIME(t0) $
																				ELSE t0_temp = t0

IF KEYWORD_SET(check) THEN BEGIN																	;Check input parameters
   IF(N_ELEMENTS(check) EQ 1) THEN BEGIN
      jdaymin = JULDAY(1, 1, 1900)																;Default lower limit on Julian day
      jdaymax = JULDAY(1, 1, 2100)																;Default upper limit on Julian day
   ENDIF ELSE BEGIN
      jdaymin = check(1)
      jdaymax = check(2)
   ENDELSE
   IF((t0_temp.jday LT jdaymin) OR (t0_temp.jday GT jdaymax)) THEN $       		;Check Julian day
      MESSAGE, 'Julian day out of range, t0 in TRAJ3D_TIME_INC'
   IF((t0_temp.seconds LT 0) OR (t0_temp.seconds GE 86400 )) THEN $					;Check seconds
      MESSAGE, 'Seconds out of range, t0 in TRAJ3D_TIME_INC'
ENDIF

IF (N_ELEMENTS(dt) GT 1) AND (N_ELEMENTS(t0) EQ 1) THEN $
	t0_temp = REPLICATE(t0_temp, N_ELEMENTS(dt))

t1 = t0_temp
ts = t0_temp.seconds + LONG64(dt)

jday    = t1.jday
seconds = t1.seconds
it      = WHERE((ts GE 0LL), tcount, COMPLEMENT = ic, NCOMPLEMENT = ccount)				;Search for positive seconds

IF (tcount GT 0) THEN BEGIN
   jday[it]    = LONG(jday[it] + (ts[it]/86400LL))
   seconds[it] = LONG(ts[it] MOD 86400LL)
ENDIF
IF (ccount GT 0) THEN BEGIN
   jday[ic]    = LONG(jday[ic] + ((ts[ic]+1L)/86400LL) - 1LL)
   seconds[ic] = LONG(((ts[ic] MOD 86400LL) + 86400LL) MOD 86400LL)
ENDIF

t1.jday    = jday
t1.seconds = seconds

IF ((TAG_NAMES(t0[0], /STRUCTURE_NAME) EQ 'CDATE')	OR $
	 (TAG_NAMES(t0[0], /STRUCTURE_NAME) EQ 'CDATE_NO_LEAP'))	THEN RETURN, TIME_TO_DATE(t1) $
																				ELSE RETURN, t1

END
