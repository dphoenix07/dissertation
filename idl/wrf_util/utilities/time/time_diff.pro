FUNCTION TIME_DIFF, t1, t0, CHECK = check

;+
;NAME:
;		TIME_DIFF
;PURPOSE:
;		This computes the time difference in seconds between two times
;		expressed as Julian day and seconds.
;CATEGORY:
;		Date and time calculations.
;CALLING SEQUENCE:
;		dt = TIME_DIFF(t1, t0)
;INPUT:
;		t1  : structure {jtime, jday: 0L, seconds: 0L} 
;				containing the date as Julian day and seconds from midnight
;				or CDATE or CDATE_NO_LEAP structure
;		t0  : structure {jtime, jday: 0L, seconds: 0L} 
;				containing the date as Julian day and seconds from midnight
;				or CDATE or CDATE_NO_LEAP structure
;OUTPUT:
;		dt  : time difference in seconds between t1 and t0 (64-bit integer)
;KEYWORDS:
;		check  : If set, input parameter range checking is turned on.
;					Check can also be set to a two-element array containing
;					the lower and upper limts on the Julian day.
;PROCEDURE:
;		This function does some simple range checking on the input parameters
;		and then computes the time difference.  The expression for the time
;		difference is arranged to reduce the possibility of overflow errors.
;MODIFICATION HISTORY:
;     KPB, 1995-08.
;     KPB, 2001-09-12.  Updated to handle CDATE structures as well as JTIME structures.
;		Cameron Homeyer, 2011-12. Vectorized.
;-

COMPILE_OPT IDL2

IF ((TAG_NAMES(t1[0], /STRUCTURE_NAME) EQ 'CDATE')	OR $
	 (TAG_NAMES(t1[0], /STRUCTURE_NAME) EQ 'CDATE_NO_LEAP'))	THEN t1_temp = DATE_TO_TIME(t1) $
																				ELSE t1_temp = t1
IF ((TAG_NAMES(t0[0], /STRUCTURE_NAME) EQ 'CDATE')	OR $
	 (TAG_NAMES(t0[0], /STRUCTURE_NAME) EQ 'CDATE_NO_LEAP'))	THEN t0_temp = DATE_TO_TIME(t0) $
																				ELSE t0_temp = t0

IF KEYWORD_SET(check) THEN BEGIN                                 ;Check input parameters
   IF(N_ELEMENTS(check) EQ 1) THEN BEGIN
      jdaymin = JULDAY(1, 1, 1900)                               ;Default lower limit on Julian day
      jdaymax = JULDAY(1, 1, 2100)                               ;Default upper limit on Julian day
   ENDIF ELSE BEGIN
      jdaymin = check(1)
      jdaymax = check(2)
   ENDELSE
   IF((t1_temp.jday LT jdaymin) OR (t1_temp.jday GT jdaymax)) THEN $       ;Check Julian day
      MESSAGE, 'Julian day out of range, t1 in TRAJ3D_TIME_DIFF'
   IF((t1_temp.seconds LT 0L) OR (t1_temp.seconds GT 86399L)) THEN $       ;Check seconds
      MESSAGE, 'Seconds out of range, t1 in TRAJ3D_TIME_DIFF'
   IF((t0_temp.jday LT jdaymin) OR (t0_temp.jday GT jdaymax)) THEN $       ;Check Julian day
      MESSAGE, 'Julian day out of range, t0 in TRAJ3D_TIME_DIFF'
   IF((t0_temp.seconds LT 0L) OR (t0_temp.seconds GT 86399L)) THEN $       ;Check seconds
      MESSAGE, 'Seconds out of range, t0 in TRAJ3D_TIME_DIFF'
ENDIF

RETURN, 86400LL*(t1_temp.jday-t0_temp.jday) + t1_temp.seconds-t0_temp.seconds

END
