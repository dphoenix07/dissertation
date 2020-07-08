FUNCTION TROPOPAUSE, T, Z, P, NTROPS = ntrops

;+
; Name:
;		TROPOPAUSE
; Purpose:
;		This is a function to calculate tropopause height using the WMO
;		lapse-rate tropopause definition given temperature, altitude, 
;		and pressure.
; Calling sequence:
;		value = TROPOPAUSE(T, Z, P)
; Inputs:
;		T      : Given temperature column. Expected in degrees Celsius/Kelvin.
;		Z      : Given height column. Expected in kilometers.
;		P      : Given pressure column. Expected in hPa. 
; Output:
;		value  : Tropopause height in kilometers.
; Keywords:
;		NTROPS : Number of tropopauses to return. Default is 1. 
; Author and history:
;		Cameron R. Homeyer  2008-07-24.
;
;-

COMPILE_OPT IDL2																	;Set compile options

IF (N_ELEMENTS(ntrops) EQ 0) THEN ntrops = 1								;Default number of trops

IF (ntrops GT 1) THEN $
	trop = MAKE_ARRAY([ntrops], VALUE = !VALUES.F_NAN) ELSE $
	trop = !VALUES.F_NAN															;Default tropopause is NaN

itrop  = 0																			;Set index for trops

z_sort = SORT(Z)
Z_col  = Z[z_sort]																;Sort alitudes
T_col  = T[z_sort]																;Sort temperatures
P_col  = P[z_sort]																;Sort pressure

lapse_rate = -1.0*((SHIFT(T_col, -1) - T_col)/$							;Calculate lapse rate
						 (SHIFT(Z_col, -1) - Z_col))

good = WHERE(P_col LE 500.0, good_count)
IF (good_count GT 0) THEN BEGIN
	lapse_rate = lapse_rate[good]												;Use data LT 500 hPa in pressure
	T_col      =      T_col[good]
	Z_col      =      Z_col[good]
ENDIF ELSE RETURN, trop

good = WHERE(FINITE(lapse_rate), good_count)								;Get finite values
IF (good_count GT 0) THEN BEGIN
	lapse_rate = lapse_rate[good]
	T_col      =      T_col[good]
	Z_col      =      Z_col[good]
ENDIF ELSE RETURN, trop

n = N_ELEMENTS(Z_col)															;Calculate number of elements

FOR i = 0, n -1 DO BEGIN
find:																					;Marker for tropopause calculation
	IF (lapse_rate[i] LE 2.0) THEN BEGIN
		zmax = Z_col[i] + 2.0													;Store height 2km above current
		
		k  = WHERE(((Z_col GT Z_col[i]) AND (Z_col LE zmax)), $		;Find all points at 2km or less above current
				kcount)	
		k2 = WHERE((Z_col GT zmax), k2count)								;Make sure there is profile above
		
		IF ((kcount GT 0) AND (k2count GT 0)) THEN BEGIN
			lapse_rate2 = (T_col[i] - T_col[k])/$							;Calculate lapse rates for every level within 2 km
							  (Z_col[k] - Z_col[i])
			
			invalid = WHERE((lapse_rate2 GT 2.0), ivcount)				;Check for any lapse rate GT 2
			IF (ivcount GT 0) THEN void = 1 $								;If one exists, tropopause invalid
									ELSE trop[itrop] = Z_col[i]				;Else, set tropopause height
								
			CASE LONG(TOTAL(FINITE(trop))) OF
				ntrops     : RETURN, trop										;Check completion
				(itrop +1) : GOTO, multiple
				ELSE       : void = 1
			ENDCASE
		ENDIF ELSE RETURN, trop
	ENDIF
ENDFOR

multiple:																			;Marker for multiple tropopauses
itrop = itrop + 1																	;Increment tropopause index

FOR i = i, n -1 DO BEGIN
	zmax = Z_col[i] + 1.0														;Store height 1km above current point
	
	k  = WHERE(((Z_col GT Z_col[i]) AND (Z_col LE zmax)), kcount)	;Find all points at 1km or less above current
	k2 = WHERE((Z_col GT zmax), k2count)									;Make sure there is profile above

	IF ((kcount GT 0) AND (k2count GT 0)) THEN BEGIN
		lapse_rate2 = (T_col[i] - T_col[k])/(Z_col[k] - Z_col[i])
		itrue       = WHERE((lapse_rate2 GT 3.0), tcount)
		IF (tcount EQ kcount) THEN GOTO, find $							;Find multiple tropopause
							       ELSE void = 1
	ENDIF ELSE RETURN, trop
ENDFOR

RETURN, trop

END
