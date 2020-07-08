FUNCTION PERCENTILE, a, p, NOSORT = nosort, VALUE = value, NAN = nan

;+
; Name:
;		PERCENTILE
; Purpose:
;		This is a function to return the array index of a desired percentile. 
; Calling sequence:
;		value = PERCENTILE(a,p)
; Inputs:
;		a     : Array of values to return percentile.
;		p     : Desired percentile. (e.g. 10 = 10th)
; Output:
;		value : Index of desired percentile for given array a.
; Keywords:
;		NOSORT : If set, do not sort the input array.
;		VALUE  : If set, return percentile value of array rather than index.
;		NAN    : If set, return percentiles based on finite elements only.
; Author and history:
;		Cameron R. Homeyer  2010-04-21.
;								  2017-04-07. Updated to allow returning of percentile value.
;-

COMPILE_OPT IDL2																;Set Compile Options

IF KEYWORD_SET(nan) THEN BEGIN
	ifin = WHERE(FINITE(a), fcount)										;Search for finite values
	IF (fcount EQ 0) THEN RETURN, -1										;Return missing flag
	
	a2 = a[ifin]																;Extract finite values
ENDIF ELSE a2 = a

n  = N_ELEMENTS(a2)															;Count number of elements
IF ~KEYWORD_SET(nosort) THEN $
	as = SORT(a2) $															;Sort data
ELSE $
	as = LINDGEN(n)															;Data already sorted

ipercentile = (ROUND(n*(p/100.0)) -1) > 0
IF KEYWORD_SET(value) THEN BEGIN
	IF KEYWORD_SET(nan) THEN $
		RETURN, a[ifin[as[ipercentile]]] ELSE $						;Return value of desired percentile
		RETURN,      a[as[ipercentile]]
ENDIF ELSE BEGIN
	IF KEYWORD_SET(nan) THEN $
		RETURN, ifin[as[ipercentile]] ELSE $							;Return index of desired percentile
		RETURN,      as[ipercentile]
ENDELSE

END
