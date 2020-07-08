FUNCTION GRAY_LOGSCALE_24, nc, min_sat, max_sat, PS = ps

;	Cameron Homeyer

;+
; Name:
;		GRAY_LOGSCALE_COLOR_24
; Purpose:
;		This is a function for a color scheme. 
; Calling sequence:
;		GRAY_LOGSCALE_COLOR_24, nc
; Input:
;		nc : number of colors
; Output:
;		color scheme
; Author and history:
;		Cameron R. Homeyer 2008-01-09.
;-

COMPILE_OPT IDL2																;Set compile options

IF (N_ELEMENTS(min_sat) EQ 0) THEN min_sat = 0.0
IF (N_ELEMENTS(max_sat) EQ 0) THEN max_sat = 1.0

h = FLTARR(nc)
s = FLTARR(nc)
v = FLTARR(nc)
i = LINDGEN(nc)

h[*] = 0.0
s[*] = 0.0
;v[*] = MAKEN(min_sat, max_sat, nc)
v[*] = ALOG10(FINDGEN(nc) * (10.^max_sat - 10.^min_sat) / (nc - 1.) + 10.^min_sat) 				;Log color scale

COLOR_CONVERT, h, s, v, r, g, b, /HSV_RGB								;Convert to hsv

IF KEYWORD_SET(ps) THEN BEGIN
	TVLCT, r, g, b, 32
	RETURN, INDGEN(nc) + 32
ENDIF ELSE $
	RETURN, COLOR_24(r, g, b)

END
