FUNCTION BLUE_WHITE_RED_24, nc, cmin, cmax

;+
;NAME:
;     BLUE_RED_24
;PURPOSE:
;     This function computes a 24-bit color table.  The lower have is blue with
;     decreasing saturation.  The upper half is red with increasing saturation.
;CATEGORY:
;     Graphics and display.
;CALLING SEQUENCE:
;     color = BLUE_RED_24(nc)
;INPUT:
;     nc             : number of colors in color table.  For symmetry, nc should be even.
;		min_saturation : optional minimum saturation value (center of table).  Default is 0.1.
;		max_saturation : optional maximum saturation value (ends of table).    Default is 1.0.
;KEYWORDS:
;     None.
;OUTPUT:
;     Long array containing 24-bit colors.
;MODIFICATION HISTORY:
;     K. Bowman, 2004-03-03.
;
;		C. Homeyer, 2008-04-08.  Revised to define a maximum saturation value.
;-

COMPILE_OPT IDL2																		;Set compiler options

cmid = (cmin + cmax)/2											;Midpoint of color scale

;TVLCT, r, g, b, /GET												;Get current color table
;COLOR_CONVERT, r, g, b, h, s, v, /RGB_HSV					;Convert to hsv
;
;IF(nc GT N_ELEMENTS(r)) THEN $
;   MESSAGE, 'Available color table of ', N_ELEMENTS(r), 'colors is insufficient for ', nc, ' colors.'

h = FLTARR(nc)																			;Hue array
s = FLTARR(nc)																			;Saturation array
v = FLTARR(nc)																			;Value array

h[cmin  :cmid] = 240.0											;Lower half blue
h[cmid+1:cmax] =   0.0											;Upper half red
s[cmin  :cmid] = MAKEN(1.0, 0.0, cmid - cmin + 1)		;Fade to white at middle
s[cmid+1:cmax] = MAKEN(0.0, 1.0, cmax - cmid    )		;Fade from white at middle
v[cmin  :cmax] =   1.0											;Set the value

COLOR_CONVERT, h, s, v, r, g, b, /HSV_RGB										;Convert to hsv

IF KEYWORD_SET(ps) THEN BEGIN
	TVLCT, r, g, b, 32																;Load color table after 32 basic colors
	RETURN, INDGEN(nc) + 32															;Return indices
ENDIF ELSE RETURN, COLOR_24(r, g, b)											;Return color table

END


