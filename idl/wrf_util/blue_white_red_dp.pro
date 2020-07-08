PRO BLUE_WHITE_RED_DP

;NAME:
;     BLUE_WHITE_RED
;PURPOSE:
;     This procedure sets a portion of the color scale to blue-white-red.
;CATEGORY:
;     Graphics and display.
;CALLING SEQUENCE:
;     BLUE_WHITE_RED, cmin, cmax
;INPUT:
;     cmin  : minimum index of color table to use.
;     cmax  : maximum index of color table to use.
;KEYWORDS:
;     None.
;OUTPUT:
;     Changes the IDL color table.
;MODIFICATION HISTORY:
;     KPB, 1997-10-01.  Updated 2004-02-20.

COMPILE_OPT IDL2													;Set compile options

cmin = 0
cmax = 10

nc   = cmax - cmin + 1											;Number of colors
cmid = (cmin + cmax)/2											;Midpoint of color scale

TVLCT, r, g, b, /GET												;Get current color table
COLOR_CONVERT, r, g, b, h, s, v, /RGB_HSV					;Convert to hsv

IF(nc GT N_ELEMENTS(r)) THEN $
   MESSAGE, 'Available color table of ', N_ELEMENTS(r), 'colors is insufficient for ', nc, ' colors.'

h[cmin  :cmid] = 240.0											;Lower half blue
h[cmid+1:cmax] =   0.0											;Upper half red
s[cmin  :cmid] = MAKEN(1.0, 0.0, cmid - cmin + 1)		;Fade to white at middle
s[cmid+1:cmax] = MAKEN(0.0, 1.0, cmax - cmid    )		;Fade from white at middle
v[cmin  :cmax] =   1.0											;Set the value

TVLCT, h, s, v, /HSV												;Load color scale

END
