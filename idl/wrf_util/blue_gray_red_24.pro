FUNCTION BLUE_GRAY_RED_24, nc, cmid, width

;+
;NAME:
;     BLUE_GRAY_RED_24
;PURPOSE:
;     This procedure sets a portion of the color scale to blue-gray-red.
;CATEGORY:
;     Graphics and display.
;CALLING SEQUENCE:
;     color = BLUE_GRAY_RED_24(nc)
;INPUT:
;     nc  : number of colors in color table.
;KEYWORDS:
;     None.
;OUTPUT:
;     Long array containing 24-bit colors.
;MODIFICATION HISTORY:
;     K. Bowman, 2010-12-02.
;-

COMPILE_OPT IDL2																		;Set compiler options

IF (N_ELEMENTS(cmid)  EQ 0) THEN cmid  = nc/2 -1							;Midpoint of color scale
IF (N_ELEMENTS(width) EQ 0) THEN width = 0.4									;Half-width of gray region

cminus = cmid - LONG(width*cmid)													;Lower limit of gray region
cplus  = cmid + LONG(width*cmid)													;Upper limit of gray region

h = FLTARR(nc)
s = FLTARR(nc)
v = FLTARR(nc)

h[0     :cmid ]   = 240.0															;Lower half blue
h[cmid+1:*    ]   =   0.0															;Upper half red

s[0       :cminus ] = MAKEN(1.0, 0.1, cminus + 1)							;Blue to almost white
s[cminus+1:cmid   ] = MAKEN(0.1, 0.0, cmid - cminus)
s[cmid  +1:cplus  ] = MAKEN(0.0, 0.1, cplus - cmid)							;Almost white to red
s[cplus +1:*      ] = MAKEN(0.1, 1.0, nc - cplus - 1)

v[*]              = 1.0																;Set the value
v[cminus+1:cmid ] = MAKEN(0.9, 0.5, cmid  - cminus)					;Almost white to gray
v[cmid  +1:cplus] = MAKEN(0.5, 0.9, cplus - cmid  )					;gray to almost white

COLOR_CONVERT, h, s, v, r, g, b, /HSV_RGB										;Convert to hsv

RETURN, COLOR_24(r, g, b)															;Return color table

END


