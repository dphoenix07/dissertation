PRO PLOT_WRF_GRID_ILLUSTRATION, EPS = eps, PNG = png

;+
; Name:
;		PLOT_WRF_GRID_ILLUSTRATION
; Purpose:
;		This is a template for creating IDL procedure files. 
; Calling sequence:
;		PLOT_WRF_GRID_ILLUSTRATION, arg1, arg2
; Input:
;		arg1 : positional parameter 1
; Output:
;		arg2 : positional parameter 2
; Keywords:
;		key1 : keyword parameter 1
; Author and history:
;		Cameron R. Homeyer  date.
;-

COMPILE_OPT IDL2																									;Set compile options


;eta_levels = [1.0000, 0.997, 0.994, 0.991, 0.988, 0.985, 0.975, $
;			 0.97, 0.96, 0.95, 0.94, 0.93, 0.92, 0.91, 0.895, $
;			 0.88, 0.865, 0.85, 0.825, 0.8, 0.7832, 0.7627, $
;			 0.7423, 0.7218, 0.7014, 0.6810, 0.6608, 0.6407, $ 
;			 0.6210, 0.6014, 0.5822, 0.5632, 0.5447, 0.5267, $
;			 0.5091, 0.4918, 0.4751, 0.4586, 0.4426, 0.4270, $
;			 0.4118, 0.3970, 0.3826, 0.3685, 0.3548, 0.3414, $
;			 0.3286, 0.3160, 0.3036, 0.2917, 0.2801, 0.2688, $
;			 0.2579, 0.2472, 0.2369, 0.2268, 0.2170, 0.2076, $
;			 0.1985, 0.1899, 0.1815, 0.1736, 0.1659, 0.1584, $
;			 0.1513, 0.1445, 0.1379, 0.1316, 0.1254, 0.1195, $
;			 0.1137, 0.1081, 0.1028, 0.0975, 0.0926, 0.0877, $
;			 0.0830, 0.0785, 0.0741, 0.0699, 0.0658, 0.0619, $
;			 0.0581, 0.0544, 0.0510, 0.0476, 0.0445, 0.0414, $
;			 0.0385, 0.0356, 0.0329, 0.0303, 0.0278, 0.0254, $
;			 0.0231, 0.0210, 0.0190, 0.0170, 0.0151, 0.0133, $
;			 0.0115, 0.0099, 0.0084, 0.0068, 0.0054, 0.0039, $
;			 0.0026, 0.0012, 0.0000]



 eta_levels = [1.0000, 0.9942, 0.9780, 0.9606, 0.9419, 0.9229, $							;eta levels for research
 			   0.9036, 0.8839, 0.8641, 0.8440, 0.8238, 0.8035, $
               0.7832, 0.7627, 0.7423, 0.7218, 0.7014, 0.6810, $
               0.6608, 0.6407, 0.6210, 0.6014, 0.5822, 0.5632, $
               0.5447, 0.5267, 0.5091, 0.4918, 0.4751, 0.4586, $
               0.4426, 0.4270, 0.4118, 0.3970, 0.3826, 0.3685, $
               0.3548, 0.3414, 0.3286, 0.3160, 0.3036, 0.2917, $
               0.2801, 0.2688, 0.2579, 0.2472, 0.2369, 0.2268, $
               0.2170, 0.2076, 0.1985, 0.1899, 0.1815, 0.1736, $
               0.1659, 0.1584, 0.1513, 0.1445, 0.1379, 0.1316, $
               0.1254, 0.1195, 0.1137, 0.1081, 0.1028, 0.0975, $
               0.0926, 0.0877, 0.0830, 0.0785, 0.0741, 0.0699, $
               0.0658, 0.0619, 0.0581, 0.0544, 0.0510, 0.0476, $
               0.0445, 0.0414, 0.0385, 0.0356, 0.0329, 0.0303, $
               0.0278, 0.0254, 0.0231, 0.0210, 0.0190, 0.0170, $
               0.0151, 0.0133, 0.0115, 0.0099, 0.0084, 0.0068, $
               0.0054, 0.0039, 0.0026, 0.0012, 0.0000]

p = 990.0*eta_levels + 10.0
z = PRESSURE_ALTITUDE(p, /km)

PRINT, z
epsfile = '~/wrfgrid.eps'
pngfile = '~/wrfgrid.eps'

IF KEYWORD_SET(eps) THEN BEGIN	
	PS_ON, FILENAME = epsfile, PAGE_SIZE = [7.0, 8.0], MARGIN = 0.0, /INCHES					;Switch to Postscript device
	DEVICE, /ENCAPSULATED
	!P.FONT     = 0																								;Hardware fonts
	!P.CHARSIZE = 1.5
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS																							;Load basic color definitions
ENDIF ELSE BEGIN
	SET_PLOT, 'X'
	WINDOW, XSIZE = 700, YSIZE = 800																		;Open graphics window
	!P.COLOR      = COLOR_24('black')																		;Foreground color
	!P.BACKGROUND = COLOR_24('white')																		;Background color
	!P.CHARSIZE   = 2.75	
	!P.FONT       = -1																							;Use Hershey fonts
ENDELSE

dz     = 0.5*(SHIFT(z, -1) - SHIFT(z, 1))
dz[ 0] = z[ 1] - z[ 0]
dz[-1] = z[-1] - z[-2]

PLOT, GAUSS_SMOOTH(dz,3, /EDGE_TRUNCATE), z, THICK = 2, $
	XRANGE = [0,1], $
	XSTYLE = 1, $
	XTITLE = 'Vertical Resolution (km)', $
	YRANGE = [0.0, 20.0], $
	YSTYLE = 1, $
	YTITLE = 'Altitude (km)'

z2 = z[0] + TOTAL(GAUSS_SMOOTH(dz,3, /EDGE_TRUNCATE), /CUM)

USERSYM_CIRCLE, /FILL
PLOTS, REPLICATE(0.9, N_ELEMENTS(z)), z2, PSYM = 8, SYMSIZE = 2 - 0.5*KEYWORD_SET(eps), NOCLIP = 0

IF KEYWORD_SET(eps) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file


END
