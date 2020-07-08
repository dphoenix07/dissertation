PRO DC3_PLOT_GV_TRACER_RALT, flight_name, $
	BINNED      = binned, $
	PV_RELATIVE = pv_relative, $
	EPS         = eps, $
	PNG         = png

;+
; Name:
;		DC3_PLOT_GV_TRACER_RALT
; Purpose:
;		This is a procedure to plot DC3 trace gases in relative altitude to the tropopause. 
; Calling sequence:
;		DC3_PLOT_GV_TRACER_RALT
; Input:
;		arg1 : positional parameter 1
; Output:
;		arg2 : positional parameter 2
; Keywords:
;		key1 : keyword parameter 1
; Author and history:
;		Cameron R. Homeyer  2012-08-09.
;-

COMPILE_OPT IDL2																									;Set compile options

IF (N_ELEMENTS(flight_name) EQ 0) THEN fname = 'all' $
											 ELSE fname = flight_name[0]

IF KEYWORD_SET(pv_relative) THEN rname = '_2pvu' $
									 ELSE rname = '_trop'

epsfile = '~/dc3_' + fname + '_tracer_ralt' + rname + '.eps'
pngfile = '~/dc3_' + fname + '_tracer_ralt' + rname + '.png'

IF KEYWORD_SET(eps) THEN BEGIN	
	PS_ON, FILENAME = epsfile, PAGE_SIZE = [6.0, 4.0], MARGIN = 0.0, /INCHES					;Switch to Postscript device
	DEVICE, /ENCAPSULATED
	!P.FONT     = 0																								;Hardware fonts
	!P.CHARSIZE = 0.8
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS																							;Load basic color definitions
ENDIF ELSE BEGIN
	SET_PLOT, 'X'
	WINDOW, XSIZE = 1200, YSIZE = 800																		;Open graphics window
	!P.COLOR      = COLOR_24('black')																		;Foreground color
	!P.BACKGROUND = COLOR_24('white')																		;Background color
	!P.CHARSIZE   = 2.5		
	!P.FONT       = -1																							;Use Hershey fonts
ENDELSE
!P.MULTI = [0, 4, 2]

z = DC3_READ_VAR('GGALT', flight_name)																		;Read aircraft altitude
IF KEYWORD_SET(pv_relative) THEN BEGIN
	ytitle = '2-pvu Relative (km)'
	z_trop = DC3_READ_VAR('GFS_2PVU_HGT', flight_name)													;Read tropopause level
ENDIF ELSE BEGIN	
	ytitle = 'Tropopause Relative (km)'
	z_trop = DC3_READ_VAR('GFS_TROP_HGT', flight_name)
ENDELSE

var = DC3_READ_VAR('FO3_ACD', flight_name)																;Read ozone data
PLOT_TRACER_RALT, var.values, 0.001*(z.values - z_trop.values), $
	TITLE    = 'ACOM', $
	XRANGE   = [0, 400], $
	XTITLE   = 'Ozone (ppbv)', $
	YTITLE   = ytitle, $
	BINNED   = binned, $
	NOWINDOW = 1

var = DC3_READ_VAR('CO', flight_name)																		;Read carbon monoxide data
PLOT_TRACER_RALT, var.values, 0.001*(z.values - z_trop.values), $
	TITLE    = 'RAF', $
	XRANGE   = [0, 200], $
	XTITLE   = 'Carbon Monoxide (ppbv)', $
	YTITLE   = ytitle, $
	BINNED   = binned, $
	NOWINDOW = 1

var = DC3_READ_VAR('X_H2O', flight_name)																	;Read water vapor data
PLOT_TRACER_RALT, var.values, 0.001*(z.values - z_trop.values), $
	TITLE    = 'VCSEL', $
	XRANGE   = [1, 10000], $
	XLOG     = 1, $
	XTITLE   = 'Water Vapor (ppmv)', $
	YTITLE   = ytitle, $
	BINNED   = binned, $
	NOWINDOW = 1

var = DC3_READ_VAR('HNO3', flight_name)																	;Read nitric acid data
PLOT_TRACER_RALT, var.values, 0.001*(z.values - z_trop.values), $
	TITLE    = 'GT-CIMS', $
	XRANGE   = [0, 3000], $
	XTICKS   = 3, $
	XMINOR   = 4, $
	XTITLE   = 'Nitric Acid (pptv)', $
	YTITLE   = ytitle, $
	BINNED   = binned, $
	NOWINDOW = 1

var = DC3_READ_VAR('NO+NO2', flight_name)																	;Read nitrogen oxide data
PLOT_TRACER_RALT, var.values, 0.001*(z.values - z_trop.values), $
	TITLE    = 'RAF', $
	XRANGE   = [0, 3000], $
	XTICKS   = 3, $
	XMINOR   = 4, $
	XTITLE   = 'NOx (pptv)', $
	YTITLE   = ytitle, $
	BINNED   = binned, $
	NOWINDOW = 1

var = DC3_READ_VAR('SO2', flight_name)																		;Read carbon dioxide data
PLOT_TRACER_RALT, var.values, 0.001*(z.values - z_trop.values), $
	TITLE    = 'GT-CIMS', $
	XRANGE   = [0, 300], $
	XTICKS   = 3, $
	XMINOR   = 4, $
	XTITLE   = 'Sulfur Dioxide (pptv)', $
	YTITLE   = ytitle, $
	BINNED   = binned, $
	NOWINDOW = 1

var = DC3_READ_VAR('Methane', flight_name)																;Read methane data
PLOT_TRACER_RALT, var.values, 0.001*(z.values - z_trop.values), $
	TITLE   = 'Picarro', $
	XRANGE   = [1700, 2000], $
	XTICKS   = 3, $
	XMINOR   = 4, $
	XTITLE   = 'Methane (ppbv)', $
	YTITLE   = ytitle, $
	BINNED   = binned, $
	NOWINDOW = 1

var = DC3_READ_VAR('CH2O_CAMS_pptv', flight_name)														;Read formaldehyde data
PLOT_TRACER_RALT, var.values, 0.001*(z.values - z_trop.values), $
	TITLE    = 'CAMS', $
	XRANGE   = [0, 3000], $
	XTICKS   = 3, $
	XMINOR   = 4, $
	XTITLE   = 'Formaldehyde (pptv)', $
	YTITLE   = ytitle, $
	BINNED   = binned, $
	NOWINDOW = 1

!P.MULTI = 0
IF KEYWORD_SET(eps) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file

END
