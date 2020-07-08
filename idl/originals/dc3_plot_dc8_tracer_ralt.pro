PRO DC3_PLOT_DC8_TRACER_RALT, flight_name, $
	BINNED      = binned, $
	PV_RELATIVE = pv_relative, $
	EPS         = eps, $
	PNG         = png

;+
; Name:
;		DC3_PLOT_DC8_TRACER_RALT
; Purpose:
;		This is a procedure to plot DC3 trace gases in relative altitude to the tropopause. 
; Calling sequence:
;		DC3_PLOT_DC8_TRACER_RALT
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

z = DC3_READ_VAR('G_ALT', flight_name, /DC8)			;G_ALT								;Read aircraft altitude

IF KEYWORD_SET(pv_relative) THEN BEGIN
	ytitle = '2-pvu Relative (km)'
	z_trop = DC3_READ_VAR('GFS_2PVU_HGT', flight_name, /DC8)											;Read tropopause level
ENDIF ELSE BEGIN	
	ytitle = 'Tropopause Relative (km)'
	z_trop = DC3_READ_VAR('GFS_TROP_HGT', flight_name, /DC8)
ENDELSE

var = DC3_READ_VAR('O3_CL', flight_name, /DC8)																				;Read ozone data
PLOT_TRACER_RALT, var.values, 0.001*(z.values - z_trop.values), $
	TITLE    = 'CL', $
	XRANGE   = [0, 400], $
	XTITLE   = 'Ozone (ppbv)', $
	YTITLE   = ytitle, $
	BINNED   = binned, $
	NOWINDOW = 1

var = DC3_READ_VAR('CO_ppbv_DACOM', flight_name, /DC8)												;Read carbon monoxide data
PLOT_TRACER_RALT, var.values, 0.001*(z.values - z_trop.values), $
	TITLE    = 'DACOM', $
	XRANGE   = [0, 200], $
	XTITLE   = 'Carbon Monoxide (ppbv)', $
	YTITLE   = ytitle, $
	BINNED   = binned, $
	NOWINDOW = 1

var = DC3_READ_VAR('H2O_vapor_ppmv_DLH', flight_name, /DC8)											;Read water vapor data
PLOT_TRACER_RALT, var.values, 0.001*(z.values - z_trop.values), $
	TITLE    = 'DLH', $
	XRANGE   = [1, 10000], $
	XLOG     = 1, $
	XTITLE   = 'Water Vapor (ppmv)', $
	YTITLE   = ytitle, $
	BINNED   = binned, $
	NOWINDOW = 1

var = DC3_READ_VAR('HNO3_SAGA', flight_name, /DC8)			;HNO3_CIT								;Read nitric acid data
PLOT_TRACER_RALT, var.values, 0.001*(z.values - z_trop.values), $
	TITLE    = 'SAGA', $
	XRANGE   = [0, 3000], $
	XTITLE   = 'Nitric Acid (pptv)', $
	XTICKS   = 3, $
	XMINOR   = 4, $
	YTITLE   = ytitle, $
	BINNED   = binned, $
	NOWINDOW = 1

var = DC3_READ_VAR('NO2_LIF', flight_name, /DC8)					;NO2_LIF							;Read nitrogen oxide data
PLOT_TRACER_RALT, var.values, 0.001*(z.values - z_trop.values), $
	TITLE    = 'LIF', $
	XRANGE   = [0, 3000], $
	XTICKS   = 3, $
	XMINOR   = 4, $
	XTITLE   = 'NO!D2!N (pptv)', $
	YTITLE   = ytitle, $
	BINNED   = binned, $
	NOWINDOW = 1

var = DC3_READ_VAR('SO2_GTCIMS', flight_name, /DC8)													;Read carbon dioxide data
PLOT_TRACER_RALT, var.values, 0.001*(z.values - z_trop.values), $
	TITLE    = 'GT-CIMS', $
	XRANGE   = [0, 300], $
	XTICKS   = 3, $
	XMINOR   = 4, $
	XTITLE   = 'Sulfur Dioxide (pptv)', $
	YTITLE   = ytitle, $
	BINNED   = binned, $
	NOWINDOW = 1

var = DC3_READ_VAR('CH4_ppbv_DACOM', flight_name, /DC8)												;Read methane data
PLOT_TRACER_RALT, var.values, 0.001*(z.values - z_trop.values), $
	TITLE   = 'DACOM', $
	XRANGE   = [1700, 2000], $
	XTICKS   = 3, $
	XMINOR   = 4, $
	XTITLE   = 'Methane (ppbv)', $
	YTITLE   = ytitle, $
	BINNED   = binned, $
	NOWINDOW = 1

var = DC3_READ_VAR('CH2O_DFGAS_pptv', flight_name, /DC8)				;CH2O_LIF					;Read formaldehyde data
PLOT_TRACER_RALT, var.values, 0.001*(z.values - z_trop.values), $
	TITLE    = 'DFGAS', $
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
