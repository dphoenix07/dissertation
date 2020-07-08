PRO DC3_PLOT_TRACER_RALT, flight_name, threshold, $
	BINNED      = binned, $
	PV_RELATIVE = pv_relative, $
	IN_CLOUD	= in_cloud, $
	ALT			= alt, $
	EPS         = eps, $
	PNG         = png

;+
; Name:
;		DC3_PLOT_TRACER_RALT
; Purpose:
;		This is a procedure to plot DC3 trace gases in relative altitude to the tropopause. 
; Calling sequence:
;		DC3_PLOT_TRACER_RALT
; Input:
;		flight_name : String variable of flight date (e.g., '20120519')
; Output:
;		Scatter plot of DC8 and GV aircraft measurements for 6 trace gases.
; Keywords:
;		Binned : Set to sort data into bins.
; Author and history:
;		Cameron R. Homeyer  2012-08-09.
;		Daniel B. Phoenix	2016-01-26.	Combined DC8 and GV measurements
;-

COMPILE_OPT IDL2																									;Set compile options

IF (N_ELEMENTS(threshold  ) EQ 0) THEN threshold = 1000.0
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
	!P.CHARSIZE   = 3.1		
	!P.FONT       = -1																							;Use Hershey fonts
ENDELSE
!P.MULTI = [0, 4, 2]

gv_cloud  = (DC3_READ_VAR('CONC1DC100_LWIO', flight_name)).values
dc8_cloud = (DC3_READ_VAR('cloud', flight_name, /DC8)).values
			
gv_z  = (DC3_READ_VAR('GGALT', flight_name)).values														;Read aircraft altitude
dc8_z = (DC3_READ_VAR('G_ALT', flight_name, /DC8)).values

IF KEYWORD_SET(pv_relative) THEN BEGIN
	ytitle = '2-pvu Relative (km)'
	z_trop = DC3_READ_VAR('GFS_2PVU_HGT', flight_name)													;Read tropopause level
ENDIF ELSE BEGIN	
	ytitle = 'Tropopause Relative (km)'
	gv_z_trop  = (DC3_READ_VAR('GFS_TROP_HGT', flight_name)).values
 	dc8_z_trop = (DC3_READ_VAR('GFS_TROP_HGT', flight_name, /DC8)).values
ENDELSE


IF KEYWORD_SET(in_cloud) THEN BEGIN																;Sort values in-cloud vs out of cloud
	dc8_values = WHERE((dc8_cloud EQ 2.0) OR (dc8_cloud EQ 3.0), dc8_count)					;Find DC8 in-cloud values
	IF (dc8_count GT 0) THEN BEGIN
		dc8_cloud  = dc8_cloud  [dc8_values]
		dc8_z      = dc8_z      [dc8_values]
		dc8_z_trop = dc8_z_trop [dc8_values]
	ENDIF
	gv_values = WHERE(gv_cloud GT 0.0, gv_count)										;Find GV in-cloud values
	IF (gv_count GT 0) THEN BEGIN
		gv_cloud  = gv_cloud  [gv_values]
		gv_z      = gv_z      [gv_values]
		gv_z_trop = gv_z_trop [gv_values]
	ENDIF
ENDIF ELSE BEGIN
	dc8_values  = WHERE(dc8_cloud EQ 0.0, dc8_count)										;Find DC8 non-cloud values
	IF (dc8_count GT 0) THEN BEGIN
		dc8_cloud  = dc8_cloud  [dc8_values]
		dc8_z      = dc8_z      [dc8_values]
		dc8_z_trop = dc8_z_trop [dc8_values]
	ENDIF
	gv_values = WHERE(gv_cloud EQ 0.0, gv_count)											;Find GV non-cloud values
	IF (gv_count GT 0) THEN BEGIN
		gv_cloud  = gv_cloud  [gv_values]
		gv_z      = gv_z      [gv_values]
		gv_z_trop = gv_z_trop [gv_values]
	ENDIF
ENDELSE

gv_z_trop  = CALC_TROP_MODE_OBS(gv_z_trop , threshold)
dc8_z_trop = CALC_TROP_MODE_OBS(dc8_z_trop, threshold)

z = [gv_z, dc8_z]
z_trop = [gv_z_trop, dc8_z_trop]
ralt = (z - z_trop)

IF KEYWORD_SET(alt) THEN ralt = z 

title = 'DC8 + GV'

var_gv  = (DC3_READ_VAR('FO3_ACD', flight_name)).values												;Read ozone data
var_dc8 = (DC3_READ_VAR('O3_CL', flight_name, /DC8)).values
PLOT_TRACER_RALT, var_gv, var_dc8, 0.001*ralt, gv_values, dc8_values, $
	TITLE    = title, $
	XRANGE   = [0, 400], $
	XTITLE   = 'Ozone (ppbv)', $
	YTITLE   = ytitle, $
	BINNED   = binned, $
	ALT 	 = alt, $
	NOWINDOW = 1

var_gv  = (DC3_READ_VAR('CO', flight_name)).values																		;Read carbon monoxide data
var_dc8 = (DC3_READ_VAR('CO_ppbv_DACOM', flight_name, /DC8)).values
PLOT_TRACER_RALT, var_gv, var_dc8, 0.001*ralt, gv_values, dc8_values, $
	TITLE    = title, $
	XRANGE   = [0, 200], $
	XTITLE   = 'Carbon Monoxide (ppbv)', $
	YTITLE   = ytitle, $
	BINNED   = binned, $
	ALT		 = alt, $
	NOWINDOW = 1

var_gv  = (DC3_READ_VAR('X_H2O', flight_name)).values																		;Read water vapor data
var_dc8 = (DC3_READ_VAR('H2O_vapor_ppmv_DLH', flight_name, /DC8)).values	
PLOT_TRACER_RALT, var_gv, var_dc8, 0.001*ralt, gv_values, dc8_values, $
	TITLE    = title, $
	XRANGE   = [1, 500], $
	XTITLE   = 'Water Vapor (ppmv)', $
	YTITLE   = ytitle, $
	BINNED   = binned, $
	ALT 	 = alt, $
	NOWINDOW = 1

var_gv  = (DC3_READ_VAR('HNO3', flight_name)).values																		;Read nitric acid data
var_dc8 = (DC3_READ_VAR('HNO3_SAGA', flight_name, /DC8)).values	
PLOT_TRACER_RALT, var_gv, var_dc8, 0.001*ralt, gv_values, dc8_values, $
	TITLE    = title, $
	XRANGE   = [0, 3000], $
	XTICKS   = 3, $
	XMINOR   = 4, $
	XTITLE   = 'Nitric Acid (pptv)', $
	YTITLE   = ytitle, $
	BINNED   = binned, $
	ALT 	 = alt, $
	NOWINDOW = 1

var_gv  = (DC3_READ_VAR('NO+NO2', flight_name)).values																		;Read nitrogen oxide data
var_dc8 = (DC3_READ_VAR('NO2_LIF', flight_name, /DC8)).values	
PLOT_TRACER_RALT, var_gv, var_dc8, 0.001*ralt, gv_values, dc8_values, $
	TITLE    = title, $
	XRANGE   = [0, 3000], $
	XTICKS   = 3, $
	XMINOR   = 4, $
	XTITLE   = 'NOx (pptv)', $
	YTITLE   = ytitle, $
	BINNED   = binned, $
	ALT 	 = alt, $
	NOWINDOW = 1

var_gv  = (DC3_READ_VAR('SO2', flight_name)).values																			;Read carbon dioxide data
var_dc8 = (DC3_READ_VAR('SO2_GTCIMS', flight_name, /DC8)).values	
PLOT_TRACER_RALT, var_gv, var_dc8, 0.001*ralt, gv_values, dc8_values, $
	TITLE    = title, $
	XRANGE   = [0, 300], $
	XTICKS   = 3, $
	XMINOR   = 4, $
	XTITLE   = 'Sulfur Dioxide (pptv)', $
	YTITLE   = ytitle, $
	BINNED   = binned, $
	ALT 	 = alt, $	
	NOWINDOW = 1

var_gv  = (DC3_READ_VAR('CH2O_CAMS_pptv', flight_name)).values														;Read formaldehyde data
var_dc8 = (DC3_READ_VAR('CH2O_DFGAS_pptv', flight_name, /DC8)).values	
PLOT_TRACER_RALT, var_gv, var_dc8, 0.001*ralt, gv_values, dc8_values, $
	TITLE    = title, $
	XRANGE   = [0, 3000], $
	XTICKS   = 3, $
	XMINOR   = 4, $
	XTITLE   = 'Formaldehyde (pptv)', $
	YTITLE   = ytitle, $
	BINNED   = binned, $
	ALT 	 = alt, $
	NOWINDOW = 1

var_gv  = (DC3_READ_VAR('Methane', flight_name)).values																;Read methane data
var_dc8 = (DC3_READ_VAR('CH4_ppbv_DACOM', flight_name, /DC8)).values	
PLOT_TRACER_RALT, var_gv, var_dc8, 0.001*ralt, gv_values, dc8_values, $
	TITLE    = title, $
	XRANGE   = [1700, 2000], $
	XTICKS   = 3, $
	XMINOR   = 4, $
	XTITLE   = 'CH4 (ppbv)', $
	YTITLE   = ytitle, $
	BINNED   = binned, $
	ALT 	 = alt, $
	NOWINDOW = 1
	
!P.MULTI = 0
IF KEYWORD_SET(eps) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file

END