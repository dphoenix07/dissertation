PRO DC3_PLOT_TRACER_TRACER, flight_name, $
	BINNED      = binned, $
	PV_RELATIVE = pv_relative, $
	ZRELATIVE   = zrelative, $
	CLOUD       = cloud, $
	WATER       = water, $
	EPS         = eps, $
	PNG         = png

;+
; Name:
;		DC3_PLOT_TRACER_TRACER
; Purpose:
;		This is a procedure to plot DC3 trace gases against eachother. 
; Calling sequence:
;		DC3_PLOT_TRACER_TRACER
; Input:
;		flight_name : String variable of flight date (e.g., '20120519')
; Output:
;		Tracer-tracer plot of DC8 and GV aircraft measurements for 6 trace gases.
; Keywords:
;		Binned : Set to sort data into bins.
; Author and history:
;		Cameron R. Homeyer  2012-08-09.
;		Daniel B. Phoenix	2016-01-26.	Combined DC8 and GV measurements
;-

COMPILE_OPT IDL2																									;Set compile options

IF (N_ELEMENTS(flight_name) EQ 0) THEN fname = 'all' $
											 ELSE fname = flight_name[0]

IF KEYWORD_SET(pv_relative) THEN rname = '_2pvu' $
									 ELSE rname = '_trop'


gv_cloud  = (DC3_READ_VAR('CONC1DC100_LWIO', flight_name)).values
dc8_cloud = (DC3_READ_VAR('cloud', flight_name, /DC8)).values
			
gv_z  = (DC3_READ_VAR('GGALT', flight_name)).values														;Read aircraft altitude
dc8_z = (DC3_READ_VAR('G_ALT', flight_name, /DC8)).values

gv_z_trop  = (DC3_READ_VAR('GFS_TROP_HGT', flight_name)).values
dc8_z_trop = (DC3_READ_VAR('GFS_TROP_HGT', flight_name, /DC8)).values

gv_z_trop  = CALC_TROP_MODE_OBS(gv_z_trop , 1000.0)											 
dc8_z_trop = CALC_TROP_MODE_OBS(dc8_z_trop, 1000.0)

z = [gv_z, dc8_z]
z_trop = [gv_z_trop, dc8_z_trop]

symsize = 1.0 - 0.25*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf))

IF KEYWORD_SET(zrelative) THEN BEGIN
	ztracer = 0.001*(z - z_trop)
	zrange  = [-2.5, 2.5]
	ztitle  = 'Tropopause Relative (km)'
	title   = 'zrelative'
ENDIF ELSE IF KEYWORD_SET(cloud) THEN BEGIN	
	ztrsort = 0.001*(z - z_trop)
	gv_cloud  = ((DC3_READ_VAR('CONC1DC100_LWIO', flight_name)).values GT 0.0)
	dc8_cloud = ((DC3_READ_VAR('cloud', flight_name, /DC8)).values EQ 2.0 AND 3.0)
	ztracer = [gv_cloud, dc8_cloud]
	zrange  = [0, 1]
	ztitle  = 'Cloud Particles (#)'
	title   = 'cloud_particles'
ENDIF ELSE IF KEYWORD_SET(water) THEN BEGIN
	ztrsort = 0.001*(z - z_trop)
	gv_h2o  = (DC3_READ_VAR('X_H2O', flight_name)).values
	dc8_h2o = (DC3_READ_VAR('H2O_vapor_ppmv_DLH', flight_name, /DC8)).values
	ztracer = [gv_h2o, dc8_h2o]
	zrange  = [0, 400]
	ztitle  = 'Water Vapor (ppmv)'
	title   = 'water_vapor'
ENDIF 

outdir  = !WRF_DIRECTORY + flight_name + '/aircraft/plots/chemistry_tracer/' 
epsfile = outdir + STRING(flight_name) + '_' + title + '_aircraft.eps'
pngfile = outdir + STRING(flight_name) + '_' + title + '_aircraft.png'

FILE_MKDIR, outdir

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
	!P.CHARSIZE   = 2.9		
	!P.FONT       = -1																							;Use Hershey fonts
ENDELSE
!P.MULTI = [0, 2, 1]
IF KEYWORD_SET(group2) THEN !P.MULTI = [0, 3, 2]

var1_gv  = (DC3_READ_VAR('CO', flight_name)).values																;Read carbon monoxide data
var2_gv  = (DC3_READ_VAR('FO3_ACD', flight_name)).values	
var1_dc8 = (DC3_READ_VAR('CO_ppbv_DACOM', flight_name, /DC8)).values															;Read carbon monoxide data
var2_dc8 = (DC3_READ_VAR('O3_CL', flight_name, /DC8)).values																;Read ozone data
var1 	 = [var1_gv , var1_dc8]
var2 	 = [var2_gv , var2_dc8]
PLOT_TRACER_TRACER_DP, var1, var2, ztracer, $
	TITLE    = 'Aircraft', $
	ZTRSORT  = ztrsort, $
	XRANGE   = [0, 200], $
	XTITLE   = 'Carbon Monoxide (ppbv)', $
	YRANGE   = [0, 600], $
	YTITLE   = 'Ozone (ppbv)', $
	ZRANGE   = zrange, $
	ZTITLE   = ztitle, $
	BINNED   = binned, $
	CREVERSE = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $
	SYMSIZE  = symsize, $
	NOWINDOW = 1


var1_gv  = (DC3_READ_VAR('X_H2O', flight_name)).values																;Read carbon monoxide data
var2_gv  = (DC3_READ_VAR('FO3_ACD', flight_name)).values	
var1_dc8 = (DC3_READ_VAR('H2O_vapor_ppmv_DLH', flight_name, /DC8)).values															;Read carbon monoxide data
var2_dc8 = (DC3_READ_VAR('O3_CL', flight_name, /DC8)).values																;Read ozone data
var1 	 = [var1_gv , var1_dc8]
var2 	 = [var2_gv , var2_dc8]
PLOT_TRACER_TRACER_DP, var1, var2, ztracer, $
	TITLE    = 'Aircraft', $
	ZTRSORT  = ztrsort, $
	XRANGE   = [0, 500], $
	XTITLE   = 'Water Vapor (ppmv)', $
	XLOG     = 0, $
	YRANGE   = [0, 600], $
	YTITLE   = 'Ozone (ppbv)', $
	ZRANGE   = zrange, $
	ZTITLE   = ztitle, $
	BINNED   = binned, $
	CREVERSE = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $
	SYMSIZE  = symsize, $
	NOWINDOW = 1
	
!P.MULTI = 0
IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN $
		PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS													;Convert eps to pdf
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file

END
