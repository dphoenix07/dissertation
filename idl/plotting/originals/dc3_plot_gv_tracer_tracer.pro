PRO DC3_PLOT_GV_TRACER_TRACER, flight_name, $
	BINNED    = binned, $
	ZRELATIVE = zrelative, $
	CLOUD     = cloud, $
	WATER     = water, $
	GROUP2    = group2, $
	EPS       = eps, $
	PDF       = pdf, $
	PNG       = png

;+
; Name:
;		DC3_PLOT_TRACER_TRACER
; Purpose:
;		This is a procedure to plot DC3 trace gases in tracer-tracer space. 
; Calling sequence:
;		DC3_PLOT_TRACER_TRACER
; Input:
;		None.
; Output:
;		Set of tracer-tracer plots.
; Keywords:
;		BINNED    : If set, plot joint-histograms. 
;		ZRELATIVE : If set, color measurements by relative altitude to the tropopause.
;		CLOUD     : If set, color measurements by cloud proxy.
;		GROUP2    : If set, plot 2ng group of 6 tracer tracer plots.
;		EPS       : If set, output to PostScript.
;		PNG       : If set, write PNG image.
; Author and history:
;		Cameron R. Homeyer  2012-08-09.
;-

COMPILE_OPT IDL2																									;Set compile options

IF (N_ELEMENTS(flight_name) EQ 0) THEN fname = 'all' $
											 ELSE fname = flight_name[0]

IF KEYWORD_SET(zrelative) THEN ename = '_ralt'  ELSE $
IF KEYWORD_SET(cloud    ) THEN ename = '_cloud' ELSE $
										 ename = ''
											 
epsfile = '~/dc3_' + fname + '_tracer_tracer' + ename + '.eps'
pdffile = '~/dc3_' + fname + '_tracer_tracer' + ename + '.pdf'
pngfile = '~/dc3_' + fname + '_tracer_tracer' + ename + '.png'

IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN	
	PS_ON, FILENAME = epsfile, PAGE_SIZE = [8.0, 6.0], MARGIN = 0.0, /INCHES					;Switch to Postscript device
	DEVICE, /ENCAPSULATED
	!P.FONT     = 0																								;Hardware fonts
	!P.CHARSIZE = 1.2
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS																							;Load basic color definitions
ENDIF ELSE BEGIN
	SET_PLOT, 'X'
	WINDOW, XSIZE = 1200, YSIZE = 900																		;Open graphics window
	!P.COLOR      = COLOR_24('black')																		;Foreground color
	!P.BACKGROUND = COLOR_24('white')																		;Background color
	!P.CHARSIZE   = 2.0		
	!P.FONT       = -1																							;Use Hershey fonts
ENDELSE
!P.MULTI = [0, 3, 2]

ztrop = DC3_READ_VAR('GFS_TROP_HGT', flight_name)
IF (flight_name EQ '20120530') THEN BEGIN
	date  = DC3_READ_VAR('Time', flight_name)
	it    = WHERE((date.values.hour EQ 1) AND $																;Get indices of timeseries
					((date.values.minute + date.values.second/60.0) GE 20.0) AND $
					((date.values.minute + date.values.second/60.0) LE 50.0), nt)
	ztrop.values[it] = 12000.0
ENDIF

IF KEYWORD_SET(zrelative) THEN BEGIN
	ztracer = 0.001*((DC3_READ_VAR('GGALT', flight_name)).values - ztrop.values)
	zrange  = [-2.5, 2.5]
	ztitle  = 'Tropopause Relative (km)'
ENDIF ELSE IF KEYWORD_SET(cloud) THEN BEGIN	
	ztrsort = 0.001*((DC3_READ_VAR('GGALT', flight_name)).values - ztrop.values)
	ztracer = ((DC3_READ_VAR('CONC1DC100_LWIO', flight_name)).values GT 0.0)
	zrange  = [0, 1]
	ztitle  = '>= 100u Cloud Particles (#/L)'
ENDIF ELSE IF KEYWORD_SET(water) THEN BEGIN
	ztrsort = 0.001*((DC3_READ_VAR('GGALT', flight_name)).values - ztrop.values)
	ztracer = (DC3_READ_VAR('X_H2O', flight_name)).values
	zrange  = [0, 400]
	ztitle  = 'Water Vapor (ppmv)'
ENDIF

symsize = 1.0 - 0.25*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf))

IF KEYWORD_SET(group2) THEN BEGIN
	var1 = DC3_READ_VAR('NO+NO2', flight_name)																;Read NOx data
	var2 = DC3_READ_VAR('FO3_ACD',  flight_name)																;Read ozone data
	PLOT_TRACER_TRACER, var1.values, var2.values, ztracer, $
		ZTRSORT  = ztrsort, $
		XRANGE   = [0, 4000], $
		XTITLE   = 'NOx (pptv)', $
		YRANGE   = [0, 400], $
		YTITLE   = 'Ozone (ppbv)', $
		ZRANGE   = zrange, $
		ZTITLE   = ztitle, $
		BINNED   = binned, $
		CREVERSE = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $
		SYMSIZE  = symsize, $
		NOWINDOW = 1
	
	var1 = DC3_READ_VAR('H2O2', flight_name)																;Read hydrogen peroxide data
	var2 = DC3_READ_VAR('FO3_ACD',   flight_name)																;Read ozone data
	PLOT_TRACER_TRACER, var1.values, var2.values, ztracer, $
		ZTRSORT  = ztrsort, $
		XRANGE   = [0, 6000], $
		XTITLE   = 'Hydrogen Peroxide (pptv)', $
		YRANGE   = [0, 400], $
		YTITLE   = 'Ozone (ppbv)', $
		ZRANGE   = zrange, $
		ZTITLE   = ztitle, $
		BINNED   = binned, $
		CREVERSE = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $
		SYMSIZE  = symsize, $
		NOWINDOW = 1
	
	var1 = DC3_READ_VAR('CO'  , flight_name)																	;Read carbon monoxide data
	var2 = DC3_READ_VAR('HNO3', flight_name)																	;Read nitric acid data
	PLOT_TRACER_TRACER, var1.values, var2.values, ztracer, $
		ZTRSORT  = ztrsort, $
		XRANGE   = [0, 200], $
		XTITLE   = 'Carbon Monoxide (ppbv)', $
		YRANGE   = [0, 4000], $
		YTITLE   = 'Nitric Acid (pptv)', $
		ZRANGE   = zrange, $
		ZTITLE   = ztitle, $
		BINNED   = binned, $
		CREVERSE = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $
		SYMSIZE  = symsize, $
		NOWINDOW = 1
	
	var1 = DC3_READ_VAR('CO2'  , flight_name)																	;Read carbon dioxide data
	var2 = DC3_READ_VAR('HNO3', flight_name)																	;Read nitric acid data
	PLOT_TRACER_TRACER, var1.values, var2.values, ztracer, $
		ZTRSORT  = ztrsort, $
		XRANGE   = [380, 450], $
		XTITLE   = 'Carbon Dioxide (ppmv)', $
		YRANGE   = [0, 4000], $
		YTITLE   = 'Nitric Acid (pptv)', $
		ZRANGE   = zrange, $
		ZTITLE   = ztitle, $
		BINNED   = binned, $
		CREVERSE = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $
		SYMSIZE  = symsize, $
		NOWINDOW = 1
	
	var1 = DC3_READ_VAR('CO2'           , flight_name)														;Read carbon dioxide data
	var2 = DC3_READ_VAR('CH2O_CAMS_pptv', flight_name)														;Read formaldehyde data
	PLOT_TRACER_TRACER, var1.values, var2.values, ztracer, $
		ZTRSORT  = ztrsort, $
		XRANGE   = [380, 450], $
		XTITLE   = 'Carbon Dioxide (ppmv)', $
		YRANGE   = [0, 3000], $
		YTITLE   = 'Formaldehyde (pptv)', $
		ZRANGE   = zrange, $
		ZTITLE   = ztitle, $
		BINNED   = binned, $
		CREVERSE = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $
		SYMSIZE  = symsize, $
		NOWINDOW = 1
	
	var1 = DC3_READ_VAR('Methane', flight_name)																;Read methane data
	var2 = DC3_READ_VAR('CO2'    , flight_name)																;Read carbon dioxide data
	PLOT_TRACER_TRACER, var1.values, var2.values, ztracer, $
		ZTRSORT  = ztrsort, $
		XRANGE   = [1600, 2000], $
		XTITLE   = 'Methane (ppbv)', $
		YRANGE   = [380, 450], $
		YTITLE   = 'Carbon Dioxide (ppmv)', $
		ZRANGE   = zrange, $
		ZTITLE   = ztitle, $
		BINNED   = binned, $
		CREVERSE = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $
		SYMSIZE  = symsize, $
		NOWINDOW = 1
ENDIF ELSE BEGIN
	var1 = DC3_READ_VAR('X_H2O', flight_name)																;Read water vapor data
	var2 = DC3_READ_VAR('FO3_ACD',    flight_name)																;Read ozone data
	PLOT_TRACER_TRACER, var1.values, var2.values, ztracer, $
		ZTRSORT  = ztrsort, $
		XRANGE   = [1, 1000], $
		XTITLE   = 'Water Vapor (ppmv)', $
		XLOG     = 1, $
		YRANGE   = [0, 600], $
		YTITLE   = 'Ozone (ppbv)', $
		ZRANGE   = zrange, $
		ZTITLE   = ztitle, $
		BINNED   = binned, $
		CREVERSE = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $
		SYMSIZE  = symsize, $
		NOWINDOW = 1
	
	var1 = DC3_READ_VAR('FO3_ACD',      flight_name);DC3_READ_VAR('Methane', flight_name)																;Read methane data
	var2 = DC3_READ_VAR('FO3_ACD',      flight_name)																;Read ozone data
	PLOT_TRACER_TRACER, var1.values, var2.values, ztracer, $
		ZTRSORT  = ztrsort, $
		XRANGE   = [1600, 2000], $
		XTITLE   = 'Methane (ppbv)', $
		YRANGE   = [0, 400], $
		YTITLE   = 'Ozone (ppbv)', $
		ZRANGE   = zrange, $
		ZTITLE   = ztitle, $
		BINNED   = binned, $
		CREVERSE = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $
		SYMSIZE  = symsize, $
		NOWINDOW = 1
	
	var1 = DC3_READ_VAR('NO', flight_name)																;Read nitrogen oxide data
	var2 = DC3_READ_VAR('FO3_ACD', flight_name)																;Read ozone data
	PLOT_TRACER_TRACER, var1.values, var2.values, ztracer, $
		ZTRSORT  = ztrsort, $
		XRANGE   = [0, 1000], $
		XTITLE   = 'NO (pptv)', $
		YRANGE   = [0, 600], $
		YTITLE   = 'Ozone (ppbv)', $
		ZRANGE   = zrange, $
		ZTITLE   = ztitle, $
		BINNED   = binned, $
		CREVERSE = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $
		SYMSIZE  = symsize, $
		NOWINDOW = 1
	
	var1 = DC3_READ_VAR('CO', flight_name)																;Read carbon monoxide data
	var2 = DC3_READ_VAR('FO3_ACD', flight_name)																;Read ozone data
	PLOT_TRACER_TRACER, var1.values, var2.values, ztracer, $
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
	
	var1 = DC3_READ_VAR('X_H2O', flight_name)																	;Read water vapor data
	var2 = DC3_READ_VAR('NO'   , flight_name)																	;Read nitrogen oxide data
	PLOT_TRACER_TRACER, var1.values, var2.values, ztracer, $
		ZTRSORT  = ztrsort, $
		XRANGE   = [1, 10000], $
		XTITLE   = 'Water Vapor (ppmv)', $
		XLOG     = 1, $
		YRANGE   = [0, 1000], $
		YTITLE   = 'NO (pptv)', $
		ZRANGE   = zrange, $
		ZTITLE   = ztitle, $
		BINNED   = binned, $
		CREVERSE = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $
		SYMSIZE  = symsize, $
		NOWINDOW = 1
	
	var1 = DC3_READ_VAR('CO', flight_name)																		;Read carbon monoxide data
	var2 = DC3_READ_VAR('NO', flight_name)																		;Read nitrogen oxide data
	PLOT_TRACER_TRACER, var1.values, var2.values, ztracer, $
		ZTRSORT  = ztrsort, $
		XRANGE   = [0, 200], $
		XTITLE   = 'Carbon Monoxide (ppbv)', $
		YRANGE   = [0, 2000], $
		YTITLE   = 'NO (pptv)', $
		ZRANGE   = zrange, $
		ZTITLE   = ztitle, $
		BINNED   = binned, $
		CREVERSE = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $
		SYMSIZE  = symsize, $
		NOWINDOW = 1
ENDELSE


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
