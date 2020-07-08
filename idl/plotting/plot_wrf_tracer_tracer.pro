PRO PLOT_WRF_TRACER_TRACER, event, scheme, date, $
	BINNED    = binned, $
	THRESHOLD = threshold, $
	ZRELATIVE = zrelative, $
	CLOUD     = cloud, $
	WATER     = water, $
	GROUP2    = group2, $
	DOMAIN    = domain, $
	EPS       = eps, $
	PDF       = pdf, $
	PNG       = png
	
;+
; Name:
;		PLOT_WRF_TRACER_TRACER
; Purpose:
;		This is a procedure to plot DC3 trace gases in tracer-tracer space. 
; Calling sequence:
;		PLOT_WRF_TRACER_TRACER
; Input:
;		event  : String variable of event (e.g., '20120519')
;		scheme : String variable of parameterization scheme (e.g., 'morrison')
;		date   : Date to plot (e.g., MAKE_DATE(2012,5,19,22))
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
;		Daniel B. Phoenix	2016-01-29.		Adjusted for WRF output.
;-

COMPILE_OPT IDL2																			;Set compile options

IF (N_ELEMENTS (date	  ) EQ 0) THEN date 	  =  MAKE_DATE(2012,5,19,22,00)
IF (N_ELEMENTS (event	  ) EQ 0) THEN event      = '20120519'
IF (N_ELEMENTS (scheme	  ) EQ 0) THEN scheme     = 'morrison'
IF (N_ELEMENTS (domain    ) EQ 0) THEN domain     = 1
IF (N_ELEMENTS (threshold )	EQ 0) THEN threshold  = 1000.0 

dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")												;Set domain string
date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string

IF KEYWORD_SET(cloud) THEN key = 'cloud'
IF KEYWORD_SET(water) THEN key = 'water'
IF KEYWORD_SET(zrelative) THEN key = 'zrelative'

outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/plots/chemistry_tracer/'
epsfile = outdir + scheme + '_' + key + '_' + date_string + '.eps'										;EPS filename
pdffile = outdir + scheme + '_' + key + '_' + date_string + '.pdf'										;PDF filename
pngfile = outdir + scheme + '_' + key + '_' + date_string + '.png'										;PNG filename

FILE_MKDIR, outdir

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

!P.MULTI = [0, 2, 1]
IF KEYWORD_SET(group2) THEN !P.MULTI = [0, 3, 2]

z = (WRF_READ_VAR('Z', date, event, scheme, DOMAIN = domain)).values
dim = SIZE(z, /DIMENSIONS)	

z_trop   = (WRF_READ_VAR('Z_trop', date, event, scheme, DOMAIN = domain)).values
z_trop   = CALC_TROP_MODE(z_trop, scheme, threshold) 
xyz_trop = REBIN(z_trop, dim[0], dim[1], dim[2], /SAMPLE)

IF KEYWORD_SET(zrelative) THEN BEGIN
	ztracer = 0.001*(z - xyz_trop)
	zrange  = [-2.5, 2.5]
	ztitle  = 'Tropopause Relative (km)'
ENDIF ELSE IF KEYWORD_SET(cloud) THEN BEGIN	
	ztrsort = 0.001*(z - xyz_trop)
	ztracer = ((((WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, $
				DOMAIN = domain)).values) * 1.0E3) GT 0.01)
	zrange  = [0, 1]
	ztitle  = 'Cloud Particles (#)'
ENDIF ELSE IF KEYWORD_SET(water) THEN BEGIN
	ztrsort  = 0.001*(z - xyz_trop)
	ztracer = (WRF_READ_VAR('H2O', date, event, scheme, DOMAIN = domain)).values
	ztracer = ztracer*1.0E6
	zrange  = [0, 400]
	ztitle  = 'Water Vapor (ppmv)'
ENDIF 

symsize = 1.0 - 0.25*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf))

var1 = WRF_READ_VAR('CO', date, event, scheme, DOMAIN = domain)																;Read carbon monoxide data
var1.values = 1.0E3*var1.values 	
var2 = WRF_READ_VAR('O3', date, event, scheme, DOMAIN = domain)																;Read ozone data
var2.values = 1.0E3*var2.values 	
WRF_TRACER_TRACER, var1.values, var2.values, ztracer, $
	TITLE    = scheme + '_' + date_string, $
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

var1 = WRF_READ_VAR('H2O', date, event, scheme, DOMAIN = domain)							;Read water vapor data
var1.values = 1.0E6*var1.values 	
var2 = WRF_READ_VAR('O3' , date, event, scheme, DOMAIN = domain)	
var2.values = 1.0E3*var2.values 															;Read ozone data
WRF_TRACER_TRACER, var1.values, var2.values, ztracer, $
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

IF KEYWORD_SET(group2) THEN BEGIN

var1 = WRF_READ_VAR('H2O', date, event, scheme, DOMAIN = domain)							;Read water vapor data
var1.values = 1.0E6*var1.values 	
var2 = WRF_READ_VAR('O3' , date, event, scheme, DOMAIN = domain)	
var2.values = 1.0E3*var2.values 															;Read ozone data
WRF_TRACER_TRACER, var1.values, var2.values, ztracer, $
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
		
var1 = WRF_READ_VAR('NO', date, event, scheme, DOMAIN = domain)																;Read nitrogen oxide data
var1.values = 1.0E3*var1.values
var2 = WRF_READ_VAR('O3', date, event, scheme, DOMAIN = domain)																;Read ozone data
var2.values = 1.0E3*var2.values 	
WRF_TRACER_TRACER, var1.values, var2.values, ztracer, $
	ZTRSORT  = ztrsort, $
	XRANGE   = [0, 2000], $
	XTITLE   = 'NO2 (pptv)', $
	YRANGE   = [0, 800], $
	YTITLE   = 'Ozone (ppbv)', $
	ZRANGE   = zrange, $
	ZTITLE   = ztitle, $
	BINNED   = binned, $
	CREVERSE = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $
	SYMSIZE  = symsize, $
	NOWINDOW = 1
	
	
var1 = WRF_READ_VAR('H2O', date, event, scheme, DOMAIN = domain)																	;Read water vapor data
var2 = WRF_READ_VAR('NO2', date, event, scheme, DOMAIN = domain)																	;Read nitrogen oxide data
WRF_TRACER_TRACER, var1.values, var2.values, ztracer, $
	ZTRSORT  = ztrsort, $
	XRANGE   = [1, 10000], $
	XTITLE   = 'Water Vapor (ppmv)', $
	XLOG     = 1, $
	YRANGE   = [0, 2000], $
	YTITLE   = 'NO2 (pptv)', $
	ZRANGE   = zrange, $
	ZTITLE   = ztitle, $
	BINNED   = binned, $
	CREVERSE = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $
	SYMSIZE  = symsize, $
	NOWINDOW = 1

var1 = WRF_READ_VAR('CO' , date, event, scheme, DOMAIN = domain)																		;Read carbon monoxide data
var2 = WRF_READ_VAR('NO2', date, event, scheme, DOMAIN = domain)																		;Read nitrogen oxide data
WRF_TRACER_TRACER, var1.values, var2.values, ztracer, $
	ZTRSORT  = ztrsort, $
	XRANGE   = [0, 200], $
	XTITLE   = 'Carbon Monoxide (ppbv)', $
	YRANGE   = [0, 2000], $
	YTITLE   = 'NO2 (pptv)', $
	ZRANGE   = zrange, $
	ZTITLE   = ztitle, $
	BINNED   = binned, $
	CREVERSE = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $
	SYMSIZE  = symsize, $
	NOWINDOW = 1

var1 = WRF_READ_VAR('HNO3', date, event, scheme, DOMAIN = domain)																;Read methane data
var2 = WRF_READ_VAR('HCHO', date, event, scheme, DOMAIN = domain)																;Read ozone data
WRF_TRACER_TRACER, var1.values, var2.values, ztracer, $
	ZTRSORT  = ztrsort, $
	XRANGE   = [1600, 2000], $
	XTITLE   = 'Nitric Acid (ppbv)', $
	YRANGE   = [0, 800], $
	YTITLE   = 'Formaldehyde (ppbv)', $
	ZRANGE   = zrange, $
	ZTITLE   = ztitle, $
	BINNED   = binned, $
	CREVERSE = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $
	SYMSIZE  = symsize, $
	NOWINDOW = 1

ENDIF

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
