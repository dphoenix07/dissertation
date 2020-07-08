PRO WRF_TRACER_TRACER, xtracer2, ytracer2, ztracer, $
	TITLE    = title, $
	ZTRSORT  = ztrsort, $
	XTITLE   = xtitle, $
	XRANGE   = xrange, $
	XLOG     = xlog, $
	YTITLE   = ytitle, $
	YRANGE   = yrange, $
	YLOG     = ylog, $
	ZTITLE   = ztitle, $
	ZRANGE   = zrange, $
	TWOCOLOR = twocolor, $
	NXBIN    = nxbin, $
	NYBIN    = nybin, $
	BINNED   = binned, $
	NOWINDOW = nowindow, $
	CREVERSE = creverse, $
	_EXTRA   = _extra, $
	EPS      = eps, $
	PNG      = png
	
;+
; Name:
;		WRF_TRACER_TRACER
; Purpose:
;		This is a procedure to plot two tracers from WRF output
;		against each other. e.g., o3 vs co. 
; Calling sequence:
;		WRF_TRACER_TRACER, tracer1, tracer2, tracer3
; Input:
;		xtracer : Matrix of trace gas measurements for x-axis.
;		ytracer : Matrix of trace gas measurements for y-axis.
;		ztracer : Matrix to color-scale the measurements by. (e.g., relative altitude)
; Output:
;		Trace gas measurements plotted against each other. 
; Keywords:
;		ZTRSORT  : Optional keyword to provide array to sort the measurements by. Default is to used supplied ztracer. 
;		XTITLE   : Desired trace gas axis (x-axis) title. 
;		XRANGE   : Desired trace gas axis (x-axis) range.
;		XLOG     : If set, x-axis is log scale. 
;		YTITLE   : Desired relative altitude axis (y-axis) title. 
;		YRANGE   : Desired relative altitude axis (y-axis) range.
;		YLOG     : If set, y-axis is log scale.
;		ZTITLE   : Title for color bar. 
;		ZRANGE   : Range of color-scaling variable.
;		TWOCOLOR : Optional keyword to specify 2 colors for ztracer. If 2-element array given,
;						zrange is assumed to be scalar and used to distinguish between two colors.
;		NXBIN    : Optional keyword to specify the number of bins along the x-axis for density plots.
;		NYBIN    : Optional keyword to specify the number of bins along the y-axis for density plots.
;		BINNED   : If set, then plot joint histogram (density plot). 
;		NOWINDOW : If set, then do not apply device settings. 
;		CREVERSE : If set, reverse color table for ztracer measurements. Default is blue-gray-red.
;		_EXTRA   : Passes any direct graphics keywords to plotting commands.
;		EPS      : If set, output to PostScript.
;		PNG      : If set, write PNG image.
; Author and history:
;		Cameron R. Homeyer  2012-08-08.
;		Daniel B. Phoenix	2016-01-29.	Edited plot_tracer_tracer.pro for WRF output.
;-

COMPILE_OPT IDL2																											;Set compile options

IF (N_ELEMENTS(xrange) EQ 0) THEN xrange = [  0,  800]
IF (N_ELEMENTS(xtitle) EQ 0) THEN xtitle = 'Carbon Monoxide (ppbv)'
IF (N_ELEMENTS(yrange) EQ 0) THEN yrange = [  0,   20]
IF (N_ELEMENTS(ytitle) EQ 0) THEN ytitle = 'Ozone (ppbv)'
IF (N_ELEMENTS(zrange) EQ 0) THEN zrange = [ -1,    1]
IF (N_ELEMENTS(nxbin ) EQ 0) THEN nxbin  = 100
IF (N_ELEMENTS(nybin ) EQ 0) THEN nybin  = 100
																										;Copy arrays for manipulation
;xtracer2 = xtracer																						;Copy arrays for manipulation
;ytracer2 = ytracer

IF (N_ELEMENTS(ztracer) GT 0) THEN $
	ztracer2 = ztracer

good = WHERE(((xtracer2 GE xrange[0]) AND $																			;Find data to use
				  (xtracer2 LE xrange[1]) AND $
				  (ytracer2 GE yrange[0]) AND $
				  (ytracer2 LE yrange[1])), good_count)

empty_plot = 0
IF (good_count GT 0) THEN BEGIN
	xtracer2 = xtracer2[good]
	ytracer2 = ytracer2[good]
	
	IF (N_ELEMENTS(ztracer2) GT 0) THEN $
		ztracer2 = ztracer2[good]
ENDIF ELSE empty_plot = 1

IF ((N_ELEMENTS(ztracer2) GT 0) AND (good_count GT 0)) THEN BEGIN
	isort    = SORT_CRH(ztracer2, /CENTER_NAN)
		
	xtracer2 = xtracer2[isort]
	ytracer2 = ytracer2[isort]
	ztracer2 = ztracer2[isort]
ENDIF

xlog_scl = nxbin/(ALOG10(xrange[1]/xrange[0]))																	;Set bin parameters for logaritmic scale
dx       = FLOAT(xrange[1] - xrange[0])/nxbin																	;Set bin parameters for regular scale
xbin     = 0.5*dx + dx*FINDGEN(nxbin) + xrange[0]
ylog_scl = nybin/(ALOG10(yrange[1]/yrange[0]))
dy       = FLOAT(yrange[1] - yrange[0])/nybin
ybin     = 0.5*dy + yrange[0] + dy*FINDGEN(nybin)

IF KEYWORD_SET(xlog) AND KEYWORD_SET(ylog) THEN $
	bin = LONG(xlog_scl*ALOG10(xtracer2/xrange[0])) + nxbin*LONG(ylog_scl*ALOG10(ytracer2/yrange[0])) $		;Bin data for histogram
ELSE IF KEYWORD_SET(xlog) THEN $
	bin = LONG(xlog_scl*ALOG10(xtracer2/xrange[0])) + nxbin*LONG((ytracer2-yrange[0])/dy) $
ELSE IF KEYWORD_SET(ylog) THEN $
	bin = LONG((xtracer2-xrange[0])/dx) + nxbin*LONG(ylog_scl*ALOG10(ytracer2/yrange[0])) $
ELSE $
	bin = LONG((xtracer2-xrange[0])/dx) + nxbin*LONG((ytracer2-yrange[0])/dy)


IF KEYWORD_SET(binned) THEN BEGIN
	HELP, hist
	pmax  = 100.0*(LONG(MEAN(hist) + 2*STDDEV(hist))/100 + 1)
	table = GRAYSCALE_24(40, 0.95, 0.0, PS = ps)																		;Color table for plotting 
	col   = COLOR_LOOKUP_24(hist, table, MIN_VALUE = 0.0, MAX_VALUE = pmax, MISSING = table[-1])
	none  = WHERE((hist EQ 0), none_count)
	IF (none_count GT 0) THEN col[none] = COLOR_24('white')														;Set counts of zero to white
ENDIF ELSE IF (N_ELEMENTS(ztracer2) GT 0) THEN BEGIN
	IF (zrange[0] LT 0) THEN table = BLUE_GRAY_RED_24(40,19,0.25) $
							  ELSE table = [REVERSE(WHITE_BLUE_24(39,0.1)),COLOR_24('red')]
	
	IF KEYWORD_SET(creverse) THEN table = REVERSE(table)
	
	IF (N_ELEMENTS(twocolor) EQ 2) THEN col = twocolor[(ztracer2 GT zrange[0])] $							;If 2-element color table, set color
	ELSE BEGIN
		col = COLOR_LOOKUP_24(ztracer2, table, MIN_VALUE = zrange[0], MAX_VALUE = zrange[1])
	
		below = WHERE(ztracer2 LT zrange[0], nbelow)
		above = WHERE(ztracer2 GT zrange[1], nabove)
		IF (nbelow GT 0) THEN col[below] = table[ 0]
		IF (nabove GT 0) THEN col[above] = table[-1]
	ENDELSE
ENDIF ELSE col = COLOR_24('black')

					
IF ~KEYWORD_SET(nowindow) THEN BEGIN
	IF KEYWORD_SET(eps) THEN BEGIN	
		PS_ON, FILENAME = epsfile, PAGE_SIZE = [8.0, 8.0], MARGIN = 0.0, /INCHES						;Switch to Postscript device
		DEVICE, /ENCAPSULATED
		!P.FONT     = 0																									;Hardware fonts
		!P.CHARSIZE = 2.0	
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS																								;Load basic color definitions
	ENDIF ELSE BEGIN
		SET_PLOT, 'X'
		WINDOW, XSIZE = 800, YSIZE = 850																				;Open graphics window
		!P.COLOR      = COLOR_24('black')																			;Foreground color
		!P.BACKGROUND = COLOR_24('white')																			;Background color
		!P.CHARSIZE   = 2.0		
		!P.FONT       = -1																								;Use Hershey fonts
	ENDELSE
ENDIF

IF ~empty_plot THEN BEGIN
	USERSYM_CIRCLE, /FILL																								;Load circle user plot symbol
	PLOT, xtracer2, ytracer2, /NODATA, $
		TITLE    = title, $
		XRANGE  = xrange, $
		XLOG    = xlog, $
		XSTYLE  = 1, $
		XTITLE  = xtitle, $
		YRANGE  = yrange, $
		YLOG    = ylog, $
		YMARGIN = [10,2], $
		YSTYLE  = ystyle, $
		YTITLE  = ytitle
		
	PLOTS, xtracer2, ytracer2, PSYM = 8, COLOR = col, NOCLIP = 0, _EXTRA = _extra
		
	xy   = CONVERT_COORD(xrange, yrange, /DATA, /TO_NORMAL)
	dxax = xy[0,1] - xy[0,0]
	dyax = xy[1,1] - xy[1,0]

	x1   = xy[0,0] + 0.1*dxax
	x2   = xy[0,1] - 0.1*dxax
	y1   = xy[1,0] - 0.26*dyax
	y2   = xy[1,0] - 0.24*dyax

	IF ((N_ELEMENTS(ztracer2) GT 0) AND (N_ELEMENTS(twocolor) NE 2)) THEN $
		COLOR_BAR_24_KPB, table, UNDER = table[0], OVER = table[-1], $
			RANGE = zrange, $
			TITLE = ztitle, $
			MINOR = 1, $
			POSIT = [x1,y1,x2,y2]
		
	!P.POSITION = 0
ENDIF ELSE BEGIN
	PLOT, xtracer2, ytracer2, /NODATA, $
		TITLE    = title, $
		XRANGE  = xrange, $
		XLOG    = xlog, $
		XSTYLE  = 1, $
		XTITLE  = xtitle, $
		YRANGE  = yrange, $
		YLOG    = ylog, $
		YMARGIN = [10,2], $
		YSTYLE  = ystyle, $
		YTITLE  = ytitle
ENDELSE

IF KEYWORD_SET(eps) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																					;Reset color table to linear ramp
	PS_OFF																												;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
		WRITE_PNG, pngfile, TVRD(TRUE=1)																				;Write PNG image


END