PRO PLOT_TRACER_TRACER_P95, xtracer_conv, xtracer_ncon, ytracer_conv, ytracer_ncon, ztracer_conv, ztracer_ncon,$
	TITLE     = title, $
	XTITLE    = xtitle, $
	XRANGE    = xrange, $
	XLOG      = xlog, $
	YTITLE    = ytitle, $
	YRANGE    = yrange, $
	YLOG      = ylog, $
	ZTITLE    = ztitle, $
	ZRANGE    = zrange, $
	NXBIN     = nxbin, $
	NYBIN     = nybin, $
	BINNED    = binned, $
	NOWINDOW  = nowindow, $
	CREVERSE  = creverse, $
	_EXTRA    = _extra, $
	EPS       = eps, $
	PNG       = png

;+
; Name:
;		PLOT_TRACER_TRACER_P95
; Purpose:
;		This is a procedure to plot two tracers against each other. e.g., o3 vs co. 
; Calling sequence:
;		PLOT_TRACER_TRACER, tracer1, tracer2
; Input:
;		xtracer : Array of trace gas measurements for x-axis.
;		ytracer : Array of trace gas measurements for y-axis.
;		ztracer : Array to color-scale the measurements by. (e.g., relative altitude)
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
;-

COMPILE_OPT IDL2																											;Set compile options

IF (N_ELEMENTS(xrange) EQ 0) THEN xrange = [  0,  200]
IF (N_ELEMENTS(xtitle) EQ 0) THEN xtitle = 'Carbon Monoxide (ppbv)'
IF (N_ELEMENTS(yrange) EQ 0) THEN yrange = [  0,  800]
IF (N_ELEMENTS(ytitle) EQ 0) THEN ytitle = 'Ozone (ppbv)'
IF (N_ELEMENTS(zrange) EQ 0) THEN zrange = [ -1,    1]
IF (N_ELEMENTS(nxbin ) EQ 0) THEN nxbin  = 100
IF (N_ELEMENTS(nybin ) EQ 0) THEN nybin  = 100

good = WHERE(((xtracer_ncon GE xrange[0]) AND $																			;Find data to use
				  (xtracer_ncon LE xrange[1]) AND $
				  (ytracer_ncon GE yrange[0]) AND $
				  (ytracer_ncon LE yrange[1])), good_count)

empty_plot = 0
IF (good_count GT 0) THEN BEGIN
	xtracer_ncon = xtracer_ncon[good]
	ytracer_ncon = ytracer_ncon[good]
	ztracer_ncon = ztracer_ncon[good]
ENDIF ELSE empty_plot = 1

IF ((N_ELEMENTS(ztracer_ncon) GT 0) AND (good_count GT 0)) THEN BEGIN
	isort    = SORT_CRH(ztracer_ncon, /CENTER_NAN)
		
	xtracer_ncon = xtracer_ncon[isort]
	ytracer_ncon = ytracer_ncon[isort]
	ztracer_ncon = ztracer_ncon[isort]
ENDIF

xlog_scl = nxbin/(ALOG10(xrange[1]/xrange[0]))																	;Set bin parameters for logaritmic scale
dx       = FLOAT(xrange[1] - xrange[0])/nxbin																	;Set bin parameters for regular scale
xbin     = 0.5*dx + dx*FINDGEN(nxbin) + xrange[0]
ylog_scl = nybin/(ALOG10(yrange[1]/yrange[0]))
dy       = FLOAT(yrange[1] - yrange[0])/nybin
ybin     = 0.5*dy + yrange[0] + dy*FINDGEN(nybin)

IF KEYWORD_SET(xlog) AND KEYWORD_SET(ylog) THEN $
	bin = LONG(xlog_scl*ALOG10(xtracer_ncon/xrange[0])) + nxbin*LONG(ylog_scl*ALOG10(ytracer_ncon/yrange[0])) $		;Bin data for histogram
ELSE IF KEYWORD_SET(xlog) THEN $
	bin = LONG(xlog_scl*ALOG10(xtracer_ncon/xrange[0])) + nxbin*LONG((ytracer_ncon-yrange[0])/dy) $
ELSE IF KEYWORD_SET(ylog) THEN $
	bin = LONG((xtracer_ncon-xrange[0])/dx) + nxbin*LONG(ylog_scl*ALOG10(ytracer_ncon/yrange[0])) $
ELSE $
	bin = LONG((xtracer_ncon-xrange[0])/dx) + nxbin*LONG((ytracer_ncon-yrange[0])/dy)

hist = HISTOGRAM(bin, BINSIZE = 1, MIN = 0, MAX = (nxbin*nybin -1))													;Calculate density
hist = REFORM(hist, nxbin, nybin)

layer_total = REBIN(REFORM(TOTAL(hist,1),1,nybin),nxbin,nybin)
freq 		= 100.0 * (FLOAT(hist) / layer_total)

PRINT, 'Begin Finding 95th Percentile of Non-Convective Pot. Temps'
p95  = FLTARR(nxbin*nybin)*!Values.F_NaN
n_nc = FLTARR(nxbin*nybin)*!Values.F_NaN
FOR binval = 0, nxbin*nybin-1 DO BEGIN
	ibin = WHERE((bin EQ binval), n)
	PRINT, binval
	n_nc[binval] = n
	IF (n GT 0) THEN BEGIN
		pot = ztracer_ncon[ibin]
		p95[binval] = PERCENTILE(pot, 95, /VALUE)
	ENDIF
ENDFOR
p95 = REFORM(p95,nxbin,nybin)

zrange = [280,420]
table = [REVERSE(WHITE_BLUE_24(39,0.1)),COLOR_24('red')]
col   = COLOR_LOOKUP_24(p95, table, MIN_VALUE = zrange[0], MAX_VALUE = zrange[1], MISSING = table[-1])

none  = WHERE((hist EQ 0), none_count)
IF (none_count GT 0) THEN col[none] = COLOR_24('white')														;Set counts of zero to white

epsfile = '~/tracer_tracer.eps'																						;EPS filename
pngfile = '~/tracer_tracer.png'																						;PNG filename

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

title = 'Non-Conv.(updraft)'
IF ~empty_plot THEN BEGIN
	USERSYM_CIRCLE, /FILL																								;Load circle user plot symbol
	IF KEYWORD_SET(binned) THEN BEGIN	
		IF KEYWORD_SET(xlog) AND KEYWORD_SET(ylog) THEN $
			PLOT, xbin, ybin, /NODATA, $																				;Set up plot window
				TITLE    = title, $
				XRANGE   = [0, nxbin], $
				XSTYLE   = 1, $
				XTICKNAM = REPLICATE(' ', 20), $
				XTICKLEN = 0.0001, $
				YRANGE   = [0, nybin], $
				YSTYLE   = 1, $
				YMARGIN  = [10,2], $
				YTICKNAM = REPLICATE(' ', 20) $
		ELSE IF KEYWORD_SET(xlog) THEN $
			PLOT, xbin, ybin, /NODATA, $																				;Set up plot window
				TITLE    = title, $
				XRANGE   = [0, nxbin], $
				XSTYLE   = 1, $
				XTICKNAM = REPLICATE(' ', 20), $
				XTICKLEN = 0.0001, $
				YRANGE   = yrange, $
				YSTYLE   = 1, $
				YMARGIN  = [10,2], $
				YTICKNAM = REPLICATE(' ', 20) $
		ELSE IF KEYWORD_SET(ylog) THEN $
			PLOT, xbin, ybin, /NODATA, $																				;Set up plot window
				TITLE    = title, $
				XRANGE   = xrange, $
				XSTYLE   = 1, $
				XTICKNAM = REPLICATE(' ', 20), $
				XTICKLEN = 0.0001, $
				YRANGE   = [0, nybin], $
				YSTYLE   = 1, $
				YMARGIN  = [10,2], $
				YTICKNAM = REPLICATE(' ', 20) $
		ELSE $
			PLOT, xbin, ybin, /NODATA, $																				;Set up plot window
				TITLE    = title, $
				XRANGE   = xrange, $
				XSTYLE   = 1, $
				XTICKNAM = REPLICATE(' ', 20), $
				XTICKLEN = 0.0001, $
				YRANGE   = yrange, $
				YSTYLE   = 1, $
				YMARGIN  = [10,2], $
				YTICKNAM = REPLICATE(' ', 20)

		FOR j = 0, nybin -1 DO BEGIN
			FOR i = 0, nxbin -1 DO BEGIN
				IF KEYWORD_SET(xlog) AND KEYWORD_SET(ylog) THEN $
					POLYFILL, [i    , i + 1, $																				;Draw polygons
								  i + 1, i    , $
								  i          ], $
								 [j    , j + 1, $
								  j + 1, j    , $
								  j          ], $
								 COLOR = col[i,j], /DATA $
				ELSE IF KEYWORD_SET(xlog) THEN $
					POLYFILL, [i    , i + 1, $	
								  i + 1, i    , $
								  i          ], $
								 [yrange[0] + j*dy,               yrange[0] + j*dy, $
								  yrange[0] + (j+1)*dy,           yrange[0] + (j+1)*dy, $
								  yrange[0] + j*dy], $
								 COLOR = col[i,j], /DATA $
				ELSE IF KEYWORD_SET(ylog) THEN $
					POLYFILL, [xrange[0] + i*dx,               xrange[0] + i*dx, $
								  xrange[0] + (i+1)*dx,           xrange[0] + (i+1)*dx, $
								  xrange[0] + i*dx], $
								 [j    , j + 1, $
								  j + 1, j    , $
								  j          ], $
								 COLOR = col[i,j], /DATA $
				ELSE $
					POLYFILL, [xrange[0] + i*dx,     xrange[0] + (i+1)*dx, $
								  xrange[0] + (i+1)*dx, xrange[0] + i*dx, $
								  xrange[0] + i*dx], $
								 [yrange[0] + j*dy,     yrange[0] + j*dy, $
								  yrange[0] + (j+1)*dy, yrange[0] + (j+1)*dy, $
								  yrange[0] + j*dy], $
								 COLOR = col[i,j], /DATA
			ENDFOR
		ENDFOR
	
		AXIS, YAXIS = 0, /SAVE, $																							;Redraw axes that are covered by hist
			YRANGE = yrange, $
			YLOG   = ylog, $
			YSTYLE = 1, $
			YTITLE = ytitle, $
			_EXTRA = _extra
		
		AXIS, XAXIS = 0, /SAVE, $
			XRANGE = xrange, $
			XLOG   = xlog, $
			XSTYLE = 1, $
			XTITLE = xtitle, $
			_EXTRA = _extra
		
		AXIS, YAXIS = 1, $																									;Redraw axes that are covered by hist
			YRANGE = yrange, $
			YLOG   = ylog, $
			YTICKN = REPLICATE(' ', 20), $
			YSTYLE = 1, $
			_EXTRA = _extra
		
		AXIS, XAXIS = 1, $
			XRANGE = xrange, $
			XLOG   = xlog, $
			XTICKN = REPLICATE(' ', 20), $
			XSTYLE = 1, $
			_EXTRA = _extra

		xy   = CONVERT_COORD(xrange, yrange, /DATA, /TO_NORMAL)
		dxax = xy[0,1] - xy[0,0]
		dyax = xy[1,1] - xy[1,0]
		
		x1   = xy[0,0] + 0.1*dxax
		x2   = xy[0,1] - 0.1*dxax
		y1   = xy[1,0] - 0.26*dyax
		y2   = xy[1,0] - 0.24*dyax
		
		COLOR_BAR_24, table, $
			RANGE  = zrange, $
			TITLE  = ztitle, $
			TICKS = 1, $
			POSIT = [x1,y1,x2,y2]

		!P.POSITION = 0
	ENDIF ELSE BEGIN	;if not binned
		PLOT, xtracer_ncon, ytracer_ncon, /NODATA, $
			TITLE   = title, $
			XRANGE  = xrange, $
			XLOG    = xlog, $
			XSTYLE  = 1, $
			XTITLE  = xtitle, $
			YRANGE  = yrange, $
			YLOG    = ylog, $
			YMARGIN = [10,2], $
			YSTYLE  = ystyle, $
			YTITLE  = ytitle
		
		PLOTS, xtracer_ncon, ytracer_ncon, PSYM = 8, COLOR = col, NOCLIP = 0, _EXTRA = _extra
		
		xy   = CONVERT_COORD(xrange, yrange, /DATA, /TO_NORMAL)
		dxax = xy[0,1] - xy[0,0]
		dyax = xy[1,1] - xy[1,0]

		x1   = xy[0,0] + 0.1*dxax
		x2   = xy[0,1] - 0.1*dxax
		y1   = xy[1,0] - 0.26*dyax
		y2   = xy[1,0] - 0.24*dyax

		IF ((N_ELEMENTS(ztracer_ncon) GT 0) AND (N_ELEMENTS(twocolor) NE 2)) THEN $
			COLOR_BAR_24_KPB, table, UNDER = table[0], OVER = table[-1], $
				RANGE = zrange, $
				TITLE = ztitle, $
				MINOR = 1, $
				POSIT = [x1,y1,x2,y2]
		
		!P.POSITION = 0
	ENDELSE
ENDIF ELSE BEGIN ;if empty plot
	PLOT, xtracer_ncon, ytracer_ncon, /NODATA, $
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

;;End non-convective section


;;Begin convective section
good = WHERE(((xtracer_conv GE xrange[0]) AND $																			;Find data to use
				  (xtracer_conv LE xrange[1]) AND $
				  (ytracer_conv GE yrange[0]) AND $
				  (ytracer_conv LE yrange[1])), good_count)

empty_plot = 0
IF (good_count GT 0) THEN BEGIN
	xtracer_conv = xtracer_conv[good]
	ytracer_conv = ytracer_conv[good]
	ztracer_conv = ztracer_conv[good]
ENDIF ELSE empty_plot = 1

IF ((N_ELEMENTS(ztracer_conv) GT 0) AND (good_count GT 0)) THEN BEGIN
	isort    = SORT_CRH(ztracer_conv, /CENTER_NAN)
		
	xtracer_conv = xtracer_conv[isort]
	ytracer_conv = ytracer_conv[isort]
	ztracer_conv = ztracer_conv[isort]
ENDIF

xlog_scl = nxbin/(ALOG10(xrange[1]/xrange[0]))																	;Set bin parameters for logaritmic scale
dx       = FLOAT(xrange[1] - xrange[0])/nxbin																	;Set bin parameters for regular scale
xbin     = 0.5*dx + dx*FINDGEN(nxbin) + xrange[0]
ylog_scl = nybin/(ALOG10(yrange[1]/yrange[0]))
dy       = FLOAT(yrange[1] - yrange[0])/nybin
ybin     = 0.5*dy + yrange[0] + dy*FINDGEN(nybin)

IF KEYWORD_SET(xlog) AND KEYWORD_SET(ylog) THEN $
	bin = LONG(xlog_scl*ALOG10(xtracer_conv/xrange[0])) + nxbin*LONG(ylog_scl*ALOG10(ytracer_conv/yrange[0])) $		;Bin data for histogram
ELSE IF KEYWORD_SET(xlog) THEN $
	bin = LONG(xlog_scl*ALOG10(xtracer_conv/xrange[0])) + nxbin*LONG((ytracer_conv-yrange[0])/dy) $
ELSE IF KEYWORD_SET(ylog) THEN $
	bin = LONG((xtracer_conv-xrange[0])/dx) + nxbin*LONG(ylog_scl*ALOG10(ytracer_conv/yrange[0])) $
ELSE $
	bin = LONG((xtracer_conv-xrange[0])/dx) + nxbin*LONG((ytracer_conv-yrange[0])/dy)

hist = HISTOGRAM(bin, BINSIZE = 1, MIN = 0, MAX = (nxbin*nybin -1))													;Calculate density
hist = REFORM(hist, nxbin, nybin)

layer_total = REBIN(REFORM(TOTAL(hist,1),1,nybin),nxbin,nybin)
freq 		= 100.0 * (FLOAT(hist) / layer_total)

zbin2 = [ ]
PRINT, 'Searching for Convective Pot. Temps'
FOR binval = 0, nxbin*nybin-1 DO BEGIN
	ibin = WHERE((bin EQ binval), n)
	PRINT, binval, p95[binval], n-n_nc[binval]
	zbin  = WHERE(ztracer_conv[ibin] GT p95[binval], zcount)
	IF (FINITE(p95[binval],/NAN)) THEN zcount = n
	zbin2 = [zbin2, zcount]
ENDFOR

hist = REFORM(zbin2, nxbin, nybin)
layer_total = REBIN(REFORM(TOTAL(hist,1),1,nybin),nxbin,nybin)
freq 		= 100.0 * (FLOAT(hist) / layer_total)

IF KEYWORD_SET(binned) THEN BEGIN
	pmax  = 100.0*(LONG(MEAN(hist) + 2*STDDEV(hist))/100 + 1)
	table = GRAYSCALE_24(40, 0.85, 0.0, PS = ps)																		;Color table for plotting 
    col   = COLOR_LOOKUP_24(freq, table, MIN_VALUE = 0.0, MAX_VALUE = 15.0, MISSING = table[-1])

	none  = WHERE((hist EQ 0), none_count)
	IF (none_count GT 0) THEN col[none] = COLOR_24('white')														;Set counts of zero to white
ENDIF ELSE IF (N_ELEMENTS(ztracer_conv) GT 0) THEN BEGIN
	IF (zrange[0] LT 0) THEN table = BLUE_GRAY_RED_24(40,19,0.25) $
							  ELSE table = [REVERSE(WHITE_BLUE_24(39,0.1)),COLOR_24('red')]
	
	IF KEYWORD_SET(creverse) THEN table = REVERSE(table)
	
	IF (N_ELEMENTS(twocolor) EQ 2) THEN col = twocolor[(ztracer_conv GT zrange[0])] $							;If 2-element color table, set color
	ELSE BEGIN
		col = COLOR_LOOKUP_24(ztracer_conv, table, MIN_VALUE = zrange[0], MAX_VALUE = zrange[1])
	
		below = WHERE(ztracer_conv LT zrange[0], nbelow)
		above = WHERE(ztracer_conv GT zrange[1], nabove)
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

title = 'Conv.(updraft)'
IF ~empty_plot THEN BEGIN
	USERSYM_CIRCLE, /FILL																								;Load circle user plot symbol
	IF KEYWORD_SET(binned) THEN BEGIN	
		IF KEYWORD_SET(xlog) AND KEYWORD_SET(ylog) THEN $
			PLOT, xbin, ybin, /NODATA, $																				;Set up plot window
				TITLE    = title, $
				XRANGE   = [0, nxbin], $
				XSTYLE   = 1, $
				XTICKNAM = REPLICATE(' ', 20), $
				XTICKLEN = 0.0001, $
				YRANGE   = [0, nybin], $
				YSTYLE   = 1, $
				YMARGIN  = [10,2], $
				YTICKNAM = REPLICATE(' ', 20) $
		ELSE IF KEYWORD_SET(xlog) THEN $
			PLOT, xbin, ybin, /NODATA, $																				;Set up plot window
				TITLE    = title, $
				XRANGE   = [0, nxbin], $
				XSTYLE   = 1, $
				XTICKNAM = REPLICATE(' ', 20), $
				XTICKLEN = 0.0001, $
				YRANGE   = yrange, $
				YSTYLE   = 1, $
				YMARGIN  = [10,2], $
				YTICKNAM = REPLICATE(' ', 20) $
		ELSE IF KEYWORD_SET(ylog) THEN $
			PLOT, xbin, ybin, /NODATA, $																				;Set up plot window
				TITLE    = title, $
				XRANGE   = xrange, $
				XSTYLE   = 1, $
				XTICKNAM = REPLICATE(' ', 20), $
				XTICKLEN = 0.0001, $
				YRANGE   = [0, nybin], $
				YSTYLE   = 1, $
				YMARGIN  = [10,2], $
				YTICKNAM = REPLICATE(' ', 20) $
		ELSE $
			PLOT, xbin, ybin, /NODATA, $																				;Set up plot window
				TITLE    = title, $
				XRANGE   = xrange, $
				XSTYLE   = 1, $
				XTICKNAM = REPLICATE(' ', 20), $
				XTICKLEN = 0.0001, $
				YRANGE   = yrange, $
				YSTYLE   = 1, $
				YMARGIN  = [10,2], $
				YTICKNAM = REPLICATE(' ', 20)

		FOR j = 0, nybin -1 DO BEGIN
			FOR i = 0, nxbin -1 DO BEGIN
				IF KEYWORD_SET(xlog) AND KEYWORD_SET(ylog) THEN $
					POLYFILL, [i    , i + 1, $																				;Draw polygons
								  i + 1, i    , $
								  i          ], $
								 [j    , j + 1, $
								  j + 1, j    , $
								  j          ], $
								 COLOR = col[i,j], /DATA $
				ELSE IF KEYWORD_SET(xlog) THEN $
					POLYFILL, [i    , i + 1, $	
								  i + 1, i    , $
								  i          ], $
								 [yrange[0] + j*dy,               yrange[0] + j*dy, $
								  yrange[0] + (j+1)*dy,           yrange[0] + (j+1)*dy, $
								  yrange[0] + j*dy], $
								 COLOR = col[i,j], /DATA $
				ELSE IF KEYWORD_SET(ylog) THEN $
					POLYFILL, [xrange[0] + i*dx,               xrange[0] + i*dx, $
								  xrange[0] + (i+1)*dx,           xrange[0] + (i+1)*dx, $
								  xrange[0] + i*dx], $
								 [j    , j + 1, $
								  j + 1, j    , $
								  j          ], $
								 COLOR = col[i,j], /DATA $
				ELSE $
					POLYFILL, [xrange[0] + i*dx,     xrange[0] + (i+1)*dx, $
								  xrange[0] + (i+1)*dx, xrange[0] + i*dx, $
								  xrange[0] + i*dx], $
								 [yrange[0] + j*dy,     yrange[0] + j*dy, $
								  yrange[0] + (j+1)*dy, yrange[0] + (j+1)*dy, $
								  yrange[0] + j*dy], $
								 COLOR = col[i,j], /DATA
			ENDFOR
		ENDFOR
	
		AXIS, YAXIS = 0, /SAVE, $																							;Redraw axes that are covered by hist
			YRANGE = yrange, $
			YLOG   = ylog, $
			YSTYLE = 1, $
			YTITLE = ytitle, $
			_EXTRA = _extra
		
		AXIS, XAXIS = 0, /SAVE, $
			XRANGE = xrange, $
			XLOG   = xlog, $
			XSTYLE = 1, $
			XTITLE = xtitle, $
			_EXTRA = _extra
		
		AXIS, YAXIS = 1, $																									;Redraw axes that are covered by hist
			YRANGE = yrange, $
			YLOG   = ylog, $
			YTICKN = REPLICATE(' ', 20), $
			YSTYLE = 1, $
			_EXTRA = _extra
		
		AXIS, XAXIS = 1, $
			XRANGE = xrange, $
			XLOG   = xlog, $
			XTICKN = REPLICATE(' ', 20), $
			XSTYLE = 1, $
			_EXTRA = _extra

		xy   = CONVERT_COORD(xrange, yrange, /DATA, /TO_NORMAL)
		dxax = xy[0,1] - xy[0,0]
		dyax = xy[1,1] - xy[1,0]
		
		x1   = xy[0,0] + 0.1*dxax
		x2   = xy[0,1] - 0.1*dxax
		y1   = xy[1,0] - 0.26*dyax
		y2   = xy[1,0] - 0.24*dyax
		
		COLOR_BAR_24, table, $
			RANGE = [0.0, 15.0], $
			TITLE = 'Frequency (%)', $
			TICKS = 1, $
			POSIT = [x1,y1,x2,y2]


		!P.POSITION = 0
	ENDIF ELSE BEGIN	;if not binned
		PLOT, xtracer_conv, ytracer_conv, /NODATA, $
			TITLE   = title, $
			XRANGE  = xrange, $
			XLOG    = xlog, $
			XSTYLE  = 1, $
			XTITLE  = xtitle, $
			YRANGE  = yrange, $
			YLOG    = ylog, $
			YMARGIN = [10,2], $
			YSTYLE  = ystyle, $
			YTITLE  = ytitle
		
		PLOTS, xtracer_conv, ytracer_conv, PSYM = 8, COLOR = col, NOCLIP = 0, _EXTRA = _extra
		
		xy   = CONVERT_COORD(xrange, yrange, /DATA, /TO_NORMAL)
		dxax = xy[0,1] - xy[0,0]
		dyax = xy[1,1] - xy[1,0]

		x1   = xy[0,0] + 0.1*dxax
		x2   = xy[0,1] - 0.1*dxax
		y1   = xy[1,0] - 0.26*dyax
		y2   = xy[1,0] - 0.24*dyax

		IF ((N_ELEMENTS(ztracer_conv) GT 0) AND (N_ELEMENTS(twocolor) NE 2)) THEN $
			COLOR_BAR_24_KPB, table, UNDER = table[0], OVER = table[-1], $
				RANGE = zrange, $
				TITLE = ztitle, $
				MINOR = 1, $
				POSIT = [x1,y1,x2,y2]
		
		!P.POSITION = 0
	ENDELSE
ENDIF ELSE BEGIN ;if empty plot
	PLOT, xtracer_conv, ytracer_conv, /NODATA, $
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

IF ~KEYWORD_SET(nowindow) THEN BEGIN
	IF KEYWORD_SET(eps) THEN BEGIN
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS, /RESET																					;Reset color table to linear ramp
		PS_OFF																												;Turn PS off
	ENDIF ELSE IF KEYWORD_SET(png) THEN $
		WRITE_PNG, pngfile, TVRD(TRUE=1)																				;Write PNG image
ENDIF
END
