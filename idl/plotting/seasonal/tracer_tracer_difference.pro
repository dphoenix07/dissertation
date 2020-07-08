PRO TRACER_TRACER_DIFFERENCE, xtracer, ytracer, col,$
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
    XBIN 	 = xbin, $
    YBIN	 = ybin, $
	DX		 = dx, $
	DY		 = dy, $
	BINNED   = binned, $
	CHARSIZE = charsize, $
	TABLE	 = table, $
	NOWINDOW = nowindow, $
	CREVERSE = creverse, $
	_EXTRA   = _extra, $
	EPS      = eps, $
	PNG      = png

;+
; Name:
;		PLOT_TRACER_TRACER
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

xtracer2 = xtracer																										;Copy arrays for manipulation
ytracer2 = ytracer

;IF (N_ELEMENTS(ztracer) GT 0) THEN $
;	ztracer2 = ztracer
;
;good = WHERE(((xtracer2 GE xrange[0]) AND $																			;Find data to use
;				  (xtracer2 LE xrange[1]) AND $
;				  (ytracer2 GE yrange[0]) AND $
;				  (ytracer2 LE yrange[1])), good_count)
;
empty_plot = 0
;IF (good_count GT 0) THEN BEGIN
;	xtracer2 = xtracer2[good]
;	ytracer2 = ytracer2[good]
;	
;	IF (N_ELEMENTS(ztracer2) GT 0) THEN $
;		ztracer2 = ztracer2[good]
;ENDIF ELSE empty_plot = 1
;
;IF ((N_ELEMENTS(ztracer2) GT 0) AND (good_count GT 0)) THEN BEGIN
;	isort    = SORT_CRH(ztracer2, /CENTER_NAN)
;		
;	xtracer2 = xtracer2[isort]
;	ytracer2 = ytracer2[isort]
;	ztracer2 = ztracer2[isort]
;ENDIF

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
		    	CHARSIZE = charsize, $
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
		    	CHARSIZE = charsize, $
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
		    	CHARSIZE = charsize, $
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
		   	    CHARSIZE = charsize, $
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
	
		AXIS, YAXIS  = 0, /SAVE, $																							;Redraw axes that are covered by hist
			YRANGE   = yrange, $
			YLOG     = ylog, $
			YSTYLE   = 1, $
			CHARSIZE = charsize, $
			YTITLE   = ytitle, $
			_EXTRA   = _extra
		
		AXIS, XAXIS  = 0, /SAVE, $
			XRANGE   = xrange, $
			XLOG     = xlog, $
			XSTYLE   = 1, $
			CHARSIZE = charsize, $
			XTITLE   = xtitle, $
			_EXTRA   = _extra
		
		AXIS, YAXIS  = 1, $																									;Redraw axes that are covered by hist
			YRANGE   = yrange, $
			YLOG     = ylog, $
			YTICKN   = REPLICATE(' ', 20), $
			CHARSIZE = charsize, $
			YSTYLE   = 1, $
			_EXTRA   = _extra
		
		AXIS, XAXIS  = 1, $
			XRANGE   = xrange, $
			XLOG     = xlog, $
			XTICKN   = REPLICATE(' ', 20), $
			CHARSIZE = charsize, $
			XSTYLE   = 1, $
			_EXTRA   = _extra

		xy   = CONVERT_COORD(xrange, yrange, /DATA, /TO_NORMAL)
		dxax = xy[0,1] - xy[0,0]
		dyax = xy[1,1] - xy[1,0]
		
		x1   = xy[0,0] + 0.1*dxax
		x2   = xy[0,1] - 0.1*dxax
		y1   = xy[1,0] - 0.26*dyax
		y2   = xy[1,0] - 0.24*dyax
		
;		COLOR_BAR_24_KPB, table, OVER = table[-1], $
;			RANGE = [0, pmax], $
;			TITLE = 'Count', $
;			TICKS = 1, $
;			POSIT = [x1,y1,x2,y2]

		IF (title EQ 'Frequency Difference') THEN BEGIN
			COLOR_BAR_24, table, $
				RANGE = [-15.0, 15.0], $
				TITLE = 'Frequency (%)', $
				TICKS = 1, $
				POSIT = [x1,y1,x2,y2]
		ENDIF ELSE BEGIN
			COLOR_BAR_24, table, $
				RANGE = [0.0, 15.0], $
				TITLE = 'Frequency (%)', $
				TICKS = 1, $
				POSIT = [x1,y1,x2,y2]
		ENDELSE

		!P.POSITION = 0
	ENDIF ELSE BEGIN
		PLOT, xtracer2, ytracer2, /NODATA, $
			TITLE    = title, $
			XRANGE   = xrange, $
			XLOG     = xlog, $
			XSTYLE   = 1, $
			XTITLE   = xtitle, $
			YRANGE   = yrange, $
			YLOG     = ylog, $
			YMARGIN  = [10,2], $
			YSTYLE   = ystyle, $
			YTITLE   = ytitle, $
		    CHARSIZE = charsize
		
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
	ENDELSE
ENDIF ELSE BEGIN
	PLOT, xtracer2, ytracer2, /NODATA, $
		TITLE    = title, $
		XRANGE   = xrange, $
		XLOG     = xlog, $
		XSTYLE   = 1, $
		XTITLE   = xtitle, $
		YRANGE   = yrange, $
		YLOG     = ylog, $
		YMARGIN  = [10,2], $
		YSTYLE   = ystyle, $
		YTITLE   = ytitle, $
		CHARSIZE = charsize
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
