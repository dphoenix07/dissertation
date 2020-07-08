PRO WRF_OBS_TRACER_RALT, tracer, ralt, refl, cloud, tracer_gv, tracer_dc8, ralt_obs, gv_values, dc8_values, $
	TITLE      = title, $
	XTITLE     = xtitle, $
	XRANGE     = xrange, $
	XLOG       = xlog, $
	YTITLE     = ytitle, $
	YRANGE     = yrange, $
	NXBIN      = nxbin, $
	NYBIN      = nybin, $
	BINNED     = binned, $
	FILTERING  = filtering, $
	IN_CLOUD   = in_cloud, $
	UPPER_TROP = upper_trop, $
	NOWINDOW   = nowindow, $
	_EXTRA     = _extra, $
	EPS        = eps, $
	PNG        = png

;+
; Name:
;		WRF_OBS_TRACER_RALT
; Purpose:
;		This is a procedure to plot trace gas concentrations from
;		a WRF model simulation at tropopause relative altitudes
;		 
; Calling sequence:
;		WRF_OBS_TRACER_RALT, tracer, ralt, refl, cloud
; Input:
;		tracer : Matrix of trace gas concentrations and geopotential heights.
;		ralt   : Matrix of tropopause relative altitudes.
;		refl   : Matrix of reflectivity values.
;		cloud  : Matrix of cloud number concentrations.
; Output:
;		Trace gas measurements at simulated altitudes. 
; Keywords:
;		XTITLE    : Desired trace gas axis (x-axis) title. 
;		XRANGE    : Desired trace gas axis (x-axis) range.
;		XLOG      : If set, x-axis is log scale. 
;		YTITLE    : Desired relative altitude axis (y-axis) title. 
;		YRANGE    : Desired relative altitude axis (y-axis) range.
;		FILTERING : Filter filter out values where REFL > 30 dBZ.
;		IN_CLOUD  : Set to plot values where cloud is simulated (> 1 L-1)
;		HIST   	  : Set to variable name to return two-dimensional histogram. 
;		NOPLOT    : If set, then do not plot histogram. 
;		PS        : If set, output to PostScript
;		PNG       : If set, write PNG image
; Author and history:
;		Cameron R. Homeyer  2011-03-01.
;		Daniel B. Phoenix	2016-01-16.		Modified plot_tracer_ralt for WRF output
;-

COMPILE_OPT IDL2																				;Set compile options

IF (N_ELEMENTS(xrange	) EQ 0) THEN xrange    = [  0, 800]
IF (N_ELEMENTS(xtitle	) EQ 0) THEN xtitle    = 'Ozone (ppbv)'
IF (N_ELEMENTS(yrange	) EQ 0) THEN yrange    = [-15,   5]
IF (N_ELEMENTS(ytitle	) EQ 0) THEN ytitle    = 'Relative Altitude'
IF (N_ELEMENTS(nxbin 	) EQ 0) THEN nxbin     = 50
IF (N_ELEMENTS(nybin 	) EQ 0) THEN nybin     = 50
IF (N_ELEMENTS(filtering) EQ 0) THEN filtering = 1

dy   = FLOAT(yrange[1] - yrange[0])/nybin														;Compute y bin spacing
ybin = 0.5*dy + yrange[0] + dy*FINDGEN(nybin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;											;Aircraft

tracer_gv  = tracer_gv [gv_values]
tracer_dc8 = tracer_dc8[dc8_values]
tracer_obs = [tracer_gv, tracer_dc8]

good = WHERE(((tracer_obs GE xrange[0]) AND $																			;Find data to use
				  (tracer_obs LE xrange[1]) AND $
				  (ralt_obs   GE yrange[0]) AND $
				  (ralt_obs   LE yrange[1])), good_count)

empty_plot = 0
IF (good_count GT 0) THEN BEGIN
	tracer_obs = tracer_obs[good]
	ralt_obs   =   ralt_obs[good]
ENDIF ELSE empty_plot = 1


IF KEYWORD_SET(xlog) THEN BEGIN
	log_scl = nxbin/(ALOG10(xrange[1]/xrange[0]))																;Set bin parameters for logaritmic scale
	xbin    = FINDGEN(nxbin) 
	dx      = 10.0^((xbin+1)/log_scl)
	bin     = LONG(log_scl*ALOG10(tracer_obs/xrange[0])) + nxbin*(LONG((ralt_obs-yrange[0])/dy))			;Bin data for histogram
ENDIF ELSE BEGIN
	dx      = FLOAT(xrange[1] - xrange[0])/nxbin																	;Set bin parameters for regular scale
	xbin    = 0.5*dx + dx*FINDGEN(nxbin) + xrange[0]
	bin_obs     = LONG((tracer_obs-xrange[0])/dx) + nxbin*(LONG((ralt_obs-yrange[0])/dy))
ENDELSE

hist_obs = HISTOGRAM(bin_obs, BINSIZE = 1, MIN = 0, MAX = (nxbin*nybin -1))									;Calculate density
hist_obs = REFORM(hist_obs, nxbin, nybin)

pmax_obs  = 25.0*(LONG(MEAN(hist_obs) + 2*STDDEV(hist_obs))/25 + 1)
table_obs = WHITE_RED_24(40, 0.35, 1.0);, PS = ps)														;Color table for plotting 
col_obs   = COLOR_LOOKUP_24(hist_obs, table_obs, MIN_VALUE = 0.0, MAX_VALUE = pmax_obs, MISSING = table_obs[-1])

none = WHERE((hist_obs EQ 0), none_count)
IF (none_count GT 0) THEN col_obs[none] = COLOR_24('white')																	;Set counts of zero to white
										
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cloud_values = WHERE(cloud GE 0.1, cld_count, COMPLEMENT = non_cloud, $							;Find values in cloud 
					NCOMPLEMENT = ncld_count)

IF KEYWORD_SET(in_cloud) THEN BEGIN																;Sort values in cloud vs out of cloud
	IF (cld_count GT 0) THEN BEGIN
		tracer = tracer[cloud_values]
		ralt   = ralt  [cloud_values]
	ENDIF
ENDIF ELSE BEGIN
	IF (ncld_count GT 0) THEN BEGIN
		tracer = tracer[non_cloud]
		ralt   = ralt  [non_cloud]
	ENDIF
ENDELSE
	
IF KEYWORD_SET(dbl_trop) THEN BEGIN																;If secondary tropopause is present, filter out those values
	prim_trop = WHERE (z_trop LT upper_trop, trop_count)
	
	IF (trop_count GT 0) THEN BEGIN
		tracer = tracer[prim_trop]
		ralt   = ralt  [prim_trop]
	ENDIF
ENDIF	
			
IF KEYWORD_SET(filtering) THEN BEGIN
	  good = WHERE(((refl   LE 30.0) 	AND $												;Filter out refl values above 30 dBZ
				   	(tracer GE xrange[0]) 	AND $												
				   	(tracer LE xrange[1]) 	AND $
				  	(ralt   GE yrange[0])   AND $
				   	(ralt   LE yrange[1])), good_count)										
ENDIF ELSE BEGIN
	 good = WHERE(((tracer GE xrange[0]) 	AND $												;Use specified data range
				   (tracer LE xrange[1]) 	AND $												;Only used if filtering not desired
				   (ralt   GE yrange[0])   	AND $
				   (ralt   LE yrange[1])), good_count)
ENDELSE	

				  
empty_plot = 0
IF (good_count GT 0) THEN BEGIN																	;Save good data points
	tracer  = tracer[good]
	ralt 	= ralt  [good] 
ENDIF ELSE empty_plot = 1


dx      = FLOAT(xrange[1] - xrange[0])/nxbin													;Set bin parameters for regular scale
xbin    = 0.5*dx + dx*FINDGEN(nxbin) + xrange[0]
bin     = LONG((tracer-xrange[0])/dx) + nxbin*(LONG((ralt-yrange[0])/dy))

hist = HISTOGRAM(bin, BINSIZE = 1, MIN = 0, MAX = (nxbin*nybin -1))								;Calculate density
hist = REFORM(hist, nxbin, nybin)

layer_total = REBIN(REFORM(TOTAL(hist,1),1,nybin),nxbin,nybin)
freq 		= 100.0 * (FLOAT(hist) / layer_total)

pmax   = 25.0*(LONG(MEAN(hist) + 2*STDDEV(hist))/25 + 1)											;Calculate maximum count for each gas concentration w/in 2 STD DEV												
table  = GRAY_LOGSCALE_24(20, 0.75, 0.0, PS = ps)													;Color table for plotting 
col    = COLOR_LOOKUP_24(freq, table, MIN_VALUE = 0.0, MAX_VALUE = 20.0, MISSING = table[-1])

none = WHERE((hist EQ 0), none_count)
IF (none_count GT 0) THEN col[none] = COLOR_24('white')											;Set counts of zero to white

epsfile = '~/tracer_ralt.eps'																	;EPS filename
pngfile = '~/tracer_ralt.png'

IF ~KEYWORD_SET(nowindow) THEN BEGIN
	IF KEYWORD_SET(eps) THEN BEGIN	
		PS_ON, FILENAME = epsfile, PAGE_SIZE = [6.0, 8.0], MARGIN = 0.0, /INCHES				;Switch to Postscript device
		DEVICE, /ENCAPSULATED
		!P.FONT     = 0																			;Hardware fonts
		!P.CHARSIZE = 1.0	
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS																	;Load basic color definitions
	ENDIF ELSE BEGIN
		SET_PLOT, 'X'
		WINDOW, XSIZE = 600, YSIZE = 800														;Open graphics window
		!P.COLOR      = COLOR_24('black')														;Foreground color
		!P.BACKGROUND = COLOR_24('white')														;Background color
		!P.CHARSIZE   = 2.0		
		!P.FONT       = -1																		;Use Hershey fonts
	ENDELSE
ENDIF

IF ~empty_plot THEN BEGIN
	USERSYM_CIRCLE, /FILL																		;Load circle user plot symbol
	IF KEYWORD_SET(binned) THEN BEGIN	
		IF KEYWORD_SET(xlog) THEN $
			PLOT, xbin, ybin, /NODATA, $														;Set up plot window for log scale
				TITLE    = title, $
				XRANGE   = [0, nxbin], $
				XSTYLE   = 1, $
				XTICKNAM = REPLICATE(' ', 20), $
				XTICKLEN = 0.0001, $
				YRANGE   = yrange, $
				YSTYLE   = 1, $
				YMARGIN  = [8,2], $
				YTICKNAM = REPLICATE(' ', 20) $
		ELSE $
			PLOT, xbin, ybin, /NODATA, $														;Set up plot window for normal scale
				TITLE    = title, $
				XRANGE   = xrange, $
				XSTYLE   = 1, $
				XTICKNAM = REPLICATE(' ', 20), $
				XTICKLEN = 0.0001, $
				YRANGE   = yrange, $
				YSTYLE   = 1, $
				YMARGIN  = [8,2], $
				YTICKNAM = REPLICATE(' ', 20)

		FOR j = 0, nybin -1 DO BEGIN
			FOR i = 0, nxbin -1 DO BEGIN
				IF KEYWORD_SET(xlog) THEN $
					POLYFILL, [i    , i + 1, $													;Draw polygons (for log scale)
								  i + 1, i    , $
								  i          ], $
								 [yrange[0] + j*dy,               yrange[0] + j*dy, $
								  yrange[0] + (j+1)*dy,           yrange[0] + (j+1)*dy, $
								  yrange[0] + j*dy], $
								 COLOR = col[i,j], /DATA $
				ELSE $
					POLYFILL, [xrange[0] + i*dx,     xrange[0] + (i+1)*dx, $					;Draw polygons (for normal scale)
								  xrange[0] + (i+1)*dx, xrange[0] + i*dx, $
								  xrange[0] + i*dx], $
								 [yrange[0] + j*dy,     yrange[0] + j*dy, $
								  yrange[0] + (j+1)*dy, yrange[0] + (j+1)*dy, $
								  yrange[0] + j*dy], $
								 COLOR = col[i,j], /DATA
								 
			ENDFOR
		ENDFOR
		
		PLOTS, tracer_obs, ralt_obs, PSYM = 3, COLOR = 230,  _EXTRA = _extra
		
		AXIS, YAXIS = 0, $																		;Redraw axes that are covered by hist
			YRANGE = yrange, $
			YSTYLE = 1, $
			YTITLE = ytitle, $
			_EXTRA = _extra
	
		AXIS, XAXIS = 0, /SAVE, $
			XRANGE = xrange, $
			XLOG   = xlog, $
			XSTYLE = 1, $
			XTITLE = xtitle, $
			_EXTRA = _extra
		
		AXIS, YAXIS = 1, $																		;Redraw axes that are covered by hist
			YRANGE = yrange, $
			YTICKN = REPLICATE(' ', 20), $
			YSTYLE = 1, $
			_EXTRA = _extra
	
		AXIS, XAXIS = 1, $
			XRANGE = xrange, $
			XLOG   = xlog, $
			XTICKN = REPLICATE(' ', 20), $
			XSTYLE = 1, $
			_EXTRA = _extra

		xy   = CONVERT_COORD(xrange, yrange, /DATA, /TO_NORMAL)									;Normalize x and y coordinates to [0,1]
		dxax = xy[0,1] - xy[0,0]																;Compute difference between xy
		dyax = xy[1,1] - xy[1,0]
					
		x1   = xy[0,0] + 0.1*dxax																;Compute coordinates to center color bar
		x2   = xy[0,1] - 0.1*dxax
		y1   = xy[1,0] - 0.26*dyax
		y2   = xy[1,0] - 0.24*dyax
		
		OPLOT, xrange, [0.0, 0.0]																;Plot RALT 0 reference line	

		COLOR_BAR_24, table, $
			RANGE = [0, 20.0], $
			TITLE = 'Frequency (%)', $
			TICKS = 1, $
			POSIT = [x1,y1,x2,y2]

;		PLOT, tracer_obs, ralt_obs, /NODATA, $															;Overplot obs
;			TITLE   = title, $
;			XRANGE  = xrange, $
;			XLOG    = xlog, $
;			XSTYLE  = 1, $
;			XTITLE  = xtitle, $
;			YRANGE  = yrange, $
;			YSTYLE  = ystyle, $
;			YTITLE  = ytitle
;		
;		OPLOT, tracer_obs, ralt_obs, PSYM = 8, _EXTRA = _extra
;		OPLOT, xrange, [0.0, 0.0]


	ENDIF ELSE BEGIN
		PLOT, tracer, ralt, /NODATA, $															;If BINNED not set, plots scatterplot
			TITLE   = title, $
			XRANGE  = xrange, $
			XLOG    = xlog, $
			XSTYLE  = 1, $
			XTITLE  = xtitle, $
			YRANGE  = yrange, $
			YSTYLE  = ystyle, $
			YTITLE  = ytitle
		

		PLOTS, tracer, ralt, PSYM = 8, _EXTRA = _extra
		PLOTS, tracer_obs, ralt_obs, PSYM = 8, COLOR = 230, _EXTRA = _extra
		OPLOT, xrange, [0.0, 0.0]
	ENDELSE
ENDIF ELSE BEGIN
	PLOT, tracer, ralt, /NODATA, $																;Plots empty plot
		TITLE   = title, $
		XRANGE  = xrange, $
		XLOG    = xlog, $
		XSTYLE  = 1, $
		XTITLE  = xtitle, $
		YRANGE  = yrange, $
		YSTYLE  = ystyle, $
		YTITLE  = ytitle

	OPLOT, xrange, [0.0, 0.0]
ENDELSE

IF ~KEYWORD_SET(nowindow) THEN BEGIN
	IF KEYWORD_SET(eps) THEN BEGIN
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS, /RESET															;Reset color table to linear ramp
		PS_OFF																					;Turn PS off
	ENDIF ELSE IF KEYWORD_SET(png) THEN $
		WRITE_PNG, pngfile, TVRD(TRUE=1)														;Write PNG image
ENDIF

END