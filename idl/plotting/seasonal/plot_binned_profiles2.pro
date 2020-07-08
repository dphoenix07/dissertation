PRO PLOT_BINNED_PROFILES2, event1, event2, $
	DOMAIN       = domain, $
	PERCENTILE   = percentile, $
	PERCENT_DIFF = percent_diff, $
	PNG	         = png, $
	EPS   	     = eps


;+
; Name:
;		PLOT_BINNED_PROFILES
; Purpose:
;		Show changes in O3, CO, and H2O from convection in environments with different
;		tropopause heights 
; Calling sequence:
;		OVERSHOOT_ZTROP_BIN, run, start_date, end_date
; Example: OVERSHOOT_ZTROP_BIN,'20110518','20110520T0000Z','20110520T0300Z'
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;		end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
; Output:
;		Mean O3, CO, and H2O concentrations binned in tropopause height vs tropopause
;		relative height altitude for convective and non-convective air
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
; Author and history:
;		Daniel B. Phoenix	    2019-04-30. 
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain
IF (N_ELEMENTS(in_cloud  ) EQ 0) THEN in_cloud   = 1
IF (N_ELEMENTS(threshold ) EQ 0) THEN threshold  = 1000.0
IF (N_ELEMENTS(xrange	 ) EQ 0) THEN xrange     = [  0, 800]
IF (N_ELEMENTS(xtitle	 ) EQ 0) THEN xtitle     = 'Ozone (ppbv)'
IF (N_ELEMENTS(yrange	 ) EQ 0) THEN yrange     = [-5,   5]
IF (N_ELEMENTS(ytitle	 ) EQ 0) THEN ytitle     = 'Relative Altitude'
IF (N_ELEMENTS(nxbin 	 ) EQ 0) THEN nxbin      = 50
IF (N_ELEMENTS(nybin 	 ) EQ 0) THEN nybin      = 50
IF (N_ELEMENTS(var2		 ) EQ 0) THEN var2		 = 0

outdir  = !WRF_DIRECTORY + 'seasonal/chemistry_profiles/'
epsfile = outdir + 'binned_profiles_combined.eps'	
pdffile = outdir + 'binned_profiles_combined.pdf'	
pngfile = outdir + 'binned_profiles_combined.png'	

FILE_MKDIR, outdir

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
		WINDOW, XSIZE = 1200, YSIZE = 900														;Open graphics window
		!P.COLOR      = COLOR_24('black')														;Foreground color
		!P.BACKGROUND = COLOR_24('white')														;Background color
		!P.CHARSIZE   = 2.5		
		!P.FONT       = -1																		;Use Hershey fonts
	ENDELSE
ENDIF

!P.MULTI=[0,3,3]

events 		= [event1, event2]

h2o_c1      = FLTARR(8,16)
o3_c1       = FLTARR(8,16)
co_c1       = FLTARR(8,16)
nval_c1     = FLTARR(8,16)
h2o_nc1     = FLTARR(8,16)
o3_nc1      = FLTARR(8,16)
co_nc1      = FLTARR(8,16)
nval_nc1    = FLTARR(8,16)
			
ralt_arr_c  = [ ] 
trop_3d_c   = [ ]
ralt_arr_nc = [ ] 
trop_3d_nc  = [ ]

FOREACH event, events DO BEGIN
	CASE event OF
		'20110518' : BEGIN
			scheme = ['seasonal_final/bigger_domain']
			start_date = '20110519T0000Z'
			end_date   = '20110527T1200Z'
			END
		'20130805' : BEGIN
			scheme = ['nest_final']
			start_date = '20130806T0000Z'
			end_date   = '20130815T1200Z'
			END
	ENDCASE

	IF domain EQ 1 THEN $
	date_arr = MK_DATE_ARR_D01(event, scheme, start_date, end_date, /DATE)	

	IF domain EQ 2 THEN $
	date_arr = MK_DATE_ARR_D02(event, scheme, start_date, end_date, /DATE)	

	FOREACH date, date_arr DO BEGIN
		PRINT, "Processing date: ", date

		h2o_c   = (WRF_READ_VAR('H2O_Conv'   , date, event, scheme, DOMAIN = domain, INDICES = region, BIN_PRO=1)).values
		o3_c    = (WRF_READ_VAR('O3_Conv'    , date, event, scheme, DOMAIN = domain, INDICES = region, BIN_PRO=1)).values
		co_c    = (WRF_READ_VAR('CO_Conv'    , date, event, scheme, DOMAIN = domain, INDICES = region, BIN_PRO=1)).values
		h2o_nc  = (WRF_READ_VAR('H2O_NonConv', date, event, scheme, DOMAIN = domain, INDICES = region, BIN_PRO=1)).values
		o3_nc   = (WRF_READ_VAR('O3_NonConv' , date, event, scheme, DOMAIN = domain, INDICES = region, BIN_PRO=1)).values
		co_nc   = (WRF_READ_VAR('CO_NonConv' , date, event, scheme, DOMAIN = domain, INDICES = region, BIN_PRO=1)).values
		nval_c  = (WRF_READ_VAR('NVAL_Conv'  , date, event, scheme, DOMAIN = domain, INDICES = region, BIN_PRO=1)).values 
		nval_nc = (WRF_READ_VAR('NVAL_NConv' , date, event, scheme, DOMAIN = domain, INDICES = region, BIN_PRO=1)).values 
	
		h2o_c1  = [[[h2o_c1  ]], [[h2o_c  ]]] 
		o3_c1   = [[[o3_c1   ]], [[o3_c   ]]]
		co_c1   = [[[co_c1   ]], [[co_c   ]]]
		nval_c1 = [[[nval_c1 ]], [[nval_c ]]]

		h2o_nc1  = [[[h2o_nc1 ]], [[h2o_nc ]]]
		o3_nc1   = [[[o3_nc1  ]], [[o3_nc  ]]]
		co_nc1   = [[[co_nc1  ]], [[co_nc  ]]]
		nval_nc1 = [[[nval_nc1]], [[nval_nc]]]
		
	ENDFOREACH ;time
ENDFOREACH ;event

conv_counts  = TOTAL(nval_c1 , 3, /NAN)
nconv_counts = TOTAL(nval_nc1, 3, /NAN)

conv_total  = TOTAL(nval_c1 , /NAN)
nconv_total = TOTAL(nval_nc1, /NAN)

conv_frac  = conv_counts / conv_total*100.0
nconv_frac = nconv_counts/nconv_total*100.0

inan_c  = WHERE(conv_frac  LT 0.001)
inan_nc = WHERE(nconv_frac LT 0.001)

h2o_c1  = MEAN(h2o_c1, DIM=3, /NAN)
o3_c1   = MEAN(o3_c1 , DIM=3, /NAN)
co_c1   = MEAN(co_c1 , DIM=3, /NAN)

h2o_nc1 = MEAN(h2o_nc1, DIM=3, /NAN)
o3_nc1  = MEAN(o3_nc1 , DIM=3, /NAN)
co_nc1  = MEAN(co_nc1 , DIM=3, /NAN)

h2o_c1 [inan_c ] = 0.0
o3_c1  [inan_c ] = 0.0
co_c1  [inan_c ] = 0.0

h2o_nc1[inan_nc] = 0.0
o3_nc1 [inan_nc] = 0.0
co_nc1 [inan_nc] = 0.0

yrange = [-4,4]
nybin  = 16
dy     = FLOAT(yrange[1] - yrange[0])/nybin															;Compute y bin spacing
ybin   = 0.5*dy + yrange[0] + dy*FINDGEN(nybin)

xrange = [10,18]
nxbin  = 8
dx     = FLOAT(xrange[1] - xrange[0])/nxbin														;Set bin parameters for regular scale
xbin   = 0.5*dx + dx*FINDGEN(nxbin) + xrange[0]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
;Plotting - save for now until plot routine is written		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
var_arr = ['H2O', 'O3', 'CO']
FOREACH var, var_arr DO BEGIN
	FOR p = 0, 2 DO BEGIN
		IF p EQ 0 THEN BEGIN
			IF var EQ 'H2O'  THEN BEGIN
				tracer = h2o_c1
				position1 = [3,2,1]
				bar_title  = 'H2O (ppmv)'
				tr_min = 1.0
				tr_max = 200.0
				position = [0.25,0.69,0.47,0.71]
			ENDIF
			IF var EQ 'O3'   THEN BEGIN
				tracer = o3_c1
				position1 = [3,2,2]
				bar_title   = 'O3 (ppbv)'
				tr_min = 0.0
				tr_max = 600.0
				position = [0.25,0.36,0.47,0.38]
			ENDIF
			IF var EQ 'CO'   THEN BEGIN
				tracer = co_c1
				position1 = [3,2,3]
				bar_title   = 'CO (ppbv)'
				tr_min = 0.0
				tr_max = 120.0
				position = [0.25,0.04,0.47,0.06]
			ENDIF
		ENDIF 
		IF p EQ 1 THEN BEGIN
			IF var EQ 'H2O'  THEN BEGIN
				tracer = h2o_nc1
				position1 = [3,2,4]
				bar_title   = 'H2O (ppmv)'
				tr_min = 1.0
				tr_max = 200.0
			ENDIF
			IF var EQ 'O3'   THEN BEGIN
				tracer = o3_nc1
				position1 = [3,2,5]
				bar_title   = 'O3 (ppbv)'
				IF KEYWORD_SET(percent_diff) THEN xtitle   = 'O3 (%)'
				tr_min = 0.0
				tr_max = 600.0
			ENDIF
			IF var EQ 'CO'   THEN BEGIN
				tracer = co_nc1
				position1 = [3,2,6]
				bar_title   = 'CO (ppbv)'
				IF KEYWORD_SET(percent_diff) THEN xtitle   = 'CO (%)'
				tr_min = 30.0
				tr_max = 120.0
			ENDIF
		ENDIF
		IF p EQ 2 THEN BEGIN
			IF var EQ 'H2O'  THEN BEGIN
				;bar_title  = 'H2O (ppmv)'
				bar_title   = 'H2O % Difference'
				tr_min_diff = -300.0
				tr_max_diff =  300.0
				position_diff = [0.75,0.69,0.95,0.71]
			ENDIF
			IF var EQ 'O3'   THEN BEGIN
				;bar_title   = 'O3 (ppbv)'
				bar_title   = 'O3 % Difference'
				tr_min_diff = -100.0
				tr_max_diff =  100.0
				position_diff = [0.75,0.36,0.95,0.38]
			ENDIF
			IF var EQ 'CO'   THEN BEGIN
				;bar_title   = 'CO (ppbv)'
				bar_title   = 'CO % Difference'
				tr_min_diff = -50.0
				tr_max_diff =  50.0
				position_diff = [0.75,0.04,0.95,0.06]
			ENDIF
		ENDIF 
	
		IF (p EQ 0) THEN tracer_mean_c = tracer
		IF (p EQ 1) THEN tracer_mean_nc = tracer

		IF (p NE 2) THEN BEGIN
			pmax   = 25.0*(LONG(MEAN(tracer) + 2*STDDEV(tracer))/25 + 1)											;Calculate maximum count for each gas concentration w/in 2 STD DEV												
			table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])
			col    = COLOR_LOOKUP_24(tracer, table, MIN_VALUE = tr_min, MAX_VALUE = tr_max, MISSING = table[-1])

			none   = WHERE(tracer EQ 0, none_count)
			IF (none_count GT 0) THEN col[none] = COLOR_24('white')												;Set counts of zero to white
		ENDIF ELSE BEGIN
			tracer_mean_diff = ((tracer_mean_c - tracer_mean_nc)/tracer_mean_nc)*100.0

;			PRINT, tracer_mean_diff
			pmax   = 25.0*(LONG(MEAN(tracer_mean_diff) + 2*STDDEV(tracer_mean_diff))/25 + 1)											;Calculate maximum count for each gas concentration w/in 2 STD DEV												
			table  = BLUE_RED_24(1000, 0.1, 1.0, PS = ps)
			col    = COLOR_LOOKUP_24(tracer_mean_diff, table, MIN_VALUE = tr_min_diff, MAX_VALUE = tr_max_diff, MISSING = COLOR_24('white')	)
;			PRINT, tr_min_diff
;			PRINT, tr_max_diff
			none   = WHERE(((tracer_mean_c EQ 0) OR (tracer_mean_nc EQ 0)), none_count)
			IF (none_count GT 0) THEN col[none] = COLOR_24('white')												;Set counts of zero to white

		ENDELSE

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

				POLYFILL, [xrange[0] + i*dx,     xrange[0] + (i+1)*dx, $					;Draw polygons (for normal scale)
							  xrange[0] + (i+1)*dx, xrange[0] + i*dx, $
							  xrange[0] + i*dx], $
							 [yrange[0] + j*dy,     yrange[0] + j*dy, $
							  yrange[0] + (j+1)*dy, yrange[0] + (j+1)*dy, $
							  yrange[0] + j*dy], $
							 COLOR = col[i,j], /DATA
			ENDFOR
		ENDFOR

		AXIS, YAXIS = 0, $																		;Redraw axes that are covered by hist
			YRANGE = yrange, $
			YSTYLE = 1, $
			YTITLE = ytitle, $
			_EXTRA = _extra

		AXIS, XAXIS = 0, /SAVE, $
			XRANGE = xrange, $
			XLOG   = xlog, $
			XSTYLE = 1, $
			XTITLE = 'Tropopause Height (km)', $
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

		IF (p NE 2) THEN BEGIN
			COLOR_BAR_24, table, $
				RANGE = [tr_min, tr_max], $
				TITLE = bar_title, $
				TICKS = 1, $
				POSIT = position
		ENDIF

		IF (p EQ 2) THEN BEGIN
			COLOR_BAR_24, table, $
				RANGE = [tr_min_diff, tr_max_diff], $
				TITLE = bar_title, $
				TICKS = 1, $
				POSIT = position_diff
		ENDIF
	ENDFOR
ENDFOREACH ;var
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
;End plotting routine	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		

!P.MULTI = 0
IF KEYWORD_SET(eps) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																;Reset color table to linear ramp
	PS_OFF																						;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)															;Write PNG file

END