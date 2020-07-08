PRO O3_TIMESERIES_PLOTS, event, scheme, start_date, end_date, $
	DOMAIN   = domain, $
	REGION   = region, $
	TROP_REL = trop_rel, $
	REFL	 = refl, $
	PNG	     = png, $
	EPS   	 = eps


;+
; Name:
;		O3_TIMESERIES_PLOTS
; Purpose:
;		Computes fraction of domain where updraft_tracer = 1
; Calling sequence:
;		O3_TIMESERIES_PLOTS, run, scheme, start_date, end_date
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		scheme 	   : Model initial state. Typically 'morrison' or 'nssl'.
;		start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;		end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
; Output:
;		Timeseries of fraction of convection in domain and chemical signature 
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel B. Phoenix	    2017-07-18. 
;								2017-09-09. Same as CONVECTIVE_FRACTION but with map plots
;											and track the location on the timeseries
;								2018-02-02. Modified H2O_TIMESERIES_PLOTS for O3
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain


outdir  = !WRF_DIRECTORY + event + '/paper/plots/o3_timeseries/'
FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(event, scheme, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(event, scheme, start_date, end_date, /DATE)	

conv_fraction = [ ]
max_o3		  = [ ]
ave_o3_nc	  = [ ]
ave_o3_c	  = [ ]
ave_o3_nc	  = [ ]
ave_o3_c	  = [ ]

;; calculate first date 
	updraft  = (WRF_READ_VAR('Updraft_tracer', date_arr[0], event, scheme, DOMAIN=domain, INDICES=region)).values
	cld_conv = (WRF_READ_VAR('Cloud_tracer'  , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region)).values
	h2o  	 = (WRF_READ_VAR('H2O'           , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E6
    o3       = (WRF_READ_VAR('O3'            , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3					
    co       = (WRF_READ_VAR('CO'            , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3					
    utls_o3 = (WRF_READ_VAR('UTLS_tracer'    , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3
    y       = (WRF_READ_VAR('Latitude'       , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region)).values
    x       = (WRF_READ_VAR('Longitude'      , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region)).values
    z       = (WRF_READ_VAR('Z'		         , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region)).values
    ztrop   = (WRF_READ_VAR('Z_trop'	     , date_arr[0], event, scheme, DOMAIN=domain, INDICES=region)).values	
	cloud   =  WRF_READ_VAR('CLOUD_MIX_TOTAL', date_arr[0], event, scheme, DOMAIN=domain, INDICES=region)
 	theta	   	 = WRF_READ_VAR('T', date_arr[0], event, scheme, DOMAIN = domain)							;Read temperature variable from WRF output
	theta.values = ((1000.0/(WRF_READ_VAR('P', date_arr[0], event, scheme, DOMAIN = domain)).values)^(!Rair/!Cp))*(theta.values)

	dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)

offset = 0
y0 = y[          offset ,          offset ]														;Set domain boundary points
y1 = y[          offset ,dim[1]-(1+offset)]
y2 = y[dim[0]-(1+offset),dim[1]-(1+offset)]
y3 = y[dim[0]-(1+offset),          offset ]
x0 = x[          offset ,          offset ]
x1 = x[          offset ,dim[1]-(1+offset)]
x2 = x[dim[0]-(1+offset),dim[1]-(1+offset)]
x3 = x[dim[0]-(1+offset),          offset ]

xc = INTERPOLATE(x, 0.5*(dim[0]-1), 0.5*(dim[1]-1))											;Get central grid point
yc = INTERPOLATE(y, 0.5*(dim[0]-1), 0.5*(dim[1]-1))

table   = [COLOR_24(200, 200, 200),VISUALIZE_88D_COLOR()]												;Set reflectivity color table
rlevels = [-100.0, 5.0*FINDGEN(N_ELEMENTS(table))]													;Set reflectivity contour levels
;rlevels = [-100.0, 5.0 + 5.0*FINDGEN(N_ELEMENTS(table))]											;***for comparison with t-matrix
wfactor = 400.0/(dim[0]) + 400.0/(dim[1])																	;Set map factor

dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
date_string = MAKE_ISO_DATE_STRING(date_arr[0], PREC='MINUTE', /COMPACT, /UTC)							;Create date string

;map_pos = [0.0, 0.0, 1.0, 1.0]																				;map position for paper
map_pos = [0.05, 0.15, 0.95, 0.95]																			;Set map position
bar_pos = [0.25, 0.10, 0.75, 0.12]																			;Set color bar position

;map_pos1 = [0.05, 0.15, 0.45, 0.95]																			;Set map position
;bar_pos1 = [0.10, 0.10, 0.40, 0.12]																			;Set color bar position
;
;map_pos2 = [0.55, 0.15, 0.95, 0.95]																			;Set map position
;bar_pos2 = [0.60, 0.10, 0.90, 0.12]																			;Set color bar position

map_pos1 = [0.02, 0.10, 0.48, 0.50]																			;Set map position
bar_pos1 = [0.10, 0.05, 0.40, 0.07]																			;Set color bar position

map_pos2 = [0.52, 0.10, 0.98, 0.50]																			;Set map position
bar_pos2 = [0.60, 0.05, 0.90, 0.07]																			;Set color bar position


IF KEYWORD_SET(z_buff) THEN BEGIN
	SET_PLOT, 'Z'																									;Output to Z buffer
	DEVICE, SET_PIXEL_DEPTH = 24, SET_RESOLUTION = [wfactor*(dim[0]), wfactor*(dim[1])], $	;Set device resolution and bit depth
		SET_CHARACTER_SIZE = [12, 20]
	!P.COLOR      = COLOR_24('black')																		;Foreground color
	!P.BACKGROUND = COLOR_24('white')																		;Background color
	!P.CHARSIZE   = 1.5																							;Set character size
	!P.FONT       = -1
ENDIF ELSE BEGIN
	IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN	
		PS_ON, FILENAME = epsfile, PAGE_SIZE = [4.0,4.0], MARGIN = 0.0, /INCHES;PAGE_SIZE = 0.001*dim*wfactor			;Switch to Postscript device
		DEVICE, /ENCAPSULATED
		!P.FONT     = 0																								;Hardware fonts
		!P.CHARSIZE = 0.75	
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS																							;Load basic color definitions
	ENDIF ELSE BEGIN
		SET_PLOT, 'X'
		WINDOW, XSIZE = 1.5*wfactor*(dim[0]), YSIZE = wfactor*(dim[1])										;Open graphics window
		!P.COLOR      = COLOR_24('black')																		;Foreground color
		!P.BACKGROUND = COLOR_24('white')																		;Background color
		!P.CHARSIZE   = 2.0		
		!P.FONT       = -1		
		thick_scale   = 1
	ENDELSE
ENDELSE

!P.MULTI = [0, 1, 3]

 ;	zc_arr  = [ ]
;	cld_arr = [ ]
 ;	FOR k = 0, dim[2]-2 DO BEGIN
 ;		zc 		= (z[*,*,k] + z[*,*,k+1]) / 2
 ;		cld_c   = (cloud.values[*,*,k] + cloud.values[*,*,k+1]) / 2
 ;		zc_arr  = [[[zc_arr ]], [[zc   ]]]
 ;		cld_arr = [[[cld_arr]], [[cld_c]]]
 ;	ENDFOR

	convection = FLOAT(WHERE(updraft GT 0.1, count, COMPLEMENT=nconv))

	o3_strat = WHERE(((theta.values GT 350.0) AND (theta.values LT 380.0)), so3_count, COMPLEMENT=o3_trop)
	h2o_strat = WHERE(((theta.values GT 350.0) AND (theta.values LT 380.0)), sh2o_count, COMPLEMENT=h2o_trop)
	overshoot = WHERE(cloud.values[h2o_strat] GT 1.0E-5, count, COMPLEMENT = nover)
	
	conv_vol = FINDGEN(dim[0],dim[1],dim[2])*0.0
		
	conv_vol[overshoot ] = 1.0
	conv_vol[nover	   ] = 0.0
		
	volume1 	  = TOTAL(conv_vol * 9 * z)	
	vol_size      = SIZE(conv_vol,/DIMENSIONS)		
	total_size    = TOTAL(z * 9)	
	conv_fraction = [TEMPORARY(conv_fraction), FLOAT((volume1/total_size))] 

	dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
    xyz_trop = REBIN(ztrop, dim[0], dim[1], dim[2], /SAMPLE)
     
 	o3_c  = FINDGEN(dim[0],dim[1],dim[2])*0.0
 	o3_nc = FINDGEN(dim[0],dim[1],dim[2])*0.0
 	 	
 	o3_c  [convection] = o3[convection]
 	o3_c  [nconv   	 ] = !Values.F_NaN

 	o3_nc [nconv   	 ] = o3[nconv   ]
 	o3_nc [convection] = !Values.F_NaN
 		
 	o3_strat = WHERE(((theta.values GT 350.0) AND (theta.values LT 380.0)), so3_count, COMPLEMENT=o3_trop)
	
	max_o3    = [TEMPORARY(max_o3)   , MAX (o3			   ,/NAN)]
	ave_o3_c  = [TEMPORARY(ave_o3_c ), MEAN(o3_c [o3_strat],/NAN)]
	ave_o3_nc = [TEMPORARY(ave_o3_nc), MEAN(o3_nc[o3_strat],/NAN)]
	ave_o3_bg = [TEMPORARY(ave_o3_bg), MEAN(o3   [o3_strat],/NAN)]

 	h2o_c  = FINDGEN(dim[0],dim[1],dim[2])*0.0
 	h2o_nc = FINDGEN(dim[0],dim[1],dim[2])*0.0
 	 	
 	h2o_c  [convection   ] = h2o[convection   ]
 	h2o_c  [nconv   	 ] = !Values.F_NaN

 	h2o_nc [nconv   	 ] = h2o[nconv   ]
 	h2o_nc [convection   ] = !Values.F_NaN
 		
 	h2o_strat = WHERE(((theta.values GT 350.0) AND (theta.values LT 380.0)), sh2o_count, COMPLEMENT=h2o_trop)
	
	max_h2o    = [TEMPORARY(max_h2o)   , MAX (h2o			   ,/NAN)]
	ave_h2o_c  = [TEMPORARY(ave_h2o_c ), MEAN(h2o_c [h2o_strat],/NAN)]
	ave_h2o_nc = [TEMPORARY(ave_h2o_nc), MEAN(h2o_nc[h2o_strat],/NAN)]
	ave_h2o_bg = [TEMPORARY(ave_h2o_bg), MEAN(h2o   [h2o_strat],/NAN)]

;; end

;; now calculate remaining dates
date_arr = date_arr[1:*]
i = 0
FOREACH date, date_arr DO BEGIN
	PRINT, 'Processing: ', date
	updraft  = (WRF_READ_VAR('Updraft_tracer' , date, event, scheme, DOMAIN=domain, INDICES=region)).values
	cld_conv = (WRF_READ_VAR('Cloud_tracer'   , date, event, scheme, DOMAIN=domain, INDICES=region)).values
	h2o      = (WRF_READ_VAR('H2O'            , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E6
    o3       = (WRF_READ_VAR('O3'             , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3					
    co       = (WRF_READ_VAR('CO'             , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3					
    utls_o3 = (WRF_READ_VAR('UTLS_tracer'    , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3
    y       = (WRF_READ_VAR('Latitude'       , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    x       = (WRF_READ_VAR('Longitude'      , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    z       = (WRF_READ_VAR('Z'		         , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    ztrop   = (WRF_READ_VAR('Z_trop'	     , date, event, scheme, DOMAIN=domain, INDICES=region)).values	
	cloud   =  WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, DOMAIN=domain, INDICES=region)
 	theta	   	 = WRF_READ_VAR('T', date, event, scheme, DOMAIN = domain)							;Read temperature variable from WRF output
	theta.values = ((1000.0/(WRF_READ_VAR('P', date, event, scheme, DOMAIN = domain)).values)^(!Rair/!Cp))*(theta.values)

	dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)

offset = 0
y0 = y[          offset ,          offset ]														;Set domain boundary points
y1 = y[          offset ,dim[1]-(1+offset)]
y2 = y[dim[0]-(1+offset),dim[1]-(1+offset)]
y3 = y[dim[0]-(1+offset),          offset ]
x0 = x[          offset ,          offset ]
x1 = x[          offset ,dim[1]-(1+offset)]
x2 = x[dim[0]-(1+offset),dim[1]-(1+offset)]
x3 = x[dim[0]-(1+offset),          offset ]

xc = INTERPOLATE(x, 0.5*(dim[0]-1), 0.5*(dim[1]-1))											;Get central grid point
yc = INTERPOLATE(y, 0.5*(dim[0]-1), 0.5*(dim[1]-1))

table   = [COLOR_24(200, 200, 200),VISUALIZE_88D_COLOR()]												;Set reflectivity color table
rlevels = [-100.0, 5.0*FINDGEN(N_ELEMENTS(table))]													;Set reflectivity contour levels
;rlevels = [-100.0, 5.0 + 5.0*FINDGEN(N_ELEMENTS(table))]											;***for comparison with t-matrix
wfactor = 400.0/(dim[0]) + 400.0/(dim[1])																	;Set map factor

dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string

;map_pos = [0.0, 0.0, 1.0, 1.0]																				;map position for paper
map_pos = [0.05, 0.15, 0.95, 0.95]																			;Set map position
bar_pos = [0.25, 0.10, 0.75, 0.12]																			;Set color bar position

;map_pos1 = [0.05, 0.15, 0.45, 0.95]																			;Set map position
;bar_pos1 = [0.10, 0.10, 0.40, 0.12]																			;Set color bar position
;
;map_pos2 = [0.55, 0.15, 0.95, 0.95]																			;Set map position
;bar_pos2 = [0.60, 0.10, 0.90, 0.12]																			;Set color bar position

map_pos1 = [0.02, 0.10, 0.32, 0.50]																			;Set map position
bar_pos1 = [0.04, 0.06, 0.30, 0.08]																			;Set color bar position

map_pos2 = [0.35, 0.10, 0.65, 0.50]																			;Set map position
bar_pos2 = [0.37, 0.06, 0.63, 0.08]																			;Set color bar position

map_pos3 = [0.68, 0.10, 0.98, 0.50]																			;Set map position
bar_pos3 = [0.70, 0.06, 0.96, 0.08]																			;Set color bar position


IF KEYWORD_SET(z_buff) THEN BEGIN
	SET_PLOT, 'Z'																									;Output to Z buffer
	DEVICE, SET_PIXEL_DEPTH = 24, SET_RESOLUTION = [wfactor*(dim[0]), wfactor*(dim[1])], $	;Set device resolution and bit depth
		SET_CHARACTER_SIZE = [12, 20]
	!P.COLOR      = COLOR_24('black')																		;Foreground color
	!P.BACKGROUND = COLOR_24('white')																		;Background color
	!P.CHARSIZE   = 1.5																							;Set character size
	!P.FONT       = -1
ENDIF ELSE BEGIN
	IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN	
		PS_ON, FILENAME = epsfile, PAGE_SIZE = [4.0,4.0], MARGIN = 0.0, /INCHES;PAGE_SIZE = 0.001*dim*wfactor			;Switch to Postscript device
		DEVICE, /ENCAPSULATED
		!P.FONT     = 0																								;Hardware fonts
		!P.CHARSIZE = 0.75	
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS																							;Load basic color definitions
	ENDIF ELSE BEGIN
		SET_PLOT, 'X'
		WINDOW, XSIZE = 1.5*wfactor*(dim[0]), YSIZE = wfactor*(dim[1])										;Open graphics window
		!P.COLOR      = COLOR_24('black')																		;Foreground color
		!P.BACKGROUND = COLOR_24('white')																		;Background color
		!P.CHARSIZE   = 2.0		
		!P.FONT       = -1		
		thick_scale   = 1
	ENDELSE
ENDELSE

!P.MULTI = [0, 1, 3]

 ;	zc_arr  = [ ]
;	cld_arr = [ ]
 ;	FOR k = 0, dim[2]-2 DO BEGIN
 ;		zc 		= (z[*,*,k] + z[*,*,k+1]) / 2
 ;		cld_c   = (cloud.values[*,*,k] + cloud.values[*,*,k+1]) / 2
 ;		zc_arr  = [[[zc_arr ]], [[zc   ]]]
 ;		cld_arr = [[[cld_arr]], [[cld_c]]]
 ;	ENDFOR

	convection = FLOAT(WHERE(updraft GT 0.1, count, COMPLEMENT=nconv))

	o3_strat = WHERE(((theta.values GT 350.0) AND (theta.values LT 380.0)), so3_count, COMPLEMENT=o3_trop)
	h2o_strat = WHERE(((theta.values GT 350.0) AND (theta.values LT 380.0)), sh2o_count, COMPLEMENT=h2o_trop)
	overshoot = WHERE(cloud.values[h2o_strat] GT 1.0E-5, count, COMPLEMENT = nover)
	
	conv_vol = FINDGEN(dim[0],dim[1],dim[2])*0.0
		
	conv_vol[overshoot ] = 1.0
	conv_vol[nover	   ] = 0.0
	
	volume1 	  = TOTAL(conv_vol * z)
	vol_size      = SIZE(conv_vol,/DIMENSIONS)
	total_size    = TOTAL(z)
	conv_fraction = [TEMPORARY(conv_fraction), FLOAT((volume1/total_size))] 

	dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
    ztrop    = CALC_TROP_MODE(ztrop, scheme, threshold) 												;Filter tropopause values
    xyz_trop = REBIN(ztrop, dim[0], dim[1], dim[2], /SAMPLE)
    filt     = WHERE(xyz_trop EQ 999999 , filt_count, COMPLEMENT = good, NCOMPLEMENT = good_count)
    
    xyz_trop[good] = xyz_trop[good]
    xyz_trop[filt] = !Values.F_NaN
 
 	o3_c  = FINDGEN(dim[0],dim[1],dim[2])*0.0
 	o3_nc = FINDGEN(dim[0],dim[1],dim[2])*0.0
 	 	
 	o3_c  [convection] = o3[convection]
 	o3_c  [nconv   	 ] = !Values.F_NaN

 	o3_nc [nconv   	 ] = o3[nconv     ]
 	o3_nc [convection] = !Values.F_NaN
 		
 	o3_strat = WHERE(((theta.values GT 350.0) AND (theta.values LT 380.0)), so3_count, COMPLEMENT=o3_trop)
	
	max_o3    = [TEMPORARY(max_o3)   , MAX (o3			   ,/NAN)]
	ave_o3_c  = [TEMPORARY(ave_o3_c ), MEAN(o3_c [o3_strat],/NAN)]
	ave_o3_nc = [TEMPORARY(ave_o3_nc), MEAN(o3_nc[o3_strat],/NAN)]
	ave_o3_bg = [TEMPORARY(ave_o3_bg), MEAN(o3   [o3_strat],/NAN)]

 	h2o_c  = FINDGEN(dim[0],dim[1],dim[2])*0.0
 	h2o_nc = FINDGEN(dim[0],dim[1],dim[2])*0.0
 	 	
 	h2o_c  [convection   ] = h2o[convection   ]
 	h2o_c  [nconv   	 ] = !Values.F_NaN

 	h2o_nc [nconv   	 ] = h2o[nconv   ]
 	h2o_nc [convection   ] = !Values.F_NaN
 		
 	h2o_strat = WHERE(((theta.values GT 350.0) AND (theta.values LT 380.0)), sh2o_count, COMPLEMENT=h2o_trop)
	
	max_h2o    = [TEMPORARY(max_h2o)   , MAX (h2o			   ,/NAN)]
	ave_h2o_c  = [TEMPORARY(ave_h2o_c ), MEAN(h2o_c [h2o_strat],/NAN)]
	ave_h2o_nc = [TEMPORARY(ave_h2o_nc), MEAN(h2o_nc[h2o_strat],/NAN)]
	ave_h2o_bg = [TEMPORARY(ave_h2o_bg), MEAN(h2o   [h2o_strat],/NAN)]
		
;ENDFOREACH	;date

color0 = COLOR_24('black')
color1 = COLOR_24('blue' )
color2 = COLOR_24('red'	 )
color3 = COLOR_24('darkgreen')

time = FINDGEN(N_ELEMENTS(date_arr))

PLOT, time, time, /NODATA, $																							;Set up plot
	THICK    = 2, $
	XRANGE   = [0, N_ELEMENTS(date_arr)-1], $
	XSTYLE   = 1, $
	YRANGE   = [0, 1], $
	YTICKS   = 1, $
	YTICKN   = [' ', ' '], $
	YSTYLE   = 1, $
	POSITION = [0.1, 0.55, 0.9, 0.95], $
	CHARSIZE  = 3.0, $
	TITLE    = 'Convective Fraction vs O3 concentration'

AXIS, YAXIS = 0, $																								;Draw altitude axis
	SAVE   = 1, $
	YRANGE = [0.0, 0.1], $
	YTITLE = 'Convective Fraction (%)', $
	YTICKS = 1, $
	YSTYLE = 1, $
	CHARSIZE  = 3.0, $
	COLOR  = color0

OPLOT, time, conv_fraction*100.0, COLOR = color0, THICK = 2													;Plot temperature measurments

AXIS, YAXIS = 0, $																								;Draw temperature axis
	SAVE   = 1, $
	YRANGE = [150, 500], $
	YTITLE = 'O3 concentration (ppbv)', $
	YTICKS = 1, $
	YSTYLE = 1, $
	CHARSIZE  = 3.0, $
	COLOR  = color1

OPLOT, time, ave_o3_bg, COLOR = color1, THICK = 2, LINESTYLE = 0 

AXIS, YAXIS = 1, $																								;Draw temperature axis
	SAVE   = 1, $
	YRANGE = [0, 15], $
	YTITLE = 'H2O concentration (ppbv)', $
	YTICKS = 1, $
	YSTYLE = 1, $
	CHARSIZE  = 3.0, $
	COLOR  = color2

;OPLOT, time, ave_o3_c , COLOR = color1, THICK = 2, LINESTYLE = 0													;Plot temperature measurments
;OPLOT, time, ave_o3_nc, COLOR = color1, THICK = 2, LINESTYLE = 1													;Plot temperature measurments
OPLOT, time, ave_h2o_bg, COLOR = color2, THICK = 3, LINESTYLE = 0 

PRINT, ave_o3_bg

;FOREACH date, date_arr DO BEGIN
	h2o			 = (WRF_READ_VAR('H2O'            , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E6
    o3           = (WRF_READ_VAR('O3'             , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3					
    y            = (WRF_READ_VAR('Latitude'       , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    x            = (WRF_READ_VAR('Longitude'      , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    z            = (WRF_READ_VAR('Z'		      , date, event, scheme, DOMAIN=domain, INDICES=region)).values
  	theta	   	 =  WRF_READ_VAR('T'	          , date, event, scheme, DOMAIN=domain)							;Read temperature variable from WRF output
    ztrop   	 = (WRF_READ_VAR('Z_trop'	      , date, event, scheme, DOMAIN=domain, INDICES=region)).values	
	cloud   	 =  WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, DOMAIN=domain, INDICES=region)

	theta.values = ((1000.0/(WRF_READ_VAR('P', date, event, scheme, DOMAIN = domain)).values)^(!Rair/!Cp))*(theta.values)

	dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
    xyz_trop = REBIN(ztrop, dim[0], dim[1], dim[2], /SAMPLE)

	h2o_slice   = FLTARR(dim[0],dim[1])
	o3_slice    = FLTARR(dim[0],dim[1])
	cloud_slice = FLTARR(dim[0],dim[1])
	k = 0

	FOR i = 0, dim[0]-1 DO BEGIN
		FOR j = 0, dim[1]-1 DO BEGIN
;			FOR k = 0, dim[2]-1 DO IF (theta.values[i,j,k] LT 350.0) THEN BEGIN
			FOR k = dim[2]-1, 0, -1 DO IF (theta.values[i,j,k] GT 350.0) THEN BEGIN
				ke = k
				ENDIF 
			o3_slice[i,j]    = o3[i,j,ke]
			h2o_slice[i,j]   = h2o[i,j,ke]
			cloud_slice[i,j] = cloud.values[i,j,ke]
		ENDFOR
	ENDFOR
	

MAP_SET, yc, xc, 0, CONIC = 1, /NOBORDER, $																;Draw map
	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
	ISOTROPIC = 1, $
	TITLE     = 'O3 at 350 K', $
	LATLAB	  = 1, $
	LONLAB 	  = 1, $
	LABEL	  = 1, $
	NOERASE   = 1, $
	CHARSIZE  = 2.5, $
	POSITION  = map_pos1


IF KEYWORD_SET(image) THEN BEGIN
	ij0 = CONVERT_COORD([(!X.WINDOW)[0],(!Y.WINDOW)[0]], /NORMAL, /TO_DEVICE)
	ij1 = CONVERT_COORD([(!X.WINDOW)[1],(!Y.WINDOW)[1]], /NORMAL, /TO_DEVICE)

	xsize = LONG(ij1[0] - ij0[0])
	ysize = LONG(ij1[1] - ij0[1])

	image0 = (MAX(R.values, DIM=3))[offset:(dim[0]-(1+offset)), offset:(dim[1]-(1+offset))]
	dim    = dim - (2*offset)
	
	IF KEYWORD_SET(eps) THEN $
		image0 = INTERPOLATE(image0, MAKEN(0, dim[0]-1, xsize/10), MAKEN(0, dim[1]-1, ysize/10), /GRID) ELSE $
		image0 = INTERPOLATE(image0, MAKEN(0, dim[0]-1, xsize   ), MAKEN(0, dim[1]-1, ysize   ), /GRID)

	image0 = IMAGE_24(COLOR_LOOKUP_24((image0 < 75.0), table[1:*], MIN = 0.0, MAX = 75.0, $
				MISSING = COLOR_24(200, 200, 200), /NAN))
	TV, image0, ij0[0], ij0[1], TRUE = 3, XSIZE = xsize, YSIZE = ysize, /DEVICE
ENDIF ELSE $	

;	 map_plot      = 0.001*(MAX((cloud.values GE 1.0E-5)*z, DIM = 3, /NAN))						;Set map variable to cloud top altitude
	 map_plot 	   = o3_slice
	 map_bar_title = 'o3 concentration at 350 K'														;Set color bar title
	 map_bar_min   = 150.0																						;Set echo top minimum
	 map_bar_max   = 500.0																					;Set echo top maximum
	 map_bar_ticks = 7																						;Set number of color bar ticks
;	 map_table     = [COLOR_24(230, 230, 230),VISUALIZE_88D_COLOR(3)]							;Set color table
	 map_table     = [VISUALIZE_88D_COLOR(3)]							;Set color table
	 map_levels    = [150.0 + 25.0 * FINDGEN(N_ELEMENTS(map_table))]								;Set contour levels

;	IF (KEYWORD_SET(TROP_REL)) THEN BEGIN
;		map_plot      = 0.001*((MAX((cloud.values GE 1.0E-5)*z, DIM = 3, /NAN)) - xyz_trop)						;Set map variable to cloud top altitude
;		
;		bad = WHERE(FINITE(map_plot,/NAN), bad_count, COMPLEMENT=good, NCOMPLEMENT=good_count)
;		IF (bad_count GT 0) THEN BEGIN
;			map_plot[bad ] = 0.0
;			map_plot[good] = map_plot[good]
;		ENDIF
;		
;	 	map_bar_title = 'Cloud Top Relative-Altitude (km)'														;Set color bar title
;	 	map_bar_min   = -2.5																						;Set echo top minimum
;	 	map_bar_max   = 2.5																				;Set echo top maximum
;	 	map_bar_ticks = 3																						;Set number of color bar ticks
;	 	map_table     = BLUE_GRAY_RED_24(10)							          					    ;Set color table
;	 	map_levels    = [-2.5 + 0.5*FINDGEN(11)]  									              ;Set contour levels
;	ENDIF

	IF KEYWORD_SET(refl) THEN BEGIN
		table   = [COLOR_24(200, 200, 200),VISUALIZE_88D_COLOR()]												;Set reflectivity color table
		rlevels = [-100.0, 5.0*FINDGEN(N_ELEMENTS(table))]													;Set reflectivity contour levels

		CONTOUR, MAX(R.values, DIM=3), x, y, $															;Contour reflectivity values
			OVERPLOT  = 1, $
			FILL      = 1, $
			LEVELS    = rlevels, $
	        TITLE     = date_string, $
			C_COLOR   = table, $
;      	    NOERASE   = 1, $
      	    POSITION  = map_pos1

	ENDIF ELSE BEGIN
	
       	CONTOUR, map_plot, x, y, $																	;Contour values
               OVERPLOT  = 1, $
               FILL      = 1, $
               LEVELS    = map_levels, $
               C_COLOR   = map_table, $
               TITLE     = date_string, $
 ;              NOERASE   = 1, $
               POSITION  = map_pos1
	ENDELSE


MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines

MAP_SET, yc, xc, 0, CONIC = 1, $																				;Redraw map boundaries
	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
	ISOTROPIC = 1, $
	NOERASE   = 1, $
	LATLAB	  = 1, $
	LONLAB 	  = 1, $
	LABEL	  = 1, $
	CHARSIZE  = 2.5, $
	POSITION  = map_pos1

COLOR_BAR_24_KPB, map_table[1:*], OVER = map_table[-1], UNDER = map_table[0], $						;Draw map color bar
	TICKS = map_bar_ticks, $
	RANGE = [map_bar_min, map_bar_max], $
	TITLE = map_bar_title, $
	CHARSIZE  = 1.75, $
	XCHARSIZE = 1.75, $
	POSIT = bar_pos1

;; end o3 plot ;;
;; begin h2o plot ;;
MAP_SET, yc, xc, 0, CONIC = 1, /NOBORDER, $																;Draw map
	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
	ISOTROPIC = 1, $
	TITLE     = 'H2O at 350K', $
	LATLAB	  = 1, $
	LONLAB 	  = 1, $
	LABEL	  = 1, $
	NOERASE   = 1, $
	CHARSIZE  = 2.5, $
	POSITION  = map_pos2

	 map_plot 	   = h2o_slice
	 map_bar_title = 'H2O Concentration (ppmv)'														;Set color bar title
	 map_bar_min   = 10.0																						;Set echo top minimum
	 map_bar_max   = 205.0																					;Set echo top maximum
	 map_bar_ticks = 7																						;Set number of color bar ticks
	 map_table     = [VISUALIZE_88D_COLOR(3)]							;Set color table
	 map_levels    = [10.0 + 10.0 * FINDGEN(N_ELEMENTS(map_table))]								;Set contour levels

	IF (KEYWORD_SET(TROP_REL)) THEN BEGIN
		map_plot      = 0.001*((MAX((cloud.values GE 1.0E-5)*z, DIM = 3, /NAN)) - xyz_trop)						;Set map variable to cloud top altitude
		
		bad = WHERE(FINITE(map_plot,/NAN), bad_count, COMPLEMENT=good, NCOMPLEMENT=good_count)
		IF (bad_count GT 0) THEN BEGIN
			map_plot[bad ] = -100.0
			map_plot[good] = map_plot[good]
		ENDIF
		
	 	map_bar_title = 'Cloud Top Relative-Altitude (km)'														;Set color bar title
	 	map_bar_min   = -2.5																						;Set echo top minimum
	 	map_bar_max   = 2.5																				;Set echo top maximum
	 	map_bar_ticks = 2																						;Set number of color bar ticks
	 	map_table     = [COLOR_24('gray90'), BLUE_RED_24(30,0.0)]							          					    ;Set color table
	 	map_levels    = [-100.0, MAKEN(-2.5,2.5,30)]  									              ;Set contour levels																					;Set number of color bar ticks
;		map_table 	  = BLUE_RED_24(30,0.0)
;	 	map_levels    = [MAKEN(-2.5,2.5,30)]  									              ;Set contour levels
	ENDIF

	CONTOUR, map_plot, x, y, $																	;Contour values
        OVERPLOT  = 1, $
        FILL      = 1, $
        LEVELS    = map_levels, $
        C_COLOR   = map_table, $
        TITLE     = date_string, $
        XLOG	  = 1, $
;        NOERASE   = 1, $
        POSITION  = map_pos2

MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines

IF KEYWORD_SET(section) THEN BEGIN
	IF (run EQ '20110408') THEN ij = [100, 119, 300, 157]
	IF (run EQ '20110521') THEN ij = [150, 116, 350, 130]
	IF (run EQ '20110618') THEN ij = [040, 060, 240, 080]

	xysect = MAP_2POINTS((x)[ij[0],ij[1]],(y)[ij[0],ij[1]],$
								(x)[ij[2],ij[3]],(y)[ij[2],ij[3]], NPATH = 10)
	
	OPLOT, xysect[0,*], xysect[1,*], THICK = 4
	XYOUTS, xysect[0,0], xysect[1,0], 'A', ALIGN = 1
	XYOUTS, xysect[0,-1], xysect[1,-1], 'B', ALIGN = 0
ENDIF

MAP_SET, yc, xc, 0, CONIC = 1, $																				;Redraw map boundaries
	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
	ISOTROPIC = 1, $
	NOERASE   = 1, $
	CHARSIZE  = 2.5, $
	POSITION  = map_pos2
	
COLOR_BAR_24_KPB, map_table[1:*], OVER = map_table[-1], UNDER = map_table[0], $						;Draw map color bar
	TICKS = map_bar_ticks, $
	RANGE = [map_bar_min, map_bar_max], $
	TITLE = map_bar_title, $
	CHARSIZE  = 1.75, $
	XCHARSIZE = 1.75, $
	POSIT = bar_pos2

;; end cloud tops ;;
;; start cloud slice
MAP_SET, yc, xc, 0, CONIC = 1, /NOBORDER, $																;Draw map
	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
	ISOTROPIC = 1, $
	TITLE     = 'Cloud at 350 K', $
	LATLAB	  = 1, $
	LONLAB 	  = 1, $
	LABEL	  = 1, $
	CHARSIZE  = 2.5, $
	NOERASE   = 1, $
	POSITION  = map_pos3


	 map_plot	   = cloud_slice
	 map_bar_title = 'Cloud Concentration'														;Set color bar title
	 map_bar_min   = 0.0 																						;Set echo top minimum
	 map_bar_max   = 1.0E-3 																					;Set echo top maximum
	 map_bar_ticks = 3																						;Set number of color bar ticks
	 map_table     = [COLOR_24(230, 230, 230),VISUALIZE_88D_COLOR(3)]							;Set color table
	 map_levels    = [0.0, 1.0E-9, 1.0E-8, 1.0E-7, 1.0E-6, 1.0E-5, 1.0E-4, 1.0E-3]								;Set contour levels
	
	CONTOUR, map_plot, x, y, $																	;Contour values
        OVERPLOT  = 1, $
        FILL      = 1, $
        LEVELS    = map_levels, $
        C_COLOR   = map_table, $
        TITLE     = date_string, $
        XLOG	  = 1, $
;        NOERASE   = 1, $
        POSITION  = map_pos3

MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines

IF KEYWORD_SET(section) THEN BEGIN
	IF (run EQ '20110408') THEN ij = [100, 119, 300, 157]
	IF (run EQ '20110521') THEN ij = [150, 116, 350, 130]
	IF (run EQ '20110618') THEN ij = [040, 060, 240, 080]

	xysect = MAP_2POINTS((x)[ij[0],ij[1]],(y)[ij[0],ij[1]],$
								(x)[ij[2],ij[3]],(y)[ij[2],ij[3]], NPATH = 10)
	
	OPLOT, xysect[0,*], xysect[1,*], THICK = 4
	XYOUTS, xysect[0,0], xysect[1,0], 'A', ALIGN = 1
	XYOUTS, xysect[0,-1], xysect[1,-1], 'B', ALIGN = 0
ENDIF

MAP_SET, yc, xc, 0, CONIC = 1, $																				;Redraw map boundaries
	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
	ISOTROPIC = 1, $
	NOERASE   = 1, $
	CHARSIZE  = 2.5, $
	POSITION  = map_pos3
	
COLOR_BAR_24_KPB, map_table[1:*], OVER = map_table[-1], UNDER = map_table[0], $						;Draw map color bar
	TICKS = map_bar_ticks, $
	RANGE = [map_bar_min, map_bar_max], $
	TITLE = map_bar_title, $
	CHARSIZE  = 1.75, $
	XCHARSIZE = 1.75, $
	POSIT = bar_pos3

;;end cloud slice
date1   = MAKE_ISO_DATE_STRING(date,/COMPACT,/UTC)

epsfile = outdir +  date1  + '.eps'						;EPS filename
pdffile = outdir +  date1  + '.pdf'						;PDF filename
pngfile = outdir +  date1  + '.png'						;PNG filename


IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file


ENDFOREACH

END