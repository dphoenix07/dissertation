PRO DOMINANT_HYDROMETEOR, event, scheme, start_date, end_date, $
	DOMAIN   = domain, $
	REGION   = region, $
	MASS	 = mass, $
	NUM	  	 = num, $
	BASE 	 = base, $
	PNG	     = png, $
	EPS   	 = eps


;+
; Name:
;		DOMINANT_HYDROMETEOR
; Purpose:
;		Identifying the dominant hydrometeor in overshooting events to link to amount of
;		H2O that is injected into the stratosphere.
; Calling sequence:
;		DOMINANT_HYDROMETEOR, run, scheme, start_date, end_date
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		scheme 	   : Model initial state. Typically 'morrison' or 'nssl'.
;		start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;		end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
; Output:
;		Map of overshoot locations colored by the dominant hydrometeor.
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		BASE    : Lower end potential temperature threshold
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel B. Phoenix	    2017-09-19. 
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain
IF (N_ELEMENTS(base		 ) EQ 0) THEN base		 = 350

outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/plots/dominant_hydrometeor_mass/'
FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(event, scheme, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(event, scheme, start_date, end_date, /DATE)	

conv_fraction = [ ]
max_h2o		  = [ ]
ave_h2o_nc	  = [ ]
ave_h2o_c	  = [ ]
ave_o3_nc	  = [ ]
ave_o3_c	  = [ ]

;; now calculate remaining dates
i = 0
FOREACH date, date_arr DO BEGIN
	PRINT, 'Processing: ', date
	updraft  = (WRF_READ_VAR('Updraft_tracer' , date, event, scheme, DOMAIN=domain, INDICES=region)).values
	h2o		 = (WRF_READ_VAR('H2O' 			  , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E6
	cld_conv = (WRF_READ_VAR('Cloud_tracer'   , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    y        = (WRF_READ_VAR('Latitude'       , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    x        = (WRF_READ_VAR('Longitude'      , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    z        = (WRF_READ_VAR('Z'		      , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    ztrop    = (WRF_READ_VAR('Z_trop'	      , date, event, scheme, DOMAIN=domain, INDICES=region)).values	
	cloud    =  WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, DOMAIN=domain, INDICES=region)
 	theta	 =  WRF_READ_VAR('T'			  , date, event, scheme, DOMAIN = domain)							;Read temperature variable from WRF output
	theta.values = ((1000.0/(WRF_READ_VAR('P' , date, event, scheme, DOMAIN = domain)).values)^(!Rair/!Cp))*(theta.values)

	ice_mix  = (WRF_READ_VAR('ICE_MIX'    , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values
	snow_mix = (WRF_READ_VAR('SNOW_MIX'   , date, event, scheme, DOMAIN = domain, INDICES = region)).values 
	grap_mix = (WRF_READ_VAR('GRAUPEL_MIX', date, event, scheme, DOMAIN = domain, INDICES = region)).values
	clou_mix = (WRF_READ_VAR('CLOUD_MIX'  , date, event, scheme, DOMAIN = domain, INDICES = region)).values
	rain_mix = (WRF_READ_VAR('RAIN_MIX'   , date, event, scheme, DOMAIN = domain, INDICES = region)).values
	hail_mix = (WRF_READ_VAR('HAIL_MIX'   , date, event, scheme, DOMAIN = domain, INDICES = region)).values

    ice_num  = (WRF_READ_VAR('ICE_NUM'    , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values
    snow_num = (WRF_READ_VAR('SNOW_NUM'   , date, event, scheme, DOMAIN = domain, INDICES = region)).values 
    grap_num = (WRF_READ_VAR('GRAUPEL_NUM', date, event, scheme, DOMAIN = domain, INDICES = region)).values
    clou_num = (WRF_READ_VAR('CLOUD_NUM'  , date, event, scheme, DOMAIN = domain, INDICES = region)).values
    rain_num = (WRF_READ_VAR('RAIN_NUM'   , date, event, scheme, DOMAIN = domain, INDICES = region)).values
    hail_num = (WRF_READ_VAR('HAIL_NUM'   , date, event, scheme, DOMAIN = domain, INDICES = region)).values
	
	dim  = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)

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

map_pos = [0.05, 0.15, 0.95, 0.95]																			;Set map position
bar_pos = [0.25, 0.10, 0.75, 0.12]																			;Set color bar position

map_pos1 = [0.02, 0.10, 0.32, 0.50]																			;Set map position
bar_pos1 = [0.04, 0.05, 0.30, 0.07]																			;Set color bar position

map_pos2 = [0.35, 0.10, 0.65, 0.50]																			;Set map position
bar_pos2 = [0.37, 0.05, 0.63, 0.07]																			;Set color bar position

map_pos3 = [0.68, 0.10, 0.98, 0.50]																			;Set map position
bar_pos3 = [0.70, 0.05, 0.96, 0.07]																			;Set color bar position


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

;;First identify overshoots
	convection = FLOAT(WHERE(updraft GT 0.1, count, COMPLEMENT=nconv))

	h2o_strat = WHERE(((theta.values GT base) AND (theta.values LT (base + 50.0))), sh2o_count, COMPLEMENT=h2o_trop)
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
 
 	h2o_c  = FINDGEN(dim[0],dim[1],dim[2])*0.0
 	h2o_nc = FINDGEN(dim[0],dim[1],dim[2])*0.0
 	 	
 	h2o_c  [convection   ] = h2o[convection   ]
 	h2o_c  [nconv   	 ] = !Values.F_NaN

 	h2o_nc [nconv   	 ] = h2o[nconv   ]
 	h2o_nc [convection   ] = !Values.F_NaN
 		
 	h2o_strat = WHERE(((theta.values GT base) AND (theta.values LT (base + 50.0))), sh2o_count, COMPLEMENT=h2o_trop)
	
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
	CHARSIZE = 3.0, $
	POSITION = [0.1, 0.55, 0.9, 0.95], $
	TITLE    = 'Convective Fraction vs H2O concentration (' + $
				STRTRIM(STRING(base),1) + 'K - ' + STRTRIM(STRING(base + 50),1) + 'K)'

AXIS, YAXIS = 0, $																								;Draw altitude axis
	SAVE     = 1, $
	YRANGE   = [0.0, 0.1], $
	YTITLE   = 'Convective Fraction (%)', $
	YTICKS   = 1, $
	YSTYLE   = 1, $
	CHARSIZE = 3, $
	COLOR    = color0

OPLOT, time, conv_fraction*100.0, COLOR = color0, THICK = 3													;Plot temperature measurments

AXIS, YAXIS  = 1, $																								;Draw temperature axis
	SAVE     = 1, $
	YRANGE   = [0, 10], $
	YTITLE   = 'H2O concentration (ppmv)', $
	YTICKS   = 1, $
	YSTYLE   = 1, $
	CHARSIZE = 3, $
	COLOR    = color1

OPLOT, time, ave_h2o_c , COLOR = color1, THICK = 3, LINESTYLE = 0													;Plot temperature measurments
;OPLOT, time, ave_h2o_nc, COLOR = color1, THICK = 2, LINESTYLE = 1													;Plot temperature measurments
;OPLOT, time, ave_h2o_bg, COLOR = color2, THICK = 2, LINESTYLE = 0 

;FOREACH date, date_arr DO BEGIN
;	h2o		     = (WRF_READ_VAR('H2O' 		      , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E6
;    o3           = (WRF_READ_VAR('O3'             , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3					
    y            = (WRF_READ_VAR('Latitude'       , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    x            = (WRF_READ_VAR('Longitude'      , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    z            = (WRF_READ_VAR('Z'		      , date, event, scheme, DOMAIN=domain, INDICES=region)).values
;  	theta	   	 =  WRF_READ_VAR('T'	          , date, event, scheme, DOMAIN=domain)							;Read temperature variable from WRF output
    ztrop   	 = (WRF_READ_VAR('Z_trop'	      , date, event, scheme, DOMAIN=domain, INDICES=region)).values	
;	cloud   	 =  WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, DOMAIN=domain, INDICES=region)
;
;	theta.values = ((1000.0/(WRF_READ_VAR('P', date, event, scheme, DOMAIN = domain)).values)^(!Rair/!Cp))*(theta.values)
;
;	dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)

    xx = REBIN(x, dim[0], dim[1], dim[2], /SAMPLE)
    yy = REBIN(y, dim[0], dim[1], dim[2], /SAMPLE)

    xyz_trop = REBIN(ztrop, dim[0], dim[1], dim[2], /SAMPLE)

	h2o_slice   = FLTARR(dim[0],dim[1])
	cloud_slice = FLTARR(dim[0],dim[1])

	icen_slice   = FLTARR(dim[0],dim[1])
	rainn_slice  = FLTARR(dim[0],dim[1])
	cloun_slice  = FLTARR(dim[0],dim[1])
	hailn_slice  = FLTARR(dim[0],dim[1])
	grapn_slice  = FLTARR(dim[0],dim[1])
	snown_slice  = FLTARR(dim[0],dim[1])

	icem_slice   = FLTARR(dim[0],dim[1])
	rainm_slice  = FLTARR(dim[0],dim[1])
	cloum_slice  = FLTARR(dim[0],dim[1])
	hailm_slice  = FLTARR(dim[0],dim[1])
	grapm_slice  = FLTARR(dim[0],dim[1])
	snowm_slice  = FLTARR(dim[0],dim[1])

	k = 0
	dum = FLTARR(dim[0],dim[1],dim[2])
	dots = [ ]
	FOR i = 0, dim[0]-1 DO BEGIN
		FOR j = 0, dim[1]-1 DO BEGIN
			FOR k = dim[2]-1, 0, -1 DO IF (theta.values[i,j,k] GT base) THEN BEGIN
				ke = k
				ENDIF 
			h2o_slice   [i,j] = h2o[i,j,ke]
			cloud_slice [i,j] = cloud.values[i,j,ke]
			icen_slice  [i,j] = ice_num [i,j,ke]
			rainn_slice [i,j] = rain_num[i,j,ke]
			cloun_slice [i,j] = clou_num[i,j,ke]
			hailn_slice [i,j] = hail_num[i,j,ke]
			grapn_slice [i,j] = grap_num[i,j,ke]
			snown_slice [i,j] = snow_num[i,j,ke]

			icem_slice  [i,j] = ice_mix [i,j,ke]
			rainm_slice [i,j] = rain_mix[i,j,ke]
			cloum_slice [i,j] = clou_mix[i,j,ke]
			hailm_slice [i,j] = hail_mix[i,j,ke]
			grapm_slice [i,j] = grap_mix[i,j,ke]
			snowm_slice [i,j] = snow_mix[i,j,ke]

			IF (z[i,j,ke] LT xyz_trop[i,j,ke]) THEN dum[i,j,ke]=!Values.F_NaN 				
		ENDFOR
	ENDFOR

	dot = WHERE(FINITE(dum,/NAN), count)
	dots = [dots, dot]
	
	totn_slice = icen_slice + rainn_slice + cloun_slice + hailn_slice + grapn_slice + snown_slice
;	ice_slice1 = icen_slice
		
	icen_slice  = icen_slice/totn_slice
	rainn_slice = rainn_slice/totn_slice
	cloun_slice = cloun_slice/totn_slice
	hailn_slice = hailn_slice/totn_slice
	grapn_slice = grapn_slice/totn_slice
	snown_slice = snown_slice/totn_slice

	totm_slice = icem_slice + rainm_slice + cloum_slice + hailm_slice + grapm_slice + snowm_slice
	ice_slice1 = icem_slice
	
	icem_slice  = icem_slice/totm_slice
	rainm_slice = rainm_slice/totm_slice
	cloum_slice = cloum_slice/totm_slice
	hailm_slice = hailm_slice/totm_slice
	grapm_slice = grapm_slice/totm_slice
	snowm_slice = snowm_slice/totm_slice

hydro_label = ['rain','cloud','ice','hail','graupel','snow']
	
	master_indexn = FLTARR(dim[0],dim[1])
	master_indexm = FLTARR(dim[0],dim[1])

	FOR ii=0,dim[0]-1 DO BEGIN
		FOR jj=0,dim[1]-1 DO BEGIN
			hydron_fractions = [rainn_slice[ii,jj], cloun_slice[ii,jj], icen_slice[ii,jj], $
									hailn_slice[ii,jj], grapn_slice[ii,jj], snown_slice[ii,jj]]
			hydrom_fractions = [rainm_slice[ii,jj], cloum_slice[ii,jj], icem_slice[ii,jj], $
									hailm_slice[ii,jj], grapm_slice[ii,jj], snowm_slice[ii,jj]]

			indexn = WHERE(hydron_fractions EQ MAX(hydron_fractions,/NAN))
			indexm = WHERE(hydrom_fractions EQ MAX(hydrom_fractions,/NAN))

			master_indexn [ii,jj] = indexn
			master_indexm [ii,jj] = indexm
			
		ENDFOR
	ENDFOR

;;; calculations done ;;;

;;plot h2o slice ;;
MAP_SET, yc, xc, 0, CONIC = 1, /NOBORDER, $																;Draw map
	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
	ISOTROPIC = 1, $
	TITLE     = 'H2O at ' + STRTRIM(STRING(base), 1) + 'K', $
	LATLAB	  = 1, $
	LONLAB 	  = 1, $
	LABEL	  = 1, $
	NOERASE   = 1, $
	CHARSIZE  = 2.5, $
	POSITION  = map_pos1
	
	PRINT, MAX(h2o_slice,/NAN)
	 map_plot 	   = h2o_slice
	 map_bar_title = 'H2O concentration at ' + STRTRIM(STRING(base), 1) + 'K'														;Set color bar title
	 map_bar_min   = 10.0																						;Set echo top minimum
	 map_bar_max   = 205.0																					;Set echo top maximum
	 map_bar_ticks = 7																						;Set number of color bar ticks
	 map_table     = [VISUALIZE_88D_COLOR(3)]							;Set color table
	 map_levels    = [10.0 + 13.0 * FINDGEN(N_ELEMENTS(map_table))]								;Set contour levels
	 IF (base GT 350) THEN BEGIN
	 	map_bar_max = 75.0
	 	map_levels  = [10.0 + 5.0 * FINDGEN(N_ELEMENTS(map_table))]
	ENDIF

PRINT, map_levels
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

USERSYM_CIRCLE, /FILL															;Load plane symbol at flight path orientation

;;+ Shade where 350 K line is below lapse rate tropopause
;IF (count GT 0) THEN BEGIN
;	FOR i=0, count-1 DO BEGIN
;		PLOTS, (xx)[dot[i]], (yy)[dot[i]], $																		;Overplot plane symbol
;			PSYM    = 8, $
;			SYMSIZE = 4 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
;			NOCLIP  = 0, $
;			COLOR   = COLOR_24('gray90')
;	ENDFOR
;ENDIF
;;- End


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
	POSIT = bar_pos1


;;; plot hydrometeors ;;;
MAP_SET, yc, xc, 0, CONIC = 1, /NOBORDER, $																;Draw map
	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
	ISOTROPIC = 1, $
	TITLE     = 'Dominant Hydrometeor (Mass)', $
	LATLAB	  = 1, $
	LONLAB 	  = 1, $
	LABEL	  = 1, $
	NOERASE   = 1, $
	CHARSIZE  = 2.5, $
	POSITION  = map_pos2


	map_plot = master_indexm
;	map_plot = master_indexn
	bad = WHERE(map_plot EQ -1.0, bad_count, COMPLEMENT=good, NCOMPLEMENT=good_count)
	IF (bad_count GT 0) THEN BEGIN
		map_plot[bad ] = -100.0
		map_plot[good] = map_plot[good]
	ENDIF
	
	map_bar_title = 'Dominant Hydrometeor Type'														;Set color bar title
	map_bar_min   = 0.0																					;Set echo top minimum
	map_bar_max   = 6.0																				;Set echo top maximum
	map_bar_ticks = 6																						;Set number of color bar ticks
	map_table     = [COLOR_24('gray90'),COLOR_24('red'),COLOR_24('green'),COLOR_24('blue'),$
						COLOR_24('yellow'),COLOR_24('orange'),COLOR_24('purple')]							          					    ;Set color table
    map_levels    = [-100.0, 0.0, 1.0, 2.0, 3.0, 4.0, 5.0]								;Set contour levels

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
	POSIT = bar_pos2

;; end hydro mass ;;
;; start hydro num ;;

MAP_SET, yc, xc, 0, CONIC = 1, /NOBORDER, $																;Draw map
	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
	ISOTROPIC = 1, $
;	TITLE     = 'Dominant Hydrometeor (num)', $
	TITLE	  = 'Ice Mass Mixing Ratio at ' + STRTRIM(STRING(base), 1) + 'K', $
;	TITLE	  = 'Ice Number Conc. at 350 K', $
	LATLAB	  = 1, $
	LONLAB 	  = 1, $
	LABEL	  = 1, $
	NOERASE   = 1, $
	CHARSIZE  = 2.5, $
	POSITION  = map_pos3

	map_plot = ice_slice1 ;master_indexn
	bad = WHERE(map_plot EQ -1.0, bad_count, COMPLEMENT=good, NCOMPLEMENT=good_count)
	IF (bad_count GT 0) THEN BEGIN
		map_plot[bad ] = -100.0
		map_plot[good] = map_plot[good]
	ENDIF
	
	map_bar_title = 'Ice Mixing Ratio at ' + STRTRIM(STRING(base), 1) + 'K'														;Set color bar title
	map_bar_min   = 0.0 																						;Set echo top minimum
	map_bar_max   = 1.0E-3 																					;Set echo top maximum
	map_bar_ticks = 3																						;Set number of color bar ticks
	map_table     = [COLOR_24(230, 230, 230),VISUALIZE_88D_COLOR(3)]							;Set color table
	map_levels    = [0.0, 1.0E-9, 1.0E-8, 1.0E-7, 1.0E-6, 1.0E-5, 1.0E-4, 1.0E-3]								;Set contour levels

;	map_bar_title = 'Ice Number Conc. at 350 K'
;	map_bar_min	  = 0.0
;	map_bar_max   = 1.0E7
;	map_bar_ticks = 3
;	map_table     = [COLOR_24(230, 230, 230),VISUALIZE_88D_COLOR(3)]							;Set color table
;	map_levels    = [0.0, 1.0E1, 1.0E2, 1.0E3, 1.0E4, 1.0E5, 1.0E6, 1.0E7]								;Set contour levels
	

;	map_table     = [COLOR_24('gray90'),COLOR_24('red'),COLOR_24('green'),COLOR_24('blue'),$
;						COLOR_24('yellow'),COLOR_24('orange'),COLOR_24('purple')]							          					    ;Set color table
;    map_levels    = [-100.0, 0.0, 1.0, 2.0, 3.0, 4.0, 5.0]								;Set contour levels


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