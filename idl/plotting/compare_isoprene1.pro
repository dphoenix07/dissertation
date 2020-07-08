PRO COMPARE_ISOPRENE1, event, date, $
	DOMAIN   = domain, $
	VAR2     = var2, $
	ALT		 = alt, $
	REGION   = region, $
	PNG	     = png, $
	EPS   	 = eps


;+
; Name:
;		COMPARE_ISOPRENE
; Purpose:
;		Currently set up to combine multiple time steps and produce scatter plots of 
;		trace gases using WRF_TRACER_RALT.
;		(e.g., Similar to 'PLOT_WRF_TRACER_RALT', but can do multiple time steps)
; Calling sequence:
;		COMPARE_ISOPRENE1, run, scheme, start_date, end_date
; Example: COMPARE_ISOPRENE1,'20110808', MAKE_DATE(2011,8,8,13)
; Input:
;		event: Model simulation name. (e.g., '20120519')
;		date : CDATE
; Output:
;		Scatterplots of trace gases from multiple timesteps.
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel B. Phoenix	    2016-10-07. 
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain
IF (N_ELEMENTS(in_cloud  ) EQ 0) THEN in_cloud   = 1
IF (N_ELEMENTS(threshold ) EQ 0) THEN threshold  = 1000.0
IF (N_ELEMENTS(xrange	 ) EQ 0) THEN xrange     = [  0, 600]
IF (N_ELEMENTS(xtitle	 ) EQ 0) THEN xtitle     = 'Isoprene (pptv)'
IF (N_ELEMENTS(yrange	 ) EQ 0) THEN yrange     = [-5,   5]
IF (N_ELEMENTS(ytitle	 ) EQ 0) THEN ytitle     = 'Relative Altitude'
IF (N_ELEMENTS(nxbin 	 ) EQ 0) THEN nxbin      = 50
IF (N_ELEMENTS(nybin 	 ) EQ 0) THEN nybin      = 50
IF (N_ELEMENTS(var2		 ) EQ 0) THEN var2		 = 0

scheme_arr = ['big_dom/d02_ndown_d01chem', 'big_dom/bio1', 'big_dom/bio2']
label  	   = ['MEGAN', 'GUNTHER', 'BEIS']
key    	   = 'test'

IF (KEYWORD_SET(ALT)) THEN BEGIN
	yrange = [0, 20]
	ytitle = 'Altitude'
ENDIF

color1 = [14769706,12533908,127,COLOR_24_DP('gray50')]
colors = HCL_COLOR_TABLE(4, SAT_RANGE = [0.2,1.0],HUE_RANGE = [240.0, 360.0])

rgb = COMPONENT_24(colors)

rgb = REFORM(rgb,3,4)

;;option 5
rgb[*,0]=[204,204,255]
rgb[*,1]=[191,76,191] 
rgb[*,2]=[127,0,0]
rgb[*,3]=[250,0,0]

i = 0

yrange = [-5, 5]
IF (KEYWORD_SET(ALT)) THEN yrange = [0, 20] 

event = '20110808'

FOREACH scheme, scheme_arr DO BEGIN
	PRINT, "Processing date: ", date
	PRINT, scheme
	z		 = (WRF_READ_VAR('Z'     , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read height values
	dim 	 = SIZE(z, /DIMENSIONS)																						;Get dimensions (to reform arrays later)
	z_trop   = (WRF_READ_VAR('Z_trop', date, event, scheme, DOMAIN = domain, INDICES = region)).values
;	z_trop   = CALC_TROP_MODE(z_trop, scheme, threshold) 																;Filter tropopause values

	theta   = (WRF_READ_VAR('T'			     , date, event, scheme, DOMAIN = domain, INDICES = region)).values					
	theta   = ((1000.0/(WRF_READ_VAR('P'     , date, event, scheme, DOMAIN = domain, INDICES = region)).values)^(!Rair/!Cp))*(theta)
	precip   = (WRF_READ_VAR('PRECIP'	     , date, event, scheme, DOMAIN = domain, INDICES = region)).values
	w	     = (WRF_READ_VAR('w' 	 	     , date, event, scheme, DOMAIN = domain, INDICES = region)).values
	cld_tr   = (WRF_READ_VAR('Cloud_tracer'  , date, event, scheme, DOMAIN = domain, INDICES = region)).values
	updraft  = (WRF_READ_VAR('Updraft_tracer', date, event, scheme, DOMAIN = domain, INDICES = region)).values

	xyz_trop = REBIN(z_trop, dim[0], dim[1], dim[2], /SAMPLE)
	filt     = WHERE(FINITE(xyz_trop, /NAN), filt_count, COMPLEMENT = good, NCOMPLEMENT = good_count)

;    z[filt] 	    = !Values.F_NaN
;    xyz_trop[filt]  = !Values.F_NaN

	cloud   		= (WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, DOMAIN = domain, INDICES = region)).values * 1.0E3			
;	cloud[filt]     = !Values.F_NaN

	plt_var      	= (WRF_READ_VAR('ISO'     		 , date, event, scheme, DOMAIN = domain, INDICES = region)).values					
;	plt_var[filt]   = !Values.F_NaN

	tracer = plt_var * 1.0E6
	ralt   = (z - xyz_trop) * 1.0E-3
	IF (KEYWORD_SET(ALT)) THEN ralt = z * 1.0E-3

	cloud_values = WHERE(cloud GE 0.1, cld_count, COMPLEMENT = non_cloud, $							;Find values in cloud 
						NCOMPLEMENT = ncld_count)

	IF KEYWORD_SET(in_cloud) THEN BEGIN																;Sort values in cloud vs out of cloud
		IF (cld_count GT 0) THEN BEGIN
			tracer[non_cloud] = !Values.F_NaN
			ralt  [non_cloud] = !Values.F_NaN
		ENDIF
	ENDIF ELSE BEGIN
		IF (ncld_count GT 0) THEN BEGIN
			tracer [cloud_values] = !Values.F_NaN
			ralt   [cloud_values] = !Values.F_NaN
		ENDIF
	ENDELSE

		
	good = WHERE(((tracer GE xrange[0]) 	AND $												;Use specified data range
					(tracer LE xrange[1]) 	AND $												;Only used if filtering not desired
					(ralt   GE yrange[0])   AND $
					(ralt   LE yrange[1])), good_count, COMPLEMENT=out, NCOMPLEMENT = nout)
			  
	IF (nout GT 0) THEN BEGIN																	;Save good data points
		tracer [out] = !Values.F_NaN
		ralt   [out] = !Values.F_NaN 
	ENDIF 

	tracer = REFORM(tracer,dim[0],dim[1],dim[2])
	ralt   = REFORM(ralt  ,dim[0],dim[1],dim[2])

	tracer = REFORM(tracer,dim[0]*dim[1],dim[2])
	ralt   = REFORM(ralt  ,dim[0]*dim[1],dim[2])

	tr_med1    = [ ]
	tr_min1    = [ ]
	tr_max1    = [ ] 
	ralt_mean1 = [ ]

	FOR k = 0, dim[2]-1 DO BEGIN
		lev = SORT(tracer[*,k])
		tr1 = tracer[lev,k]

		tr_med = MEDIAN(tr1)
		tr_min = MIN(tr1,/NAN)
		tr_max = MAX(tr1,/NAN)

		ralt_mean  = MEAN(ralt[*,k],/NAN) 
		ralt_mean1 = [ralt_mean1, ralt_mean]
	
		tr_med1 = [tr_med1,tr_med]
		tr_min1 = [tr_min1,tr_min]
		tr_max1 = [tr_max1,tr_max]
	ENDFOR

	numlev = [0:dim[2]-1]

	figure3 = PLOT(tr_med1, ralt_mean1, LINESTYLE =0, XMINOR =1, COLOR = rgb[*,i], THICK = 2, /OVERPLOT, LAYOUT = position)		
	figure4 = PLOT(tr_max1, ralt_mean1, LINESTYLE =2, XMINOR =1, COLOR = rgb[*,i], THICK = 2, /OVERPLOT, LAYOUT = position)
	IF KEYWORD_SET(~var2) THEN $
	figure5 = PLOT(tr_min1, ralt_mean1, LINESTYLE =2, XMINOR =1, COLOR = rgb[*,i], THICK = 2, /OVERPLOT, LAYOUT = position)

PRINT, tr_max1[0]
PRINT, tr_med1[0]
	i = i + 1
ENDFOREACH ;scheme

figure5 = PLOT(xrange,[0.0,0.0], /OVERPLOT, LAYOUT = position, XTITLE = xtitle)

t2 = TEXT(500,400,  'MEGAN', FONT_SIZE=10, /DEVICE, FONT_COLOR = rgb[*,0]);!COLOR.blue)
t3 = TEXT(500,385,  'GUNTH', FONT_SIZE=10, /DEVICE, FONT_COLOR = rgb[*,1]);!COLOR.red)
t4 = TEXT(500,370,  'BEIS' , FONT_SIZE=10, /DEVICE, FONT_COLOR = rgb[*,2]);!COLOR.magenta)

!P.MULTI = 0
IF KEYWORD_SET(eps) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																;Reset color table to linear ramp
	PS_OFF																						;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)															;Write PNG file


END