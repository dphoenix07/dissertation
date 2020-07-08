PRO PLOT_WRF_OBS_SCATTER, event, scheme, start_date, end_date, $
	DOMAIN   = domain, $
	IN_CLOUD = in_cloud, $
	BINNED   = binned, $
	REGION   = region, $
	ALT		 = alt, $
	PNG	     = png, $
	EPS   	 = eps


;+
; Name:
;		PLOT_WRF_OBS_SCATTER
; Purpose:
;		Currently set up to combine multiple time steps and produce scatter plots of 
;		trace gases using WRF_TRACER_RALT.
;		(e.g., Similar to 'PLOT_WRF_TRACER_RALT', but can do multiple time steps)
; Calling sequence:
;		PLOT_WRF_OBS_SCATTER, run, scheme, start_date, end_date
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		scheme 	   : Model initial state. Typically 'morrison' or 'nssl'.
;		start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;		end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
; Output:
;		Scatterplots of trace gases from multiple timesteps.
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel B. Phoenix	    2016-04-13. 
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 2											;Set default domain
IF (N_ELEMENTS(in_cloud  ) EQ 0) THEN in_cloud   = 1
IF (N_ELEMENTS(threshold ) EQ 0) THEN threshold  = 1000.0

IF (N_ELEMENTS(region) EQ 0) THEN BEGIN
	IF (event EQ '20120519') THEN region = [50, 50, 250, 190]
  	IF (event EQ '20120529') THEN region = [50, 80, 230, 250]
  	IF (event EQ '20120601') THEN region = [50,100, 300, 270]
  	IF (scheme EQ '15-3km' ) THEN region = [50, 90, 210, 190]
ENDIF


IF in_cloud EQ 1 THEN BEGIN 
	cld_title = 'in_cloud'
	ENDIF ELSE BEGIN
	cld_title = 'out_cloud'
ENDELSE

outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/plots/chemistry_scatter/'
epsfile = outdir + scheme + '_' + end_date + '_' + cld_title + '.eps'						;EPS filename
pdffile = outdir + scheme + '_' + end_date + '_' + cld_title + '.pdf'						;PDF filename
pngfile = outdir +  'nssl_racm-esrl_' + end_date + '_' + cld_title + '.png'						;PNG filename

FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(event, scheme, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(event, scheme, start_date, end_date, /DATE)	


IF KEYWORD_SET(eps) THEN BEGIN	
	PS_ON, FILENAME = epsfile, PAGE_SIZE = [10.0, 6.0], MARGIN = 0.0, /INCHES					;Switch to Postscript device
	DEVICE, /ENCAPSULATED
	!P.FONT     = 0																				;Hardware fonts
	!P.CHARSIZE = 1.5
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS																		;Load basic color definitions
ENDIF ELSE BEGIN
	SET_PLOT, 'X'
	WINDOW, XSIZE = 1200, YSIZE = 800															;Open graphics window
	!P.COLOR      = COLOR_24('black')															;Foreground color
	!P.BACKGROUND = COLOR_24('white')															;Background color
	!P.CHARSIZE   = 3.1		
	!P.FONT       = -1																			;Use Hershey fonts
ENDELSE
!P.MULTI = [0, 4, 2]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;											;Aircraft
gv_cloud  = (DC3_READ_VAR('CONC1DC100_LWIO', event)).values
dc8_cloud = (DC3_READ_VAR('cloud', event, /DC8)).values
			
gv_z  = (DC3_READ_VAR('GGALT', event)).values														;Read aircraft altitude
dc8_z = (DC3_READ_VAR('G_ALT', event, /DC8)).values

IF KEYWORD_SET(pv_relative) THEN BEGIN
	ytitle = '2-pvu Relative (km)'
	z_trop = DC3_READ_VAR('GFS_2PVU_HGT', event)													;Read tropopause level
ENDIF ELSE BEGIN	
	ytitle = 'Tropopause Relative (km)'
	gv_z_trop  = (DC3_READ_VAR('GFS_TROP_HGT', event)).values
 	dc8_z_trop = (DC3_READ_VAR('GFS_TROP_HGT', event, /DC8)).values
ENDELSE

IF KEYWORD_SET(in_cloud) THEN BEGIN																;Sort values in-cloud vs out of cloud
	dc8_values = WHERE((dc8_cloud EQ 2.0) OR (dc8_cloud EQ 3.0), dc8_count)					;Find DC8 in-cloud values
	IF (dc8_count GT 0) THEN BEGIN
		dc8_cloud  = dc8_cloud  [dc8_values]
		dc8_z      = dc8_z      [dc8_values]
		dc8_z_trop = dc8_z_trop [dc8_values]
	ENDIF
	gv_values = WHERE(gv_cloud GT 0.0, gv_count)										;Find GV in-cloud values
	IF (gv_count GT 0) THEN BEGIN
		gv_cloud  = gv_cloud  [gv_values]
		gv_z      = gv_z      [gv_values]
		gv_z_trop = gv_z_trop [gv_values]
	ENDIF
ENDIF ELSE BEGIN
	dc8_values  = WHERE(dc8_cloud EQ 0.0, dc8_count)										;Find DC8 non-cloud values
	IF (dc8_count GT 0) THEN BEGIN
		dc8_cloud  = dc8_cloud  [dc8_values]
		dc8_z      = dc8_z      [dc8_values]
		dc8_z_trop = dc8_z_trop [dc8_values]
	ENDIF
	gv_values = WHERE(gv_cloud EQ 0.0, gv_count)											;Find GV non-cloud values
	IF (gv_count GT 0) THEN BEGIN
		gv_cloud  = gv_cloud  [gv_values]
		gv_z      = gv_z      [gv_values]
		gv_z_trop = gv_z_trop [gv_values]
	ENDIF
ENDELSE

gv_z_trop  = CALC_TROP_MODE_OBS(gv_z_trop , threshold)
dc8_z_trop = CALC_TROP_MODE_OBS(dc8_z_trop, threshold)

z_obs      = [gv_z, dc8_z]
z_trop_obs = [gv_z_trop, dc8_z_trop]
ralt_obs   = (z_obs - z_trop_obs)

IF KEYWORD_SET(alt) THEN ralt_obs = z_obs 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;							;End Aircraft


title = STRING(scheme)
;title = 'nssl_racm-esrl'

z_arr 	    = [ ]																				;allocate arrays
xyz_troparr = [ ]
refl_arr    = [ ]
cloud_arr   = [ ]
o3_arr      = [ ] 
co_arr      = [ ] 
h2o_arr     = [ ] 
hno3_arr	= [ ]
nox_arr		= [ ]
so2_arr		= [ ]
hcho_arr	= [ ]
ch4_arr		= [ ]


FOREACH date, date_arr DO BEGIN

	z		 = (WRF_READ_VAR('Z'     , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read height values
	dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
	z_trop   = (WRF_READ_VAR('Z_trop', date, event, scheme, DOMAIN = domain, INDICES = region)).values
	z_trop   = CALC_TROP_MODE(z_trop, scheme, threshold) 												;Filter tropopause values
	xyz_trop = REBIN(z_trop, dim[0], dim[1], dim[2], /SAMPLE)
	filt     = WHERE(xyz_trop EQ 999999 , filt_count, COMPLEMENT = good, NCOMPLEMENT = good_count)

	IF (good_count GT 0) THEN BEGIN
		z_arr       = [z_arr      , z       [good]]														;concatenate arrays for each time
		xyz_troparr = [xyz_troparr, xyz_trop[good]]

		var         = (WRF_READ_VAR('REFL' 	      	  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read in reflectivity values
		refl_arr    = [refl_arr   , var     [good]]

		var   		= (WRF_READ_VAR('CLOUD_MIX_TOTAL' , date, event, scheme, DOMAIN = domain, INDICES = region)).values * 1.0E3			;Read in cloud mixing ratio values
		cloud_arr 	= [cloud_arr  , var     [good]]

		var      	= (WRF_READ_VAR('O3'      		  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values
		o3_arr   	= [o3_arr     , var     [good]]	

		var      	= (WRF_READ_VAR('CO'			  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read carbon monoxide data
		co_arr      = [co_arr	  , var  	[good]]

		var  	 	= (WRF_READ_VAR('H2O'		      , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read water vapor data 
		h2o_arr	    = [h2o_arr	  , var  	[good]]		

		var  	 	= (WRF_READ_VAR('HNO3'			  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read nitric acid data
		hno3_arr    = [hno3_arr   , var  	[good]]	

		var     	= (WRF_READ_VAR('NO'			  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read nitrogen oxide data 
		var  	   += (WRF_READ_VAR('NO2'			  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read nitrogen oxide data
		nox_arr     = [nox_arr	  , var  	[good]]		

		var  	 	= (WRF_READ_VAR('SO2'			  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read sulfur dioxide data 
		so2_arr		= [so2_arr 	  , var  	[good]]

		var  	 	= (WRF_READ_VAR('HCHO'			  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read formaldehyde data
		hcho_arr	= [hcho_arr   , var  	[good]]

		var  	 	= (WRF_READ_VAR('CH4'			  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ch4 data
		ch4_arr		= [ch4_arr	  , var  	[good]]													
	ENDIF
	 
IF KEYWORD_SET(alt) THEN BEGIN
	ytitle = 'Altitude (km)'
	yrange = [0, 20]

	var_gv  = (DC3_READ_VAR('FO3_ACD', event)).values												;Read ozone data
	var_dc8 = (DC3_READ_VAR('O3_CL', event, /DC8)).values	 
	WRF_OBS_TRACER_RALT, 1.0E3*o3_arr, 0.001*(z_arr), refl_arr, cloud_arr, $									;Call WRF_OBS_TRACER_RALT to produce scatterplots
				var_gv, var_dc8, 0.001*ralt_obs, gv_values, dc8_values, $
		TITLE     = title, $
		XRANGE    = [0, 400], $
		XTITLE    = 'Ozone (ppbv)', $
		YTITLE    = ytitle, $
		YRANGE    = yrange, $
		BINNED    = binned, $
		FILTERING = filtering, $
		IN_CLOUD  = in_cloud, $
		DBL_TROP  = dbl_trop, $
		NOERASE	  = 1, $
		NOWINDOW  = 1

	var_gv  = (DC3_READ_VAR('CO', event)).values																		;Read carbon monoxide data
	var_dc8 = (DC3_READ_VAR('CO_ppbv_DACOM', event, /DC8)).values
	WRF_OBS_TRACER_RALT, 1.0E3*co_arr, 0.001*(z_arr), refl_arr, cloud_arr, $
					var_gv, var_dc8, 0.001*ralt_obs, gv_values, dc8_values, $
		TITLE     = title, $
		XRANGE    = [0, 200], $
		XTITLE    = 'Carbon Monoxide (ppbv)', $
		YTITLE    = ytitle, $
		YRANGE    = yrange, $
		BINNED    = binned, $
		FILTERING = filtering, $
		IN_CLOUD  = in_cloud, $
		DBL_TROP  = dbl_trop, $
		NOERASE	  = 1, $
		NOWINDOW  = 1

	var_gv  = (DC3_READ_VAR('X_H2O', event)).values																		;Read water vapor data
	var_dc8 = (DC3_READ_VAR('H2O_vapor_ppmv_DLH', event, /DC8)).values
	WRF_OBS_TRACER_RALT, 1.0E6*h2o_arr, 0.001*(z_arr), refl_arr, cloud_arr, $
					var_gv, var_dc8, 0.001*ralt_obs, gv_values, dc8_values, $
		TITLE     = title, $
		XRANGE    = [1, 500], $
		XTITLE    = 'Water Vapor (ppmv)', $
		YTITLE    = ytitle, $
		YRANGE    = yrange, $
		BINNED    = binned, $
		FILTERING = filtering, $
		IN_CLOUD  = in_cloud, $
		DBL_TROP  = dbl_trop, $
		NOERASE	  = 1, $
		NOWINDOW  = 1

	var_gv  = (DC3_READ_VAR('HNO3', event)).values																		;Read nitric acid data
	var_dc8 = (DC3_READ_VAR('HNO3_SAGA', event, /DC8)).values	
;	var_dc8 = (DC3_READ_VAR('HNO3_CIT', event, /DC8)).values	
	WRF_OBS_TRACER_RALT, 1.0E6*hno3_arr, 0.001*(z_arr), refl_arr, cloud_arr, $
					var_gv, var_dc8, 0.001*ralt_obs, gv_values, dc8_values, $
		TITLE     = title, $
		XRANGE    = [0, 3000], $
		XTITLE    = 'Nitric Acid (pptv)', $
		XTICKS    = 3, $
		XMINOR    = 4, $
		YTITLE    = ytitle, $
		YRANGE    = yrange, $
		BINNED    = binned, $
		FILTERING = filtering, $
		IN_CLOUD  = in_cloud, $
		DBL_TROP  = dbl_trop, $
		NOERASE	  = 1, $
		NOWINDOW  = 1

	var_gv  = (DC3_READ_VAR('NO+NO2', event)).values																		;Read nitrogen oxide data
	var_dc8 = (DC3_READ_VAR('NO2_LIF', event, /DC8)).values	
	WRF_OBS_TRACER_RALT, 1.0E6*nox_arr, 0.001*(z_arr), refl_arr, cloud_arr, $
					 var_gv, var_dc8, 0.001*ralt_obs, gv_values, dc8_values, $
		TITLE     = title, $
		XRANGE    = [0, 4000], $
		XTICKS    = 3, $
		XMINOR    = 4, $
		XTITLE    = 'NOx (pptv)', $
		YTITLE    = ytitle, $
		YRANGE    = yrange, $
		BINNED    = binned, $
		FILTERING = filtering, $
		IN_CLOUD  = in_cloud, $
		DBL_TROP  = dbl_trop, $
		NOERASE	  = 1, $
		NOWINDOW  = 1

	var_gv  = (DC3_READ_VAR('SO2', event)).values																			;Read carbon dioxide data
	var_dc8 = (DC3_READ_VAR('SO2_GTCIMS', event, /DC8)).values
	WRF_OBS_TRACER_RALT, 1.0E6*so2_arr, 0.001*(z_arr), refl_arr, cloud_arr, $
					var_gv, var_dc8, 0.001*ralt_obs, gv_values, dc8_values, $
		TITLE     = title, $
		XRANGE    = [0, 25], $
		XTICKS    = 3, $
		XMINOR    = 4, $
		XTITLE    = 'Sulfur Dioxide (pptv)', $
		YTITLE    = ytitle, $
		YRANGE    = yrange, $
		BINNED    = binned, $
		FILTERING = filtering, $
		IN_CLOUD  = in_cloud, $
		DBL_TROP  = dbl_trop, $
		NOERASE	  = 1, $
		NOWINDOW  = 1

	var_gv  = (DC3_READ_VAR('CH2O_CAMS_pptv', event)).values														;Read formaldehyde data
	var_dc8 = (DC3_READ_VAR('CH2O_DFGAS_pptv', event, /DC8)).values	
	WRF_OBS_TRACER_RALT, 1.0E6*hcho_arr, 0.001*(z_arr), refl_arr, cloud_arr, $
					 var_gv, var_dc8, 0.001*ralt_obs, gv_values, dc8_values, $
		TITLE     = title, $
		XRANGE    = [0, 3000], $
		XTICKS    = 3, $
		XMINOR    = 4, $
		XTITLE    = 'Formaldehyde(pptv)', $
		YTITLE    = ytitle, $
		YRANGE    = yrange, $
		BINNED    = binned, $
		FILTERING = filtering, $
		IN_CLOUD  = in_cloud, $
		DBL_TROP  = dbl_trop, $
		NOERASE	  = 1, $
		NOWINDOW  = 1

	var_gv  = (DC3_READ_VAR('Methane', event)).values																;Read methane data
	var_dc8 = (DC3_READ_VAR('CH4_ppbv_DACOM', event, /DC8)).values
	WRF_OBS_TRACER_RALT, 1.0E3*ch4_arr, 0.001*(z_arr), refl_arr, cloud_arr, $
					 var_gv, var_dc8, 0.001*ralt_obs, gv_values, dc8_values, $
		TITLE     = title, $
		XRANGE    = [1700, 1900], $
		XTICKS    = 3, $
		XMINOR    = 4, $
		XTITLE    = 'ch4 (ppbv)', $
		YTITLE    = ytitle, $
		YRANGE    = yrange, $
		BINNED    = binned, $
		FILTERING = filtering, $
		IN_CLOUD  = in_cloud, $
		DBL_TROP  = dbl_trop, $ 
		NOERASE	  = 1, $
		NOWINDOW  = 1

ENDIF ELSE BEGIN

	var_gv  = (DC3_READ_VAR('FO3_ACD', event)).values												;Read ozone data
	var_dc8 = (DC3_READ_VAR('O3_CL', event, /DC8)).values	 
	WRF_OBS_TRACER_RALT, 1.0E3*o3_arr, 0.001*(z_arr-xyz_troparr), refl_arr, cloud_arr, $									;Call WRF_OBS_TRACER_RALT to produce scatterplots
					var_gv, var_dc8, 0.001*ralt_obs, gv_values, dc8_values, $
		TITLE     = title, $
		XRANGE    = [0, 400], $
		XTITLE    = 'Ozone (ppbv)', $
		YTITLE    = ytitle, $
		BINNED    = binned, $
		FILTERING = filtering, $
		IN_CLOUD  = in_cloud, $
		DBL_TROP  = dbl_trop, $
		NOERASE	  = 1, $
		NOWINDOW  = 1

	var_gv  = (DC3_READ_VAR('CO', event)).values																		;Read carbon monoxide data
	var_dc8 = (DC3_READ_VAR('CO_ppbv_DACOM', event, /DC8)).values
	WRF_OBS_TRACER_RALT, 1.0E3*co_arr, 0.001*(z_arr-xyz_troparr), refl_arr, cloud_arr, $
					var_gv, var_dc8, 0.001*ralt_obs, gv_values, dc8_values, $
		TITLE     = title, $
		XRANGE    = [0, 200], $
		XTITLE    = 'Carbon Monoxide (ppbv)', $
		YTITLE    = ytitle, $
		BINNED    = binned, $
		FILTERING = filtering, $
		IN_CLOUD  = in_cloud, $
		DBL_TROP  = dbl_trop, $
		NOERASE	  = 1, $
		NOWINDOW  = 1

	var_gv  = (DC3_READ_VAR('X_H2O', event)).values																		;Read water vapor data
	var_dc8 = (DC3_READ_VAR('H2O_vapor_ppmv_DLH', event, /DC8)).values
	WRF_OBS_TRACER_RALT, 1.0E6*h2o_arr, 0.001*(z_arr-xyz_troparr), refl_arr, cloud_arr, $
					var_gv, var_dc8, 0.001*ralt_obs, gv_values, dc8_values, $
		TITLE     = title, $
		XRANGE    = [1, 500], $
		XTITLE    = 'Water Vapor (ppmv)', $
		YTITLE    = ytitle, $
		BINNED    = binned, $
		FILTERING = filtering, $
		IN_CLOUD  = in_cloud, $
		DBL_TROP  = dbl_trop, $
		NOERASE	  = 1, $
		NOWINDOW  = 1

	var_gv  = (DC3_READ_VAR('HNO3', event)).values																		;Read nitric acid data
	var_dc8 = (DC3_READ_VAR('HNO3_SAGA', event, /DC8)).values	
	WRF_OBS_TRACER_RALT, 1.0E6*hno3_arr, 0.001*(z_arr-xyz_troparr), refl_arr, cloud_arr, $
					var_gv, var_dc8, 0.001*ralt_obs, gv_values, dc8_values, $
		TITLE     = title, $
		XRANGE    = [0, 3000], $
		XTITLE    = 'Nitric Acid (pptv)', $
		XTICKS    = 3, $
		XMINOR    = 4, $
		YTITLE    = ytitle, $
		BINNED    = binned, $
		FILTERING = filtering, $
		IN_CLOUD  = in_cloud, $
		DBL_TROP  = dbl_trop, $
		NOERASE	  = 1, $
		NOWINDOW  = 1

	var_gv  = (DC3_READ_VAR('NO+NO2', event)).values																		;Read nitrogen oxide data
	var_dc8 = (DC3_READ_VAR('NO2_LIF', event, /DC8)).values	
	WRF_OBS_TRACER_RALT, 1.0E6*nox_arr, 0.001*(z_arr-xyz_troparr), refl_arr, cloud_arr, $
					 var_gv, var_dc8, 0.001*ralt_obs, gv_values, dc8_values, $
		TITLE     = title, $
		XRANGE    = [0, 4000], $
		XTICKS    = 3, $
		XMINOR    = 4, $
		XTITLE    = 'NOx (pptv)', $
		YTITLE    = ytitle, $
		BINNED    = binned, $
		FILTERING = filtering, $
		IN_CLOUD  = in_cloud, $
		DBL_TROP  = dbl_trop, $
		NOERASE	  = 1, $
		NOWINDOW  = 1

	var_gv  = (DC3_READ_VAR('SO2', event)).values																			;Read carbon dioxide data
	var_dc8 = (DC3_READ_VAR('SO2_GTCIMS', event, /DC8)).values
	WRF_OBS_TRACER_RALT, 1.0E6*so2_arr, 0.001*(z_arr-xyz_troparr), refl_arr, cloud_arr, $
					var_gv, var_dc8, 0.001*ralt_obs, gv_values, dc8_values, $
		TITLE     = title, $
		XRANGE    = [0, 250], $
		XTICKS    = 3, $
		XMINOR    = 4, $
		XTITLE    = 'Sulfur Dioxide (pptv)', $
		YTITLE    = ytitle, $
		BINNED    = binned, $
		FILTERING = filtering, $
		IN_CLOUD  = in_cloud, $
		DBL_TROP  = dbl_trop, $
		NOERASE	  = 1, $
		NOWINDOW  = 1

	var_gv  = (DC3_READ_VAR('CH2O_CAMS_pptv', event)).values														;Read formaldehyde data
	var_dc8 = (DC3_READ_VAR('CH2O_DFGAS_pptv', event, /DC8)).values	
	WRF_OBS_TRACER_RALT, 1.0E6*hcho_arr, 0.001*(z_arr-xyz_troparr), refl_arr, cloud_arr, $
					 var_gv, var_dc8, 0.001*ralt_obs, gv_values, dc8_values, $
		TITLE     = title, $
		XRANGE    = [0, 3000], $
		XTICKS    = 3, $
		XMINOR    = 4, $
		XTITLE    = 'Formaldehyde(pptv)', $
		YTITLE    = ytitle, $
		BINNED    = binned, $
		FILTERING = filtering, $
		IN_CLOUD  = in_cloud, $
		DBL_TROP  = dbl_trop, $
		NOERASE	  = 1, $
		NOWINDOW  = 1

	var_gv  = (DC3_READ_VAR('Methane', event)).values																;Read methane data
	var_dc8 = (DC3_READ_VAR('CH4_ppbv_DACOM', event, /DC8)).values
	WRF_OBS_TRACER_RALT, 1.0E3*ch4_arr, 0.001*(z_arr-xyz_troparr), refl_arr, cloud_arr, $
					 var_gv, var_dc8, 0.001*ralt_obs, gv_values, dc8_values, $
		TITLE     = title, $
		XRANGE    = [1700, 1900], $
		XTICKS    = 3, $
		XMINOR    = 4, $
		XTITLE    = 'ch4 (ppbv)', $
		YTITLE    = ytitle, $
		BINNED    = binned, $
		FILTERING = filtering, $
		IN_CLOUD  = in_cloud, $
		DBL_TROP  = dbl_trop, $ 
		NOERASE	  = 1, $
		NOWINDOW  = 1
ENDELSE

ENDFOREACH 

	


!P.MULTI = 0
IF KEYWORD_SET(eps) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																;Reset color table to linear ramp
	PS_OFF																						;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)															;Write PNG file

END