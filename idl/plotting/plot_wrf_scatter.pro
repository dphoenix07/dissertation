PRO PLOT_WRF_SCATTER, event, scheme, start_date, end_date, $
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
;		DOMAIN  : Optional keyword to specify output domain name. Default is 2.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel B. Phoenix	    2016-07-13.	Like 'plot_wrf_obs_scatter.pro', but
;											without aircraft observations overlaid.  
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
pngfile = outdir + scheme + '_' + end_date + '_' + cld_title + '.png'						;PNG filename

FILE_MKDIR, outdir

date_arr = MK_DATE_ARR(event, scheme, start_date, end_date, /DATE)	


IF KEYWORD_SET(eps) THEN BEGIN	
	PS_ON, FILENAME = epsfile, PAGE_SIZE = [6.0, 4.0], MARGIN = 0.0, /INCHES					;Switch to Postscript device
	DEVICE, /ENCAPSULATED
	!P.FONT     = 0																				;Hardware fonts
	!P.CHARSIZE = 0.8
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

title = STRING(scheme)

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

		var  	 	= (WRF_READ_VAR('HCL'			  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ch4 data
		ch4_arr		= [ch4_arr	  , var  	[good]]													
	ENDIF
	 
IF KEYWORD_SET(alt) THEN BEGIN
	ytitle = 'Altitude (km)'
	yrange = [0, 20]

	WRF_TRACER_RALT, 1.0E3*o3_arr, 0.001*(z_arr), refl_arr, cloud_arr, $									;Call WRF_OBS_TRACER_RALT to produce scatterplots
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

	WRF_TRACER_RALT, 1.0E3*co_arr, 0.001*(z_arr), refl_arr, cloud_arr, $
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

	WRF_TRACER_RALT, 1.0E6*h2o_arr, 0.001*(z_arr), refl_arr, cloud_arr, $
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

	WRF_TRACER_RALT, 1.0E6*hno3_arr, 0.001*(z_arr), refl_arr, cloud_arr, $
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

	WRF_TRACER_RALT, 1.0E6*nox_arr, 0.001*(z_arr), refl_arr, cloud_arr, $
		TITLE     = title, $
		XRANGE    = [0, 2000], $
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

	WRF_TRACER_RALT, 1.0E6*so2_arr, 0.001*(z_arr), refl_arr, cloud_arr, $
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

	WRF_TRACER_RALT, 1.0E6*hcho_arr, 0.001*(z_arr), refl_arr, cloud_arr, $
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

	WRF_TRACER_RALT, 1.0E3*ch4_arr, 0.001*(z_arr), refl_arr, cloud_arr, $
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

	WRF_TRACER_RALT, 1.0E3*o3_arr, 0.001*(z_arr-xyz_troparr), refl_arr, cloud_arr, $									;Call WRF_OBS_TRACER_RALT to produce scatterplots
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

	WRF_TRACER_RALT, 1.0E3*co_arr, 0.001*(z_arr-xyz_troparr), refl_arr, cloud_arr, $
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

	WRF_TRACER_RALT, 1.0E6*h2o_arr, 0.001*(z_arr-xyz_troparr), refl_arr, cloud_arr, $
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

	WRF_TRACER_RALT, 1.0E6*hno3_arr, 0.001*(z_arr-xyz_troparr), refl_arr, cloud_arr, $
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

	WRF_TRACER_RALT, 1.0E6*nox_arr, 0.001*(z_arr-xyz_troparr), refl_arr, cloud_arr, $
		TITLE     = title, $
		XRANGE    = [0, 2000], $
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

	WRF_TRACER_RALT, 1.0E6*so2_arr, 0.001*(z_arr-xyz_troparr), refl_arr, cloud_arr, $
		TITLE     = title, $
		XRANGE    = [0, 25], $
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

	WRF_TRACER_RALT, 1.0E6*hcho_arr, 0.001*(z_arr-xyz_troparr), refl_arr, cloud_arr, $
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

	WRF_TRACER_RALT, 1.0E3*ch4_arr, 0.001*(z_arr-xyz_troparr), refl_arr, cloud_arr, $
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