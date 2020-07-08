PRO PLOT_WRF_HYDROMETEOR_RALT, date, date1, event, scheme, $
	DOMAIN		= domain, $
	BINNED      = binned, $
	FILTERING   = filtering, $
	THRESHOLD   = threshold, $
	IN_CLOUD    = in_cloud, $
	PDF			= pdf, $
	EPS         = eps, $
	PNG         = png

;+
; Name:
;		PLOT_WRF_HYDROMETEOR_RALT
; Purpose:
;		This is a procedure to call the procedure to
;	    plot WRF hydrometeors in relative altitude to the tropopause. 
; Calling sequence:
;		PLOT_WRF_HYDROMETEOR_RALT
; Input:
;		date     : analysis date (e.g., '20120519T2230Z')
;		case     : string of case date (e.g., '20120519')
;		scheme   : microphysics scheme (e.g., 'morrison')
; Output:
;		plots of trace gases at relative altitudes to the tropopause
; Keywords:
;		DOMAIN    : Simulation domain number. Default is 2. 
;		BINNED    : Set to bin gas concentrations.
;		FILTERING : Set to filter out values where REFL > 30 dBZ.
;		THRESHOLD : Distance above tropopause mode to reset to mode (e.g., 2000.0 m)
;		IN_CLOUD  : Set to plot values where cloud is simulated (> 1 L-1)
;		EPS       : If set, output to PostScript.
;		PDF       : If set, output to PDF.
;		PNG       : If set, write PNG image.
; Author and history:
;		Daniel B. Phoenix	2016-01-12
;-

COMPILE_OPT IDL2		
																								;Set compile options
IF (N_ELEMENTS (date	  ) EQ 0) THEN date 	  =  MAKE_DATE(2012,5,19,22,00)
IF (N_ELEMENTS (event	  ) EQ 0) THEN event      = '20120519'
IF (N_ELEMENTS (scheme	  ) EQ 0) THEN scheme     = 'morrison'
IF (N_ELEMENTS (domain    ) EQ 0) THEN domain     = 2
IF (N_ELEMENTS (threshold )	EQ 0) THEN threshold  = 1000.0 
IF (N_ELEMENTS (in_cloud  )	EQ 0) THEN in_cloud   = 1

IF in_cloud EQ 1 THEN BEGIN 
	cld_title = 'in_cloud'
	ENDIF ELSE BEGIN
	cld_title = 'out_cloud'
ENDELSE

date_arr = MK_DATE_ARR(event, scheme, date, date1, /DATE)	

;dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")												;Set domain string
;date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string
;
;outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/plots/hydrometeor/'
;epsfile = outdir + scheme + '_' + date_string + '_hydrometeor.eps'							;EPS filename
;pdffile = outdir + scheme + '_' + date_string + '_hydrometeor.pdf'							;PDF filename
;pngfile = outdir + scheme + '_' + date_string + '_hydrometeor.png'							;PNG filename
;
;FILE_MKDIR, outdir

IF KEYWORD_SET(eps) THEN BEGIN	
	PS_ON, FILENAME = epsfile, PAGE_SIZE = [6.0, 4.0], MARGIN = 0.0, /INCHES					;Switch to Postscript device
	DEVICE, /ENCAPSULATED
	!P.FONT     = 0																				;Hardware fonts
	!P.CHARSIZE = 0.8
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS																		;Load basic color definitions
ENDIF ELSE BEGIN
	SET_PLOT, 'X'
	WINDOW, XSIZE = 900, YSIZE = 800															;Open graphics window
	!P.COLOR      = COLOR_24('black')															;Foreground color
	!P.BACKGROUND = COLOR_24('white')															;Background color
	!P.CHARSIZE   = 3.1		
	!P.FONT       = -1																			;Use Hershey fonts
ENDELSE
!P.MULTI = [0, 3, 2]

;z = WRF_READ_VAR('Z', date, event, scheme, DOMAIN = domain)										;Read geopotential height data (x,y,z)
;dim = SIZE(z.values, /DIMENSIONS)																;Get geopotential height dimensions
;z = z.values
;
;ytitle = 'Tropopause Relative (km)'																;Set tropopause relative y-label
;z_trop = (WRF_READ_VAR('Z_trop', date, event, scheme, DOMAIN = domain)).values					;Read tropopause height data (x,y)
;z_trop = CALC_TROP_MODE(z_trop, scheme, threshold) 												;Filter tropopause values
;xyz_trop = REBIN(z_trop, dim[0], dim[1], dim[2], /SAMPLE)										;Rebin tropopause height to dimensions of geopotential height data
;
;filt = WHERE(xyz_trop EQ 999999, filt_count, COMPLEMENT = good, NCOMPLEMENT = good_count)
;
;refl  = (WRF_READ_VAR('REFL' 	, date, event, scheme, DOMAIN = domain)).values							;Read in reflectivity values
;cloud = WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, DOMAIN = domain)							;Read in cloud mixing ratio values
;cloud = cloud.values * 1.0E3																	;Convert mixing ratio to g kg-1
;
;IF (good_count GT 0) THEN BEGIN
;	cloud    = cloud   [good]
;	xyz_trop = xyz_trop[good]
;	z	     = z	   [good]
;ENDIF

title = STRING(scheme)

;;;;;;;;;;;;;;
z_arr 	    = [ ]																				;allocate arrays
xyz_troparr = [ ]
refl_arr    = [ ]
cldtot_arr  = [ ]
cld_arr     = [ ] 
rain_arr    = [ ] 
snow_arr    = [ ] 
grau_arr	= [ ]
hail_arr    = [ ]
ice_arr		= [ ]



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

		var   		= (WRF_READ_VAR('CLOUD_MIX_TOTAL' , date, event, scheme, DOMAIN = domain, INDICES = region)).values 		;Read in cloud mixing ratio values
		cldtot_arr 	= [cldtot_arr , var     [good]]

		var      	= (WRF_READ_VAR('CLOUD_MIX'       , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values
		cld_arr   	= [cld_arr    , var     [good]]	

		var      	= (WRF_READ_VAR('RAIN_MIX'	      , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read carbon monoxide data
		rain_arr    = [rain_arr	  , var  	[good]]

		var  	 	= (WRF_READ_VAR('SNOW_MIX'	      , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read water vapor data 
		snow_arr	= [snow_arr	  , var     [good]]		

		var  	 	= (WRF_READ_VAR('GRAUPEL_MIX'	  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read nitric acid data
		grau_arr    = [grau_arr   , var  	[good]]	

		var  	 	= (WRF_READ_VAR('ICE_MIX'		  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read sulfur dioxide data 
		ice_arr		= [ice_arr 	  , var  	[good]]


	ENDIF
	 
;;;;;;;;;;;;;;

;var = (WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, DOMAIN = domain)).values								;Read formaldehyde data
;var = var[good]
WRF_TRACER_RALT, 1.0E3*cldtot_arr, 0.001*(z-xyz_trop), cldtot_arr, $
	TITLE     = title, $
	XRANGE    = [0, 10], $
	XTICKS    = 3, $
	XMINOR    = 4, $
	XTITLE    = 'Total Cloud Mixing Ratio (g/kg)', $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	FILTERING = filtering, $
	IN_CLOUD  = in_cloud, $
	DBL_TROP  = dbl_trop, $
	NOERASE	  = 1, $
	NOWINDOW  = 1

;var = (WRF_READ_VAR('CLOUD_MIX', date, event, scheme, DOMAIN = domain)).values							;Read ozone data
;var = var[good]
WRF_TRACER_RALT, 1.0E3*cld_arr, 0.001*(z-xyz_trop), cldtot_arr, $
	TITLE     = title, $
	XRANGE    = [0, 2], $
	XTITLE    = 'Cloud (g/kg)', $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	FILTERING = filtering, $
	IN_CLOUD  = in_cloud, $
	DBL_TROP  = dbl_trop, $
	NOERASE	  = 1, $
	NOWINDOW  = 1

;var = (WRF_READ_VAR('RAIN_MIX', date, event, scheme, DOMAIN = domain)).values							;Read carbon monoxide data
;var = var[good]
WRF_TRACER_RALT, 1.0E3*rain_arr, 0.001*(z-xyz_trop), cldtot_arr, $
	TITLE     = title, $
	XRANGE    = [0, 10], $
	XTITLE    = 'Rain (g/kg)', $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	FILTERING = filtering, $
	IN_CLOUD  = in_cloud, $
	DBL_TROP  = dbl_trop, $
	NOERASE	  = 1, $
	NOWINDOW  = 1

;var = (WRF_READ_VAR('SNOW_MIX', date, event, scheme, DOMAIN = domain)).values									;Read water vapor data
;var = var[good]
WRF_TRACER_RALT, 1.0E3*snow_arr, 0.001*(z-xyz_trop), cldtot_arr, $
	TITLE     = title, $
	XRANGE    = [0, 500000000], $
	XTITLE    = 'Snow (g/kg)', $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	FILTERING = filtering, $
	IN_CLOUD  = in_cloud, $
	DBL_TROP  = dbl_trop, $
	NOERASE	  = 1, $
	NOWINDOW  = 1

;var = (WRF_READ_VAR('GRAUPEL_MIX', date, event, scheme, DOMAIN = domain)).values								;Read nitric acid data
;var = var[good]
WRF_TRACER_RALT, 1.0E3*grau_arr, 0.001*(z-xyz_trop), cldtot_arr, $
	TITLE     = title, $
	XRANGE    = [0, 50000000], $
	XTITLE    = 'Graupel (g/kg)', $
	XTICKS    = 3, $
	XMINOR    = 4, $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	FILTERING = filtering, $
	IN_CLOUD  = in_cloud, $
	DBL_TROP  = dbl_trop, $
	NOERASE	  = 1, $
	NOWINDOW  = 1

;var = (WRF_READ_VAR('ICE_MIX', date, event, scheme, DOMAIN = domain)).values									;Read nitrogen oxide data
;var = var[good]
WRF_TRACER_RALT, 1.0E3*ice_arr, 0.001*(z-xyz_trop), cldtot_arr, $
	TITLE     = title, $
	XRANGE    = [0.1, 30000000000], $
	XTICKS    = 4, $
	XMINOR    = 3, $
	XTITLE    = 'Ice (g/kg)', $
	YTITLE    = ytitle, $
	XLOG	  = 1, $
	BINNED    = binned, $
	FILTERING = filtering, $
	IN_CLOUD  = in_cloud, $
	DBL_TROP  = dbl_trop, $
	NOERASE	  = 1, $
	NOWINDOW  = 1

;IF scheme NE 'morrison' THEN BEGIN
;;	var = (WRF_READ_VAR('HAIL_MIX', date, event, scheme, DOMAIN = domain)).values									;Read carbon dioxide data
;;	var = var[good]
;	WRF_OBS_TRACER_RALT, 1.0E3*hail_arr, 0.001*(z-xyz_trop), cldtot_arr, $
;		TITLE     = title, $
;		XRANGE    = [0, 10], $
;		XTICKS    = 3, $
;		XMINOR    = 4, $
;		XTITLE    = 'Hail (g/kg)', $
;		YTITLE    = ytitle, $
;		BINNED    = binned, $
;		FILTERING = filtering, $
;		IN_CLOUD  = in_cloud, $
;		DBL_TROP  = dbl_trop, $
;		NOWINDOW  = 1
;ENDIF

ENDFOREACH

!P.MULTI = 0
IF KEYWORD_SET(eps) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																;Reset color table to linear ramp
	PS_OFF																						;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)															;Write PNG file

END