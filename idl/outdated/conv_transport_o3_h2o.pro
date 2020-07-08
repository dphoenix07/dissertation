PRO CONV_TRANSPORT_O3_H2O, event, scheme, start_date, end_date, $
	DOMAIN     = domain, $
	BINNED     = binned, $
	UPDRAFT_TR = updraft_tr, $
	REGION     = region, $
	ALT		   = alt, $
	PNG	       = png, $
	EPS   	   = eps


;+
; Name:
;		CONV_TRANSPORT_O3_H2O
; Purpose:
;		Look at bulk convective transport of o3 and h2o
; Calling sequence:
;		CONV_TRANSPORT_O3_H2O, run, scheme, start_date, end_date
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
;		Daniel B. Phoenix	    2017-07-25. 
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20110518T1200Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20110520T1200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain
IF (N_ELEMENTS(in_cloud  ) EQ 0) THEN in_cloud   = 1


outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/plots/chemistry_scatter/'
epsfile = outdir + scheme + '_' + end_date + '.eps'						;EPS filename
pdffile = outdir + scheme + '_' + end_date + '.pdf'						;PDF filename
pngfile = outdir +  'nssl_racm-esrl_' + end_date + '.png'						;PNG filename

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
	WINDOW, XSIZE = 900, YSIZE = 800															;Open graphics window
	!P.COLOR      = COLOR_24('black')															;Foreground color
	!P.BACKGROUND = COLOR_24('white')															;Background color
	!P.CHARSIZE   = 3.1		
	!P.FONT       = -1																			;Use Hershey fonts
ENDELSE
!P.MULTI = [0, 3, 2]

title = 'Convective-cloud'
IF KEYWORD_SET(updraft_tr) THEN title = 'Convective-updraft'


z_arr 	    = [ ]																				;allocate arrays
xyz_troparr = [ ]
utls_arr    = [ ]
cloud_arr   = [ ]
o3_arr      = [ ] 
co_arr      = [ ] 
h2o_arr     = [ ] 
cld_tr_arr  = [ ]
updraft_arr = [ ] 

FOREACH date, date_arr DO BEGIN
	PRINT, date
	
	z		 = (WRF_READ_VAR('Z'     , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read height values
	dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
	z_trop   = (WRF_READ_VAR('Z_trop', date, event, scheme, DOMAIN = domain, INDICES = region)).values
	z_trop   = CALC_TROP_MODE(z_trop, scheme, threshold) 												;Filter tropopause values
	xyz_trop = REBIN(z_trop, dim[0], dim[1], dim[2], /SAMPLE)
	filt     = WHERE(xyz_trop EQ 999999 , filt_count, COMPLEMENT = good, NCOMPLEMENT = good_count)

	IF (good_count GT 0) THEN BEGIN
		z_arr       = [z_arr       , z       [good]]														;concatenate arrays for each time
		xyz_troparr = [xyz_troparr , xyz_trop[good]]

		var         = (WRF_READ_VAR('UTLS_tracer' 	  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read in reflectivity values
		utls_arr    = [utls_arr    , var     [good]]

		var   		= (WRF_READ_VAR('CLOUD_MIX_TOTAL' , date, event, scheme, DOMAIN = domain, INDICES = region)).values * 1.0E3			;Read in cloud mixing ratio values
		cloud_arr 	= [cloud_arr   , var     [good]]

		var      	= (WRF_READ_VAR('O3'      		  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values
		o3_arr   	= [o3_arr      , var     [good]]	

		var      	= (WRF_READ_VAR('CO'			  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read carbon monoxide data
		co_arr      = [co_arr	   , var  	[good]]

		var  	 	= (WRF_READ_VAR('H2O'		      , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read water vapor data 
		h2o_arr	    = [h2o_arr	   , var  	[good]]		

		var      	= (WRF_READ_VAR('Cloud_tracer'	  , date, event, scheme, DOMAIN = domain, INDICES = region)).values
		cld_tr_arr  = [cld_tr_arr  , var 	[good]]

		var      	= (WRF_READ_VAR('Updraft_tracer'  , date, event, scheme, DOMAIN = domain, INDICES = region)).values
		updraft_arr  = [updraft_arr, var 	[good]]

	ENDIF
	 
IF KEYWORD_SET(alt) THEN BEGIN
	ytitle = 'Altitude (km)'
	yrange = [0, 20]

	PLOT_CONV_TRANSPORT, 1.0E3*o3_arr, 0.001*(z_arr), updraft_arr, cld_tr_arr, $									;Call PLOT_CONV_TRANSPORT to produce scatterplots
		TITLE      = title, $
		XRANGE     = [0, 400], $
		XTITLE     = 'Ozone (ppbv)', $
		YTITLE     = ytitle, $
		YRANGE     = yrange, $
		BINNED     = 1, $
		FILTERING  = filtering, $
		CONVECTIVE = 1, $
		UPDRAFT_TR = updraft_tr, $
		DBL_TROP   = dbl_trop, $
		NOERASE	   = 1, $
		NOWINDOW   = 1

	PLOT_CONV_TRANSPORT, 1.0E3*co_arr, 0.001*(z_arr), updraft_arr, cld_tr_arr, $	
		TITLE      = title, $
		XRANGE     = [0, 200], $
		XTITLE     = 'Carbon Monoxide (ppbv)', $
		YTITLE     = ytitle, $
		YRANGE     = yrange, $
		BINNED     = 1, $
		FILTERING  = filtering, $
		CONVECTIVE = 1, $
		UPDRAFT_TR	   = updraft_tr, $
		DBL_TROP   = dbl_trop, $
		NOERASE	   = 1, $
		NOWINDOW   = 1

	PLOT_CONV_TRANSPORT, 1.0E6*h2o_arr, 0.001*(z_arr), updraft_arr, cld_tr_arr, $	
		TITLE      = title, $
		XRANGE     = [1, 500], $
		XTITLE     = 'Water Vapor (ppmv)', $
		YTITLE     = ytitle, $
		YRANGE     = yrange, $
		BINNED     = 1, $
		FILTERING  = filtering, $
		CONVECTIVE = 1, $
		UPDRAFT_TR	   = updraft_tr, $
		DBL_TROP   = dbl_trop, $
		NOERASE	   = 1, $
		NOWINDOW   = 1

	PLOT_CONV_TRANSPORT, 1.0E3*o3_arr, 0.001*(z_arr), updraft_arr, cld_tr_arr, $									;Call PLOT_CONV_TRANSPORT to produce scatterplots
		TITLE      = 'Non-convective', $
		XRANGE     = [0, 400], $
		XTITLE     = 'Ozone (ppbv)', $
		YTITLE     = ytitle, $
		YRANGE     = yrange, $
		BINNED     = 1, $
		FILTERING  = filtering, $
		CONVECTIVE = 0, $
		UPDRAFT_TR	   = updraft_tr, $
		DBL_TROP   = dbl_trop, $
		NOERASE	   = 1, $
		NOWINDOW   = 1

	PLOT_CONV_TRANSPORT, 1.0E3*co_arr, 0.001*(z_arr), updraft_arr, cld_tr_arr, $	
		TITLE      = 'Non-convective', $
		XRANGE     = [0, 200], $
		XTITLE     = 'Carbon Monoxide (ppbv)', $
		YTITLE     = ytitle, $
		YRANGE     = yrange, $
		BINNED     = 1, $
		FILTERING  = filtering, $
		CONVECTIVE = 0, $
		UPDRAFT_TR	   = updraft_tr, $
		DBL_TROP   = dbl_trop, $
		NOERASE	   = 1, $
		NOWINDOW   = 1

	PLOT_CONV_TRANSPORT, 1.0E6*h2o_arr, 0.001*(z_arr), updraft_arr, cld_tr_arr, $	
		TITLE      = 'Non-convective', $
		XRANGE     = [1, 500], $
		XTITLE     = 'Water Vapor (ppmv)', $
		YTITLE     = ytitle, $
		YRANGE     = yrange, $
		BINNED     = 1, $
		FILTERING  = filtering, $
		CONVECTIVE = 0, $
		UPDRAFT_TR	   = updraft_tr, $
		DBL_TROP   = dbl_trop, $
		NOERASE	   = 1, $
		NOWINDOW   = 1

ENDIF ELSE BEGIN

	PLOT_CONV_TRANSPORT, 1.0E3*o3_arr, 0.001*(z_arr-xyz_troparr), updraft_arr, cld_tr_arr, $										;Call PLOT_CONV_TRANSPORT to produce scatterplots
		TITLE      = title, $
		XRANGE     = [0, 400], $
		XTITLE     = 'Ozone (ppbv)', $
		YTITLE     = ytitle, $
		BINNED     = 1, $
		FILTERING  = filtering, $
		CONVECTIVE = 1, $
		UPDRAFT_TR = updraft_tr, $
		DBL_TROP   = dbl_trop, $
		NOERASE	   = 1, $
		NOWINDOW   = 1

	PLOT_CONV_TRANSPORT, 1.0E3*co_arr, 0.001*(z_arr-xyz_troparr), updraft_tr_arr, cld_tr_arr, $	
		TITLE      = title, $
		XRANGE     = [0, 200], $
		XTITLE     = 'Carbon Monoxide (ppbv)', $
		YTITLE     = ytitle, $
		BINNED     = 1, $
		FILTERING  = filtering, $
		CONVECTIVE = 1, $
		UPDRAFT_TR = updraft_tr, $
		DBL_TROP   = dbl_trop, $
		NOERASE	   = 1, $
		NOWINDOW   = 1

	PLOT_CONV_TRANSPORT, 1.0E6*h2o_arr, 0.001*(z_arr-xyz_troparr), updraft_tr_arr, cld_tr_arr, $	
		TITLE      = title, $
		XRANGE     = [1, 500], $
		XTITLE     = 'Water Vapor (ppmv)', $
		YTITLE     = ytitle, $
		BINNED     = 1, $
		FILTERING  = filtering, $
		CONVECTIVE = 1, $
		UPDRAFT_TR = updraft_tr, $
		DBL_TROP   = dbl_trop, $
		NOERASE	   = 1, $
		NOWINDOW   = 1

	PLOT_CONV_TRANSPORT, 1.0E3*o3_arr, 0.001*(z_arr-xyz_troparr), updraft_tr_arr, cld_tr_arr, $										;Call PLOT_CONV_TRANSPORT to produce scatterplots
		TITLE      = 'Non-convective', $
		XRANGE     = [0, 400], $
		XTITLE     = 'Ozone (ppbv)', $
		YTITLE     = ytitle, $
		BINNED     = 1, $
		FILTERING  = filtering, $
		CONVECTIVE = 0, $
		UPDRAFT_TR = updraft_tr, $
		DBL_TROP   = dbl_trop, $
		NOERASE	   = 1, $
		NOWINDOW   = 1

	PLOT_CONV_TRANSPORT, 1.0E3*co_arr, 0.001*(z_arr-xyz_troparr), updraft_tr_arr, cld_tr_arr, $	
		TITLE      = 'Non-convective', $
		XRANGE     = [0, 200], $
		XTITLE     = 'Carbon Monoxide (ppbv)', $
		YTITLE     = ytitle, $
		BINNED     = 1, $
		FILTERING  = filtering, $
		CONVECTIVE = 0, $
		UPDRAFT_TR = updraft_tr, $
		DBL_TROP   = dbl_trop, $
		NOERASE	   = 1, $
		NOWINDOW   = 1

	PLOT_CONV_TRANSPORT, 1.0E6*h2o_arr, 0.001*(z_arr-xyz_troparr), updraft_tr_arr, cld_tr_arr, $	
		TITLE      = 'Non-convective', $
		XRANGE     = [1, 500], $
		XTITLE     = 'Water Vapor (ppmv)', $
		YTITLE     = ytitle, $
		BINNED     = 1, $
		FILTERING  = filtering, $
		CONVECTIVE = 0, $
		UPDRAFT_TR = updraft_tr, $
		DBL_TROP   = dbl_trop, $
		NOERASE	   = 1, $
		NOWINDOW   = 1

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
