PRO PLOT_MULTI_TRACER, event, scheme, start_date, end_date, $
	ZRELATIVE = zrelative, $
	CLOUD     = cloud, $
	WATER     = water, $
	GROUP2    = group2, $
	DOMAIN    = domain, $
	BINNED    = binned, $
	PNG	      = png


;+
; Name:
;		PLOT_MULTI_TRACER
; Purpose:
;		Currently set up to combine multiple time steps and produce scatter plots of 
;		trace gases using WRF_TRACER_TRACER.
;		(e.g., Similar to 'PLOT_WRF_TRACER_TRACER', but can do multiple time steps)
; Calling sequence:
;		PLOT_MULTI_TRACER, run, scheme, start_date, end_date
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
;		Daniel B. Phoenix	    2016-02-18. 
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 2											;Set default domain


IF (event EQ '20120519') THEN region = [50, 50, 250, 190]
;IF (scheme EQ '15-3km'  ) THEN region = [50, 90, 210, 190]

date_arr = MK_DATE_ARR(event, scheme, start_date, end_date, /DATE)	

z_arr 	    = [ ]																				;allocate arrays
xyz_troparr = [ ]
refl_arr    = [ ]
c1oud_arr   = [ ]
o3_arr      = [ ] 
co_arr      = [ ] 
h2o_arr     = [ ] 
hno3_arr	= [ ]
nox_arr		= [ ]
so2_arr		= [ ]
hcho_arr	= [ ]
;ch4_arr	= [ ]
cld_tr_arr  = [ ]

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

 	  	c1oud 	 	= (WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, DOMAIN = domain, INDICES = region)).values * 1.0E3			;Read in cloud mixing ratio values
		c1oud_arr   = [c1oud_arr  , c1oud   [good]]		
		
		o3       	= (WRF_READ_VAR('O3'      		  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values
		o3_arr      = [o3_arr     , o3 		[good]]	

		co	    	= (WRF_READ_VAR('CO'			  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read carbon monoxide data
		co_arr      = [co_arr	  , co		[good]]

		h2o	 	 	= (WRF_READ_VAR('H2O'		      , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read water vapor data 
		h2o_arr	    = [h2o_arr	  , h2o		[good]]		

		cld_tr		= ((((WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, DOMAIN = domain)).values) * 1.0E3) GT 0.01)
		cld_tr_arr  = [cld_tr_arr , cld_tr  [good]]		

		IF KEYWORD_SET(group2) THEN BEGIN 
			hno3	 	 = (WRF_READ_VAR('HNO3'			  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read nitric acid data
		 	hno3_arr     = [hno3_arr   , hno3	[good]]	

			nox	    	 = (WRF_READ_VAR('NO'			  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read nitrogen oxide data 
			nox			+= (WRF_READ_VAR('NO2'			  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read nitrogen oxide data
		    nox_arr      = [nox_arr	  , nox		[good]]		
	
			so2			 = (WRF_READ_VAR('SO2'			  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read sulfur dioxide data 
			so2_arr	     = [so2_arr   , so2		[good]]

			hcho		 = (WRF_READ_VAR('HCHO'			  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read formaldehyde data
		    hcho_arr	 = [hcho_arr  , hcho	[good]]

			ch4			 = (WRF_READ_VAR('CH4'			  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ch4 data
			ch4_arr		 = [ch4_arr	  , ch4		[good]]
		ENDIF
	ENDIF	 											
ENDFOREACH 

;z   	 = z_arr																				;Rename arrays to original variable names								
;xyz_trop = xyz_troparr																			;Easier than changing variable names when...  
;refl 	 = refl_arr	 																			;...calling WRF_TRACER_RALT
;c1oud 	 = c1oud_arr 	
;o3   	 = o3_arr		
;co   	 = co_arr		
;h2o   	 = h2o_arr 		
;hno3   	 = hno3_arr		
;nox		 = nox_arr		
;so2	 	 = so2_arr		
;hcho	 = hcho_arr 	
;ch4		 = ch4_arr 
;cld_tr   = cld_tr_arr

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
	!P.CHARSIZE   = 2.9		
	!P.FONT       = -1																			;Use Hershey fonts
ENDELSE
!P.MULTI = [0, 2, 1]


IF KEYWORD_SET(zrelative) THEN BEGIN
	ztracer = 0.001*(z_arr - xyz_troparr)
	zrange  = [-2.5, 2.5]
	ztitle  = 'Tropopause Relative (km)'
	name    = 'zrelative'
ENDIF ELSE IF KEYWORD_SET(cloud) THEN BEGIN	
	ztrsort = 0.001*(z_arr - xyz_troparr)
	ztracer = cld_tr_arr
	zrange  = [0, 1]
	ztitle  = 'Cloud Particles (#)'
	name    = 'cloud'
ENDIF ELSE IF KEYWORD_SET(water) THEN BEGIN
	ztrsort  = 0.001*(z_arr - xyz_troparr)
	ztracer = h2o_arr
	ztracer = ztracer*1.0E6
	zrange  = [0, 400]
	ztitle  = 'Water Vapor (ppmv)'
	name    = 'water'
ENDIF 

outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/plots/chemistry_tracer/'
epsfile = outdir + scheme + '_' + end_date + '_' + name + '.eps'						;EPS filename
pdffile = outdir + scheme + '_' + end_date + '_' + name + '.pdf'						;PDF filename
pngfile = outdir + scheme + '_' + end_date + '_' + name + '.png'						;PNG filename

FILE_MKDIR, outdir

title = STRING(scheme)
symsize = 1.0 - 0.25*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf))

WRF_TRACER_TRACER, co_arr*1.0E3, o3_arr*1.0E3, ztracer, $
	TITLE    = title, $
	ZTRSORT  = ztrsort, $
	XRANGE   = [0, 200], $
	XTITLE   = 'Carbon Monoxide (ppbv)', $
	YRANGE   = [0, 600], $
	YTITLE   = 'Ozone (ppbv)', $
	ZRANGE   = zrange, $
	ZTITLE   = ztitle, $
	BINNED   = binned, $
	CREVERSE = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $		
	SYMSIZE  = symsize, $
	NOWINDOW = 1
															;Read ozone data
WRF_TRACER_TRACER, h2o_arr*1.0E6, o3_arr*1.0E3, ztracer, $
	TITLE    = title, $
	ZTRSORT  = ztrsort, $
	XRANGE   = [0, 500], $
	XTITLE   = 'Water Vapor (ppmv)', $
	XLOG     = 0, $
	YRANGE   = [0, 600], $
	YTITLE   = 'Ozone (ppbv)', $
	ZRANGE   = zrange, $
	ZTITLE   = ztitle, $
	BINNED   = binned, $
	CREVERSE = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $
	SYMSIZE  = symsize, $
	NOWINDOW = 1

IF KEYWORD_SET(group2) THEN BEGIN
		
var1 = WRF_READ_VAR('NO', date, event, scheme, DOMAIN = domain)																;Read nitrogen oxide data
var1.values = 1.0E3*var1.values
var2 = WRF_READ_VAR('O3', date, event, scheme, DOMAIN = domain)																;Read ozone data
var2.values = 1.0E3*var2.values 	
WRF_TRACER_TRACER, var1.values, var2.values, ztracer, $
	ZTRSORT  = ztrsort, $
	XRANGE   = [0, 2000], $
	XTITLE   = 'NO2 (pptv)', $
	YRANGE   = [0, 800], $
	YTITLE   = 'Ozone (ppbv)', $
	ZRANGE   = zrange, $
	ZTITLE   = ztitle, $
	BINNED   = binned, $
	CREVERSE = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $
	SYMSIZE  = symsize, $
	NOWINDOW = 1
	
	
var1 = WRF_READ_VAR('H2O', date, event, scheme, DOMAIN = domain)																	;Read water vapor data
var2 = WRF_READ_VAR('NO2', date, event, scheme, DOMAIN = domain)																	;Read nitrogen oxide data
WRF_TRACER_TRACER, var1.values, var2.values, ztracer, $
	ZTRSORT  = ztrsort, $
	XRANGE   = [1, 10000], $
	XTITLE   = 'Water Vapor (ppmv)', $
	XLOG     = 1, $
	YRANGE   = [0, 2000], $
	YTITLE   = 'NO2 (pptv)', $
	ZRANGE   = zrange, $
	ZTITLE   = ztitle, $
	BINNED   = binned, $
	CREVERSE = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $
	SYMSIZE  = symsize, $
	NOWINDOW = 1

var1 = WRF_READ_VAR('CO' , date, event, scheme, DOMAIN = domain)																		;Read carbon monoxide data
var2 = WRF_READ_VAR('NO2', date, event, scheme, DOMAIN = domain)																		;Read nitrogen oxide data
WRF_TRACER_TRACER, var1.values, var2.values, ztracer, $
	ZTRSORT  = ztrsort, $
	XRANGE   = [0, 200], $
	XTITLE   = 'Carbon Monoxide (ppbv)', $
	YRANGE   = [0, 2000], $
	YTITLE   = 'NO2 (pptv)', $
	ZRANGE   = zrange, $
	ZTITLE   = ztitle, $
	BINNED   = binned, $
	CREVERSE = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $
	SYMSIZE  = symsize, $
	NOWINDOW = 1

var1 = WRF_READ_VAR('HNO3', date, event, scheme, DOMAIN = domain)																;Read methane data
var2 = WRF_READ_VAR('HCHO', date, event, scheme, DOMAIN = domain)																;Read ozone data
WRF_TRACER_TRACER, var1.values, var2.values, ztracer, $
	ZTRSORT  = ztrsort, $
	XRANGE   = [1600, 2000], $
	XTITLE   = 'Nitric Acid (ppbv)', $
	YRANGE   = [0, 800], $
	YTITLE   = 'Formaldehyde (ppbv)', $
	ZRANGE   = zrange, $
	ZTITLE   = ztitle, $
	BINNED   = binned, $
	CREVERSE = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $
	SYMSIZE  = symsize, $
	NOWINDOW = 1

ENDIF

!P.MULTI = 0
IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN $
		PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS													;Convert eps to pdf
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file

END
