PRO PLOT_MULTI_SCATTER_ALT, event, scheme, start_date, end_date, $
	DOMAIN   = domain, $
	IN_CLOUD = in_cloud, $
	BINNED   = binned, $
	PNG	     = png


;+
; Name:
;		PLOT_MULTI_SCATTER_ALT
; Purpose:
;		Currently set up to combine multiple time steps and produce scatter plots of 
;		trace gases using WRF_TRACER_ALT.
;		(e.g., Similar to 'PLOT_WRF_TRACER_ALT', but can do multiple time steps)
; Calling sequence:
;		PLOT_MULTI_SCATTER_ALT, run, scheme, start_date, end_date
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
;		Daniel B. Phoenix	    2016-03-24. 
;		D.B.P.					2016-06-04. Note this should be used if observations 
;											are not desired. Use 'plot_wrf_obs_scatter.pro'
;											instead, that is the most recent version of 
;											this routine. There is a keyword for 'alt'
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 2											;Set default domain
IF (N_ELEMENTS(in_cloud  ) EQ 0) THEN in_cloud   = 1

IF (event EQ '20120519') THEN region = [50, 50, 250, 190]
IF (event EQ '20120529') THEN region = [50, 80, 230, 250]

IF in_cloud EQ 1 THEN BEGIN 
	cld_title = 'in_cloud'
	ENDIF ELSE BEGIN
	cld_title = 'out_cloud'
ENDELSE

outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/plots/chemistry_scatter/'
epsfile = outdir + scheme + '_' + end_date + '_' + cld_title + '_alt.eps'						;EPS filename
pdffile = outdir + scheme + '_' + end_date + '_' + cld_title + '_alt.pdf'						;PDF filename
pngfile = outdir + scheme + '_' + end_date + '_' + cld_title + '_alt.png'						;PNG filename

FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(event, scheme, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(event, scheme, start_date, end_date, /DATE)	

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

	z		 = (WRF_READ_VAR('Z'              , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read height values
    cloud 	 = (WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, DOMAIN = domain, INDICES = region)).values * 1.0E3			;Read in cloud mixing ratio values
	o3       = (WRF_READ_VAR('O3'      		  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values
	co	     = (WRF_READ_VAR('CO'			  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read carbon monoxide data
	h2o	 	 = (WRF_READ_VAR('H2O'		      , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read water vapor data 
	hno3	 = (WRF_READ_VAR('HNO3'			  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read nitric acid data
	no	     = (WRF_READ_VAR('NO'			  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read nitrogen oxide data 
	no2		 = (WRF_READ_VAR('NO2'			  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read nitrogen oxide data
	nox		 = no + no2
	so2		 = (WRF_READ_VAR('SO2'			  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read sulfur dioxide data 
	hcho	 = (WRF_READ_VAR('HCHO'			  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read formaldehyde data
	ch4		 = (WRF_READ_VAR('CH4'			  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ch4 data
	 
	z_arr       = [z_arr      , z       ]														;concatenate arrays for each time
	cloud_arr   = [cloud_arr  , cloud   ]
	o3_arr      = [o3_arr     , o3      ]	
	co_arr      = [co_arr	  , co		]
	h2o_arr	    = [h2o_arr	  , h2o		]		
	hno3_arr    = [hno3_arr   , hno3	]	
	nox_arr     = [nox_arr	  , nox		]		
	so2_arr		= [so2_arr 	  , so2		]
	hcho_arr	= [hcho_arr   , hcho	]
	ch4_arr		= [ch4_arr	  , ch4		]													
ENDFOREACH 

z   	 = z_arr																				;Rename arrays to original variable names																										 																			
cloud 	 = cloud_arr 																			;Easier than changing variable names when...  
o3   	 = o3_arr																				;...calling WRF_TRACER_ALT
co   	 = co_arr		
h2o   	 = h2o_arr 		
hno3   	 = hno3_arr		
nox		 = nox_arr		
so2	 	 = so2_arr		
hcho	 = hcho_arr 	
ch4		 = ch4_arr 		


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

WRF_TRACER_ALT, 1.0E3*o3, 0.001*z, cloud, $														;Call WRF_TRACER_ALT to produce scatterplots
	TITLE     = title, $
	XRANGE    = [0, 800], $
	XTITLE    = 'Ozone (ppbv)', $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	IN_CLOUD  = in_cloud, $
	NOWINDOW  = 1

WRF_TRACER_ALT, 1.0E3*co, 0.001*z, cloud, $
	TITLE     = title, $
	XRANGE    = [0, 500], $
	XTITLE    = 'Carbon Monoxide (ppbv)', $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	IN_CLOUD  = in_cloud, $
	NOWINDOW  = 1

WRF_TRACER_ALT, 1.0E6*h2o, 0.001*z, cloud, $
	TITLE     = title, $
	XRANGE    = [1, 10E3], $
	XLOG      = 1, $
	XTITLE    = 'Water Vapor (ppmv)', $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	IN_CLOUD  = in_cloud, $
	NOWINDOW  = 1

WRF_TRACER_ALT, 1.0E3*hno3, 0.001*z, cloud, $
	TITLE     = title, $
	XRANGE    = [0, 6], $
	XTITLE    = 'Nitric Acid (ppbv)', $
	XTICKS    = 3, $
	XMINOR    = 4, $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	IN_CLOUD  = in_cloud, $
	NOWINDOW  = 1

WRF_TRACER_ALT, 1.0E3*nox, 0.001*z, cloud, $
	TITLE     = title, $
	XRANGE    = [0, 50], $
	XTICKS    = 3, $
	XMINOR    = 4, $
	XTITLE    = 'NOx (ppbv)', $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	IN_CLOUD  = in_cloud, $
	NOWINDOW  = 1

WRF_TRACER_ALT, 1.0E6*so2, 0.001*z, cloud, $
	TITLE     = title, $
	XRANGE    = [0, 500], $
	XTICKS    = 3, $
	XMINOR    = 4, $
	XTITLE    = 'Sulfur Dioxide (pptv)', $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	IN_CLOUD  = in_cloud, $
	NOWINDOW  = 1

WRF_TRACER_ALT, 1.0E3*hcho, 0.001*z, cloud, $
	TITLE     = title, $
	XRANGE    = [0, 3], $
	XTICKS    = 3, $
	XMINOR    = 4, $
	XTITLE    = 'Formaldehyde(ppbv)', $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	IN_CLOUD  = in_cloud, $
	NOWINDOW  = 1

WRF_TRACER_ALT, 1.0E3*ch4, 0.001*z, cloud, $
	TITLE     = title, $
	XRANGE    = [1700, 2000], $
	XTICKS    = 3, $
	XMINOR    = 4, $
	XTITLE    = 'CH4 (ppbv)', $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	IN_CLOUD  = in_cloud, $
	NOWINDOW  = 1

!P.MULTI = 0
IF KEYWORD_SET(eps) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																;Reset color table to linear ramp
	PS_OFF																						;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)															;Write PNG file

END