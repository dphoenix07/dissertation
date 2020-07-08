PRO PLOT_WRF_TRACER_ALT, date, event, scheme, $
	DOMAIN		= domain, $
	BINNED      = binned, $
	IN_CLOUD    = in_cloud, $
	PDF			= pdf, $
	EPS         = eps, $
	PNG         = png

;+
; Name:
;		PLOT_WRF_TRACER_ALT
; Purpose:
;		This is a procedure to call the procedure to
;	    plot WRF trace gases in relative altitude to the tropopause. 
; Calling sequence:
;		PLOT_WRF_TRACER_ALT
; Input:
;		date     : analysis date (e.g., MAKE_DATE(2012,5,19,22,00))
;		case     : string of case date (e.g., '20120519')
;		scheme   : microphysics scheme (e.g., 'morrison')
; Output:
;		plots of trace gases at relative altitudes to the tropopause
; Keywords:
;		DOMAIN    : Simulation domain number. Default is 2. 
;		BINNED    : Set to bin gas concentrations.
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
IF (N_ELEMENTS (in_cloud  )	EQ 0) THEN in_cloud   = 1

IF in_cloud EQ 1 THEN BEGIN 
	cld_title = 'in_cloud'
	ENDIF ELSE BEGIN
	cld_title = 'out_cloud'
ENDELSE

dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")												;Set domain string
date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string

outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/plots/chemistry_scatter/'
epsfile = outdir + scheme + '_' + date_string + '_' + cld_title + '_alt.eps'						;EPS filename
pdffile = outdir + scheme + '_' + date_string + '_' + cld_title + '_alt.pdf'						;PDF filename
pngfile = outdir + scheme + '_' + date_string + '_' + cld_title + '_alt.png'						;PNG filename

FILE_MKDIR, outdir

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

z = WRF_READ_VAR('Z', date, event, scheme, DOMAIN = domain)										;Read geopotential height data (x,y,z)
dim = SIZE(z.values, /DIMENSIONS)																;Get geopotential height dimensions
z = z.values

ytitle = 'Altitude (km)'																		;Set y-label
cloud = WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, DOMAIN = domain)					;Read in cloud mixing ratio values
cloud = cloud.values * 1.0E9																	;Convert mixing ratio to g kg-1
PRINT, MEAN(cloud)

title = STRING(scheme)

var = (WRF_READ_VAR('O3', date, event, scheme, DOMAIN = domain)).values							;Read ozone data
WRF_TRACER_ALT, 1.0E3*var, 0.001*z, cloud, $
	TITLE     = title, $
	XRANGE    = [0, 400], $
	XTITLE    = 'Ozone (ppbv)', $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	IN_CLOUD  = in_cloud, $
	NOWINDOW  = 1

var = (WRF_READ_VAR('CO', date, event, scheme, DOMAIN = domain)).values							;Read carbon monoxide data
WRF_TRACER_ALT, 1.0E3*var, 0.001*z, cloud, $
	TITLE     = title, $
	XRANGE    = [0, 200], $
	XTITLE    = 'Carbon Monoxide (ppbv)', $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	IN_CLOUD  = in_cloud, $
	NOWINDOW  = 1

var = (WRF_READ_VAR('H2O', date, event, scheme, DOMAIN = domain)).values									;Read water vapor data
WRF_TRACER_ALT, 1.0E6*var, 0.001*z, cloud, $
	TITLE     = title, $
	XRANGE    = [1, 500], $
	XTITLE    = 'Water Vapor (ppmv)', $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	IN_CLOUD  = in_cloud, $
	NOWINDOW  = 1

var = (WRF_READ_VAR('HNO3', date, event, scheme, DOMAIN = domain)).values								;Read nitric acid data
WRF_TRACER_ALT, 1.0E3*var, 0.001*z, cloud, $
	TITLE     = title, $
	XRANGE    = [0, 3], $
	XTITLE    = 'Nitric Acid (ppbv)', $
	XTICKS    = 3, $
	XMINOR    = 4, $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	IN_CLOUD  = in_cloud, $
	NOWINDOW  = 1

var = (WRF_READ_VAR('NO2', date, event, scheme, DOMAIN = domain)).values									;Read nitrogen oxide data
WRF_TRACER_ALT, 1.0E3*var, 0.001*z, cloud, $
	TITLE     = title, $
	XRANGE    = [0, 50], $
	XTICKS    = 3, $
	XMINOR    = 4, $
	XTITLE    = 'NO!D2!N (pptv)', $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	IN_CLOUD  = in_cloud, $
	NOWINDOW  = 1

var = (WRF_READ_VAR('SO2', date, event, scheme, DOMAIN = domain)).values									;Read carbon dioxide data
WRF_TRACER_ALT, 1.0E6*var, 0.001*z, cloud, $
	TITLE     = title, $
	XRANGE    = [0, 500], $
	XTICKS    = 3, $
	XMINOR    = 4, $
	XTITLE    = 'Sulfur Dioxide (pptv)', $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	IN_CLOUD  = in_cloud, $
	NOWINDOW  = 1

var = (WRF_READ_VAR('HCHO', date, event, scheme, DOMAIN = domain)).values								;Read formaldehyde data
WRF_TRACER_ALT, 1.0E3*var, 0.001*z, cloud, $
	TITLE     = title, $
	XRANGE    = [0, 3], $
	XTICKS    = 3, $
	XMINOR    = 4, $
	XTITLE    = 'Formaldehyde(ppbv)', $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	IN_CLOUD  = in_cloud, $
	NOWINDOW  = 1

var = (WRF_READ_VAR('CH4', date, event, scheme, DOMAIN = domain)).values									;Read HCL data
WRF_TRACER_ALT, 1.0E3*var, 0.001*z, cloud, $
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