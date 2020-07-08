PRO PLOT_WRF_TRACER_RANVIL, date, event, scheme, $
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
;		PLOT_WRF_TRACER_RANVIL
; Purpose:
;		This is a procedure to call the procedure to
;	    plot WRF trace gases in relative altitude to the tropopause. 
; Calling sequence:
;		PLOT_WRF_TRACER_RANVIL
; Input:
;		date     : analysis date (e.g., MAKE_DATE(2012,5,19,22,00))
;		case     : string of case date (e.g., '20120519')
;		scheme   : microphysics scheme (e.g., 'morrison')
;		variable : variable to plot (e.g., 'O3')
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
;		Daniel B. Phoenix	2016-03-15.
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

dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")												;Set domain string
date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string

outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/plots/chemistry_scatter/'
epsfile = outdir + scheme + '_' + date_string + '_' + cld_title + '.eps'						;EPS filename
pdffile = outdir + scheme + '_' + date_string + '_' + cld_title + '.pdf'						;PDF filename
pngfile = outdir + scheme + '_' + date_string + '_' + cld_title + '.png'						;PNG filename

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

ytitle = 'Tropopause Relative (km)'																;Set tropopause relative y-label
z_trop = (WRF_READ_VAR('Z_trop', date, event, scheme, DOMAIN = domain)).values					;Read tropopause height data (x,y)
z_trop = CALC_TROP_MODE(z_trop, scheme, threshold) 												;Filter tropopause values
xyz_trop = REBIN(z_trop, dim[0], dim[1], dim[2], /SAMPLE)										;Rebin tropopause height to dimensions of geopotential height data

filt = WHERE(xyz_trop EQ 999999, filt_count, COMPLEMENT = good, NCOMPLEMENT = good_count)

refl  = (WRF_READ_VAR('REFL' 	, date, event, scheme, DOMAIN = domain)).values							;Read in reflectivity values
cloud = WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, DOMAIN = domain)							;Read in cloud mixing ratio values
cloud = cloud.values * 1.0E9																	;Convert mixing ratio to g kg-1

icloud = ((cloud GE 0.1)*z)
icloud = REBIN(icloud, dim[0],dim[1],dim[2], /SAMPLE)
izero  = WHERE(icloud EQ 0, count)
IF (count GT 0) THEN BEGIN
	icloud [izero] = !Values.F_NaN
ENDIF
anvil_top = MAX(icloud, DIM = 3, /NAN)
;;;;;;
bad = WHERE(FINITE(anvil_top) EQ 0, count, COMPLEMENT = good, NCOMPLEMENT = ngood)
IF (ngood GT 0) THEN z_anvil = anvil_top[good]
HELP, z_anvil
z_anvil = REBIN(z_anvil,dim[0],dim[1],dim[2], /SAMPLE)
HELP, z_anvil
;;;;;;
;IF (good_count GT 0) THEN BEGIN
;IF (ngood GT 0) THEN BEGIN
;	cloud     = cloud     [good]
;	xyz_trop  = xyz_trop  [good]
;	z_new	  = z	      [good]
;	anvil_new = anvil_top [good]
;ENDIF

title = STRING(scheme)

var = (WRF_READ_VAR('O3', date, event, scheme, DOMAIN = domain)).values							;Read ozone data
var = var[good]
WRF_TRACER_RALT, 1.0E3*var, 0.001*(z-anvil_top), refl, cloud, $
	TITLE     = title, $
	XRANGE    = [0, 400], $
	YRANGE    = [-5,  5], $
	XTITLE    = 'Ozone (ppbv)', $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	FILTERING = filtering, $
	IN_CLOUD  = in_cloud, $
	DBL_TROP  = dbl_trop, $
	NOWINDOW  = 1

var = (WRF_READ_VAR('CO', date, event, scheme, DOMAIN = domain)).values							;Read carbon monoxide data
var = var[good]
WRF_TRACER_RALT, 1.0E3*var, 0.001*(anvil_top-z), refl, cloud, $
	TITLE     = title, $
	XRANGE    = [0, 200], $
	XTITLE    = 'Carbon Monoxide (ppbv)', $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	FILTERING = filtering, $
	IN_CLOUD  = in_cloud, $
	DBL_TROP  = dbl_trop, $
	NOWINDOW  = 1

var = (WRF_READ_VAR('H2O', date, event, scheme, DOMAIN = domain)).values									;Read water vapor data
var = var[good]
WRF_TRACER_RALT, 1.0E6*var, 0.001*(anvil_top-z), refl, cloud, $
	TITLE     = title, $
	XRANGE    = [1, 10E3], $
	XLOG      = 1, $
	XTITLE    = 'Water Vapor (ppmv)', $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	FILTERING = filtering, $
	IN_CLOUD  = in_cloud, $
	DBL_TROP  = dbl_trop, $
	NOWINDOW  = 1

var = (WRF_READ_VAR('HNO3', date, event, scheme, DOMAIN = domain)).values								;Read nitric acid data
var = var[good]
WRF_TRACER_RALT, 1.0E3*var, 0.001*(anvil_top-z), refl, cloud, $
	TITLE     = title, $
	XRANGE    = [0, 3], $
	XTITLE    = 'Nitric Acid (ppbv)', $
	XTICKS    = 3, $
	XMINOR    = 4, $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	FILTERING = filtering, $
	IN_CLOUD  = in_cloud, $
	DBL_TROP  = dbl_trop, $
	NOWINDOW  = 1

var = (WRF_READ_VAR('NO2', date, event, scheme, DOMAIN = domain)).values									;Read nitrogen oxide data
var = var[good]
WRF_TRACER_RALT, 1.0E3*var, 0.001*(anvil_top-z), refl, cloud, $
	TITLE     = title, $
	XRANGE    = [0, 50], $
	XTICKS    = 3, $
	XMINOR    = 4, $
	XTITLE    = 'NO!D2!N (pptv)', $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	FILTERING = filtering, $
	IN_CLOUD  = in_cloud, $
	DBL_TROP  = dbl_trop, $
	NOWINDOW  = 1

var = (WRF_READ_VAR('SO2', date, event, scheme, DOMAIN = domain)).values									;Read carbon dioxide data
var = var[good]
WRF_TRACER_RALT, 1.0E6*var, 0.001*(anvil_top-z), refl, cloud, $
	TITLE     = title, $
	XRANGE    = [0, 500], $
	XTICKS    = 3, $
	XMINOR    = 4, $
	XTITLE    = 'Sulfur Dioxide (pptv)', $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	FILTERING = filtering, $
	IN_CLOUD  = in_cloud, $
	DBL_TROP  = dbl_trop, $
	NOWINDOW  = 1

var = (WRF_READ_VAR('HCHO', date, event, scheme, DOMAIN = domain)).values								;Read formaldehyde data
var = var[good]
WRF_TRACER_RALT, 1.0E3*var, 0.001*(anvil_top-z), refl, cloud, $
	TITLE     = title, $
	XRANGE    = [0, 3], $
	XTICKS    = 3, $
	XMINOR    = 4, $
	XTITLE    = 'Formaldehyde(ppbv)', $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	FILTERING = filtering, $
	IN_CLOUD  = in_cloud, $
	DBL_TROP  = dbl_trop, $
	NOWINDOW  = 1

var = (WRF_READ_VAR('HCL', date, event, scheme, DOMAIN = domain)).values									;Read HCL data
var = var[good]
WRF_TRACER_RALT, 1.0E6*var, 0.001*(anvil_top-z), refl, cloud, $
	TITLE     = title, $
	XRANGE    = [0, 50], $
	XTICKS    = 3, $
	XMINOR    = 4, $
	XTITLE    = 'HCL (pptv)', $
	YTITLE    = ytitle, $
	BINNED    = binned, $
	FILTERING = filtering, $
	IN_CLOUD  = in_cloud, $
	DBL_TROP  = dbl_trop, $ 
	NOWINDOW  = 1

!P.MULTI = 0
IF KEYWORD_SET(eps) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																;Reset color table to linear ramp
	PS_OFF																						;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)															;Write PNG file

END