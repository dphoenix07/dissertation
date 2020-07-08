PRO PLOT_WRF_MULTISPEC_TRACER_SECTION, date, experiment, state, $
	DOMAIN      = domain, $
	INDICES     = indices, $
	ENHANCEMENT = enhancement, $
	EPS         = eps, $
	PDF         = pdf, $
	PNG         = png


;+
; Name:
;		PLOT_WRF_MULTISPEC_TRACER_SECTION
; Purpose:
;		This is a procedure to plot a multi-spectral vertical section of a WRF tracer. 
; Calling sequence:
;		PLOT_WRF_MULTISPEC_TRACER_SECTION, date, experiment, state
; Input:
;		date       : Analysis date {CDATE}
;		experiment : WRF simulation name. (e.g., 'APR04')
;		state      : WRF simulation state name. (e.g., 'era_mp8')
; Output:
;		A vertical section plot.
; Keywords:
;		INDICES : WRF grid indices to compute vertical section for. e.g., [10, 200, 300, 100]
;		EPS     : If set, output to PostScript.
;		PDF     : If set, write and convert PostScript to PDF.
;		PNG     : If set, write PNG image.
; Author and history:
;		Cameron R. Homeyer  2012-11-28.
;-

COMPILE_OPT IDL2																									;Set compile options

IF (N_ELEMENTS(date      ) EQ 0) THEN date       = MAKE_DATE(2011, 4, 9, 0)					;Set default analysis date
IF (N_ELEMENTS(experiment) EQ 0) THEN experiment = '20110408'										;Set default model run
IF (N_ELEMENTS(state     ) EQ 0) THEN state      = 'era_mp10'

IF (N_ELEMENTS(indices) EQ 0) THEN indices = [100, 119, 300, 157]									;Set default section grid indices
IF (N_ELEMENTS(domain ) EQ 0) THEN domain  = 1

IF (N_ELEMENTS(enhancement) EQ 0) THEN enhancement = 0
CASE domain OF
	1 : dx = 15000.0
	1 : dx =  3000.0
	ELSE : MESSAGE, 'Grid resolution not defined!'
ENDCASE

date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Set date string

;IF domain EQ 1 THEN $
;date = MK_DATE_ARR_D01(date, experiement, start_date, end_date, /DATE)	
;
;IF domain EQ 2 THEN $
;date = MK_DATE_ARR_D02(event, scheme, start_date, end_date, /DATE)	

z       = WRF_READ_VAR('Z'              , date, experiment, state, DOMAIN = domain)
trop    = WRF_READ_VAR('Z_trop'         , date, experiment, state, DOMAIN = domain)
ttracer = WRF_READ_VAR('TR_tracer'      , date, experiment, state, DOMAIN = domain)
btracer = WRF_READ_VAR('BL_tracer'      , date, experiment, state, DOMAIN = domain)
stracer = WRF_READ_VAR('UTLS_tracer'    , date, experiment, state, DOMAIN = domain)
cloud   = WRF_READ_VAR('CLOUD_MIX_TOTAL', date, experiment, state, DOMAIN = domain)

dim   = SIZE(Z.values, /DIMENSIONS)
nx    = dim[0]
ny    = dim[1]
nz    = dim[2]

nsect = 400
isect = MAKEN(indices[0], indices[2], nsect)
jsect = MAKEN(indices[1], indices[3], nsect)
ksect = FINDGEN(nz)

iisect = REBIN(       isect,         nsect, nz, /SAMPLE)
jjsect = REBIN(       jsect,         nsect, nz, /SAMPLE)
kksect = REBIN(REFORM(ksect, 1, nz), nsect, nz, /SAMPLE)

zsect = INTERPOLATE(z.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)
zout0 = 0.0
zout1 = 18000.0
dz    = 50.0
nzout = LONG((zout1 - zout0)/dz) + 1
zout  = zout0 + dz*FINDGEN(nzout)

iisect = REBIN(isect, nsect, nzout, /SAMPLE)
jjsect = REBIN(jsect, nsect, nzout, /SAMPLE)
kksect = FLTARR(nsect, nzout)
FOR i = 0, nsect -1 DO $
	kksect[i,*] = INTERPOL(FINDGEN(nz),zsect[i,*],zout)

trpsect     = INTERPOLATE(trop.values,    isect,  jsect                                  )
ttracersect = INTERPOLATE(ttracer.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)
btracersect = INTERPOLATE(btracer.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)
stracersect = INTERPOLATE(stracer.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)
cloudsect   = INTERPOLATE(  cloud.values, iisect, jjsect, kksect, MISSING = !Values.F_NaN)

epsfile = '~/wrf_ms_' + date_string + '_section.eps'
pdffile = '~/wrf_ms_' + date_string + '_section.pdf'
pngfile = '~/wrf_ms_' + date_string + '_section.png'

IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN	
	PS_ON, FILENAME = epsfile, PAGE_SIZE = [8.0, 5.0], MARGIN = 0.0, /INCHES					;Switch to Postscript device
	DEVICE, /ENCAPSULATED
	!P.FONT     = 0																								;Hardware fonts
	!P.CHARSIZE = 1.25
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS																							;Load basic color definitions
ENDIF ELSE BEGIN
	SET_PLOT, 'X'
	WINDOW, XSIZE = 800, YSIZE = 500																			;Open graphics window
	!P.COLOR      = COLOR_24('black')																		;Foreground color
	!P.BACKGROUND = COLOR_24('white')																		;Background color
	!P.CHARSIZE   = 2.0		
	!P.FONT       = -1																							;Use Hershey fonts
ENDELSE

position = [0.1, 0.1, 0.95, 0.95]

ij0 = CONVERT_COORD(position[0:1], /NORMAL, /TO_DEVICE)
ij1 = CONVERT_COORD(position[2:*], /NORMAL, /TO_DEVICE)

xsize = LONG(ij1[0] - ij0[0])
ysize = LONG(ij1[1] - ij0[1])

btracersect = INTERPOLATE(btracersect, MAKEN(0, nsect-1, xsize), MAKEN(0, nzout-1, ysize), /GRID)
ttracersect = INTERPOLATE(ttracersect, MAKEN(0, nsect-1, xsize), MAKEN(0, nzout-1, ysize), /GRID)
stracersect = INTERPOLATE(stracersect, MAKEN(0, nsect-1, xsize), MAKEN(0, nzout-1, ysize), /GRID)

CASE enhancement OF
	0 : color = COLOR_24(50.0 + 200.0*(btracersect/0.025 < 1.0), 25.0 + 200.0*(stracersect/0.025 < 1.0), 50.0 + 200.0*(ttracersect/0.025 < 1.0))
	1 : color = COLOR_24(85.0 + 170.0*(btracersect/0.025 < 1.0), 170.0*(ttracersect/0.025 < 1.0), 255.0*(stracersect/0.025 < 1.0))
	2 : color = COLOR_24(255.0*(stracersect/0.025 < 1.0), 170.0*(ttracersect/0.025 < 1.0), 255.0*(btracersect/0.025 < 1.0))
ENDCASE
image = IMAGE_24(color)

ERASE
TV, image, ij0[0], ij0[1], TRUE = 3, XSIZE = xsize, YSIZE = ysize, /DEVICE

CONTOUR, ttracersect, FINDGEN(xsize), FINDGEN(ysize), /NODATA, /NOERASE, $
	CELL_FILL= 1, $
	C_COLOR  = table, $
	LEVELS   = levels, $
	XRANGE   = [0, nsect -1], $
	XTICKS   = 1, $
	XTICKN   = ['A', 'B'], $
	XSTYLE   = 1, $
	YRANGE   = 0.001*[zout0, zout1], $
	YSTYLE   = 1, $
	YTITLE   = 'Altitude (km)', $
;	TITLE    = 'Vertical Section', $
	POSITION = position

CONTOUR, cloudsect, FINDGEN(nsect), 0.001*zout, $
	OVERPLOT = 1, $
	LEVELS   = 0.01, $
	THICK    = 4, $
	COLOR    = COLOR_24('gray50')

OPLOT, FINDGEN(nsect), 0.001*trpsect,  THICK = 4, COLOR = COLOR_24('black'), LINES = 2

IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert PostScript to PDF
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file

END
