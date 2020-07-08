PRO MAP_WRF_ECHO_TOP, date, run, experiment, $
	ECHO   = echo, $
	OFFSET = offset, $
	DOMAIN = domain, $
	REGION = region, $
	Z_buff = z_buff, $
	IMAGE  = image, $
	SECTION = section, $
	EPS    = eps, $
	PDF    = pdf, $
	PNG    = png

;+
; Name:
;		MAP_WRF_ECHO_TOP
; Purpose:
;		This is a template for creating IDL procedure files. 
; Calling sequence:
;		MAP_WRF_ECHO_TOP, run, experiment, date
; Input:
;		run   : String variable of run name. (e.g., '20110404')
;		experiment : String variable of initial state. (e.g., 'era')
;		date  : Desired date {CDATE}.
; Output:
;		A map of simulated composite reflectivity. 
; Keywords:
;		ECHO   : Optional keyword to specify echo top reflectivity threshold. Default is 15 dBZ.
;		DOMAIN : Simulation domain number. Default is 1. 
;		EPS    : If set, output to PostScript.
;		PDF    : If set, output to PDF.
;		PNG    : If set, write PNG image.
; Author and history:
;		Cameron R. Homeyer  2015-05-14.
;-

COMPILE_OPT IDL2																									;Set compile options

print_bounds = 0

IF (N_ELEMENTS(run       ) EQ 0) THEN run        = '20110408'
IF (N_ELEMENTS(experiment) EQ 0) THEN experiment = 'era_mp10'
IF (N_ELEMENTS(date      ) EQ 0) THEN date       = MAKE_DATE(2011, 4, 9, 00)
IF (N_ELEMENTS(tracer    ) EQ 0) THEN tracer     = 'BL_tracer'
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 2
IF (N_ELEMENTS(offset    ) EQ 0) THEN offset     = 0 ELSE print_bounds = 1
IF (N_ELEMENTS(echo      ) EQ 0) THEN echo       = 15.0

x  = WRF_READ_VAR('Longitude', date, run, experiment, DOMAIN = domain, INDICES = region)	;Read variables
y  = WRF_READ_VAR('Latitude',  date, run, experiment, DOMAIN = domain, INDICES = region)
R  = WRF_READ_VAR('REFL',      date, run, experiment, DOMAIN = domain, INDICES = region)
Z  = WRF_READ_VAR('Z',         date, run, experiment, DOMAIN = domain, INDICES = region)

map = MAX((R.values GE echo)*Z.values*0.001, DIM = 3)
PRINT, 'Echo top maximum: ' + STRING(MAX(map))
i0  = WHERE((map EQ 0.0), n0)
IF (n0 GT 0) THEN map[i0] = !Values.F_NaN

dim = SIZE(x.values, /DIMENSIONS)																			;Get dimension sizes

y0 = y.values[          offset ,          offset ]														;Set domain boundary points
y1 = y.values[          offset ,dim[1]-(1+offset)]
y2 = y.values[dim[0]-(1+offset),dim[1]-(1+offset)]
y3 = y.values[dim[0]-(1+offset),          offset ]
x0 = x.values[          offset ,          offset ]
x1 = x.values[          offset ,dim[1]-(1+offset)]
x2 = x.values[dim[0]-(1+offset),dim[1]-(1+offset)]
x3 = x.values[dim[0]-(1+offset),          offset ]

xc = INTERPOLATE(x.values, 0.5*(dim[0]-1), 0.5*(dim[1]-1))											;Get central grid point
yc = INTERPOLATE(y.values, 0.5*(dim[0]-1), 0.5*(dim[1]-1))

IF (print_bounds) THEN $
	PRINT, xc, yc, '  [',STRING([y0,x0,y1,x1,y2,x2,y3,x3], FORMAT="(F8.3,',')"), ']'

table   = VISUALIZE_88D_COLOR()																				;Set reflectivity color table
rlevels = FINDGEN(N_ELEMENTS(table))																		;Set reflectivity contour levels
wfactor = 400.0/(dim[0]) + 400.0/(dim[1])																	;Set map factor

dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string

outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/plots/echo'
epsfile = outdir + experiment + '_echo_' + date_string + '.eps'											;EPS filename
pdffile = outdir + experiment + '_echo_' + date_string + '.pdf'											;PDF filename
pngfile = outdir + experiment + '_echo_' + date_string + '.png'											;PNG filename

FILE_MKDIR, outdir																								;Create output directory, if necessary

map_pos = [0.05, 0.15, 0.95, 0.95]																			;Set map position
bar_pos = [0.25, 0.10, 0.75, 0.12]																			;Set color bar position

IF KEYWORD_SET(z_buff) THEN BEGIN
	SET_PLOT, 'Z'																									;Output to Z buffer
	DEVICE, SET_PIXEL_DEPTH = 24, SET_RESOLUTION = [wfactor*(dim[0]), wfactor*(dim[1])], $	;Set device resolution and bit depth
		SET_CHARACTER_SIZE = [12, 20]
	!P.COLOR      = COLOR_24('black')																		;Foreground color
	!P.BACKGROUND = COLOR_24('white')																		;Background color
	!P.CHARSIZE   = 1.5																							;Set character size
	!P.FONT       = -1
ENDIF ELSE BEGIN
	IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN	
		PS_ON, FILENAME = epsfile, PAGE_SIZE = [4.0,3.0], MARGIN = 0.0, /INCHES;PAGE_SIZE = 0.001*dim*wfactor			;Switch to Postscript device
		DEVICE, /ENCAPSULATED
		!P.FONT     = 0																								;Hardware fonts
		!P.CHARSIZE = 0.75	
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS																							;Load basic color definitions
	ENDIF ELSE BEGIN
		SET_PLOT, 'X'
		WINDOW, XSIZE = wfactor*(dim[0]), YSIZE = wfactor*(dim[1])										;Open graphics window
		!P.COLOR      = COLOR_24('black')																		;Foreground color
		!P.BACKGROUND = COLOR_24('white')																		;Background color
		!P.CHARSIZE   = 2.0		
		!P.FONT       = -1																							;Use Hershey fonts
	ENDELSE
ENDELSE

MAP_SET, yc, xc, 0, CONIC = 1, /NOBORDER, $																;Draw map
	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
	ISOTROPIC = 1, $
	TITLE     = 'WRF-' + experiment + ' ' + dom_string + ' valid ' + date_string, $
	POSITION  = map_pos

IF KEYWORD_SET(image) THEN BEGIN
	ij0 = CONVERT_COORD([(!X.WINDOW)[0],(!Y.WINDOW)[0]], /NORMAL, /TO_DEVICE)
	ij1 = CONVERT_COORD([(!X.WINDOW)[1],(!Y.WINDOW)[1]], /NORMAL, /TO_DEVICE)

	xsize = LONG(ij1[0] - ij0[0])
	ysize = LONG(ij1[1] - ij0[1])

	image0 = map[offset:(dim[0]-(1+offset)), offset:(dim[1]-(1+offset))]
	dim    = dim - (2*offset)
	
	IF KEYWORD_SET(eps) THEN $
		image0 = INTERPOLATE(image0, MAKEN(0, dim[0]-1, xsize/10), MAKEN(0, dim[1]-1, ysize/10), /GRID) ELSE $
		image0 = INTERPOLATE(image0, MAKEN(0, dim[0]-1, xsize   ), MAKEN(0, dim[1]-1, ysize   ), /GRID)

	image0 = IMAGE_24(COLOR_LOOKUP_24((image0 < 20.0), table, MIN = 0.0, MAX = 15.0, $
				MISSING = COLOR_24(200, 200, 200), /NAN))
	TV, image0, ij0[0], ij0[1], TRUE = 3, XSIZE = xsize, YSIZE = ysize, /DEVICE
ENDIF ELSE $	
	CONTOUR, map, x.values, y.values, $																		;Contour reflectivity values
		OVERPLOT  = 1, $
		FILL      = 1, $
		LEVELS    = rlevels, $
		C_COLOR   = table

MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines

IF KEYWORD_SET(section) THEN BEGIN
	IF (run EQ '20110408') THEN ij = [100, 119, 300, 157]
	IF (run EQ '20110521') THEN ij = [150, 116, 350, 130]
	IF (run EQ '20110618') THEN ij = [040, 060, 240, 080]

	xysect = MAP_2POINTS((x.values)[ij[0],ij[1]],(y.values)[ij[0],ij[1]],$
								(x.values)[ij[2],ij[3]],(y.values)[ij[2],ij[3]], NPATH = 10)
	
	OPLOT, xysect[0,*], xysect[1,*], THICK = 4
	XYOUTS, xysect[0,0], xysect[1,0], 'A', ALIGN = 1
	XYOUTS, xysect[0,-1], xysect[1,-1], 'B', ALIGN = 0
ENDIF

MAP_SET, yc, xc, 0, CONIC = 1, $																				;Redraw map boundaries
	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
	ISOTROPIC = 1, $
	NOERASE   = 1, $
	POSITION  = map_pos
	
COLOR_BAR_24_KPB, table, OVER = table[-1], $
	RANGE = [0, 15], $
	TICKS = 3, $
	TITLE = 'Echo Top (km)', $
	POSIT = bar_pos

IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file

END
