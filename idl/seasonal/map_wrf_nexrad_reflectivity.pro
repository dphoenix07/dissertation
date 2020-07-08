PRO MAP_WRF_NEXRAD_REFLECTIVITY, date, run, experiment, $
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
;		MAP_WRF_REFLECTIVITY
; Purpose:
;		This is a template for creating IDL procedure files. 
; Calling sequence:
;		MAP_WRF_REFLECTIVITY, run, experiment, date
; Input:
;		run   	   : String variable of run name. (e.g., '20120519')
;		experiment : String variable of initial state. (e.g., 'morrison')
;		date  	   : Desired date {CDATE}.
; Output:
;		A map of simulated composite reflectivity. 
; Keywords:
;		DOMAIN : Simulation domain number. Default is 1. 
;		EPS    : If set, output to PostScript.
;		PDF    : If set, output to PDF.
;		PNG    : If set, write PNG image.
; Author and history:
;		Cameron R. Homeyer  2012-10-05.
;-

COMPILE_OPT IDL2																									;Set compile options

print_bounds = 0

IF (N_ELEMENTS(run       ) EQ 0) THEN run        = '20120519'
IF (N_ELEMENTS(experiment) EQ 0) THEN experiment = 'morrison'
IF (N_ELEMENTS(date      ) EQ 0) THEN date       = MAKE_DATE(2012, 5, 19, 12)
IF (N_ELEMENTS(tracer    ) EQ 0) THEN tracer     = 'BL_tracer'
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1
IF (N_ELEMENTS(offset    ) EQ 0) THEN offset     = 0 ELSE print_bounds = 1

nexrad  = NEXRAD_3_1_LEVEL3_3D_READ_NCDF(date, version = '3_1', product = '3d')
nexrad  = NEXRAD_FILTER(nexrad)

values   = nexrad.dbz.values
map_plot = MAX((values)*(FINITE(SHIFT(values,0,0,1)))*$						;Compute altitude of reflectivity surface
				(FINITE(SHIFT(values,0,0,2))), DIM = 3, /NAN)

STOP

izero = WHERE((map_plot EQ 0.0), n0)															;Find 0s
IF (n0 GT 0) THEN map_plot[izero] = !Values.F_NaN											;Remove altitudes for areas with no echo

map_plot = MAX(values, DIM=3, /NAN)
   
x     = WRF_READ_VAR('Longitude', date, run, experiment, DOMAIN = domain, INDICES = region)		;Read variables
y     = WRF_READ_VAR('Latitude' , date, run, experiment, DOMAIN = domain, INDICES = region)
R     = WRF_READ_VAR('REFL'     , date, run, experiment, DOMAIN = domain, INDICES = region)
z     = (WRF_READ_VAR('Z'       , date, run, experiment, DOMAIN = domain, INDICES = region)).values
ztrop = (WRF_READ_VAR('Z_trop'  , date, run, experiment, DOMAIN = domain, INDICES = region)).values	

dim 	 = SIZE(R.values, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
ztrop    = MEDIAN(ztrop, 30) 												;Filter tropopause values
xyz_trop = REBIN(ztrop, dim[0], dim[1], dim[2], /SAMPLE)

ix = INTERPOL(FINDGEN(nexrad.x.n),nexrad.x.values,x.values+360.0)
iy = INTERPOL(FINDGEN(nexrad.y.n),nexrad.y.values,y.values	     )

nex_interp  = INTERPOLATE(map_plot,ix,iy)

bad = WHERE (R.values EQ 0.0, bad_count, COMPLEMENT = good, NCOMPLEMENT = good_count)
R.values [bad ] = -35.0000
R.values [good] = R.values [good]

bad = WHERE (nex_interp EQ 0.0, bad_count, COMPLEMENT = good, NCOMPLEMENT = good_count)
nex_interp [bad ] = -35.0000
nex_interp [good] = nex_interp [good]

!P.MULTI = [0, 2, 1]

map_pos1 = [0.05, 0.15, 0.48, 0.95]																			;Set map position
map_pos2 = [0.52, 0.15, 0.95, 0.95]																			;Set map position

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

table   = [COLOR_24(200, 200, 200),VISUALIZE_88D_COLOR()]												;Set reflectivity color table
rlevels = [-100.0, 5.0*FINDGEN(N_ELEMENTS(table))]													;Set reflectivity contour levels
wfactor = 400.0/(dim[0]) + 400.0/(dim[1])																	;Set map factor

dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string

outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/plots/wrf_nex_refl/'

epsfile = outdir + experiment + '_' + dom_string + '_' + date_string + '.eps'											;EPS filename
pdffile = outdir + experiment + '_' + dom_string + '_' + date_string + '.pdf'											;PDF filename
pngfile = outdir + experiment + '_' + dom_string + '_' + date_string + '.png'											;PNG filename

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
		PS_ON, FILENAME = epsfile, PAGE_SIZE = [4.0,4.0], MARGIN = 0.0, /INCHES;PAGE_SIZE = 0.001*dim*wfactor			;Switch to Postscript device
		DEVICE, /ENCAPSULATED
		!P.FONT     = 0																								;Hardware fonts
		!P.CHARSIZE = 0.75	
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS																							;Load basic color definitions
	ENDIF ELSE BEGIN
		SET_PLOT, 'X'
		WINDOW, XSIZE = 1.5*wfactor*(dim[0]), YSIZE = wfactor*(dim[1])										;Open graphics window
		!P.COLOR      = COLOR_24('black')																		;Foreground color
		!P.BACKGROUND = COLOR_24('white')																		;Background color
		!P.CHARSIZE   = 2.0		
		!P.FONT       = -1																							;Use Hershey fonts
	ENDELSE
ENDELSE

;;+ WRF plot
MAP_SET, yc, xc, 0, CONIC = 1, /NOBORDER, $																;Draw map
	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
	ISOTROPIC = 1, $
	TITLE     = 'WRF-' + experiment + ' ' + dom_string + ' valid ' + date_string, $
	NOERASE   = 1, $
	POSITION  = map_pos1

CONTOUR, MAX(R.values, DIM=3), x.values, y.values, $												;Contour reflectivity values
	OVERPLOT  = 1, $
	FILL      = 1, $
	LEVELS    = rlevels, $
	NOERASE   = 1, $
	C_COLOR   = table

MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines

MAP_SET, yc, xc, 0, CONIC = 1, $																				;Redraw map boundaries
	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
	ISOTROPIC = 1, $
	NOERASE   = 1, $
	POSITION  = map_pos1
;;- WRF plot

;;+ Nexrad plot
MAP_SET, yc, xc, 0, CONIC = 1, /NOBORDER, $																;Draw map
	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
	ISOTROPIC = 1, $
	TITLE     = 'NEXRAD-' + experiment + ' ' + dom_string + ' valid ' + date_string, $
	NOERASE   = 1, $
	POSITION  = map_pos2

CONTOUR, nex_interp, x.values, y.values, $												;Contour reflectivity values
	OVERPLOT  = 1, $
	FILL      = 1, $
	LEVELS    = rlevels, $
	NOERASE   = 1, $
	C_COLOR   = table

MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines

MAP_SET, yc, xc, 0, CONIC = 1, $																				;Redraw map boundaries
	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
	ISOTROPIC = 1, $
	NOERASE   = 1, $
	POSITION  = map_pos2
;;- Nexrad plot

	
COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
	RANGE = [0, 75], $
	TICKS = 5, $
	TITLE = 'Reflectivity (dBZ)', $
	POSIT = bar_pos

IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file


END
