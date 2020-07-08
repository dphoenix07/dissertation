PRO MAP_WRF_TROP_OZONE, run, experiment, date, $
	DOMAIN 	  = domain, $
	THRESHOLD = threshold, $
	PNG = png

;+
; Name:
;		MAP_WRF_TROP_OZONE
; Purpose:
;		This is a procedure to plot ozone at the tropopause.
; Calling sequence:
;		MAP_WRF_TROP_OZONE, run, date
; Input:
;		run        : Desired WRF run. (e.g., '20110408')
;		experiment : Desired experiment. (e.g., 'era_mp8')
;		date       : Desired forecast date. {CDATE}
; Output:
;		A plot of ozone at the tropopause. 
; Keywords:
;		DOMAIN : Optional keyword to specify the domain number. Default is 2.
;		EPS    : If set, output to postscript.
;		PNG    : If set, write PNG image.
; Author and history:
;		Cameron R. Homeyer  2013-04-02.
;		Daniel B. Phoenix	2016-01-07. Edited 'MAP_WRF_TROP_W' to plot ozone at 
;										tropopause and added cloud boundary.
;
;-
COMPILE_OPT IDL2																									;Set compile options

IF (N_ELEMENTS(run       ) EQ 0) THEN run        = '20120519'
IF (N_ELEMENTS(experiment) EQ 0) THEN experiment = 'morrison'
IF (N_ELEMENTS(date      ) EQ 0) THEN date       = MAKE_DATE(2012, 5, 19, 12)
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 2
IF (N_ELEMENTS(sec_trop  ) EQ 0) THEN sec_trop	 = 0
IF (N_ELEMENTS(threshold ) EQ 0) THEN threshold	 = 1000.0

date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Set date string

z     	  = WRF_READ_VAR('Z',      		  date, run, experiment, DOMAIN = domain)							;Read variables
trop  	  = WRF_READ_VAR('Z_trop', 	 	  date, run, experiment, DOMAIN = domain)
o     	  = WRF_READ_VAR('O3',     		  date, run, experiment, DOMAIN = domain)
cloud     = WRF_READ_VAR('CLOUD_MIX',     date, run, experiment, DOMAIN = domain)

trop = CALC_TROP_MODE(trop, threshold)
o.values = o.values * 1.0E3																		;Convert ozone from ppm to ppb

dim = SIZE(z.values, /DIMENSION)
nx  = dim[0]
ny  = dim[1]
nz  = dim[2]

i   = REBIN(FINDGEN(nx), nx, ny)
j   = REBIN(REFORM(FINDGEN(ny), 1, ny), nx, ny)
k   = REBIN(REFORM(FINDGEN(nz), 1, 1, nz), nx, ny, nz)
;k   = MAX((ABS(Z.values - REBIN(trop.values,nx,ny,nz, /SAMPLE) LE 1.0))*k, DIM = 3)			;Original
k   = MAX((ABS(Z.values - REBIN(trop,nx,ny,nz, /SAMPLE) LE 1.0))*k, DIM = 3)					;Deleted '.values' off trop


map 	= INTERPOLATE(o.values, i, j, k)
cld_map = INTERPOLATE(cloud.values, i, j, k)

outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/plots/tropo3/'
epsfile = outdir + 'tropo3_' + date_string + '.eps'											;EPS filename
pdffile = outdir + 'tropo3_' + date_string + '.pdf'											;PDF filename
pngfile = outdir + 'tropo3_' + date_string + '.png'											;PNG filename

FILE_MKDIR, outdir

IF KEYWORD_SET(eps) THEN BEGIN	
	PS_ON, FILENAME = epsfile, PAGE_SIZE = [8.0, 6.0], MARGIN = 0.0, /INCHES					;Switch to Postscript device
	DEVICE, /ENCAPSULATED
	!P.FONT     = 0																								;Hardware fonts
	!P.CHARSIZE = 1.0	
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS																							;Load basic color definitions
ENDIF ELSE BEGIN
	SET_PLOT, 'X'
	WINDOW, XSIZE = 1000, YSIZE = 800																		;Open graphics window
	!P.COLOR      = COLOR_24('black')																		;Foreground color
	!P.BACKGROUND = COLOR_24('white')																		;Background color
	!P.CHARSIZE   = 2.0		
	!P.FONT       = -1																							;Use Hershey fonts
ENDELSE

table  = BLUE_RED_24(40, 0.0)																;blue and red table
;table   = [COLOR_24(200, 200, 200),VISUALIZE_88D_COLOR()]						
levels = 10.0 * FINDGEN(40)

cld_levels = MAKEN(0,1.0E-6, 10)

x  = WRF_READ_VAR('Longitude', date, run, experiment, DOMAIN = domain)							;Read coordinates
y  = WRF_READ_VAR('Latitude',  date, run, experiment, DOMAIN = domain)
y0 = y.values[       0,       0]																				;Set domain boundary points
y1 = y.values[       0,dim[1]-1]
y2 = y.values[dim[0]-1,dim[1]-1]
y3 = y.values[dim[0]-1,       0]
x0 = x.values[       0,       0]
x1 = x.values[       0,dim[1]-1]
x2 = x.values[dim[0]-1,dim[1]-1]
x3 = x.values[dim[0]-1,       0]

xc = x.values[(dim[0]-1)/2,(dim[1]-1)/2]																	;Get central grid point
yc = y.values[(dim[0]-1)/2,(dim[1]-1)/2]

MAP_SET, yc, xc, 0, CONIC = 1, $																				;Draw map
	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
	ISOTROPIC = 1, $
	TITLE     = date_string + ' Ozone at Tropopause Level: ' + experiment, $
	POSITION  = [0.1, 0.15, 0.9, 0.95]

CONTOUR, map, x.values, y.values, $
	OVERPLOT = 1, $
	FILL     = 1, $
	C_COLOR  = table, $
	LEVELS   = levels

CONTOUR, cld_map, x.values, y.values, $
	OVERPLOT = 1, $
	FILL     = 0, $
	LEVELS   = cld_levels
	
MAP_CONTINENTS, /USA, /CONT

COLOR_BAR_24_KPB, table, OVER = table[-1], UNDER = table[0], $
	RANGE    = [0, 400], $
	TICKS    = 10, $
	TITLE    = 'Ozone Concentration (ppb)', $
	POSITION = [0.25, 0.10, 0.75, 0.12]


!P.POSITION = 0

IF KEYWORD_SET(eps) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file

END
