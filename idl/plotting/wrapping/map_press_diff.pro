PRO MAP_PRESS_DIFF, date, run, $
	DOMAIN       = domain, $
	LEV			 = lev, $
	REGION  	 = region, $
	EPS          = eps, $
	PDF          = pdf, $
	PNG          = png

;+
; Name:
;		MAP_PRESS_DIFF
; Purpose:
;		Compare vertical profile of pressure between d03 and d01 
; Calling sequence:
;		MAP_PRESS_DIFF, run, experiment, date
; Input:
;		run   	   : String variable of run name. (e.g., '20120519')
;		experiment : String variable of initial state. (e.g., 'morrison')
;		date  	   : Desired date {CDATE}.
; Output:
;		Difference in pressure between parent and nested domain
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

IF (N_ELEMENTS(run       ) EQ 0) THEN run        = '20120530_ncar'
IF (N_ELEMENTS(date      ) EQ 0) THEN date       = MAKE_DATE(2012, 5, 19, 12)
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1
IF (N_ELEMENTS(lev		 ) EQ 0) THEN lev		 = 10500
IF (N_ELEMENTS(offset    ) EQ 0) THEN offset     = 0 ELSE print_bounds = 1

experiment = ['d01_30km','d03_30km']
press_arr = [ ]

x_parent = (WRF_READ_VAR('Longitude'      , date, run, experiment[0], DOMAIN = domain, INDICES = region)).values					;Read variables
y_parent = (WRF_READ_VAR('Latitude'       , date, run, experiment[0], DOMAIN = domain, INDICES = region)).values
p_parent = (WRF_READ_VAR('P' 	          , date, run, experiment[0], DOMAIN = domain, INDICES = region)).values
z_parent = (WRF_READ_VAR('Z'              , date, run, experiment[0], DOMAIN = domain, INDICES = region)).values
      
x_nest   = (WRF_READ_VAR('Longitude'      , date, run, experiment[1], DOMAIN = domain, INDICES = region)).values					;Read variables
y_nest   = (WRF_READ_VAR('Latitude'       , date, run, experiment[1], DOMAIN = domain, INDICES = region)).values
p_nest   = (WRF_READ_VAR('P' 	          , date, run, experiment[1], DOMAIN = domain, INDICES = region)).values
z_nest   = (WRF_READ_VAR('Z'              , date, run, experiment[1], DOMAIN = domain, INDICES = region)).values
ztrop    = (WRF_READ_VAR('Z_trop'         , date, run, experiment[1], DOMAIN = domain, INDICES = region)).values					;Read variables
u        = (WRF_READ_VAR('u' 	          , date, run, experiment[1], DOMAIN = domain, INDICES = region)).values
v        = (WRF_READ_VAR('v' 		      , date, run, experiment[1], DOMAIN = domain, INDICES = region)).values
w        = (WRF_READ_VAR('w'              , date, run, experiment[1], DOMAIN = domain, INDICES = region)).values
o 		 = (WRF_READ_VAR('O3_tracer'      , date, run, experiment[1], DOMAIN = domain, INDICES = region)).values * 1.0E3
cloud    = (WRF_READ_VAR('CLOUD_MIX_TOTAL', date, run, experiment[1], DOMAIN = domain, INDICES = region)).values

wind = SQRT(u^2 + v^2)
dim = SIZE(z_nest, /DIMENSIONS)																			;Get dimension sizes

ztrop    = MEDIAN(ztrop, 30)
xyz_trop = REBIN(ztrop, dim[0], dim[1], dim[2], /SAMPLE)

overshoot = 0.001*((MAX((cloud GE 1.0E-5)*z_nest, DIM = 3, /NAN)) - xyz_trop)						;Set map variable to cloud top altitude
core      = 0.001*((MAX((w GE 10.0)*z_nest, DIM = 3, /NAN)) - xyz_trop)

dim = SIZE(z_parent, /DIMENSIONS)																					;Get dimension sizes
dim0 = SIZE(z_nest, /DIMENSIONS)																					;Get dimension sizes

;;+ Save for future reference for interpolation
;nx = dim[0]
;ny = dim[1]
;nz = dim[2]
;ix_d02 = REBIN(FINDGEN(nx), nx, ny)
;iy_d02 = REBIN(REFORM(FINDGEN(ny), 1, ny), nx, ny)
;
;ix = INTERPOL(ix_d02,x_parent,x_nest)  
;iy = INTERPOL(iy_d02,y_parent,y_nest)  
;
;press_p2n  = INTERPOLATE(p_parent[*,*,lev],ix,iy,CUBIC=-0.5) 	
;press_diff = p_nest[*,*,lev] - press_p2n
;;- End save

;Make dummy x & y interpolation indices for "lev" map
nx = dim[0]
ny = dim[1]
nz = dim[2]
ix = REBIN(FINDGEN(nx), nx, ny)
iy = REBIN(REFORM(FINDGEN(ny), 1, ny), nx, ny)

;Calculate interpolation index for the 10.5 km level in each model grid column
iz = FLTARR(nx, ny)
FOR i = 0, nx-1 DO FOR j = 0, ny-1 DO iz[i,j] = INTERPOL(FINDGEN(nz), REFORM(z_parent[i,j,*], nz), lev)

parent_lev = INTERPOLATE(p_parent, ix, iy, iz)
;press_p2n  = CONGRID(parent_lev[120:524,115:272], dim0[0], dim0[1], /INTERP)				;For 2.5km-500m interpolation
;press_p2n  = CONGRID(parent_lev[94:175,78:110], dim0[0], dim0[1], /INTERP)					;For 12.5km-500m interpolation


;Make dummy x & y interpolation indices for "lev" map
nx = dim0[0]
ny = dim0[1]
nz = dim0[2]
ix = REBIN(FINDGEN(nx), nx, ny)
iy = REBIN(REFORM(FINDGEN(ny), 1, ny), nx, ny)

;Calculate interpolation index for the 10.5 km level in each model grid column
iz = FLTARR(nx, ny)
FOR i = 0, nx-1 DO FOR j = 0, ny-1 DO iz[i,j] = INTERPOL(FINDGEN(nz), REFORM(z_nest[i,j,*], nz), lev)

nest_lev  = INTERPOLATE(p_nest, ix, iy, iz)
ozone_map = INTERPOLATE(o,      ix, iy, iz)

press_p2n = MEAN(nest_lev)
press_diff = nest_lev - press_p2n

dim = SIZE(x_nest,/DIMENSIONS)

y0 = y_nest[          offset ,          offset ]														;Set domain boundary points
y1 = y_nest[          offset ,dim[1]-(1+offset)]
y2 = y_nest[dim[0]-(1+offset),dim[1]-(1+offset)]
y3 = y_nest[dim[0]-(1+offset),          offset ]
x0 = x_nest[          offset ,          offset ]
x1 = x_nest[          offset ,dim[1]-(1+offset)]
x2 = x_nest[dim[0]-(1+offset),dim[1]-(1+offset)]
x3 = x_nest[dim[0]-(1+offset),          offset ]

xc = INTERPOLATE(x_nest, 0.5*(dim[0]-1), 0.5*(dim[1]-1))											;Get central grid point
yc = INTERPOLATE(y_nest, 0.5*(dim[0]-1), 0.5*(dim[1]-1))

IF (print_bounds) THEN $
	PRINT, xc, yc, '  [',STRING([y0,x0,y1,x1,y2,x2,y3,x3], FORMAT="(F8.3,',')"), ']'

table   = BLUE_RED_24(16)																	;Set color table
rlevels = -2.0 + 0.25*FINDGEN(N_ELEMENTS(table)) 															;Set reflectivity contour levels

;map_max = MAX(press_diff,/NAN)
;map_min = MIN(press_diff,/NAN)
;rlevels = (((map_max-map_min)/20.0)*FINDGEN(20) + map_min)
;table   = HCL_COLOR_TABLE(20, HUE = [150.0, 360.0])																	;Set color table

wfactor = 400.0/(dim[0]) + 400.0/(dim[1])																	;Set map factor

dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string

outdir  = !WRF_DIRECTORY + run + '/' + 'paper/plots/interpolated_pressure_parent/' 
epsfile = outdir + STRMID(STRING(lev),7) + '_' + date_string + '.eps'											;EPS filename
pdffile = outdir + STRMID(STRING(lev),7) + '_' + date_string + '.pdf'											;PDF filename
pngfile = outdir + STRMID(STRING(lev),7) + '_' + date_string + '.png'											;PNG filename

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
	TITLE 	  = 'Mean Trop = ' + STRMID(STRING(MEAN(ztrop, /NAN)),5) + 'm Level = ' + $
					STRMID(STRING(lev),6) + 'm valid ' + date_string, $
	POSITION  = map_pos

IF KEYWORD_SET(image) THEN BEGIN
	ij0 = CONVERT_COORD([(!X.WINDOW)[0],(!Y.WINDOW)[0]], /NORMAL, /TO_DEVICE)
	ij1 = CONVERT_COORD([(!X.WINDOW)[1],(!Y.WINDOW)[1]], /NORMAL, /TO_DEVICE)

	xsize = LONG(ij1[0] - ij0[0])
	ysize = LONG(ij1[1] - ij0[1])

	image0 = (MAX(R.values, DIM=3))[offset:(dim[0]-(1+offset)), offset:(dim[1]-(1+offset))]
	dim    = dim - (2*offset)
	
	IF KEYWORD_SET(eps) THEN $
		image0 = INTERPOLATE(image0, MAKEN(0, dim[0]-1, xsize/10), MAKEN(0, dim[1]-1, ysize/10), /GRID) ELSE $
		image0 = INTERPOLATE(image0, MAKEN(0, dim[0]-1, xsize   ), MAKEN(0, dim[1]-1, ysize   ), /GRID)

	image0 = IMAGE_24(COLOR_LOOKUP_24((image0 < 75.0), table[1:*], MIN = 0.0, MAX = 75.0, $
				MISSING = COLOR_24(200, 200, 200), /NAN))
	TV, image0, ij0[0], ij0[1], TRUE = 3, XSIZE = xsize, YSIZE = ysize, /DEVICE
ENDIF ELSE $	

	CONTOUR, press_diff, x_nest, y_nest, $											;Surface nox
		OVERPLOT  = 1, $
		FILL      = 1, $
		LEVELS    = rlevels, $
		C_COLOR   = table

CONTOUR, overshoot, x_nest, y_nest, $
	OVERPLOT = 1, $
	LEVELS   = 0.0, $
	C_COLOR  = COLOR_24('gray50'), $
	C_THICK  = 2

nxy = 60

;xreg = MAKEN(MIN(x_nest), MAX(x_nest), nxy)
;yreg = MAKEN(MIN(y_nest), MAX(y_nest), nxy)
;
;TRIANGULATE, x_nest, y_nest, tri
;
;ureg = TRIGRID(x_nest, y_nest, u[*,*,lev], tri, XOUT = xreg, YOUT = yreg)
;vreg = TRIGRID(x_nest, y_nest, v[*,*,lev], tri, XOUT = xreg, YOUT = yreg)
;
;VELOVECT, ureg, vreg, xreg, yreg, OVERPLOT  = 1, LENGTH = 3.0
				
CONTOUR, overshoot, x_nest, y_nest, $
	OVERPLOT = 1, $
	LEVELS   = 0.0, $
	C_COLOR  = COLOR_24('gray50'), $
	C_THICK  = 2

CONTOUR, core, x_nest, y_nest, $
	OVERPLOT = 1, $
	LEVELS   = 0.0, $
	C_COLOR  = COLOR_24('black'), $
	C_THICK  = 2

CONTOUR, ozone_map, x_nest, y_nest, $
    OVERPLOT = 1, $
    LEVELS   = 200.0, $
    C_COLOR  = COLOR_24('black'), $
    C_THICK  = 3

CONTOUR, ozone_map, x_nest, y_nest, $
	OVERPLOT = 1, $
	LEVELS   = 225.0, $
	C_COLOR  = COLOR_24('red'), $
	C_THICK  = 3

MAP_CONTINENTS, /CONT, /USA	
																				
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

COLOR_BAR_24_KPB, table, OVER = table[-1], UNDER = table[0], $
	RANGE = [-2.0, 2.0], $
;	RANGE = [MIN(press_diff,/nan), MAX(press_diff,/nan)], $
	TICKS = 5, $
	TITLE = 'Pressure (hPa)', $
	POSIT = bar_pos

IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file

END
