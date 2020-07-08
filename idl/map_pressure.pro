PRO MAP_PRESSURE, date, run, experiment, $
	LEV    = lev, $
	OFFSET = offset, $
	DOMAIN = domain, $
	REGION = region, $
	Z_buff = z_buff, $
	PRINT_BOUNDS = print_bounds, $
	IMAGE  = image, $
	SECTION = section, $
	EPS    = eps, $
	PDF    = pdf, $
	PNG    = png

;+
; Name:
;		MAP_SFC_WIND
; Purpose:
;		This is a template for creating IDL procedure files. 
; Calling sequence:
;		MAP_SFC_WIND, run, experiment, date
; Input:
;		run   	   : String variable of run name. (e.g., '20120519')
;		experiment : String variable of initial state. (e.g., 'morrison')
;		date  	   : Desired date {CDATE}.
; Output:
;		A map of simulated surface ozone. 
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
IF (N_ELEMENTS(experiment) EQ 0) THEN experiment = 'd03_30km'
IF (N_ELEMENTS(date      ) EQ 0) THEN date       = MAKE_DATE(2012, 5, 31, 00)
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1
IF (N_ELEMENTS(offset    ) EQ 0) THEN offset     = 0 ELSE print_bounds = 1
IF (N_ELEMENTS(lev       ) EQ 0) THEN lev 		 = 10000

IF (run EQ '20120519') THEN region = [50, 50, 250, 190]
marysville_d01 = [92,79,95,83]
marysville_d02 = [76,135,77,136]

x     = WRF_READ_VAR('Longitude'       , date, run, experiment, DOMAIN = domain, INDICES = region)		;Read variables
y     = WRF_READ_VAR('Latitude'        , date, run, experiment, DOMAIN = domain, INDICES = region)
IF ((experiment EQ 'd03_30km') OR (experiment EQ 'd03_30km_icloud')) THEN $
	o = (WRF_READ_VAR('O3_tracer', date, run, experiment, DOMAIN = domain, INDICES = region)).values * 1.0E3
IF (experiment EQ 'd02_30km') THEN $
	o = (WRF_READ_VAR('O3', date, run, experiment, DOMAIN = domain, INDICES = region)).values * 1.0E3
z     = (WRF_READ_VAR('Z' 	           , date, run, experiment, DOMAIN = domain, INDICES = region)).values
ztrop = (WRF_READ_VAR('Z_trop'         , date, run, experiment, DOMAIN = domain, INDICES = region)).values
u     = (WRF_READ_VAR('u' 	           , date, run, experiment, DOMAIN = domain, INDICES = region)).values
v     = (WRF_READ_VAR('v' 		       , date, run, experiment, DOMAIN = domain, INDICES = region)).values
w     = (WRF_READ_VAR('w'              , date, run, experiment, DOMAIN = domain, INDICES = region)).values
p	  = (WRF_READ_VAR('P'              , date, run, experiment, DOMAIN = domain, INDICES = region)).values
cloud = (WRF_READ_VAR('CLOUD_MIX_TOTAL', date, run, experiment, DOMAIN = domain, INDICES = region)).values


STOP
wind = SQRT(u^2 + v^2)
dim = SIZE(z, /DIMENSIONS)																			;Get dimension sizes

ztrop    = MEDIAN(ztrop, 30)
xyz_trop = REBIN(ztrop, dim[0], dim[1], dim[2], /SAMPLE)

overshoot  = 0.001*((MAX((cloud GE 1.0E-5)*z, DIM = 3, /NAN)) - xyz_trop)						;Set map variable to cloud top altitude
core       = 0.001*((MAX((w GE 10.0)*z, DIM = 3, /NAN)) - xyz_trop)

centers = where(core GE 0.0,count)
x_mean = MEAN(x.values[centers],/NAN)
y_mean = MEAN(y.values[centers],/NAN)

x_med = MEDIAN(x.values[centers])
y_med = MEDIAN(y.values[centers])

;zr = 3.0
;radius_list = [ ]
;radius = SQRT((ABS(x.values - x_mean))^2 + (ABS(y.values - y_mean))^2)	            
;radius_3d = REBIN(radius,dim[0],dim[1],dim[2],/SAMPLE)
;zone = WHERE(radius_3d LT zr, zone_count, COMPLEMENT = non_zone)	
;zone = WHERE((radius LT zr) AND (radius GT (zr-0.02)), zone_count, COMPLEMENT = non_zone)	

anvil_edge = WHERE(overshoot EQ 0.0, count)
zr = 1.0

zone = [ ]
FOR i = 0, N_ELEMENTS(anvil_edge)-1 DO BEGIN
	radius  = SQRT((ABS(x.values - x.values[anvil_edge[i]]))^2 + (ABS(y.values - y.values[anvil_edge[i]]))^2)	            
	wrapped = WHERE((radius LT zr) AND (radius GT (zr-0.02)), count)
	zone = [zone, wrapped]
ENDFOR

zone1 = zone[UNIQ(zone, SORT(zone))]

;Make dummy x & y interpolation indices for 10.5 km map
nx = dim[0]
ny = dim[1]
nz = dim[2]
;ix = REBIN(FINDGEN(nx), nx, ny)
;iy = REBIN(REFORM(FINDGEN(ny), 1, ny), nx, ny)

;nz1 = 320
;ix = REBIN(FINDGEN(nx), nx, ny, nz1)
;iy = REBIN(REFORM(FINDGEN(ny), 1, ny), nx, ny, nz1)

;Calculate interpolation index for the 10.5 km level in each model grid column
iz = FLTARR(nx, ny)
;iz = FLTARR(nx, ny, nz1)
FOR i = 0, nx-1 DO FOR j = 0, ny-1 DO iz[i,j] = INTERPOL(FINDGEN(nz), REFORM(z[i,j,*], nz), lev)
;FOR i = 0, nx-1 DO FOR j = 0, ny-1 DO iz[i,j,*] = INTERPOL(FINDGEN(nz), REFORM(z[i,j,*], nz), FINDGEN(nz1)*100.0)

;Interpolate ozone volume to 10.5 km altitude map
IF (experiment NE 'd01_30km') THEN ozone_map = INTERPOLATE(o,    ix, iy, iz)
press_map = INTERPOLATE(p,	  ix, iy, iz)
wind_map  = INTERPOLATE(wind, ix, iy, iz)
u_map     = INTERPOLATE(u,    ix, iy, iz)
v_map     = INTERPOLATE(v,    ix, iy, iz)


STOP
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

map_max = 260.1;MAX(press_map,/NAN)
map_min = 252.7;MIN(press_map,/NAN)
rlevels   = (((map_max-map_min)/20.0)*FINDGEN(20) + map_min)
table   = HCL_COLOR_TABLE(20, HUE = [150.0, 360.0])																	;Set color table
wfactor = 400.0/(dim[0]) + 400.0/(dim[1])														;Set map factor

dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string

outdir  = !WRF_DIRECTORY + run + '/' + 'paper/plots/pressure_radius_map/' 
epsfile = outdir + STRMID(STRING(lev),10) + '_' + date_string + '.eps'											;EPS filename
pdffile = outdir + STRMID(STRING(lev),10) + '_' + date_string + '.pdf'											;PDF filename
pngfile = outdir + STRMID(STRING(lev),10) + '_' + date_string + '.png'											;PNG filename

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
	CONTOUR, press_map, x.values, y.values, $													;Surface ozone
		OVERPLOT  = 1, $
		FILL      = 1, $
		LEVELS    = rlevels, $
		C_COLOR   = table

nxy = 60
xreg = MAKEN(MIN(x.values), MAX(x.values), nxy)
yreg = MAKEN(MIN(y.values), MAX(y.values), nxy)

TRIANGULATE, x.values, y.values, tri

ureg = TRIGRID(x.values, y.values, u_map, tri, XOUT = xreg, YOUT = yreg)
vreg = TRIGRID(x.values, y.values, v_map, tri, XOUT = xreg, YOUT = yreg)

VELOVECT, ureg, vreg, xreg, yreg, OVERPLOT  = 1, LENGTH = 3.0

CONTOUR, overshoot, x.values, y.values, $
	OVERPLOT = 1, $
	LEVELS   = 0.0, $
	C_COLOR  = COLOR_24('gray50'), $
	C_THICK  = 2

CONTOUR, core, x.values, y.values, $
	OVERPLOT = 1, $
	LEVELS   = 0.0, $
	C_COLOR  = COLOR_24('green'), $
	C_THICK  = 2

CONTOUR, core, x.values, y.values, $
	OVERPLOT = 1, $
	LEVELS   = 0.0, $
	C_COLOR  = COLOR_24('black'), $
	C_THICK  = 2

IF (experiment NE 'd01_30km') THEN BEGIN
	CONTOUR, ozone_map, x.values, y.values, $
	    OVERPLOT = 1, $
	    LEVELS   = 200.0, $
	    C_COLOR  = COLOR_24('black'), $
	    C_THICK  = 3

    CONTOUR, ozone_map, x.values, y.values, $
    	OVERPLOT = 1, $
    	LEVELS   = 225.0, $
    	C_COLOR  = COLOR_24('red'), $
    	C_THICK  = 3
ENDIF

;USERSYM_STAR, /FILL																			;Load plane symbol at flight path orientation
;PLOTS, x_mean, y_mean, $																	;Overplot plane symbol
;	PSYM    = 8, $
;	SYMSIZE = 4 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
;	NOCLIP  = 0, $
;	COLOR   = COLOR_24('red')
;
;PLOTS, x_med, y_med, $																	;Overplot plane symbol
;	PSYM    = 8, $
;	SYMSIZE = 4 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
;	NOCLIP  = 0, $
;	COLOR   = COLOR_24('black')
;
;USERSYM_CIRCLE																					;Load plane symbol at flight path orientation
;FOR i=0, N_ELEMENTS(zone)-1 DO BEGIN
;	PLOTS, (x.values)[zone1[i]], (y.values)[zone1[i]], $																		;Overplot plane symbol
;		PSYM    = 8, $
;		SYMSIZE = 4 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
;		NOCLIP  = 0, $
;		COLOR   = COLOR_24('green')
;ENDFOR

				
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
	
COLOR_BAR_24_KPB, table, OVER = table[-1], $
	RANGE = [map_min, map_max], $
	TICKS = 4, $
	TITLE = 'Pressure (hPa)', $
	POSIT = bar_pos

IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file

STOP
END
