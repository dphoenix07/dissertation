PRO MAP_SFC_WIND, date, run, experiment, $
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

IF (N_ELEMENTS(run       ) EQ 0) THEN run        = '20120519'
IF (N_ELEMENTS(experiment) EQ 0) THEN experiment = 'morrison'
IF (N_ELEMENTS(date      ) EQ 0) THEN date       = MAKE_DATE(2012, 5, 19, 12)
IF (N_ELEMENTS(tracer    ) EQ 0) THEN tracer     = 'BL_tracer'
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
cloud = (WRF_READ_VAR('CLOUD_MIX_TOTAL', date, run, experiment, DOMAIN = domain, INDICES = region)).values

wind = SQRT(u^2 + v^2)
dim = SIZE(z, /DIMENSIONS)																			;Get dimension sizes

ztrop    = MEDIAN(ztrop, 30)
xyz_trop = REBIN(ztrop, dim[0], dim[1], dim[2], /SAMPLE)

overshoot = 0.001*((MAX((cloud GE 1.0E-5)*z, DIM = 3, /NAN)) - xyz_trop)						;Set map variable to cloud top altitude
core      = 0.001*((MAX((w GE 10.0)*z, DIM = 3, /NAN)) - xyz_trop)

;Make dummy x & y interpolation indices for 10.5 km map
nx = dim[0]
ny = dim[1]
nz = dim[2]
ix = REBIN(FINDGEN(nx), nx, ny)
iy = REBIN(REFORM(FINDGEN(ny), 1, ny), nx, ny)

;Calculate interpolation index for the 10.5 km level in each model grid column
iz = FLTARR(nx, ny)
FOR i = 0, nx-1 DO FOR j = 0, ny-1 DO iz[i,j] = INTERPOL(FINDGEN(nz), REFORM(z[i,j,*], nz), lev)

;Interpolate ozone volume to 10.5 km altitude map
ozone_map = INTERPOLATE(o,    ix, iy, iz)
wind_map  = INTERPOLATE(wind, ix, iy, iz)
u_map     = INTERPOLATE(u,    ix, iy, iz)
v_map     = INTERPOLATE(v,    ix, iy, iz)

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

table   = HCL_COLOR_TABLE(26, HUE = [150.0, 360.0])																	;Set color table
rlevels = 2.0*FINDGEN(N_ELEMENTS(table))															;Set reflectivity contour levels
wfactor = 400.0/(dim[0]) + 400.0/(dim[1])														;Set map factor

dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string

outdir  = !WRF_DIRECTORY + run + '/' + 'paper/plots/wind_mag_dir_10250m/' 
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
	CONTOUR, wind_map, x.values, y.values, $													;Surface ozone
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
	C_COLOR  = COLOR_24('black'), $
	C_THICK  = 2

IF (experiment NE 'd01_30km') THEN BEGIN
	CONTOUR, ozone_map, x.values, y.values, $
	    OVERPLOT = 1, $
	    LEVELS   = 150.0, $
	    C_COLOR  = COLOR_24('black'), $
	    C_THICK  = 3

    CONTOUR, ozone_map, x.values, y.values, $
    	OVERPLOT = 1, $
    	LEVELS   = 200.0, $
    	C_COLOR  = COLOR_24('red'), $
    	C_THICK  = 3
ENDIF
				
MAP_CONTINENTS, /CONT, /USA	
																				
IF KEYWORD_SET(section) THEN BEGIN
	IF (run EQ '20110408'     ) THEN ij = [100, 119, 0300, 157]
	IF (run EQ '20110521'     ) THEN ij = [150, 116, 0350, 130]
	IF (run EQ '20110618'     ) THEN ij = [040, 060, 0240, 080]
	IF (run EQ '20120530_ncar') THEN ij = [075, 700, 1919, 100]

	npath1 = 5

	xysect = MAP_2POINTS((x.values)[ij[0],ij[1]],(y.values)[ij[0],ij[1]],$
								(x.values)[ij[2],ij[3]],(y.values)[ij[2],ij[3]], NPATH = npath1)
	
	parmsect = MAP_2POINTS((x.values)[ij[0],ij[1]],(y.values)[ij[0],ij[1]],$
								(x.values)[ij[2],ij[3]],(y.values)[ij[2],ij[3]], /RHUMB)
								
	perp = parmsect[1]*!DDTOR																								;Find angle of cross section path
	
	xy02 = LL_ARC_DISTANCE([((x.values)[ij[0],ij[1]]),((y.values)[ij[0],ij[1]])]*!DDTOR, 20.0/6371.0,perp+(!PI*0.5))		;Find point 20km from starting point perpendicular to section
	xy03 = LL_ARC_DISTANCE([((x.values)[ij[0],ij[1]]),((y.values)[ij[0],ij[1]])]*!DDTOR, 20.0/6371.0,perp-(!PI*0.5))

	xy04 = LL_ARC_DISTANCE([((x.values)[ij[2],ij[3]]),((y.values)[ij[2],ij[3]])]*!DDTOR, 20.0/6371.0,perp+(!PI*0.5))
	xy05 = LL_ARC_DISTANCE([((x.values)[ij[2],ij[3]]),((y.values)[ij[2],ij[3]])]*!DDTOR, 20.0/6371.0,perp-(!PI*0.5))
	
	
	xysect2 = MAP_2POINTS((x.values)[ij[0],ij[1]]*!DDTOR,(y.values)[ij[0],ij[1]]*!DDTOR,$
								xy02[0],xy02[1], NPATH = npath1, /RADIANS)
	
	xysect3 = MAP_2POINTS((x.values)[ij[0],ij[1]]*!DDTOR,(y.values)[ij[0],ij[1]]*!DDTOR,$
								xy03[0],xy03[1], NPATH = npath1, /RADIANS)

	xysect4 = MAP_2POINTS((x.values)[ij[2],ij[3]]*!DDTOR,(y.values)[ij[2],ij[3]]*!DDTOR,$
								xy04[0],xy04[1], NPATH = npath1, /RADIANS)
	
	xysect5 = MAP_2POINTS((x.values)[ij[2],ij[3]]*!DDTOR,(y.values)[ij[2],ij[3]]*!DDTOR,$
								xy05[0],xy05[1], NPATH = npath1, /RADIANS)
	
	
	OPLOT , xysect[0,*], xysect[1,*], THICK = 4
	XYOUTS, xysect[0,0], xysect[1,0], 'A', ALIGN = 1
	XYOUTS, xysect[0,-1], xysect[1,-1], 'B', ALIGN = 0
	
	OPLOT , xysect2[0,*]*!DRADDEG, xysect2[1,*]*!DRADDEG, THICK = 4
	XYOUTS, xysect2[0,0]*!DRADDEG, xysect2[1,-1]*!DRADDEG, 'A1', ALIGN = 1

	OPLOT , xysect3[0,*]*!DRADDEG, xysect3[1,*]*!DRADDEG, THICK = 4
	XYOUTS, xysect3[0,0]*!DRADDEG, xysect3[1,-1]*!DRADDEG, 'A2', ALIGN = 1

	OPLOT , xysect4[0,*]*!DRADDEG, xysect4[1,*]*!DRADDEG, THICK = 4
	XYOUTS, xysect4[0,0]*!DRADDEG, xysect4[1,-1]*!DRADDEG, 'B1', ALIGN = 1

	OPLOT , xysect5[0,*]*!DRADDEG, xysect5[1,*]*!DRADDEG, THICK = 4
	XYOUTS, xysect5[0,0]*!DRADDEG, xysect5[1,-1]*!DRADDEG, 'B2', ALIGN = 1

	;; Do northern path
	FOR ii = 0, npath1-1 DO BEGIN
		region = [xysect3[0:1,ii],xysect5[0:1,ii]]*!DRADDEG
		x.values      = (x.values + 360.0) MOD 360.0												;Ensure longitudes on 0-360 grid
		region[[0,2]] = (region[[0,2]] + 360.0) MOD 360.0
		x0=region[0]
		x1=region[2]
		y0=region[1]
		y1=region[3]
		
		;;Find index of nearest start point
		i0 = WHERE((ABS(x.values - x0) LT 0.005) AND (ABS(y.values - y0) LT 0.005))
		
		store_arr = []
		FOR xy = 0, N_ELEMENTS(i0)-1 DO BEGIN
			store = SQRT((x.values[i0[xy]] - x0)^2 + (y.values[i0[xy]] - y0)^2)
			store_arr = [store_arr, store]
		ENDFOR
		
		ij_start = WHERE(MIN(store_arr,/NAN) EQ store_arr)
		ij_start = ARRAY_INDICES(x.values, i0[ij_start])

		
		;;Find index of nearest end point
		i1 = WHERE((ABS(x.values - x1) LT 0.005) AND (ABS(y.values - y1) LT 0.005))
		
		store_arr = []
		FOR xy = 0, N_ELEMENTS(i1)-1 DO BEGIN
			store = SQRT((x.values[i1[xy]] - x0)^2 + (y.values[i1[xy]] - y0)^2)
			store_arr = [store_arr, store]
		ENDFOR
		
		ij_end = WHERE(MIN(store_arr,/NAN) EQ store_arr)
		ij_end = ARRAY_INDICES(x.values, i1[ij_end])
				
		USERSYM_CIRCLE, /FILL																			;Load plane symbol at flight path orientation
		PLOTS, region[0], region[1], $															;Overplot plane symbol
			PSYM    = 8, $
			SYMSIZE = 4 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
			NOCLIP  = 0, $
			COLOR   = COLOR_24('black')

		PLOTS, region[2], region[3], $															;Overplot plane symbol
			PSYM    = 8, $
			SYMSIZE = 4 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
			NOCLIP  = 0, $
			COLOR   = COLOR_24('black')

		region_new = [ij_start[0],ij_start[1],ij_end[0],ij_end[1]]
		x0 = ij_start[0]
		y0 = ij_start[1]
		x1 = ij_end[0]
		y1 = ij_end[1]

		PRINT, region, (x.values)[x0,y0],(y.values)[x0,y0],(x.values)[x1,y1],(y.values)[x1,y1]

		xysect_ii = MAP_2POINTS((x.values)[region_new[0],region_new[1]],(y.values)[region_new[0],region_new[1]],$
								(x.values)[region_new[2],region_new[3]],(y.values)[region_new[2],region_new[3]], NPATH = npath1)

		
		USERSYM_STAR, /FILL																			;Load plane symbol at flight path orientation
		PLOTS, (x.values)[x0,y0], y.values[x0,y0], $												;Overplot plane symbol
			PSYM    = 8, $
			SYMSIZE = 4 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
			NOCLIP  = 0, $
			COLOR   = COLOR_24('red')

		PLOTS, (x.values)[x1,y1], y.values[x1,y1], $												;Overplot plane symbol
			PSYM    = 8, $
			SYMSIZE = 4 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
			NOCLIP  = 0, $
			COLOR   = COLOR_24('red')

		OPLOT , xysect_ii[0,*], xysect_ii[1,*], THICK = 4, COLOR = COLOR_24('red')
	ENDFOR

	;; Do southern path
	FOR ii = 0, npath1-1 DO BEGIN
		region = [xysect2[0:1,ii],xysect4[0:1,ii]]*!DRADDEG
		x.values      = (x.values + 360.0) MOD 360.0												;Ensure longitudes on 0-360 grid
		region[[0,2]] = (region[[0,2]] + 360.0) MOD 360.0
		x0=region[0]
		x1=region[2]
		y0=region[1]
		y1=region[3]
		
		;;Find index of nearest start point
		i0 = WHERE((ABS(x.values - x0) LT 0.005) AND (ABS(y.values - y0) LT 0.005))
		
		store_arr = []
		FOR xy = 0, N_ELEMENTS(i0)-1 DO BEGIN
			store = SQRT((x.values[i0[xy]] - x0)^2 + (y.values[i0[xy]] - y0)^2)
			store_arr = [store_arr, store]
		ENDFOR
		
		ij_start = WHERE(MIN(store_arr,/NAN) EQ store_arr)
		ij_start = ARRAY_INDICES(x.values, i0[ij_start])

		
		;;Find index of nearest end point
		i1 = WHERE((ABS(x.values - x1) LT 0.005) AND (ABS(y.values - y1) LT 0.005))
		
		store_arr = []
		FOR xy = 0, N_ELEMENTS(i1)-1 DO BEGIN
			store = SQRT((x.values[i1[xy]] - x0)^2 + (y.values[i1[xy]] - y0)^2)
			store_arr = [store_arr, store]
		ENDFOR
		
		ij_end = WHERE(MIN(store_arr,/NAN) EQ store_arr)
		ij_end = ARRAY_INDICES(x.values, i1[ij_end])
				
		USERSYM_CIRCLE, /FILL																			;Load plane symbol at flight path orientation
		PLOTS, region[0], region[1], $															;Overplot plane symbol
			PSYM    = 8, $
			SYMSIZE = 4 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
			NOCLIP  = 0, $
			COLOR   = COLOR_24('black')

		PLOTS, region[2], region[3], $															;Overplot plane symbol
			PSYM    = 8, $
			SYMSIZE = 4 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
			NOCLIP  = 0, $
			COLOR   = COLOR_24('black')

		region_new = [ij_start[0],ij_start[1],ij_end[0],ij_end[1]]
		x0 = ij_start[0]
		y0 = ij_start[1]
		x1 = ij_end[0]
		y1 = ij_end[1]

		PRINT, region, (x.values)[x0,y0],(y.values)[x0,y0],(x.values)[x1,y1],(y.values)[x1,y1]

		xysect_ii = MAP_2POINTS((x.values)[region_new[0],region_new[1]],(y.values)[region_new[0],region_new[1]],$
								(x.values)[region_new[2],region_new[3]],(y.values)[region_new[2],region_new[3]], NPATH = npath1)

		
		USERSYM_STAR, /FILL																			;Load plane symbol at flight path orientation
		PLOTS, (x.values)[x0,y0], y.values[x0,y0], $												;Overplot plane symbol
			PSYM    = 8, $
			SYMSIZE = 4 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
			NOCLIP  = 0, $
			COLOR   = COLOR_24('red')

		PLOTS, (x.values)[x1,y1], y.values[x1,y1], $												;Overplot plane symbol
			PSYM    = 8, $
			SYMSIZE = 4 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
			NOCLIP  = 0, $
			COLOR   = COLOR_24('red')

		OPLOT , xysect_ii[0,*], xysect_ii[1,*], THICK = 4, COLOR = COLOR_24('red')
	ENDFOR

ENDIF


MAP_SET, yc, xc, 0, CONIC = 1, $																				;Redraw map boundaries
	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
	ISOTROPIC = 1, $
	NOERASE   = 1, $
	POSITION  = map_pos
	
COLOR_BAR_24_KPB, table, OVER = table[-1], $
	RANGE = [0, 52], $
	TICKS = 4, $
	TITLE = 'Wind Speed (m/s)', $
	POSIT = bar_pos

IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file

END
