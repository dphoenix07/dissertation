PRO DC3_PLOT_DC8_FLIGHTPATH, flight_name, date, $
	BINNED      = binned, $
	EPS         = eps, $
	PNG         = png

;+
; Name:
;		DC3_PLOT_DC8_FLIGHTPATH
; Purpose:
;		This is a procedure to plot DC3 trace gases in relative altitude to the tropopause. 
; Calling sequence:
;		DC3_PLOT_DC8_FLIGHTPATH
; Input:
;		arg1 : positional parameter 1
; Output:
;		arg2 : positional parameter 2
; Keywords:
;		key1 : keyword parameter 1
; Author and history:
;		Daniel B. Phoenix 	2017-01-03.
;-

COMPILE_OPT IDL2																									;Set compile options

IF (N_ELEMENTS(flight_name) EQ 0) THEN fname = 'all' $
											 ELSE fname = flight_name[0]

IF KEYWORD_SET(pv_relative) THEN rname = '_2pvu' $
									 ELSE rname = '_trop'

epsfile = '~/dc3_' + fname + '_tracer_ralt' + rname + '.eps'
pngfile = '~/dc3_' + fname + '_tracer_ralt' + rname + '.png'

IF KEYWORD_SET(eps) THEN BEGIN	
	PS_ON, FILENAME = epsfile, PAGE_SIZE = [6.0, 4.0], MARGIN = 0.0, /INCHES					;Switch to Postscript device
	DEVICE, /ENCAPSULATED
	!P.FONT     = 0																								;Hardware fonts
	!P.CHARSIZE = 0.8
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS																							;Load basic color definitions
ENDIF ELSE BEGIN
	SET_PLOT, 'X'
	WINDOW, XSIZE = 1200, YSIZE = 800																		;Open graphics window
	!P.COLOR      = COLOR_24('black')																		;Foreground color
	!P.BACKGROUND = COLOR_24('white')																		;Background color
	!P.CHARSIZE   = 2.5		
	!P.FONT       = -1																							;Use Hershey fonts
ENDELSE
!P.MULTI = [4,2,0]


;zdc8  = DC3_READ_VAR('G_ALT' , flight_name, /DC8)			;G_ALT								;Read aircraft altitude
;ydc8  = DC3_READ_VAR('G_LAT' , flight_name, /DC8)
;xdc8  = DC3_READ_VAR('G_LONG', flight_name, /DC8)

;; FOR DC8 ;;
;xdc8.values = xdc8.values + 0.45
;ydc8.values = ydc8.values + 0.95

zdc8  = DC3_READ_VAR('GGALT' , flight_name)
ydc8  = DC3_READ_VAR('GGLAT' , flight_name)
xdc8  = DC3_READ_VAR('GGLON', flight_name)
tdc8  = DC3_READ_VAR('Time', flight_name)

;; FOR GV ;;
xdc8.values = xdc8.values - 0.1
ydc8.values = ydc8.values + 0.65


run 		= flight_name
experiment 	= 'nssl_ysu'
domain 		= 2
offset 		= 0

x   = WRF_READ_VAR('Longitude', date, run, experiment, DOMAIN = domain)		;Read variables
y   = WRF_READ_VAR('Latitude' , date, run, experiment, DOMAIN = domain)
r   = WRF_READ_VAR('REFL'     , date, run, experiment, DOMAIN = domain)
z   = WRF_READ_VAR('Z'		  , date, run, experiment, DOMAIN = domain)
o   = WRF_READ_VAR('O3'       , date, run, experiment, DOMAIN = domain)

dim = SIZE(z.values, /DIMENSIONS)																			;Get dimension sizes

dc8_off = -5400
dtdc8 = TIME_DIFF(tdc8.values, date)
idc8  = WHERE((dtdc8 GE dc8_off) AND (dtdc8 LE (dc8_off + 300)), ndc8)
idc80 = WHERE(dtdc8 EQ dc8_off, npsym)
 

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

map_pos = [0.05, 0.15, 0.95, 0.95]																			;Set map position
table   = [COLOR_24(200, 200, 200),VISUALIZE_88D_COLOR()]												;Set reflectivity color table
rlevels = [-100.0, 5.0*FINDGEN(N_ELEMENTS(table))]													;Set reflectivity contour levels


;; Set up map
MAP_SET, yc, xc, 0, CONIC = 1, /NOBORDER, $																;Draw map
	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
	ISOTROPIC = 1, $
	TITLE     = 'Flight Path + WRF Reflectivity', $
	POSITION  = map_pos

STOP
;; Contour reflectivity values
CONTOUR, MAX(R.values, DIM=3), x.values, y.values, $												;Contour reflectivity values
		OVERPLOT  = 1, $
		FILL      = 1, $
		LEVELS    = rlevels, $
		C_COLOR   = table

;; Overplot aircraft path from beginning to current time
OPLOT, (xdc8.values[0:idc8[0]]), (ydc8.values[0:idc8[0]]), THICK = 3
PLOTS, [x.values[50],y.values[50]],[x.values[250],y.values[50]], /DATA, THICK=5
	
;; Computing the airplane orientation
dx = (xdc8.values)[idc80] - (xdc8.values)[(idc80-30) > 0]							;Compute coordinate lengths for past 30 seconds										
dy = (ydc8.values)[idc80] - (ydc8.values)[(idc80-30) > 0]
d  = SQRT(dx^2 + dy^2)
po = ACOS(dy/d) + 2.0*(!Pi - ACOS(dy/d))*(dx LT 0)
						
USERSYM_PLANE, /FILL, ORIENTATION = po[0]/!DDTOR									;Load plane symbol at flight path orientation

PLOTS, (xdc8.values)[idc80], (ydc8.values)[idc80], $								;Overplot plane symbol
	PSYM    = 8, $
	SYMSIZE = 8 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
	NOCLIP  = 0, $
	COLOR   = COLOR_24('white')

MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines

STOP

;flight_x = MAKE_ARRAY(dim[0],dim[1],/FLOAT,value = xdc8.values[idc80])
;flight_y = MAKE_ARRAY(dim[0],dim[1],/FLOAT,value = ydc8.values[idc80])
;flight_z = MAKE_ARRAY(dim[2],/FLOAT,value = zdc8.values[idc80])

;FOR xx=0,dim[0]-1 DO BEGIN
;  FOR yy=0,dim[1]-1 DO BEGIN
;	min_val[xx,yy] = (ABS(grid[xx,yy,0] - xdc8.values[idc80]) + ABS(grid[xx,yy,1] - ydc8.values[idc80]))
;  ENDFOR
;ENDFOR


;; PUT IN CHECK FOR TIME ;;

min_val = FLTARR(dim[0],dim[1])
grid	= FLTARR(dim[0],dim[1],2)
grid    = [[[x.values]],[[y.values]]]
ozone   = [ ]

s1 = idc80-(30*60)
s2 = idc80
FOR ss=0,1800,5 DO BEGIN
	flight_x = MAKE_ARRAY(dim[0],dim[1],/FLOAT,value = xdc8.values[ss+3300])
	flight_y = MAKE_ARRAY(dim[0],dim[1],/FLOAT,value = ydc8.values[ss+3300])
	flight_z = MAKE_ARRAY(dim[2],/FLOAT,value = zdc8.values[ss+3300])
	
	FOR xx=0,dim[0]-1 DO BEGIN
	  FOR yy=0,dim[1]-1 DO BEGIN
		min_val[xx,yy] = (ABS(grid[xx,yy,0] - xdc8.values[ss+3300]) + ABS(grid[xx,yy,1] - ydc8.values[ss+3300]))
	  ENDFOR
	ENDFOR
	
	good = WHERE(MIN(min_val[*,*]) EQ min_val[*,*])
	ind  = ARRAY_INDICES(min_val[*,*], good)

	k_good = WHERE(MIN(ABS(z.values[ind[0],ind[1],*] - flight_z)) EQ $
			(ABS(z.values[ind[0],ind[1],*] - flight_z)))

	ozone = [ozone, o.values[ind[0],ind[1],k_good]]
;	PRINT, z.values[ind[0],ind[1],k_good], mean(flight_z)
ENDFOR

;good  = WHERE(MIN(min_val) EQ min_val)
;ind   = ARRAY_INDICES(min_val, good)

;PRINT, 'x difference', x.values[ind[0],ind[1]] - MEAN(flight_x)
;PRINT, 'y difference', y.values[ind[0],ind[1]] - MEAN(flight_y)
;
;k_good = WHERE(MIN(ABS(z.values[ind[0],ind[1],*] - flight_z)) EQ $
;			(ABS(z.values[ind[0],ind[1],*] - flight_z)))
;
;PRINT, z.values[ind[0],ind[1],k_good]
;PRINT, MEAN(flight_z)
;PRINT, 'z difference', (z.values[ind[0],ind[1],k_good]) - MEAN(flight_z)

;PRINT, ozone
	


IF KEYWORD_SET(pv_relative) THEN BEGIN
	ytitle = '2-pvu Relative (km)'
	z_trop = DC3_READ_VAR('GFS_2PVU_HGT', flight_name, /DC8)											;Read tropopause level
ENDIF ELSE BEGIN	
	ytitle = 'Tropopause Relative (km)'
	z_trop = DC3_READ_VAR('GFS_TROP_HGT', flight_name, /DC8)
ENDELSE


!P.MULTI = 0
IF KEYWORD_SET(eps) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file

END
