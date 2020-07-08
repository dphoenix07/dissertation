PRO SENSITIVITY_DOMAINS, $
	EPS         = eps, $
	PNG         = png

;+
; Name:
;		SENSITIVITY_DOMAINS
; Purpose:
;		This is a procedure to plots the DC3 flight paths for each case simulated and 
;		shows the location of the nested domains used for analysis
; Calling sequence:
;		SENSITIVITY_DOMAINS
; Input:
;		arg1 : positional parameter 1
; Output:
;		arg2 : positional parameter 2
; Keywords:
;		key1 : keyword parameter 1
; Author and history:
;		Daniel B. Phoenix 	2017-03-07.
;-

COMPILE_OPT IDL2																									;Set compile options

epsfile = '~/sensitivity_domains.eps'
pngfile = '~/sensitivity_domains.png'

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


;color 	       = [COLOR_24_DP('RED'), COLOR_24_DP('BLUE'), COLOR_24_DP('gray50')]
color 	       = [COLOR_24_DP('gray80'), COLOR_24_DP('gray50'), COLOR_24_DP('BLACK')]



flight_name_arr = ['20120519','20120529','20120601']
date_arr		= [MAKE_DATE(2012,5,19,12),MAKE_DATE(2012,5,29,12),MAKE_DATE(2012,6,1,12)]
experiment		= ['nssl_ysu','nssl','nssl']

min_x = []
max_x = []
min_y = []
max_y = []

i=0
FOREACH date, date_arr DO BEGIN

x   = WRF_READ_VAR('Longitude', date, flight_name_arr[i], experiment[i], DOMAIN = domain)		;Read variables
y   = WRF_READ_VAR('Latitude' , date, flight_name_arr[i], experiment[i], DOMAIN = domain)

min_x = [min_x, MIN(x.values)]
max_x = [max_x, MAX(x.values)]
min_y = [min_y, MIN(y.values)]
max_y = [max_y, MAX(y.values)]

i=i+1
ENDFOREACH

x_total = REBIN(MAKEN(MIN(min_x), MAX(max_x)+1.0,600), 600, 540)
y_total = REBIN(MAKEN(MIN(min_y)-1.0, MAX(max_y)+2.0,600), 600, 540)


dim = SIZE(x_total, /DIMENSIONS)																			;Get dimension sizes

offset = 0 
y0 = y_total[          offset ,          offset ]														;Set domain boundary points
y1 = y_total[dim[0]-(1+offset),dim[1]-(1+offset)]
y2 = y_total[dim[0]-(1+offset),dim[1]-(1+offset)]
y3 = y_total[          offset ,          offset ]	
x0 = x_total[          offset ,          offset ]
x1 = x_total[          offset ,dim[1]-(1+offset)]
x2 = x_total[dim[0]-(1+offset),dim[1]-(1+offset)]
x3 = x_total[dim[0]-(1+offset),          offset ]

xc = INTERPOLATE(x_total, 0.5*(dim[0]-1), 0.5*(dim[1]-1))											;Get central grid point
yc = INTERPOLATE(y_total, 0.5*(dim[0]-1), 0.5*(dim[1]-1))

map_pos = [0.05, 0.15, 0.95, 0.95]																			;Set map position
table   = [COLOR_24(200, 200, 200),VISUALIZE_88D_COLOR()]												;Set reflectivity color table
rlevels = [-100.0, 5.0*FINDGEN(N_ELEMENTS(table))]													;Set reflectivity contour levels

;; Set up map
MAP_SET, yc, xc, 0, CONIC = 1, $ 																;Draw map
	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
	ISOTROPIC = 1, $
	TITLE     = 'Flight Path + WRF Domains', $
	POSITION  = map_pos

i=0
FOREACH date, date_arr DO BEGIN

zdc8  = DC3_READ_VAR('G_ALT' , flight_name_arr[i], /DC8)											;Read aircraft altitude
ydc8  = DC3_READ_VAR('G_LAT' , flight_name_arr[i], /DC8)
xdc8  = DC3_READ_VAR('G_LONG', flight_name_arr[i], /DC8)
tdc8  = DC3_READ_VAR('Time'  , flight_name_arr[i], /DC8)

zgv  = DC3_READ_VAR('GGALT'  , flight_name_arr[i])
ygv  = DC3_READ_VAR('GGLAT'  , flight_name_arr[i])
xgv  = DC3_READ_VAR('GGLON'  , flight_name_arr[i])
tgv  = DC3_READ_VAR('Time'   , flight_name_arr[i])

idc8 =  SIZE(tdc8.values,/DIMENSIONS)
igv  =  SIZE(tgv.values ,/DIMENSIONS)

x   = WRF_READ_VAR('Longitude', date, flight_name_arr[i], experiment[i], DOMAIN = domain)		;Read variables
y   = WRF_READ_VAR('Latitude' , date, flight_name_arr[i], experiment[i], DOMAIN = domain)
r   = WRF_READ_VAR('REFL'     , date, flight_name_arr[i], experiment[i], DOMAIN = domain)
dim = SIZE(x.values, /DIMENSION)


;x(0:1,0),y(0:1,0)
;x(0,0:1),y(0,0:1)
;x(1,0:1),y(1,0:1)
;x(0:1,1),y(0:1,1)

OPLOT, x.values[0:dim[0]-1,0], y.values[0:dim[0]-1,0], COLOR = color[i], THICK = 5
OPLOT, x.values[0,0:dim[1]-1], y.values[0,0:dim[1]-1], COLOR = color[i], THICK = 5
OPLOT, x.values[dim[0]-1,0:dim[1]-1], y.values[dim[0]-1,0:dim[1]-1], COLOR = color[i], THICK = 5
OPLOT, x.values[0:dim[0]-1,dim[1]-1], y.values[0:dim[0]-1,dim[1]-1], COLOR = color[i], THICK = 5


;; Overplot aircraft path from beginning to current time
OPLOT, (xdc8.values[0:idc8-1]), (ydc8.values[0:idc8-1]), THICK = 2, COLOR = color[i]
OPLOT, (xgv.values [0:igv -1]), (ygv.values [0:igv -1]), THICK = 2, COLOR = color[i]

i = i+1
ENDFOREACH ;date



MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines




	


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
