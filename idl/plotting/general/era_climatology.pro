PRO ERA_CLIMATOLOGY,  $
	PNG		  = png, $
	EPS		  = eps, $
	CLOBBER   = clobber

;+
;NAME:
;     TRAJ3D_RAP_P
;PURPOSE:
;     This copies variables from ERA-Interim analysis into a single
;     file in pressure coordinates for use in TRAJ3D.
;     W at the surface is set to zero.  One pressure level is added at 
;     the top of the domain (p = 0), where w is also set to zero.
;CATEGORY:
;     Data handling utility.
;CALLING SEQUENCE:
;     TRAJ3D_RAP_P, date0, outfile
;INPUT:
;		start_date : user specified start date to plot in yyyymmdd format (e.g., '20170101')
;		end_date   : user specified end date to plot in yyyymmdd format (e.g., '20180101')
;KEYWORDS:
;     PLOT      : If set, plot sample maps.
;     DIRECTORY : Output directory for wind file.
;	  CLOBBER   : If set, overwrite existing file. This is the default.
;OUTPUT:
;     Netcdf file.
;MODIFICATION HISTORY:
;		D. Phoenix:       2019-02-08.
;						  2019-02-11. Read monthly averaged data over 1980-2017 period
;										and compute period averages.
;-

COMPILE_OPT IDL2																									;Set compile options

yr_arr = ['1980','1981','1982','1983','1984','1985','1986','1987','1988','1989','1990', $
			'1991','1992','1993','1994','1995','1996','1997','1998','1999','2000', $
			'2001','2002','2003','2004','2005','2006','2007','2008','2009','2010', $
			'2011','2012','2013','2014','2015','2016']
;yr_arr = ['2011','2012','2013']

aug_gph =FLTARR(360,181,N_ELEMENTS(yr_arr))
aug_u   =FLTARR(360,181,N_ELEMENTS(yr_arr))
aug_v   =FLTARR(360,181,N_ELEMENTS(yr_arr))
aug_temp=FLTARR(360,181,N_ELEMENTS(yr_arr))

i=0
FOREACH year1, yr_arr DO BEGIN
	data = READ_REANALYSIS_MONTHLY_MEAN(year1,8,'MERRA2')
    x = data.X.values
    y = data.Y.values
    
    yc = 32.85
    xc = 360.0-96.86
    
    x0 = 360.0-105.0
    y0 = 22.0
    x1 = 360.0-62.73
    y1 = 52.5
    
    ix0 = WHERE(ROUND(x0) EQ x)
    ix1 = WHERE(ROUND(x1) EQ x) 
    iy0 = WHERE(ROUND(y0) EQ y)
    iy1 = WHERE(ROUND(y1) EQ y) 

	aug_gph [ix0:ix1,iy0:iy1,i] = data.GPH.values[ix0:ix1,iy0:iy1,4]
	aug_u   [ix0:ix1,iy0:iy1,i] = data.U.values  [ix0:ix1,iy0:iy1,4]
	aug_v   [ix0:ix1,iy0:iy1,i] = data.V.values  [ix0:ix1,iy0:iy1,4]
	aug_temp[ix0:ix1,iy0:iy1,i] = data.T.values  [ix0:ix1,iy0:iy1,4]
	i+=1
ENDFOREACH

x = data.X.values
y = data.Y.values

aug_gph_ave  = MEAN(aug_gph,  DIM=3, /NAN)
aug_u_ave	 = MEAN(aug_u,    DIM=3, /NAN)
aug_v_ave 	 = MEAN(aug_v,    DIM=3, /NAN)
aug_temp_ave = MEAN(aug_temp, DIM=3, /NAN)

map_pos = [0.05, 0.15, 0.95, 0.95]																			;Set map position
bar_pos = [0.25, 0.10, 0.75, 0.12]																			;Set color bar position

table  = (HCL_COLOR_TABLE(100, HUE_RANGE = [100.0, 300.0]))
levels = 600+FINDGEN(100)

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
		WINDOW, XSIZE = 1200, YSIZE = 800										;Open graphics window
		!P.COLOR      = COLOR_24('black')																		;Foreground color
		!P.BACKGROUND = COLOR_24('white')																		;Background color
		!P.CHARSIZE   = 2.0		
		!P.FONT       = -1																							;Use Hershey fonts
	ENDELSE
ENDELSE

MAP_SET, yc, xc, 0, CONIC = 1, $																;Draw map
	LIMIT     = [y0,x0,y1,x1], $
	ISOTROPIC = 1, $
	TITLE     = 'August GPH at 950 mb', $
	POSITION  = map_pos

;CONTOUR, aug_temp_ave, x, y, $												;Contour reflectivity values
CONTOUR,aug_gph_ave[ix0:ix1,iy0:iy1] ,x[ix0:ix1],y[iy0:iy1],$
	OVERPLOT  = 1, $
	FILL      = 1, $
	LEVELS    = levels, $
	C_COLOR   = table

MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines

COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
	RANGE = [600,700], $
	TICKS = 5, $
	TITLE = 'GPH (m)', $
	POSIT = bar_pos


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IF (N_ELEMENTS(date1) EQ 0) THEN date1 = MAKE_DATE(1980,01,01,00)
IF (N_ELEMENTS(date2) EQ 0) THEN date2 = MAKE_DATE(2018,01,01,00)
nt = DATE_DIFF(date2,date1) / 21600.

pressure = (ERA_INTERIM_READ_VAR('P',date1)).values
dim = SIZE(pressure,/DIMENSIONS)

pressure1 = FLTARR(dim[0],dim[1], nt)
temp1     = FLTARR(dim[0],dim[1], nt)
z1        = FLTARR(dim[0],dim[1], nt)
u1        = FLTARR(dim[0],dim[1], nt)
v1        = FLTARR(dim[0],dim[1], nt)

x = FINDGEN(360)
y = FINDGEN(181)-90.0
FOR i=0, nt-1 DO BEGIN
    pressure = (ERA_INTERIM_READ_VAR('P',date)).values
    temp	 = (ERA_INTERIM_READ_VAR('T',date)).values
    Z	 	 = (ERA_INTERIM_READ_VAR('Z',date)).values
    u	 	 = (ERA_INTERIM_READ_VAR('u',date)).values
    v		 = (ERA_INTERIM_READ_VAR('v',date)).values

	;Create time arrays
	pressure1[*,*,i] = pressure[*,*,index]
    temp1[*,*,i]     = temp[*,*,index]
    z1 [*,*,i]       = Z[*,*,index]
    u1 [*,*,i]       = u[*,*,index]
    v1 [*,*,i]       = v[*,*,index]
	PRINT, date1
	date1 = TIME_INC(date1, 21600)
ENDFOR
month_z1 = pressure1
month_z1 = z1

y0 = 5.0
y1 = 65.0
x0 = -110.0
x1 = -20.0

range = where(((x) GT x0) AND ((x) LT x1) AND((y) GT y0) AND ((y) LT y1))  

yc = 40.0
xc = -80.0

map_pos = [0.05, 0.15, 0.95, 0.95]																			;Set map position
bar_pos = [0.25, 0.10, 0.75, 0.12]																			;Set color bar position

table  = (HCL_COLOR_TABLE(145, HUE_RANGE = [100.0, 300.0]))
levels = 880.0+FINDGEN(145)
levels = MEAN(month_z1)+FINDGEN(145)

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
		WINDOW, XSIZE = 1200, YSIZE = 800										;Open graphics window
		!P.COLOR      = COLOR_24('black')																		;Foreground color
		!P.BACKGROUND = COLOR_24('white')																		;Background color
		!P.CHARSIZE   = 2.0		
		!P.FONT       = -1																							;Use Hershey fonts
	ENDELSE
ENDELSE

MAP_SET, yc, xc, 0, CONIC = 1, $																;Draw map
	LIMIT     = [y0,x0,y1,x1], $
	ISOTROPIC = 1, $
	TITLE     = 'July Ave Temp at 950 mb', $
	POSITION  = map_pos

CONTOUR, month_z1, x, y, $												;Contour reflectivity values
	OVERPLOT  = 1, $
	FILL      = 1, $
	LEVELS    = levels, $
	C_COLOR   = table

MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines

COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
	RANGE = [880, 1024], $
	TICKS = 5, $
	TITLE = 'Pressure (hPa)', $
	POSIT = bar_pos

STOP
hurr_data = READ_HURDAT2()

dates  =  hurr_data.field01[1,*]  
period = where((dates GE start_date) AND (dates LT end_date)) 

lat   = hurr_data.field01[5,*] 
lon   = ((-1)*hurr_data.field01[6,*]) 
press = hurr_data.field01[8,*] 

inan  = WHERE(press LT -1)
press[inan] = !Values.F_NaN

period_0 = min(period)
period_1 = max(period)  

lon   = lon[period_0:period_1] 
lat   = lat[period_0:period_1] 
press = press[period_0:period_1]

table  = REVERSE(HCL_COLOR_TABLE(145, HUE_RANGE = [100.0, 300.0]))
levels = 880.0+FINDGEN(145)

y0 = 5.0
y1 = 65.0
x0 = -110.0
x1 = -20.0

yc = 40.0
xc = -80.0

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
		WINDOW, XSIZE = 1200, YSIZE = 800										;Open graphics window
		!P.COLOR      = COLOR_24('black')																		;Foreground color
		!P.BACKGROUND = COLOR_24('white')																		;Background color
		!P.CHARSIZE   = 2.0		
		!P.FONT       = -1																							;Use Hershey fonts
	ENDELSE
ENDELSE


MAP_SET, yc, xc, 0, CONIC = 1, $																;Draw map
	LIMIT     = [y0,x0,y1,x1], $
	ISOTROPIC = 1, $
	TITLE     = 'Hurricane Tracks from: ' + STRING(start_date) + ' to ' + STRING(end_date), $
	POSITION  = map_pos

;PLOTS, ((-1)*hurr_data.field01[6,*]),hurr_data.field01[5,*], $
;	PSYM   = 0

FOR ii=0,N_ELEMENTS(lon)-1 DO BEGIN
	icolor = WHERE(levels EQ press[ii])
	PLOTS, lon[ii], lat[ii], $
		PSYM 	 = 0, $
		NOCLIP   = 0, $
		CONTINUE = 1, $
		THICK	 = 3, $
		COLOR 	 = table[icolor]
ENDFOR
	
MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines

COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
	RANGE = [880, 1024], $
	TICKS = 5, $
	TITLE = 'Pressure (hPa)', $
	POSIT = bar_pos


STOP

END
