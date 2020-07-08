PRO MAP_OZONE_NOSTORMS, date1, lev, $
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
;						  2019-02-11. Updated to include ERA-Interim Reanalysis data
;									  of 950 hPa geopotential heights and markers of 
;									  the O3 Concentration at Dallas
;-

COMPILE_OPT IDL2																									;Set compile options

IF (N_ELEMENTS(date1) EQ 0) THEN date1 = MAKE_DATE(1980,01,01,00)
lev = 850.0
;lev=950.0

pressure = (ERA_INTERIM_READ_VAR('P',date1)).values
dim = SIZE(pressure,/DIMENSIONS)

dallas_x = (360.0-96.86)
dallas_y = 32.85

x = FINDGEN(360)
y = FINDGEN(181)-90.0

yc = 32.85
xc = 360.0-96.86

;x0 = 360.0-105.0
;y0 = 22.0
;x1 = 360.0-62.73
;y1 = 52.5
x0 = 360.0-150.0
y0 = 15.0
x1 = 360.0-50.0
y1 = 52.5
;x1 for Gulf storms: x1 = 360.0-80.0

ix0 = WHERE(ROUND(x0) EQ x)
ix1 = WHERE(ROUND(x1) EQ x) 
iy0 = WHERE(ROUND(y0) EQ y)
iy1 = WHERE(ROUND(y1) EQ y) 

pressure1 = FLTARR((ix1-ix0)+1,(iy1-iy0)+1)
pv1		  = FLTARR((ix1-ix0)+1,(iy1-iy0)+1)
temp1     = FLTARR((ix1-ix0)+1,(iy1-iy0)+1)
z1        = FLTARR((ix1-ix0)+1,(iy1-iy0)+1)
u1        = FLTARR((ix1-ix0)+1,(iy1-iy0)+1)
v1        = FLTARR((ix1-ix0)+1,(iy1-iy0)+1)

date2 = STRMID(MAKE_ISO_DATE_STRING(date1),0,4) + STRMID(MAKE_ISO_DATE_STRING(date1),5,2) + $
		 STRMID(MAKE_ISO_DATE_STRING(date1),8,2) + STRMID(MAKE_ISO_DATE_STRING(date1),11,2) + $
	  	 STRMID(MAKE_ISO_DATE_STRING(date1),14,2) 

year = date1.year
o3_data  	   = OZONE_DALLAS_NAAQS(STRTRIM(year,1))	
;nox_data 	   = NOX_DALLAS_NAAQS(STRTRIM(year,1))
;co_data 	   = CO_DALLAS_NAAQS(STRTRIM(year,1))
;temp_data 	   = TEMP_DALLAS_NAAQS(STRTRIM(year,1))
;press_data 	   = PRESS_DALLAS_NAAQS(STRTRIM(year,1))
;wind_spd_data  = WIND_SPD_DALLAS_NAAQS(STRTRIM(year,1))
;wind_dir_data  = WIND_DIR_DALLAS_NAAQS(STRTRIM(year,1))


 pressure = (ERA_INTERIM_READ_VAR('P' ,date1)).values
 pv		  = (ERA_INTERIM_READ_VAR('PV',date1)).values
 temp	  = (ERA_INTERIM_READ_VAR('T' ,date1)).values
 Z	 	  = (ERA_INTERIM_READ_VAR('Z' ,date1)).values
 u	 	  = (ERA_INTERIM_READ_VAR('u' ,date1)).values
 v		  = (ERA_INTERIM_READ_VAR('v' ,date1)).values

 ;interpolate to 950 hPa pressure level	
 dim = SIZE(Z, /DIMENSIONS)
 nx = dim[0]
 ny = dim[1]
 nz = dim[2]
 ix = REBIN(FINDGEN(nx), nx, ny)
 iy = REBIN(REFORM(FINDGEN(ny), 1, ny), nx, ny)
 
 ;Calculate interpolation index for the 10.5 km level in each model grid column
 iz = FLTARR(nx, ny)
 FOR ii = 0, nx-1 DO FOR jj = 0, ny-1 DO iz[ii,jj] = INTERPOL(FINDGEN(nz), REFORM(pressure[ii,jj,*], nz), lev)
 
 ;Interpolate ozone volume to 10.5 km altitude map
 temp_map  = INTERPOLATE(temp, ix, iy, iz)
 pv_map    = INTERPOLATE(pv,   ix, iy, iz)
 gph_map   = INTERPOLATE(Z,    ix, iy, iz)
 u_map     = INTERPOLATE(u,    ix, iy, iz)
 v_map     = INTERPOLATE(v,    ix, iy, iz)

 map_pos = [0.05, 0.15, 0.95, 0.95]																			;Set map position
 bar_pos = [0.05, 0.10, 0.45, 0.12]	
 bar_pos2= [0.55, 0.10, 0.95, 0.12]																		
 
table   = [COLOR_24(200, 200, 200),VISUALIZE_88D_COLOR(0)]												;Set reflectivity color table
 levels = FINDGEN(N_ELEMENTS(table))
 ;ws_levels = levels*4.0
 ws_levels = levels*2.0
 pv_levels = levels - 5.0
 ;levels = levels*50.0 + 10200.0
 ;levels = levels*50.0 + 9000.0
 ;levels = levels*50.0 + 5300.0
 levels = levels*25.0 + 1300.0
 
 ;levels = levels*15 + 450
 ;levels = levels*30 + 200
 
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

;x0 = 360.0-105.0
;y0 = 22.0
;x1 = 360.0-62.73
;y1 = 52.5
x0 = 360.0-110.0
y0 = 15.0
x1 = 360.0-60.0
y1 = 52.0

 MAP_SET, yc, xc, 0, CONIC = 1, $																;Draw map
 	LIMIT     = [y0,x0,y1,x1], $
 	ISOTROPIC = 1, $
	TITLE     = STRING(MAKE_ISO_DATE_STRING(date1)) +  STRMID(lev,1) + " hPa", $
 	POSITION  = map_pos

 MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines
 
 wspd = SQRT(u1^2 + v1^2)
 wspd_map = SQRT(u_map^2 + v_map^2)
;CONTOUR, pv_map[ix0:ix1,iy0:iy1], x[ix0:ix1], y[iy0:iy1], $												;Contour reflectivity values
;	OVERPLOT  = 1, $
;	FILL      = 1, $
;	LEVELS    = pv_levels, $
;	C_COLOR   = table

CONTOUR, wspd_map[ix0:ix1,iy0:iy1], x[ix0:ix1], y[iy0:iy1], $												;Contour reflectivity values
	OVERPLOT  = 1, $
	FILL      = 1, $
	LEVELS    = ws_levels, $
	C_COLOR   = table

CONTOUR, gph_map[ix0:ix1,iy0:iy1]*1.0E-3, x[ix0:ix1], y[iy0:iy1], $												;Contour reflectivity values
	OVERPLOT   = 1, $
	FILL       = 0, $
	LEVELS     = levels*1.0E-3, $
	C_THICK    = 3, $
	C_LABELS   = [1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0], $
	C_CHARSIZE = 1.5

; CONTOUR, gph_map[ix0:ix1,iy0:iy1], x[ix0:ix1], y[iy0:iy1], $												;Contour reflectivity values
; 	OVERPLOT   = 1, $
; 	FILL       = 1, $
; 	LEVELS     = levels, $
;  	C_COLOR    = table

nxy = 60
xreg = MAKEN(MIN(x[ix0:ix1]), MAX(x[ix0:ix1]), nxy)
yreg = MAKEN(MIN(y[iy0:iy1]), MAX(y[iy0:iy1]), nxy)

x1=x[ix0:ix1]
y1=y[iy0:iy1]
dim = SIZE(u1,/DIMENSIONS)
xgrid = REBIN(x1,dim[0],dim[1],/SAMPLE)
ygrid = REBIN(REFORM(y1,1,dim[1]),dim[0],dim[1],/SAMPLE)

TRIANGULATE, xgrid, ygrid, tri

 ureg = TRIGRID(xgrid, ygrid, u_map[ix0:ix1,iy0:iy1], tri, XOUT = xreg, YOUT = yreg)
 vreg = TRIGRID(xgrid, ygrid, v_map[ix0:ix1,iy0:iy1], tri, XOUT = xreg, YOUT = yreg)
 
 VELOVECT, ureg, vreg, xreg, yreg, OVERPLOT  = 1, LENGTH = 3.0

 i0 = WHERE((ABS(dallas_x - (xgrid)) LT 0.5) AND (ABS(dallas_y - ygrid) LT 0.5))
 
 USERSYM_STAR, /FILL																					;Load plane symbol at flight path orientation
 o3_8hr   = o3_data.ozone_8hr_total
 
 o3table  = (HCL_COLOR_TABLE(70, HUE_RANGE = [100.0, 300.0]))
 o3levels = 10.0+FINDGEN(N_ELEMENTS(o3table))

 datestr   = MAKE_ISO_DATE_STRING(date1)
 io3       = WHERE(STRMID(datestr,0,16) EQ (STRING(o3_data.date) 	    + ' ' + STRING(o3_data.time)))
 ;inox      = WHERE(STRMID(datestr,0,16) EQ (STRING(nox_data.date) 	    + ' ' + STRING(nox_data.time)))
 ;ico 	  = WHERE(STRMID(datestr,0,16) EQ (STRING(co_data.date) 	    + ' ' + STRING(co_data.time)))
 ;itemp 	  = WHERE(STRMID(datestr,0,16) EQ (STRING(temp_data.date) 		+ ' ' + STRING(temp_data.time)))
 ;ipress 	  = WHERE(STRMID(datestr,0,16) EQ (STRING(press_data.date) 		+ ' ' + STRING(press_data.time)))
 ;iwind_dir = WHERE(STRMID(datestr,0,16) EQ (STRING(wind_dir_data.date) 	+ ' ' + STRING(wind_dir_data.time)))
 ;iwind_spd = WHERE(STRMID(datestr,0,16) EQ (STRING(wind_spd_data.date) 	+ ' ' + STRING(wind_spd_data.time)))
;
 ozone1 = FLOAT(ROUND(MAX(o3_8hr[io3],/NAN)))
 PRINT, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
 PRINT, date1
 PRINT, 'ozone concentration: '  + STRTRIM(ozone1,1)
 ;PRINT, 'nox concentration: ' 	+ STRTRIM(FLOAT(MEAN(nox_data.nox_hourly[inox]					,/NAN)),1)
 ;PRINT, 'co concentration: ' 	+ STRTRIM(FLOAT(MEAN(co_data.co_hourly[ico]						,/NAN)),1)
 ;PRINT, temp_data.temp_hourly[itemp]	
 ;PRINT, 'temperature: '	        + STRTRIM(FLOAT(MEAN(temp_data.temp_hourly[itemp]				,/NAN)),1)
 ;PRINT, press_data.press_hourly[ipress]
 ;PRINT, 'pressure : '	        + STRTRIM(FLOAT(MEAN(press_data.press_hourly[ipress]			,/NAN)),1)
 ;PRINT, wind_dir_data.wind_dir_hourly[iwind_dir]
 ;PRINT, 'wind direction: ' 		+ STRTRIM(FLOAT(MEAN(wind_dir_data.wind_dir_hourly[iwind_dir]	,/NAN)),1)
 ;PRINT, wind_spd_data.wind_spd_hourly[iwind_spd]
 ;PRINT, 'wind speed: ' 			+ STRTRIM(FLOAT(MEAN(wind_spd_data.wind_spd_hourly[iwind_spd]	,/NAN)),1)
 ;PRINT, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'

 icolor = WHERE(ABS(ozone1 - o3levels) LT 1.0)
 PLOTS, xgrid[i0], ygrid[i0], $																		;Overplot plane symbol
 	PSYM     = 8, $
 	SYMSIZE  = 6 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
 	NOCLIP   = 0, $
 	CONTINUE = 1, $
	COLOR    = o3table[icolor[-1]]
 
 MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines
 
 COLOR_BAR_24_KPB, o3table[1:*], OVER = o3table[-1], $
 	RANGE = [10, 80], $
 	TICKS = 5, $
 	TITLE = 'Ozone (ppb)', $
 	POSIT = bar_pos

;COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
;	RANGE    = [450, 675], $
;	TICKS    = 5, $
;	VERTICAL = 0, $
;	TITLE    = '950 hPa Geopotential Height (m)', $
;	POSIT    = bar_pos2
 
; COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
; 	RANGE    = [-5.0,9.0], $
; 	TICKS    = 5, $
; 	VERTICAL = 0, $
; 	TITLE    = 'Potential Vorticity (PVU)', $
; 	POSIT    = bar_pos2

 COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
 	RANGE    = [0.0,30.0], $
 	TICKS    = 5, $
 	VERTICAL = 0, $
 	TITLE    = 'Wind Speed (m/s)', $
 	POSIT    = bar_pos2

filename = MAKE_ISO_DATE_STRING(date1,/COMPACT,/UTC)
outdir = '/data3/dphoenix/wrf/general/reanalysis_plots/o3_gph_wind/'
FILE_MKDIR, outdir
pngfile = outdir + 'o3_gph_wind_' + filename + '.png' 
epsfile = outdir + 'us_latlon_map.eps' 
 
 IF ~KEYWORD_SET(nowindow) THEN BEGIN
 	IF KEYWORD_SET(eps) THEN BEGIN
 		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
 			LOAD_BASIC_COLORS, /RESET															;Reset color table to linear ramp
 		PS_OFF																					;Turn PS off
 	ENDIF ELSE IF KEYWORD_SET(png) THEN $
 		WRITE_PNG, pngfile, TVRD(TRUE=1)														;Write PNG image
 ENDIF

date1 = TIME_INC(date1, 21600)

STOP
END
