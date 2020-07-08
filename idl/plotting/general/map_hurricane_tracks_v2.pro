PRO MAP_HURRICANE_TRACKS_V2, date1, date2, lev, $
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

;IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = 20170101
;IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = start_date+10000

IF (N_ELEMENTS(date1) EQ 0) THEN date1 = MAKE_DATE(1980,01,01,00)
IF (N_ELEMENTS(date2) EQ 0) THEN date2 = MAKE_DATE(2018,01,01,00)
nt = DATE_DIFF(date2,date1) / 21600.
lev = 950.0

pressure = (ERA_INTERIM_READ_VAR('P',date1)).values
dim = SIZE(pressure,/DIMENSIONS)

dallas_x = (-96.86)
dallas_y = 32.85

x = FINDGEN(360)
y = FINDGEN(181)-90.0

yc = 32.85
xc = 360.0-96.86

x0 = 360.0-105.0
y0 = 22.0
x1 = 360.0-62.73
y1 = 52.5
;x1 for Gulf storms: x1 = 360.0-80.0

ix0 = WHERE(ROUND(x0) EQ x)
ix1 = WHERE(ROUND(x1) EQ x) 
iy0 = WHERE(ROUND(y0) EQ y)
iy1 = WHERE(ROUND(y1) EQ y) 

pressure1 = FLTARR((ix1-ix0)+1,(iy1-iy0)+1, nt)
temp1     = FLTARR((ix1-ix0)+1,(iy1-iy0)+1, nt)
z1        = FLTARR((ix1-ix0)+1,(iy1-iy0)+1, nt)
u1        = FLTARR((ix1-ix0)+1,(iy1-iy0)+1, nt)
v1        = FLTARR((ix1-ix0)+1,(iy1-iy0)+1, nt)
HELP, pressure1

ts_data = READ_HURDAT2()
imonth = WHERE(((ts_data.date.year GE '2010') AND (ts_data.date.year LT '2011')) AND $
	((ts_data.date.month GE 8) AND (ts_data.date.month LT 10))  AND (ts_data.date.hour EQ 18))

;extr_storms = WHERE(((ts_data.class[imonth] EQ 'TD') OR (ts_data.class[imonth] EQ 'TS') OR $
;    (ts_data.class[imonth] EQ 'HU')), istorms)
extr_storms = WHERE(((ts_data.class[imonth] EQ 'HU')), istorms)
;iperiod = imonth[extr_storms]

icount = WHERE(((ts_data.x[imonth[extr_storms]] LE -50.0) AND (ts_data.x[imonth[extr_storms]] GT -100.0) AND $
		(ts_data.y[imonth[extr_storms]] LE 40.0) AND (ts_data.y[imonth[extr_storms]] GE 20.0)), count, COMPLEMENT = iout)
iperiod = imonth[extr_storms[icount]]

nt = N_ELEMENTS(ts_data.date[iperiod])

years  = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),0,4) 
months = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),5,2) 
days   = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),8,2) 
hours  = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),11,2)
;ibad   = WHERE((hours NE 00) AND (hours NE 06) AND (hours NE 12) AND (hours NE 18),badcount)
;
;REMOVE, ibad,  years
;REMOVE, ibad, months
;REMOVE, ibad,   days
;REMOVE, ibad,  hours

year = date1.year
o3_data  	   = OZONE_DALLAS_NAAQS(STRTRIM(year,1))	
nox_data 	   = NOX_DALLAS_NAAQS(STRTRIM(year,1))
co_data 	   = CO_DALLAS_NAAQS(STRTRIM(year,1))
temp_data 	   = TEMP_DALLAS_NAAQS(STRTRIM(year,1))
press_data 	   = PRESS_DALLAS_NAAQS(STRTRIM(year,1))
wind_spd_data  = WIND_SPD_DALLAS_NAAQS(STRTRIM(year,1))
wind_dir_data  = WIND_DIR_DALLAS_NAAQS(STRTRIM(year,1))

FOR i=0, nt-1 DO BEGIN
    date1 = MAKE_DATE(years[i],months[i],days[i],hours[i])
    IF (MAKE_ISO_DATE_STRING(date1) EQ (MAKE_ISO_DATE_STRING(date2))) THEN BREAK
    pressure  = (ERA_INTERIM_READ_VAR('P' ,date1)).values
    temp	  = (ERA_INTERIM_READ_VAR('T' ,date1)).values
    Z	 	  = (ERA_INTERIM_READ_VAR('Z' ,date1)).values
    u	 	  = (ERA_INTERIM_READ_VAR('u' ,date1)).values
    v		  = (ERA_INTERIM_READ_VAR('v' ,date1)).values

	;Create time arrays
	;pressure1 = pressure[ix0:ix1,iy0:iy1,4]
    ;temp1     = temp    [ix0:ix1,iy0:iy1,4]
    ;z1        =    Z    [ix0:ix1,iy0:iy1,4]
    ;u1        =    u    [ix0:ix1,iy0:iy1,4]
    ;v1        =    v    [ix0:ix1,iy0:iy1,4]

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
    gph_map   = INTERPOLATE(Z,    ix, iy, iz)
    u_map     = INTERPOLATE(u,    ix, iy, iz)
    v_map     = INTERPOLATE(v,    ix, iy, iz)

    map_pos = [0.05, 0.15, 0.95, 0.95]																			;Set map position
    bar_pos = [0.25, 0.10, 0.75, 0.12]																			;Set color bar position
    
	table   = [COLOR_24(200, 200, 200),VISUALIZE_88D_COLOR(0)]												;Set reflectivity color table
    levels = FINDGEN(N_ELEMENTS(table))
    ;levels = levels*30 + 450
    levels = levels*30 + 200
    
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
 	
 	x0 = 360.0-105.0
	y0 = 22.0
	x1 = 360.0-62.73
	y1 = 52.5
   
    MAP_SET, yc, xc, 0, CONIC = 1, $																;Draw map
    	LIMIT     = [y0,x0,y1,x1], $
    	ISOTROPIC = 1, $
    	TITLE     = MAKE_ISO_DATE_STRING(date1), $
    	POSITION  = map_pos
    
    wspd = SQRT(u1^2 + v1^2)
    CONTOUR, gph_map[ix0:ix1,iy0:iy1], x[ix0:ix1], y[iy0:iy1], $												;Contour reflectivity values
    	OVERPLOT  = 1, $
    	FILL      = 1, $
    	LEVELS    = levels, $
    	C_COLOR   = table
    
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
    
    VELOVECT, ureg, vreg, xreg, yreg, OVERPLOT  = 1, LENGTH = 2.0

    i0 = WHERE((ABS(dallas_x - (xgrid)) LT 0.5) AND (ABS(dallas_y - ygrid) LT 0.5))
    
    USERSYM_STAR, /FILL																					;Load plane symbol at flight path orientation
    o3_8hr   = o3_data.ozone_8hr_total
    
    o3table  = (HCL_COLOR_TABLE(70, HUE_RANGE = [100.0, 300.0]))
    o3levels = 10.0+FINDGEN(N_ELEMENTS(o3table))

    datestr   = MAKE_ISO_DATE_STRING(date1)
    io3       = WHERE(STRMID(datestr,0,16) EQ (STRING(o3_data.date) 	    + ' ' + STRING(o3_data.time)))
    inox      = WHERE(STRMID(datestr,0,16) EQ (STRING(nox_data.date) 	    + ' ' + STRING(nox_data.time)))
    ico 	  = WHERE(STRMID(datestr,0,16) EQ (STRING(co_data.date) 	    + ' ' + STRING(co_data.time)))
    itemp 	  = WHERE(STRMID(datestr,0,16) EQ (STRING(temp_data.date) 		+ ' ' + STRING(temp_data.time)))
    ipress 	  = WHERE(STRMID(datestr,0,16) EQ (STRING(press_data.date) 		+ ' ' + STRING(press_data.time)))
    iwind_dir = WHERE(STRMID(datestr,0,16) EQ (STRING(wind_dir_data.date) 	+ ' ' + STRING(wind_dir_data.time)))
    iwind_spd = WHERE(STRMID(datestr,0,16) EQ (STRING(wind_spd_data.date) 	+ ' ' + STRING(wind_spd_data.time)))

	distance = MEAN(SQRT((dallas_x - ts_data.x[iperiod[i]])^2 + (dallas_y - ts_data.y[iperiod[i]])^2))	

    ozone1 = FLOAT(ROUND(MAX(o3_8hr[io3],/NAN)))
    PRINT, 'ozone concentration: '  + STRTRIM(ozone1,1)
    PRINT, 'nox concentration: ' 	+ STRTRIM(FLOAT(MEAN(nox_data.nox_hourly[inox]					,/NAN)),1)
    PRINT, 'co concentration: ' 	+ STRTRIM(FLOAT(MEAN(co_data.co_hourly[ico]						,/NAN)),1)
    PRINT, temp_data.temp_hourly[itemp]	
    PRINT, 'temperature: '	        + STRTRIM(FLOAT(MEAN(temp_data.temp_hourly[itemp]				,/NAN)),1)
	PRINT, press_data.press_hourly[ipress]
	PRINT, 'pressure : '	        + STRTRIM(FLOAT(MEAN(press_data.press_hourly[ipress]			,/NAN)),1)
   	PRINT, wind_dir_data.wind_dir_hourly[iwind_dir]
    PRINT, 'wind direction: ' 		+ STRTRIM(FLOAT(MEAN(wind_dir_data.wind_dir_hourly[iwind_dir]	,/NAN)),1)
	PRINT, wind_spd_data.wind_spd_hourly[iwind_spd]
    PRINT, 'wind speed: ' 			+ STRTRIM(FLOAT(MEAN(wind_spd_data.wind_spd_hourly[iwind_spd]	,/NAN)),1)
	PRINT, 'distance from dfw: '	+ STRING(distance)
	PRINT, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
	PRINT, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
	
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

	;Plots storm tracks
	;ts_data = READ_HURDAT2()
	;ptable  = REVERSE(HCL_COLOR_TABLE(145, HUE_RANGE = [100.0, 300.0]))
	;plevels = 880.0+FINDGEN(145)
	;idates = WHERE((date1.year EQ ts_data.date.year) AND (date1.month EQ ts_data.date.month))
   ; ip1= [ ]
   ; FOR ii=0,N_ELEMENTS(idates)-1 DO BEGIN
   ;		ip = WHERE((ABS((ts_data.x[idates[ii]]+360.0) - (xgrid)) LT 1.0) $
   ; 		AND (ABS(ts_data.y[idates[ii]] - ygrid) LT 1.0))
   ;		ip1 = [ip1, ip]
   ; ENDFOR

   ; FOR ii=0,N_ELEMENTS(idates)-1 DO BEGIN
   ; 	ip = WHERE((ABS((ts_data.x[idates[ii]]+360.0) - (xgrid)) LT 1.0) $
   ; 		AND (ABS(ts_data.y[idates[ii]] - ygrid) LT 1.0))
	;   	icolor = WHERE(plevels EQ ts_data.p[idates[ii]])
	;	HELP, ip
	;	PLOTS, xgrid[ip], ygrid[ip], $
   ; 		PSYM 	 = 4, $
   ; 		NOCLIP   = 0, $
   ; 		CONTINUE = 1, $
   ; 		THICK	 = 3, $
   ; 		COLOR 	 = ptable[icolor]
   ; ENDFOR
	
	
	filename = MAKE_ISO_DATE_STRING(date1,/COMPACT,/UTC)
	outdir = '/data3/dphoenix/wrf/general/reanalysis_plots/o3_gph_wind/'
	FILE_MKDIR, outdir
	pngfile = outdir + 'o3_gph_wind_' + filename + '.png' 
    
    IF ~KEYWORD_SET(nowindow) THEN BEGIN
    	IF KEYWORD_SET(eps) THEN BEGIN
    		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
    			LOAD_BASIC_COLORS, /RESET															;Reset color table to linear ramp
    		PS_OFF																					;Turn PS off
    	ENDIF ELSE IF KEYWORD_SET(png) THEN $
    		WRITE_PNG, pngfile, TVRD(TRUE=1)														;Write PNG image
    ENDIF

	PRINT, date1
	date1 = TIME_INC(date1, 21600)

ENDFOR
STOP
END
