PRO PLOT_DFW_ANOMALIES, hour, reanalysis, $
	PNG		  = png, $
	EPS		  = eps, $
	CLOBBER   = clobber

;+
;NAME:
;     PLOT_DFW_ANOMALIES
;PURPOSE:
;     This copies variables from ERA-Interim analysis into a single
;     file in pressure coordinates for use in TRAJ3D.
;     W at the surface is set to zero.  One pressure level is added at 
;     the top of the domain (p = 0), where w is also set to zero.
;CATEGORY:
;     Data handling utility.
;CALLING SEQUENCE:
;     PLOT_DFW_ANOMALIES, date0, outfile
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
;		D. Phoenix:       2019-02-12.
;						  2019-02-14.	Consider doing this with the hourly o3 instead 
;										of the 8-hour averaged o3
;-

COMPILE_OPT IDL2																									;Set compile options

IF (N_ELEMENTS(reanalysis) EQ 0) THEN reanalysis = 'ERA_Interim'

;IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = 20170101
;IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = start_date+10000

;IF (N_ELEMENTS(date1) EQ 0) THEN date1 = MAKE_DATE(1980,01,01,00)
;IF (N_ELEMENTS(date2) EQ 0) THEN date2 = MAKE_DATE(2018,01,01,00)
;nt = DATE_DIFF(date2,date1) / 86400.

;pressure = (ERA_INTERIM_READ_VAR('P',date1)).values
;dim = SIZE(pressure,/DIMENSIONS)

dallas_x = (360.0-96.86)
dallas_y = 32.85

x = FINDGEN(360)
y = FINDGEN(181)-90.0

x0 = dallas_x - 1.0
y0 = dallas_y - 1.0
x1 = dallas_x + 1.0
y1 = dallas_y + 1.0

ix0 = WHERE(ROUND(x0) EQ x)
ix1 = WHERE(ROUND(x1) EQ x) 
iy0 = WHERE(ROUND(y0) EQ y)
iy1 = WHERE(ROUND(y1) EQ y) 

ts_data = READ_HURDAT2()

;Truncate data to only look at years with 00,06,12,18Z hours
imonth = WHERE(((ts_data.date.year GE '1996') AND (ts_data.date.year LT '2017')) AND $
	((ts_data.date.month GE 7) AND (ts_data.date.month LT 12)) AND (ts_data.date.hour EQ hour))

extr_storms = WHERE(((ts_data.class[imonth] EQ 'TD') OR (ts_data.class[imonth] EQ 'TS') OR $
    (ts_data.class[imonth] EQ 'HU')), istorms)
iperiod = imonth[extr_storms]

nt = N_ELEMENTS(ts_data.date[iperiod])

years  = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),0,4) 
months = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),5,2) 
days   = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),8,2) 
hours  = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),11,2)

;TS data has occasional non-00,06,12,18 hours. Remove those to be compatible with
;reanalysis data
REMOVE, ibad,  years
REMOVE, ibad, months
REMOVE, ibad,   days
REMOVE, ibad,  hours

;From Reanalysis monthly mean files, create month mean for entire period
;mth_arr=['08','09','10']
;day_arr=[31,30,31]

mth_arr = ['07','08','09','10','11']
day_arr = [31  ,  31,  30,  31,  30]
hr_arr = [hour]

yr_arr = ['1980','1981','1982','1983','1984','1985','1986','1987','1988','1989','1990', $
			'1991','1992','1993','1994','1995','1996','1997','1998','1999','2000', $
			'2001','2002','2003','2004','2005','2006','2007','2008','2009','2010', $
			'2011','2012','2013','2014','2015','2016','2017']

yr_arr = ['1996','1997','1998','1999','2000','2001','2002','2003','2004','2005','2006',$
		'2007','2008','2009','2010','2011','2012','2013','2014','2015','2016']
;yr_arr=['2011','2012','2013']

m=0		
ny = N_ELEMENTS(yr_arr)
mt = N_ELEMENTS(mth_arr)

;Now for each year,month,day,hour compute the anomaly during storm track periods
infile      = !WRF_DIRECTORY + 'general/monthly_reanalysis/monthly_mean_' + hour + $
				'Z_' + reanalysis + '1.nc'																;Set input file path

;Get monthly averaged data
id  = NCDF_OPEN(infile)																						;Open input file for reading	

NCDF_VARGET, id, 'V',            month_v
NCDF_VARGET, id, 'U',            month_u
NCDF_VARGET, id, 'O3',           month_o3 
NCDF_VARGET, id, 'O3_Daily_Max', month_o3max
NCDF_VARGET, id, 'WSPD', 		 month_wsp
NCDF_VARGET, id, 'GPH',		     month_gph
NCDF_VARGET, id, 'Temperature',  month_temp
NCDF_VARGET, id, 'SH',		     month_sh
NCDF_VARGET, id, 'PSFC',		 month_p
NCDF_CLOSE,  id																								;Close input file
	 	
v_array  = [ ]
o3_array = [ ]
u_array = []
gph_array=[]
temp_array=[]
pres_array=[]
wsp_array =[]
sh_array = []

;re-zero these arrays
pressure1 = []
temp1     = []
z1        = []
u1        = []
v1        = []
o31       = []
wsp       = []

nt=N_ELEMENTS(years)
FOR m=0, nt-1 DO BEGIN
    date1 = MAKE_DATE(years[m],months[m],days[m],hour);hours[i])
    PRINT, date1
    ;IF (MAKE_ISO_DATE_STRING(date1) EQ (MAKE_ISO_DATE_STRING(date2))) THEN BREAK
    pressure  = (ERA_INTERIM_READ_VAR('P'  ,date1)).values
    temp	  = (ERA_INTERIM_READ_VAR('T'  ,date1)).values
    Z	 	  = (ERA_INTERIM_READ_VAR('Z'  ,date1)).values
    u	 	  = (ERA_INTERIM_READ_VAR('u'  ,date1)).values
    v		  = (ERA_INTERIM_READ_VAR('v'  ,date1)).values
    sh		  = (ERA_INTERIM_READ_VAR('SH' ,date1)).values
   
    ;interpolate to 950 hPa pressure level	
    dim = SIZE(Z, /DIMENSIONS)
    nx = dim[0]
    ny = dim[1]
    nz = dim[2]
    ix = REBIN(FINDGEN(nx), nx, ny)
    iy = REBIN(REFORM(FINDGEN(ny), 1, ny), nx, ny)
    
    ;Calculate interpolation index for the 10.5 km level in each model grid column
    iz = FLTARR(nx, ny)
    FOR i = 0, nx-1 DO FOR j = 0, ny-1 DO iz[i,j] = INTERPOL(FINDGEN(nz), REFORM(pressure[i,j,*], nz), 950.0)
    
    ;Interpolate ozone volume to 10.5 km altitude map
    temp_map  = INTERPOLATE(temp, ix, iy, iz)
    gph_map   = INTERPOLATE(Z,    ix, iy, iz)
    u_map     = INTERPOLATE(u,    ix, iy, iz)
    v_map     = INTERPOLATE(v,    ix, iy, iz)
	sh_map    = INTERPOLATE(sh,   ix, iy, iz)
	
	pressure1 = MEAN(pressure  [ix0:ix1,iy0:iy1,0],/NAN)
    temp1     = MEAN(temp_map    [ix0:ix1,iy0:iy1],/NAN)
    z1        = MEAN( gph_map    [ix0:ix1,iy0:iy1],/NAN)
    u1        = MEAN(   u_map    [ix0:ix1,iy0:iy1],/NAN)
    v1        = MEAN(   v_map    [ix0:ix1,iy0:iy1],/NAN)
    sh1       = MEAN(  sh_map    [ix0:ix1,iy0:iy1],/NAN)
    wsp		  = SQRT(u1^2 + v1^2)
    
    ;Find anomaly for each hour
    mth = WHERE(months[m] EQ mth_arr)
	v_anom  = (v1 - month_v[mth])
	v_array = [v_array, v_anom]     

	u_anom  = (u1 - month_u[mth])
	u_array = [u_array, u_anom]     

	sh_anom  = (sh1 - month_sh[mth])
	sh_array = [sh_array, sh_anom]     

	gph_anom  = (z1 - month_gph[mth])
	gph_array = [gph_array, gph_anom]     

	temp_anom  = (temp1 - month_temp[mth])
	temp_array = [temp_array, temp_anom]     
 
 	pres_anom  = (pressure1 - month_p[mth])
	pres_array = [pres_array, pres_anom]   
	
	wsp_anom  = (wsp - month_wsp[mth])  
    wsp_array = [wsp_array, wsp_anom]
   
    ;Save o3 concentration for each hour
    IF (m EQ 0) THEN BEGIN
    	year 	 = date1.year
		o3_data  = OZONE_DALLAS_NAAQS(STRTRIM(year,1))	
	ENDIF
	IF (m GT 0) AND (years[m] NE years[m-1]) THEN BEGIN
		year = years[m]
		o3_data  = OZONE_DALLAS_NAAQS(STRTRIM(year,1))
	ENDIF

	;o3_8hr   = o3_data.ozone_8hr_total
    o3_8hr = o3_data.ozone_hourly
    o3_datestr = STRMID(o3_data.date,0,4)+STRMID(o3_data.date,5,2)+STRMID(o3_data.date,8,2) + $
    				  STRMID(o3_data.time,0,2)+STRMID(o3_data.time,3,2) 

    io3 = WHERE((STRMID(MAKE_ISO_DATE_STRING(date1,/COMPACT),0,8) + $
    	STRMID(MAKE_ISO_DATE_STRING(date1,/COMPACT),9,4)) EQ o3_datestr)
    
    PRINT, MAX(o3_8hr[io3],/NAN)
    o3_anom  = MAX(o3_8hr[io3],/NAN) - month_o3[mth]
    o3_array = [o3_array, o3_anom]
ENDFOR   

p1 = SCATTERPLOT(o3_array, wsp_array, $
	TITLE  = 'O3 vs Wind Speed Anomalies: ' + STRTRIM(hour,1) + 'Z', $
	YTITLE = 'Wind Speed Anomalies (m/s)', $ 
	XTITLE = 'O3 Anomalies (ppb)', $
	YRANGE = [-10,10], $
	XRANGE = [-40,20])
;	XRANGE = [-40,80])
	;XRANGE = [-60,60])

l1 = LINFIT(o3_array, wsp_array)
;xplot = FINDGEN(120)-40
xplot = FINDGEN(80)-40
yplot = l1[0] + (l1[1])*xplot

trend = PLOT(xplot, yplot, $
		/OVERPLOT, $
		COLOR = COLOR_24('red'), $
		THICK=2, $
		LINESTYLE=3, $
;		xrange = [-35,80])
		XRANGE = [-40,20])
;zero = PLOT([-40,80],[0,0], /OVERPLOT, THICK=2)
zero = PLOT([-40,20],[0,0], /OVERPLOT, THICK=2)

PRINT, 'O3-Wind Correlation: ' + STRING(CORRELATE(o3_array, wsp_array))
PRINT, 'O3-Wind Covariance: ' + STRING(CORRELATE(o3_array, wsp_array,/COVARIANCE))
 
p2 = SCATTERPLOT( o3_array, temp_array, $
	TITLE  = 'O3 vs Temperature Anomalies: ' + STRTRIM(hour,1) + 'Z', $
	YTITLE = 'Temperature Anomalies (K)', $ 
	XTITLE = 'O3 Anomalies (ppb)', $
	YRANGE = [-25,25], $
	XRANGE = [-40,20])

l2 = LINFIT(o3_array, temp_array)
xplot2 = FINDGEN(80)-40
yplot2 = l2[0] + (l2[1])*xplot2

trend = PLOT(xplot2, yplot2, $
		/OVERPLOT, $
		COLOR = COLOR_24('red'), $
		THICK=2, $
		LINESTYLE=3, $
		xrange = [-40,20])

zero = PLOT([-40,20],[0,0], /OVERPLOT, THICK=2)

PRINT, 'O3-Temp Correlation: ' + STRING(CORRELATE(o3_array, temp_array))
PRINT, 'O3-Temp Covariance: ' + STRING(CORRELATE(o3_array, temp_array,/COVARIANCE))

p3= SCATTERPLOT(o3_array, gph_array, $
	TITLE  = 'O3 vs GPH Anomalies: ' + STRTRIM(hour,1) + 'Z', $
	YTITLE = 'Geopotential Height Anomalies (m)', $ 
	XTITLE = 'O3 Anomalies (ppb)', $
	YRANGE = [-100,100], $
	XRANGE = [-40,20])

l3 = LINFIT(o3_array, gph_array)
xplot3 = FINDGEN(80)-40
yplot3 = l3[0] + (l3[1])*xplot3

trend3 = PLOT(xplot3, yplot3, $
		/OVERPLOT, $
		COLOR = COLOR_24('red'), $
		THICK=2, $
		LINESTYLE=3, $
		xrange = [-40,20])

zero = PLOT([-40,20],[0,0], /OVERPLOT, THICK=2)

PRINT, 'O3-GPH Correlation: ' + STRING(CORRELATE(o3_array, gph_array))
PRINT, 'O3-GPH Covariance: ' + STRING(CORRELATE(o3_array,  gph_array,/COVARIANCE))


;;Write anomalies to file in case you want to look at different relationships


STOP

END
