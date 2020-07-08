PRO PLOT_CHEM_MET_OBS_ANOMALIES_BACKUP, startmonth, hour, $
	PNG		  = png, $
	EPS		  = eps, $
	CLOBBER   = clobber

;+
;NAME:
;     PLOT_CHEM_MET_OBS_ANOMALIES
;PURPOSE:
;     This copies variables from ERA-Interim analysis into a single
;     file in pressure coordinates for use in TRAJ3D.
;     W at the surface is set to zero.  One pressure level is added at 
;     the top of the domain (p = 0), where w is also set to zero.
;CATEGORY:
;     Data handling utility.
;CALLING SEQUENCE:
;     PLOT_CHEM_MET_OBS_ANOMALIES, date0, outfile
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
;		D. Phoenix:       2019-02-15.
;-

COMPILE_OPT IDL2																									;Set compile options

IF (N_ELEMENTS(startmonth) EQ 0) THEN startmonth = '072005'

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
imonth = WHERE(((ts_data.date.year GE '2005') AND (ts_data.date.year LT '2017')) AND $
	((ts_data.date.month GE 7) AND (ts_data.date.month LT 12)) AND (ts_data.date.hour EQ hour))

extr_storms = WHERE(((ts_data.class[imonth] EQ 'TD') OR (ts_data.class[imonth] EQ 'TS') OR $
    (ts_data.class[imonth] EQ 'HU')), istorms)
iperiod = imonth[extr_storms]

nt = N_ELEMENTS(ts_data.date[iperiod])

years  = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),0,4) 
months = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),5,2) 
days   = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),8,2) 
hours  = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),11,2)

ts_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),0,4) +  $
	STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),5,2) + $
	 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),8,2)
		
;Set arrays
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

yr_arr=['2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016','2017']

m=0		
ny = N_ELEMENTS(yr_arr)
mt = N_ELEMENTS(mth_arr)

;Create 1-hour bin with 6 hour interval
time_arr = [ ]
date1 = MAKE_DATE(year,07,01,00,00)
date2 = MAKE_DATE(year,12,01,06,00)
end_datestr = STRMID(MAKE_ISO_DATE_STRING(date2),0,4) + STRMID(MAKE_ISO_DATE_STRING(date2),5,2) + $
			 	STRMID(MAKE_ISO_DATE_STRING(date2),8,2) 

nt_yr = 8760/24
FOR i = 0, nt_yr - 1 DO BEGIN
		datestr = STRMID(MAKE_ISO_DATE_STRING(date1),0,4) + STRMID(MAKE_ISO_DATE_STRING(date1),5,2) + $
			 STRMID(MAKE_ISO_DATE_STRING(date1),8,2) 
		time_arr = [time_arr, datestr]
		IF (datestr EQ end_datestr) THEN BREAK
		date1 = TIME_INC(date1, 86400)
ENDFOR 

;Now for each year,month,day,hour compute the anomaly during storm track periods
infile      = !WRF_DIRECTORY + 'general/o3_data/monthly_ave_data/monthly_ave_' + $
			startmonth + '_' + '122017.nc'															

;Get monthly averaged data
id  = NCDF_OPEN(infile)																						;Open input file for reading	

NCDF_VARGET, id, 'O3'				, month_o3   
NCDF_VARGET, id, 'VOC'   			, month_voc  
NCDF_VARGET, id, 'PM2.5' 			, month_pm  
NCDF_VARGET, id, 'NOx' 				, month_nox  
NCDF_VARGET, id, 'CO'	 			, month_co  
NCDF_VARGET, id, 'Temperature'  	, month_temp 
NCDF_VARGET, id, 'Wind_Speed'   	, month_wind_spd 
NCDF_VARGET, id, 'Wind_Direction'	, month_wind_dir
NCDF_VARGET, id, 'Pressure'			, month_press
NCDF_VARGET, id, 'RH'        		, month_rh  
NCDF_VARGET, id, 'Dew_Point'   		, month_dp	

NCDF_CLOSE,  id																								;Close input file
	 	
;re-zero these arrays
o3_array = []
voc_array = []
pm_array = []
nox_array = []
co_array = []
temp_array = []
press_array = []
wind_spd_array = []
wind_dir_array = []
rh_array = []
dp_array = []

nt=N_ELEMENTS(years)
FOR m=0, nt-1 DO BEGIN
    date1 = years[m] + months[m] + days[m] + hour + '00'
    PRINT, date1

	IF (m EQ 0) THEN o3_data = OZONE_DALLAS_NAAQS(years[m])
	IF (m GT 0) AND (years[m] NE years[m-1]) THEN BEGIN
		year = years[m]
		o3_data  = OZONE_DALLAS_NAAQS(STRTRIM(year,1))
	ENDIF
	IF (m EQ 0) THEN voc_data  =   VOC_DALLAS_NAAQS(years[m])
	IF (m GT 0) AND (years[m] NE years[m-1]) THEN BEGIN
		year = years[m]
		voc_data  = VOC_DALLAS_NAAQS(STRTRIM(year,1))
	ENDIF
	
	IF (m EQ 0) THEN pm_data  =   PM_DALLAS_NAAQS(years[m])
	IF (m GT 0) AND (years[m] NE years[m-1]) THEN BEGIN
		year = years[m]
		pm_data  = PM_DALLAS_NAAQS(STRTRIM(year,1))
	ENDIF
	
	IF (m EQ 0) THEN nox_data  =   NOX_DALLAS_NAAQS(years[m])
	IF (m GT 0) AND (years[m] NE years[m-1]) THEN BEGIN
		year = years[m]
		nox_data  = NOX_DALLAS_NAAQS(STRTRIM(year,1))
	ENDIF
	
	IF (m EQ 0) THEN co_data =  CO_DALLAS_NAAQS(years[m])
	IF (m GT 0) AND (years[m] NE years[m-1]) THEN BEGIN
		year = years[m]
		co_data  = CO_DALLAS_NAAQS(STRTRIM(year,1))
	ENDIF
	
	IF (m EQ 0) THEN temp_data  =  TEMP_DALLAS_NAAQS(years[m])
	IF (m GT 0) AND (years[m] NE years[m-1]) THEN BEGIN
		year = years[m]
		temp_data  = TEMP_DALLAS_NAAQS(STRTRIM(year,1))
	ENDIF
	
	IF (m EQ 0) THEN wind_spd_data  =  WIND_SPD_DALLAS_NAAQS(years[m])
	IF (m GT 0) AND (years[m] NE years[m-1]) THEN BEGIN
		year = years[m]
		wind_spd_data  = WIND_SPD_DALLAS_NAAQS(STRTRIM(year,1))
	ENDIF
	
	IF (m EQ 0) THEN wind_dir_data  =  WIND_DIR_DALLAS_NAAQS(years[m])
	IF (m GT 0) AND (years[m] NE years[m-1]) THEN BEGIN
		year = years[m]
		wind_dir_data  = WIND_DIR_DALLAS_NAAQS(STRTRIM(year,1))
	ENDIF
	
	IF (m EQ 0) THEN press_data = PRESS_DALLAS_NAAQS(years[m])
	IF (m GT 0) AND (years[m] NE years[m-1]) THEN BEGIN
		year = years[m]
		press_data  = PRESS_DALLAS_NAAQS(STRTRIM(year,1))
	ENDIF
	
	IF (m EQ 0) THEN rh_data = RH_DALLAS_NAAQS(years[m])
	IF (m GT 0) AND (years[m] NE years[m-1]) THEN BEGIN
		year = years[m]
		rh_data  = RH_DALLAS_NAAQS(STRTRIM(year,1))
	ENDIF
	
	IF (m EQ 0) THEN dp_data = DP_DALLAS_NAAQS(years[m])
	IF (m GT 0) AND (years[m] NE years[m-1]) THEN BEGIN
		year = years[m]
		dp_data  = DP_DALLAS_NAAQS(STRTRIM(year,1))
	ENDIF

	o3_datestr =  STRMID(o3_data.date,0,4)+STRMID(o3_data.date,5,2)+STRMID(o3_data.date,8,2) + $
				  STRMID(o3_data.time,0,2)+STRMID(o3_data.time,3,2) 
    io3 = WHERE(date1 EQ o3_datestr)

	voc_datestr =  STRMID(voc_data.date,0,4)+STRMID(voc_data.date,5,2)+STRMID(voc_data.date,8,2) + $
				  STRMID(voc_data.time,0,2)+STRMID(voc_data.time,3,2) 
    ivoc = WHERE(date1 EQ voc_datestr)

	pm_datestr =  STRMID(pm_data.date,0,4)+STRMID(pm_data.date,5,2)+STRMID(pm_data.date,8,2) + $
				  STRMID(pm_data.time,0,2)+STRMID(pm_data.time,3,2) 
    ipm = WHERE(date1 EQ pm_datestr)

	nox_datestr =  STRMID(nox_data.date,0,4)+STRMID(nox_data.date,5,2)+STRMID(nox_data.date,8,2) + $
				  STRMID(nox_data.time,0,2)+STRMID(nox_data.time,3,2) 
    inox = WHERE(date1 EQ nox_datestr)

	co_datestr =  STRMID(co_data.date,0,4)+STRMID(co_data.date,5,2)+STRMID(co_data.date,8,2) + $
				  STRMID(co_data.time,0,2)+STRMID(co_data.time,3,2) 
    ico = WHERE(date1 EQ co_datestr)

	temp_datestr =  STRMID(temp_data.date,0,4)+STRMID(temp_data.date,5,2)+STRMID(temp_data.date,8,2) + $
				  STRMID(temp_data.time,0,2)+STRMID(temp_data.time,3,2) 
    itemp = WHERE(date1 EQ temp_datestr)

	wind_spd_datestr =  STRMID(wind_spd_data.date,0,4)+STRMID(wind_spd_data.date,5,2)+STRMID(wind_spd_data.date,8,2) + $
				  STRMID(wind_spd_data.time,0,2)+STRMID(wind_spd_data.time,3,2) 
    iwind_spd = WHERE(date1 EQ wind_spd_datestr)

	wind_dir_datestr =  STRMID(wind_dir_data.date,0,4)+STRMID(wind_dir_data.date,5,2)+STRMID(wind_dir_data.date,8,2) + $
				  STRMID(wind_dir_data.time,0,2)+STRMID(wind_dir_data.time,3,2) 
    iwind_dir = WHERE(date1 EQ wind_dir_datestr)

	press_datestr =  STRMID(press_data.date,0,4)+STRMID(press_data.date,5,2)+STRMID(press_data.date,8,2) + $
				  STRMID(press_data.time,0,2)+STRMID(press_data.time,3,2) 
    ipress = WHERE(date1 EQ press_datestr)

	rh_datestr =  STRMID(rh_data.date,0,4)+STRMID(rh_data.date,5,2)+STRMID(rh_data.date,8,2) + $
				  STRMID(rh_data.time,0,2)+STRMID(rh_data.time,3,2) 
    irh = WHERE(date1 EQ rh_datestr)

	dp_datestr =  STRMID(dp_data.date,0,4)+STRMID(dp_data.date,5,2)+STRMID(dp_data.date,8,2) + $
				  STRMID(dp_data.time,0,2)+STRMID(dp_data.time,3,2) 
    idp = WHERE(date1 EQ dp_datestr)

    
    ;Find anomaly for each hour
    mth = WHERE(months[m] EQ mth_arr)
	o3_anom  = (MAX(o3_data.ozone_hourly[io3],/NAN) - month_o3[mth])
	o3_array = [o3_array, o3_anom]     
	o3_storm = [o3_storm, MAX(o3_data.ozone_hourly[io3],/NAN)]

	voc_anom  = (TOTAL(voc_data.voc_hourly[ivoc],/NAN) - month_voc[mth])
	voc_array = [voc_array, voc_anom]     
	voc_storm = [voc_storm, MAX(voc_data.voc_hourly[ivoc],/NAN)]

	pm_anom  = (MEAN(pm_data.pm_hourly[ipm],/NAN) - month_pm[mth])
	pm_array = [pm_array, pm_anom]     
	pm_storm = [pm_storm, MAX(pm_data.pm_hourly[ipm],/NAN)]

	nox_anom  = (MEAN(nox_data.nox_hourly[inox],/NAN) - month_nox[mth])
	nox_array = [nox_array, nox_anom]     
	nox_storm = [nox_storm, MAX(nox_data.nox_hourly[inox],/NAN)]

	co_anom  = (MEAN(co_data.co_hourly[ico],/NAN) - month_co[mth])
	co_array = [co_array, co_anom]     
	co_storm = [co_storm, MAX(co_data.co_hourly[ico],/NAN)]

	temp_anom  = (MEAN(temp_data.temp_hourly[itemp],/NAN) - month_temp[mth])
	temp_array = [temp_array, temp_anom]     
	temp_storm = [temp_storm, MAX(temp_data.temp_hourly[itemp],/NAN)]
 
 	press_anom  = (MEAN(press_data.press_hourly[ipress],/NAN) - month_press[mth])
	press_array = [press_array, press_anom]   
	press_storm = [press_storm, MAX(press_data.press_hourly[ipress],/NAN)]
	
	wind_spd_anom  = (MEAN(wind_spd_data.wind_spd_hourly[iwind_spd],/NAN) - month_wind_spd[mth])  
    wind_spd_array = [wind_spd_array, wind_spd_anom]
	wind_spd_storm = [wind_spd_storm, MAX(wind_spd_data.wind_spd_hourly[iwind_spd],/NAN)]

	wind_dir_anom  = (MEAN(wind_dir_data.wind_dir_hourly[iwind_dir],/NAN) - month_wind_dir[mth])  
    wind_dir_array = [wind_dir_array, wind_dir_anom]
	wind_dir_storm = [wind_dir_storm, MAX(wind_dir_data.wind_dir_hourly[iwind_dir],/NAN)]

	rh_anom  = (MEAN(rh_data.rh_hourly[irh],/NAN) - month_rh[mth])  
    rh_array = [rh_array, rh_anom]
	rh_storm = [rh_storm, MAX(rh_data.rh_hourly[irh],/NAN)]

	dp_anom  = (MEAN(dp_data.dp_hourly[idp],/NAN) - month_dp[mth])  
    dp_array = [dp_array, dp_anom]
	dp_storm = [dp_storm, MAX(dp_data.dp_hourly[idp],/NAN)]

ENDFOR   

;;Write anomalies to file in case you want to look at different relationships
outfile = 'hourly_anomalies_072001_122017.nc'
outdir  = !WRF_DIRECTORY + 'general/o3_data/hourly_data/'
FILE_MKDIR, outdir

dim = SIZE(o3_yr, /DIMENSIONS)																			;Get grid dimension sizes
CATCH, error_status																								;Catch any errors with netcdf control or file creation

IF (error_status NE 0) THEN BEGIN
	NCDF_CLOSE, oid																								;Close previous failed file
	oid = NCDF_CREATE(outdir + outfile, CLOBBER = 1)								;Create output file for writing
ENDIF ELSE $
	oid = NCDF_CREATE(outdir + outfile, CLOBBER = 1)								;Create output file for writing

tid = NCDF_DIMDEF(oid, 'Hours', dim[0])																		;Define output dimensions in netCDF file

PRINT, 'start creating variable names'
vid = NCDF_VARDEF(oid, 'O3', [tid], /FLOAT)												;Define the longitude variable
NCDF_ATTPUT, oid, 'O3', 'long_name', 'O3 anomalies 7/2001-12/2017'								;Name attribute
NCDF_ATTPUT, oid, 'O3', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'VOC', [tid], /FLOAT)												;Define the longitude variable
NCDF_ATTPUT, oid, 'VOC', 'long_name', 'VOC anomalies 7/2001-12/2017'								;Name attribute
NCDF_ATTPUT, oid, 'VOC', 'units',     'ppb'												;Units attribute

;vid = NCDF_VARDEF(oid, 'PM2.5', [tid], /FLOAT)												;Define the latitude variable
;NCDF_ATTPUT, oid, 'PM2.5', 'long_name', 'PM 2.5 anomalies 7/2001-12/2017'									;Name attribute
;NCDF_ATTPUT, oid, 'PM2.5', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'NOx', [tid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'NOx', 'long_name', 'NOy anomalies 7/2001-12/2017'									;Name attribute
NCDF_ATTPUT, oid, 'NOx', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'CO', [tid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'CO', 'long_name', 'CO anomalies 7/2001-12/2017'									;Name attribute
NCDF_ATTPUT, oid, 'CO', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'Temperature', [tid], /FLOAT)													;Define the pressure variable
NCDF_ATTPUT, oid, 'Temperature', 'long_name', 'Temperature anomalies 7/2001-12/2017'														;Name attribute
NCDF_ATTPUT, oid, 'Temperature', 'units',     'degrees F'																	;Units attribute

vid = NCDF_VARDEF(oid, 'Wind_Speed', [tid], /FLOAT)													;Define the pressure variable
NCDF_ATTPUT, oid, 'Wind_Speed', 'long_name', 'Wind Speed anomalies 7/2001-12/2017'														;Name attribute
NCDF_ATTPUT, oid, 'Wind_Speed', 'units',     'Knots'																	;Units attribute

vid = NCDF_VARDEF(oid, 'Wind_Direction', [tid], /FLOAT)													;Define the geopotential height variable
NCDF_ATTPUT, oid, 'Wind_Direction', 'long_name', 'Wind Direction anomalies 7/2001-12/2017' 											;Name attribute
NCDF_ATTPUT, oid, 'Wind_Direction', 'units',     'Degrees Compass'																	;Units attribute

vid = NCDF_VARDEF(oid, 'Pressure', [tid], /FLOAT)													;Define the geopotential height variable
NCDF_ATTPUT, oid, 'Pressure', 'long_name', 'Pressure anomalies 7/2001-12/2017'											;Name attribute
NCDF_ATTPUT, oid, 'Pressure', 'units',     'hPa'																	;Units attribute

vid = NCDF_VARDEF(oid, 'RH', [tid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'RH', 'long_name', 'Relative Humidity anomalies 7/2001-12/2017'									;Name attribute
NCDF_ATTPUT, oid, 'RH', 'units',     '%'												;Units attribute

vid = NCDF_VARDEF(oid, 'Dew_Point', [tid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'Dew_Point', 'long_name', 'Dew Point Temperature anomalies 7/2001-12/2017'									;Name attribute
NCDF_ATTPUT, oid, 'Dew_Point', 'units',     'degrees F'												;Units attribute

PRINT, 'done creating variables'
NCDF_CONTROL, oid, /ENDEF

PRINT, 'start assigning variables'
NCDF_VARPUT, oid, 'O3'				, o3_array
NCDF_VARPUT, oid, 'VOC'   			, voc_array
;NCDF_VARPUT, oid, 'PM2.5'			, pm_array
NCDF_VARPUT, oid, 'NOx' 			, nox_array
NCDF_VARPUT, oid, 'CO' 				, co_array
NCDF_VARPUT, oid, 'Temperature'  	, temp_array
NCDF_VARPUT, oid, 'Wind_Speed'   	, wind_spd_array
NCDF_VARPUT, oid, 'Wind_Direction'	, wind_dir_array
NCDF_VARPUT, oid, 'Pressure'		, press_array
NCDF_VARPUT, oid, 'RH'        		, rh_array
NCDF_VARPUT, oid, 'Dew_Point'   	, dp_array

PRINT, 'done assigning variables'

NCDF_CLOSE, oid																									;Close output file

PRINT, 'File ' + outfile + ' processed.' 

STOP


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



END
