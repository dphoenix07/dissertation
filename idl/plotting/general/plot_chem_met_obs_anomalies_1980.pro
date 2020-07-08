PRO PLOT_CHEM_MET_OBS_ANOMALIES_1980, startmonth, $
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

IF (N_ELEMENTS(startmonth) EQ 0) THEN startmonth = '071980'

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
imonth = WHERE(((ts_data.date.year GE '1980') AND (ts_data.date.year LT '2017')) AND $
	((ts_data.date.month GE 10) AND (ts_data.date.month LT 11)) AND (ts_data.date.hour EQ 18))

;extr_storms = WHERE(((ts_data.class[imonth] EQ 'TD') OR (ts_data.class[imonth] EQ 'TS') OR $
;    (ts_data.class[imonth] EQ 'HU')) AND ((ts_data.x[imonth] LE -75.0) AND $
;    	(ts_data.x[imonth] GT -100.0) AND (ts_data.y[imonth] LE 40.0) AND $
;    	(ts_data.y[imonth] GE 20.0)), istorms)
extr_storms = WHERE(((ts_data.class[imonth] EQ 'TD')) AND ((ts_data.x[imonth] LE -75.0) AND $
    	(ts_data.x[imonth] GT -100.0) AND (ts_data.y[imonth] LE 40.0) AND $
    	(ts_data.y[imonth] GE 20.0)), istorms)

iperiod = imonth[extr_storms]

nt = N_ELEMENTS(ts_data.date[iperiod]) 

years  = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),0,4) 
months = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),5,2) 
days   = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),8,2) 
hours  = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),11,2)

ts_datestr = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),0,4) +  $
	STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),5,2) + $
	 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),8,2) + $
	 STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),11,2) + '00'
		
;Set arrays
;mth_arr=['08','09','10']
;day_arr=[31,30,31]

mth_arr = ['07','08','09','10','11']
day_arr = [31  ,  31,  30,  31,  30]
;hr_arr = [hour]

yr_arr = ['1980','1981','1982','1983','1984','1985','1986','1987','1988','1989','1990',  $
			'1991','1992','1993','1994','1995','1996','1997','1998','1999','2000','2001', $
			'2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012',$
			'2013','2014','2015','2016','2017']

;yr_arr = ['1996','1997','1998','1999','2000','2001','2002','2003','2004','2005','2006',$
;		'2007','2008','2009','2010','2011','2012','2013','2014','2015','2016']

;yr_arr=['2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011',$
;			'2012','2013','2014','2015','2016','2017']

;yr_arr=['2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016','2017']


m=0		
ny = N_ELEMENTS(yr_arr)
mt = N_ELEMENTS(mth_arr)

;Now for each year,month,day,hour compute the anomaly during storm track periods
infile      = !WRF_DIRECTORY + 'general/o3_data/monthly_ave_data/monthly_ave_' + $
			startmonth + '_' + '122017.nc'															

;Get monthly averaged data
id  = NCDF_OPEN(infile)																						;Open input file for reading	

NCDF_VARGET, id, 'O3'				, month_o3   
;NCDF_VARGET, id, 'VOC'   			, month_voc  
;NCDF_VARGET, id, 'PM2.5' 			, month_pm  
NCDF_VARGET, id, 'NOx' 				, month_nox  
NCDF_VARGET, id, 'CO'	 			, month_co  
NCDF_VARGET, id, 'Temperature'  	, month_temp 
NCDF_VARGET, id, 'Wind_Speed'   	, month_wind_spd 
NCDF_VARGET, id, 'Wind_Direction'	, month_wind_dir
;NCDF_VARGET, id, 'Pressure'			, month_press
;NCDF_VARGET, id, 'RH'        		, month_rh  
;NCDF_VARGET, id, 'Dew_Point'   		, month_dp	

NCDF_CLOSE,  id																								;Close input file
	 	
;define arrays
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

o3_narray = []
voc_narray = []
pm_narray = []
nox_narray = []
co_narray = []
temp_narray = []
press_narray = []
wind_spd_narray = []
wind_dir_narray = []
rh_narray = []
dp_narray = []

o3_storm = [ ] 
o3dm_storm = []
voc_storm = []
pm_storm = []
nox_storm = []
no_storm  = []
no2_storm = []
co_storm = []
temp_storm = []
press_storm = []
wind_spd_storm = []
wind_dir_storm = []
rh_storm = []
dp_storm = []

o3_nstorm = [ ] 
o3dm_nstorm = []
voc_nstorm = []
pm_nstorm = []
nox_nstorm = []
no_nstorm  = []
no2_nstorm = []
co_nstorm = []
temp_nstorm = []
press_nstorm = []
wind_spd_nstorm = []
wind_dir_nstorm = []
rh_nstorm = []
dp_nstorm = []

nt=N_ELEMENTS(years)
FOREACH year, yr_arr DO BEGIN
    ;date1 = years[m] + months[m] + days[m] + hour + '00'
    ;PRINT, date1

    ;Create 1-hour bin with 6 hour interval
    time_arr = [ ]
    date1 = MAKE_DATE(year,10,01,18,00)
    date2 = MAKE_DATE(year,11,01,18,00)
    end_datestr = STRMID(MAKE_ISO_DATE_STRING(date2),0,4) + STRMID(MAKE_ISO_DATE_STRING(date2),5,2) + $
    			 	STRMID(MAKE_ISO_DATE_STRING(date2),8,2) + STRMID(MAKE_ISO_DATE_STRING(date1),11,2) +'00'
    
    nt_yr = 8760/6
    FOR i = 0, nt_yr - 1 DO BEGIN
    		datestr = STRMID(MAKE_ISO_DATE_STRING(date1),0,4) + STRMID(MAKE_ISO_DATE_STRING(date1),5,2) + $
    			 STRMID(MAKE_ISO_DATE_STRING(date1),8,2) + STRMID(MAKE_ISO_DATE_STRING(date1),11,2) +'00'
    		time_arr = [time_arr, datestr]
    		IF (datestr EQ end_datestr) THEN BREAK
    		date1 = TIME_INC(date1, 86400)
    ENDFOR 
    
	o3_data      = OZONE_DALLAS_NAAQS(year)
;	voc_data 	 =   VOC_DALLAS_NAAQS(year)
;	pm_data 	 =   PM_DALLAS_NAAQS(year)
	nox_data 	 =   NOX_DALLAS_NAAQS(year)
	no_data 	 =   NO_DALLAS_NAAQS(year)
	no2_data 	 =   NO2_DALLAS_NAAQS(year)
	co_data 	 =  CO_DALLAS_NAAQS(year)
	temp_data 	 =  TEMP_DALLAS_NAAQS(year)
	wind_spd_data  =  WIND_SPD_DALLAS_NAAQS(year)
	wind_dir_data  =  WIND_DIR_DALLAS_NAAQS(year)
;	press_data 	= PRESS_DALLAS_NAAQS(year)
;	rh_data 	= RH_DALLAS_NAAQS(year)
;	dp_data 	= DP_DALLAS_NAAQS(year)
	
	nt=N_ELEMENTS(time_arr)
	FOR tt = 0, nt-1 DO BEGIN
	    mth = WHERE(STRMID(time_arr[tt],4,2) EQ mth_arr)

	    o3_datestr =  STRMID(o3_data.date,0,4)+STRMID(o3_data.date,5,2)+STRMID(o3_data.date,8,2) + $
	    			  STRMID(o3_data.time,0,2)+STRMID(o3_data.time,3,2) 
        io3 = WHERE(o3_datestr EQ time_arr[tt])
        io3dm = WHERE(o3_data.ozone_datestr EQ time_arr[tt])
   
;	    voc_datestr =  STRMID(voc_data.date,0,4)+STRMID(voc_data.date,5,2)+STRMID(voc_data.date,8,2) + $
;	    			  STRMID(voc_data.time,0,2)+STRMID(voc_data.time,3,2) 
;        ivoc = WHERE(voc_datestr EQ time_arr[tt])
   
;	    pm_datestr =  STRMID(pm_data.date,0,4)+STRMID(pm_data.date,5,2)+STRMID(pm_data.date,8,2) + $
;	    			  STRMID(pm_data.time,0,2)+STRMID(pm_data.time,3,2) 
;        ipm = WHERE(pm_datestr EQ time_arr[tt])
   
	    nox_datestr =  STRMID(nox_data.date,0,4)+STRMID(nox_data.date,5,2)+STRMID(nox_data.date,8,2) + $
	    			  STRMID(nox_data.time,0,2)+STRMID(nox_data.time,3,2) 
        inox = WHERE(nox_datestr EQ time_arr[tt])

	    no_datestr =  STRMID(no_data.date,0,4)+STRMID(no_data.date,5,2)+STRMID(no_data.date,8,2) + $
	    			  STRMID(no_data.time,0,2)+STRMID(no_data.time,3,2) 
        ino = WHERE(no_datestr EQ time_arr[tt])

	    no2_datestr =  STRMID(no2_data.date,0,4)+STRMID(no2_data.date,5,2)+STRMID(no2_data.date,8,2) + $
	    			  STRMID(no2_data.time,0,2)+STRMID(no2_data.time,3,2) 
        ino2 = WHERE(no2_datestr EQ time_arr[tt])
    
	    co_datestr =  STRMID(co_data.date,0,4)+STRMID(co_data.date,5,2)+STRMID(co_data.date,8,2) + $
	    			  STRMID(co_data.time,0,2)+STRMID(co_data.time,3,2) 
        ico = WHERE(co_datestr EQ time_arr[tt])
    
	    temp_datestr =  STRMID(temp_data.date,0,4)+STRMID(temp_data.date,5,2)+STRMID(temp_data.date,8,2) + $
	    			  STRMID(temp_data.time,0,2)+STRMID(temp_data.time,3,2) 
        itemp = WHERE(temp_datestr EQ time_arr[tt])
    
	    wind_spd_datestr =  STRMID(wind_spd_data.date,0,4)+STRMID(wind_spd_data.date,5,2)+STRMID(wind_spd_data.date,8,2) + $
	    			  STRMID(wind_spd_data.time,0,2)+STRMID(wind_spd_data.time,3,2) 
        iwind_spd = WHERE(wind_spd_datestr EQ time_arr[tt])
    
	    wind_dir_datestr =  STRMID(wind_dir_data.date,0,4)+STRMID(wind_dir_data.date,5,2)+STRMID(wind_dir_data.date,8,2) + $
	    			  STRMID(wind_dir_data.time,0,2)+STRMID(wind_dir_data.time,3,2) 
        iwind_dir = WHERE(wind_dir_datestr EQ time_arr[tt])
    
;	    press_datestr =  STRMID(press_data.date,0,4)+STRMID(press_data.date,5,2)+STRMID(press_data.date,8,2) + $
;	    			  STRMID(press_data.time,0,2)+STRMID(press_data.time,3,2) 
;        ipress = WHERE(press_datestr EQ time_arr[tt])
;   
;	    rh_datestr =  STRMID(rh_data.date,0,4)+STRMID(rh_data.date,5,2)+STRMID(rh_data.date,8,2) + $
;	    			  STRMID(rh_data.time,0,2)+STRMID(rh_data.time,3,2) 
;        irh = WHERE(rh_datestr EQ time_arr[tt])
;   
;	    dp_datestr =  STRMID(dp_data.date,0,4)+STRMID(dp_data.date,5,2)+STRMID(dp_data.date,8,2) + $
;	    			  STRMID(dp_data.time,0,2)+STRMID(dp_data.time,3,2) 
;        idp = WHERE(dp_datestr EQ time_arr[tt])

		
 	   its   = WHERE(ts_datestr  EQ time_arr[tt], tscount )
 	   PRINT, tscount
 	   PRINT, time_arr[tt]
 	   IF (tscount EQ 0) THEN BEGIN
	        o3_anom  = (MAX(o3_data.ozone_hourly[io3],/NAN) - month_o3[mth])
	        o3_narray = [o3_narray, o3_anom]     
	        o3_nstorm = [o3_nstorm, MAX(o3_data.ozone_hourly[io3],/NAN)]
		    o3dm_nstorm = [o3dm_nstorm, MAX(o3_data.ozone_daily_max[io3dm],/NAN)]

;	        voc_anom  = (TOTAL(voc_data.voc_hourly[ivoc],/NAN) - month_voc[mth])
;	        voc_narray = [voc_narray, voc_anom]     
;	        voc_nstorm = [voc_nstorm, TOTAL(voc_data.voc_hourly[ivoc],/NAN)]
      
;	        pm_anom  = (MEAN(pm_data.pm_hourly[ipm],/NAN) - month_pm[mth])
;	        pm_narray = [pm_narray, pm_anom]     
;	        pm_nstorm = [pm_nstorm, MEAN(pm_data.pm_hourly[ipm],/NAN)]
      
	        nox_anom  = (MEAN(nox_data.nox_hourly[inox],/NAN) - month_nox[mth])
	        nox_narray = [nox_narray, nox_anom]     
	        nox_nstorm = [nox_nstorm, MEAN(nox_data.nox_hourly[inox],/NAN)]
      
	        no_nstorm = [no_nstorm, MEAN(no_data.no_hourly[ino],/NAN)]
	        no2_nstorm = [no2_nstorm, MEAN(no2_data.no2_hourly[ino2],/NAN)]

	        co_anom  = (MEAN(co_data.co_hourly[ico],/NAN) - month_co[mth])
	        co_narray = [co_narray, co_anom]     
	        co_nstorm = [co_nstorm, MEAN(co_data.co_hourly[ico],/NAN)]
      
	        temp_anom  = (MEAN(temp_data.temp_hourly[itemp],/NAN) - month_temp[mth])
	        temp_narray = [temp_narray, temp_anom]     
	        temp_nstorm = [temp_nstorm, MEAN(temp_data.temp_hourly[itemp],/NAN)]
       
;	        press_anom  = (MEAN(press_data.press_hourly[ipress],/NAN) - month_press[mth])
;	        press_narray = [press_narray, press_anom]   
;	        press_nstorm = [press_nstorm, MEAN(press_data.press_hourly[ipress],/NAN)]
	        
	        wind_spd_anom  = (MEAN(wind_spd_data.wind_spd_hourly[iwind_spd],/NAN) - month_wind_spd[mth])  
        	wind_spd_narray = [wind_spd_narray, wind_spd_anom]
	        wind_spd_nstorm = [wind_spd_nstorm, MEAN(wind_spd_data.wind_spd_hourly[iwind_spd],/NAN)]
      
	        wind_dir_anom  = (MEAN(wind_dir_data.wind_dir_hourly[iwind_dir],/NAN) - month_wind_dir[mth])  
            wind_dir_narray = [wind_dir_narray, wind_dir_anom]
	        wind_dir_nstorm = [wind_dir_nstorm, MEAN(wind_dir_data.wind_dir_hourly[iwind_dir],/NAN)]
      
;	        rh_anom  = (MEAN(rh_data.rh_hourly[irh],/NAN) - month_rh[mth])  
;            rh_narray = [rh_narray, rh_anom]
;	        rh_nstorm = [rh_nstorm, MEAN(rh_data.rh_hourly[irh],/NAN)]
;       
;	        dp_anom  = (MEAN(dp_data.dp_hourly[idp],/NAN) - month_dp[mth])  
;            dp_narray = [dp_narray, dp_anom]
;	        dp_nstorm = [dp_nstorm, MEAN(dp_data.dp_hourly[idp],/NAN)]
   	   ENDIF 
   	   
   	   IF (tscount GT 0) THEN BEGIN
	        o3_anom  = (MAX(o3_data.ozone_hourly[io3],/NAN) - month_o3[mth])
	        o3_array = [o3_array, o3_anom]     
	        o3_storm = [o3_storm, MAX(o3_data.ozone_hourly[io3],/NAN)]
		    o3dm_storm = [o3dm_storm, MAX(o3_data.ozone_daily_max[io3dm],/NAN)]

			PRINT, o3_storm
			PRINT, o3dm_storm
;	        voc_anom  = (TOTAL(voc_data.voc_hourly[ivoc],/NAN) - month_voc[mth])
;	        voc_array = [voc_array, voc_anom]     
;	        voc_storm = [voc_storm, TOTAL(voc_data.voc_hourly[ivoc],/NAN)]
      
;	        pm_anom  = (MEAN(pm_data.pm_hourly[ipm],/NAN) - month_pm[mth])
;	        pm_array = [pm_array, pm_anom]     
;	        pm_storm = [pm_storm, MEAN(pm_data.pm_hourly[ipm],/NAN)]
      
	        nox_anom  = (MEAN(nox_data.nox_hourly[inox],/NAN) - month_nox[mth])
	        nox_array = [nox_array, nox_anom]     
	        nox_storm = [nox_storm, MEAN(nox_data.nox_hourly[inox],/NAN)]

	        no_storm  = [no_storm , MEAN(no_data.no_hourly[ino],/NAN)]
	        no2_storm = [no2_storm, MEAN(no2_data.no2_hourly[ino2],/NAN)]
      
	        co_anom  = (MEAN(co_data.co_hourly[ico],/NAN) - month_co[mth])
	        co_array = [co_array, co_anom]     
	        co_storm = [co_storm, MEAN(co_data.co_hourly[ico],/NAN)]
      
	        temp_anom  = (MEAN(temp_data.temp_hourly[itemp],/NAN) - month_temp[mth])
	        temp_array = [temp_array, temp_anom]     
	        temp_storm = [temp_storm, MEAN(temp_data.temp_hourly[itemp],/NAN)]
       
;	        press_anom  = (MEAN(press_data.press_hourly[ipress],/NAN) - month_press[mth])
;	        press_array = [press_array, press_anom]   
;	        press_storm = [press_storm, MEAN(press_data.press_hourly[ipress],/NAN)]
	        
	        wind_spd_anom  = (MEAN(wind_spd_data.wind_spd_hourly[iwind_spd],/NAN) - month_wind_spd[mth])  
            wind_spd_array = [wind_spd_array, wind_spd_anom]
	        wind_spd_storm = [wind_spd_storm, MEAN(wind_spd_data.wind_spd_hourly[iwind_spd],/NAN)]
        
	        wind_dir_anom  = (MEAN(wind_dir_data.wind_dir_hourly[iwind_dir],/NAN) - month_wind_dir[mth])  
            wind_dir_array = [wind_dir_array, wind_dir_anom]
	        wind_dir_storm = [wind_dir_storm, MEAN(wind_dir_data.wind_dir_hourly[iwind_dir],/NAN)]
        
;	        rh_anom  = (MEAN(rh_data.rh_hourly[irh],/NAN) - month_rh[mth])  
;            rh_array = [rh_array, rh_anom]
;	        rh_storm = [rh_storm, MEAN(rh_data.rh_hourly[irh],/NAN)]
;       
;	        dp_anom  = (MEAN(dp_data.dp_hourly[idp],/NAN) - month_dp[mth])  
;            dp_array = [dp_array, dp_anom]
;	        dp_storm = [dp_storm, MEAN(dp_data.dp_hourly[idp],/NAN)]
   	   ENDIF
   	   ENDFOR
ENDFOREACH  

;;Write anomalies to file in case you want to look at different relationships
outfile = 'hourly_anomalies_101980_112017_OCT_18Z_TD_new_area.nc'
outdir  = !WRF_DIRECTORY + 'general/o3_data/hourly_data/'
FILE_MKDIR, outdir

dim = SIZE(o3_array, /DIMENSIONS)																			;Get grid dimension sizes
CATCH, error_status																								;Catch any errors with netcdf control or file creation

IF (error_status NE 0) THEN BEGIN
	NCDF_CLOSE, oid																								;Close previous failed file
	oid = NCDF_CREATE(outdir + outfile, CLOBBER = 1)								;Create output file for writing
ENDIF ELSE $
	oid = NCDF_CREATE(outdir + outfile, CLOBBER = 1)								;Create output file for writing

tid = NCDF_DIMDEF(oid, 'Storm_Anom_Hours', dim[0])																		;Define output dimensions in netCDF file

PRINT, 'start creating variable names'
vid = NCDF_VARDEF(oid, 'O3_anomaly', [tid], /FLOAT)												;Define the longitude variable
NCDF_ATTPUT, oid, 'O3_anomaly', 'long_name', 'O3 storm anomalies 1980-2017'								;Name attribute
NCDF_ATTPUT, oid, 'O3_anomaly', 'units',     'ppb'												;Units attribute

;vid = NCDF_VARDEF(oid, 'VOC_anomaly', [tid], /FLOAT)												;Define the longitude variable
;NCDF_ATTPUT, oid, 'VOC_anomaly', 'long_name', 'VOC storm anomalies 1980-2017'								;Name attribute
;NCDF_ATTPUT, oid, 'VOC_anomaly', 'units',     'ppb'												;Units attribute

;vid = NCDF_VARDEF(oid, 'PM2.5_anomaly', [tid], /FLOAT)												;Define the latitude variable
;NCDF_ATTPUT, oid, 'PM2.5_anomaly', 'long_name', 'PM 2.5 storm anomalies 1980-2017'									;Name attribute
;NCDF_ATTPUT, oid, 'PM2.5_anomaly', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'NOx_anomaly', [tid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'NOx_anomaly', 'long_name', 'NOy storm anomalies 1980-2017'									;Name attribute
NCDF_ATTPUT, oid, 'NOx_anomaly', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'CO_anomaly', [tid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'CO_anomaly', 'long_name', 'CO storm anomalies 1980-2017'									;Name attribute
NCDF_ATTPUT, oid, 'CO_anomaly', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'Temperature_anomaly', [tid], /FLOAT)													;Define the pressure variable
NCDF_ATTPUT, oid, 'Temperature_anomaly', 'long_name', 'Temperature storm anomalies 1980-2017'														;Name attribute
NCDF_ATTPUT, oid, 'Temperature_anomaly', 'units',     'degrees F'																	;Units attribute

vid = NCDF_VARDEF(oid, 'Wind_Speed_anomaly', [tid], /FLOAT)													;Define the pressure variable
NCDF_ATTPUT, oid, 'Wind_Speed_anomaly', 'long_name', 'Wind Speed storm anomalies 1980-2017'														;Name attribute
NCDF_ATTPUT, oid, 'Wind_Speed_anomaly', 'units',     'Knots'																	;Units attribute

vid = NCDF_VARDEF(oid, 'Wind_Direction_anomaly', [tid], /FLOAT)													;Define the geopotential height variable
NCDF_ATTPUT, oid, 'Wind_Direction_anomaly', 'long_name', 'Wind Direction storm anomalies 1980-2017' 											;Name attribute
NCDF_ATTPUT, oid, 'Wind_Direction_anomaly', 'units',     'Degrees Compass'																	;Units attribute

;vid = NCDF_VARDEF(oid, 'Pressure_anomaly', [tid], /FLOAT)													;Define the geopotential height variable
;NCDF_ATTPUT, oid, 'Pressure_anomaly', 'long_name', 'Pressure storm anomalies 1980-2017'											;Name attribute
;NCDF_ATTPUT, oid, 'Pressure_anomaly', 'units',     'hPa'																	;Units attribute
;
;vid = NCDF_VARDEF(oid, 'RH_anomaly', [tid], /FLOAT)												;Define the latitude variable
;NCDF_ATTPUT, oid, 'RH_anomaly', 'long_name', 'Relative Humidity storm anomalies 1980-2017'									;Name attribute
;NCDF_ATTPUT, oid, 'RH_anomaly', 'units',     '%'												;Units attribute
;
;vid = NCDF_VARDEF(oid, 'Dew_Point_anomaly', [tid], /FLOAT)												;Define the latitude variable
;NCDF_ATTPUT, oid, 'Dew_Point_anomaly', 'long_name', 'Dew Point Temperature storm anomalies 1980-2017'									;Name attribute
;NCDF_ATTPUT, oid, 'Dew_Point_anomaly', 'units',     'degrees F'												;Units attribute

dim = SIZE(o3_narray, /DIMENSIONS)																			;Get grid dimension sizes
tid = NCDF_DIMDEF(oid, 'NoStorm_Anom_Hours', dim[0])																		;Define output dimensions in netCDF file

vid = NCDF_VARDEF(oid, 'O3_nanomaly', [tid], /FLOAT)												;Define the longitude variable
NCDF_ATTPUT, oid, 'O3_nanomaly', 'long_name', 'O3 no-storm anomalies 1980-2017'								;Name attribute
NCDF_ATTPUT, oid, 'O3_nanomaly', 'units',     'ppb'												;Units attribute

;vid = NCDF_VARDEF(oid, 'VOC_nanomaly', [tid], /FLOAT)												;Define the longitude variable
;NCDF_ATTPUT, oid, 'VOC_nanomaly', 'long_name', 'VOC no-storm anomalies 1980-2017'								;Name attribute
;NCDF_ATTPUT, oid, 'VOC_nanomaly', 'units',     'ppb'												;Units attribute

;vid = NCDF_VARDEF(oid, 'PM2.5_nanomaly', [tid], /FLOAT)												;Define the latitude variable
;NCDF_ATTPUT, oid, 'PM2.5_nanomaly', 'long_name', 'PM 2.5 no-storm anomalies 1980-2017'									;Name attribute
;NCDF_ATTPUT, oid, 'PM2.5_nanomaly', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'NOx_nanomaly', [tid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'NOx_nanomaly', 'long_name', 'NOy no-storm anomalies 1980-2017'									;Name attribute
NCDF_ATTPUT, oid, 'NOx_nanomaly', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'CO_nanomaly', [tid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'CO_nanomaly', 'long_name', 'CO no-storm anomalies 1980-2017'									;Name attribute
NCDF_ATTPUT, oid, 'CO_nanomaly', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'Temperature_nanomaly', [tid], /FLOAT)													;Define the pressure variable
NCDF_ATTPUT, oid, 'Temperature_nanomaly', 'long_name', 'Temperature no-storm anomalies 1980-2017'														;Name attribute
NCDF_ATTPUT, oid, 'Temperature_nanomaly', 'units',     'degrees F'																	;Units attribute

vid = NCDF_VARDEF(oid, 'Wind_Speed_nanomaly', [tid], /FLOAT)													;Define the pressure variable
NCDF_ATTPUT, oid, 'Wind_Speed_nanomaly', 'long_name', 'Wind Speed no-storm anomalies 1980-2017'														;Name attribute
NCDF_ATTPUT, oid, 'Wind_Speed_nanomaly', 'units',     'Knots'																	;Units attribute

vid = NCDF_VARDEF(oid, 'Wind_Direction_nanomaly', [tid], /FLOAT)													;Define the geopotential height variable
NCDF_ATTPUT, oid, 'Wind_Direction_nanomaly', 'long_name', 'Wind Direction no-storm anomalies 1980-2017' 											;Name attribute
NCDF_ATTPUT, oid, 'Wind_Direction_nanomaly', 'units',     'Degrees Compass'																	;Units attribute

;vid = NCDF_VARDEF(oid, 'Pressure_nanomaly', [tid], /FLOAT)													;Define the geopotential height variable
;NCDF_ATTPUT, oid, 'Pressure_nanomaly', 'long_name', 'Pressure no-storm anomalies 1980-2017'											;Name attribute
;NCDF_ATTPUT, oid, 'Pressure_nanomaly', 'units',     'hPa'																	;Units attribute
;
;vid = NCDF_VARDEF(oid, 'RH_nanomaly', [tid], /FLOAT)												;Define the latitude variable
;NCDF_ATTPUT, oid, 'RH_nanomaly', 'long_name', 'Relative Humidity no-storm anomalies 1980-2017'									;Name attribute
;NCDF_ATTPUT, oid, 'RH_nanomaly', 'units',     '%'												;Units attribute
;
;vid = NCDF_VARDEF(oid, 'Dew_Point_nanomaly', [tid], /FLOAT)												;Define the latitude variable
;NCDF_ATTPUT, oid, 'Dew_Point_nanomaly', 'long_name', 'Dew Point Temperature no-storm anomalies 1980-2017'									;Name attribute
;NCDF_ATTPUT, oid, 'Dew_Point_nanomaly', 'units',     'degrees F'												;Units attribute

dim = SIZE(o3_storm, /DIMENSIONS)																			;Get grid dimension sizes
tid = NCDF_DIMDEF(oid, 'Storm_Conc_Hours', dim[0])																		;Define output dimensions in netCDF file

vid = NCDF_VARDEF(oid, 'O3_storm', [tid], /FLOAT)												;Define the longitude variable
NCDF_ATTPUT, oid, 'O3_storm', 'long_name', 'O3 storm concentration 1980-2017'								;Name attribute
NCDF_ATTPUT, oid, 'O3_storm', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'O3DM_storm', [tid], /FLOAT)												;Define the longitude variable
NCDF_ATTPUT, oid, 'O3DM_storm', 'long_name', 'O3 DM storm concentration 2005-2017'								;Name attribute
NCDF_ATTPUT, oid, 'O3DM_storm', 'units',     'ppb'												;Units attribute

;vid = NCDF_VARDEF(oid, 'VOC_storm', [tid], /FLOAT)												;Define the longitude variable
;NCDF_ATTPUT, oid, 'VOC_storm', 'long_name', 'VOC storm concentration 1980-2017'								;Name attribute
;NCDF_ATTPUT, oid, 'VOC_storm', 'units',     'ppb'												;Units attribute

;vid = NCDF_VARDEF(oid, 'PM2.5_storm', [tid], /FLOAT)												;Define the latitude variable
;NCDF_ATTPUT, oid, 'PM2.5_storm', 'long_name', 'PM 2.5 storm concentration 1980-2017'									;Name attribute
;NCDF_ATTPUT, oid, 'PM2.5_storm', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'NOx_storm', [tid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'NOx_storm', 'long_name', 'NOy storm concentration 1980-2017'									;Name attribute
NCDF_ATTPUT, oid, 'NOx_storm', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'NO_storm', [tid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'NO_storm', 'long_name', 'NO storm concentration 1980-2017'									;Name attribute
NCDF_ATTPUT, oid, 'NO_storm', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'NO2_storm', [tid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'NO2_storm', 'long_name', 'NO2 storm concentration 1980-2017'									;Name attribute
NCDF_ATTPUT, oid, 'NO2_storm', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'CO_storm', [tid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'CO_storm', 'long_name', 'CO storm concentration 1980-2017'									;Name attribute
NCDF_ATTPUT, oid, 'CO_storm', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'Temperature_storm', [tid], /FLOAT)													;Define the pressure variable
NCDF_ATTPUT, oid, 'Temperature_storm', 'long_name', 'Temperature storm 1980-2017'														;Name attribute
NCDF_ATTPUT, oid, 'Temperature_storm', 'units',     'degrees F'																	;Units attribute

vid = NCDF_VARDEF(oid, 'Wind_Speed_storm', [tid], /FLOAT)													;Define the pressure variable
NCDF_ATTPUT, oid, 'Wind_Speed_storm', 'long_name', 'Wind Speed storm 1980-2017'														;Name attribute
NCDF_ATTPUT, oid, 'Wind_Speed_storm', 'units',     'Knots'																	;Units attribute

vid = NCDF_VARDEF(oid, 'Wind_Direction_storm', [tid], /FLOAT)													;Define the geopotential height variable
NCDF_ATTPUT, oid, 'Wind_Direction_storm', 'long_name', 'Wind Direction storm 1980-2017' 											;Name attribute
NCDF_ATTPUT, oid, 'Wind_Direction_storm', 'units',     'Degrees Compass'																	;Units attribute

;vid = NCDF_VARDEF(oid, 'Pressure_storm', [tid], /FLOAT)													;Define the geopotential height variable
;NCDF_ATTPUT, oid, 'Pressure_storm', 'long_name', 'Pressure storm 1980-2017'											;Name attribute
;NCDF_ATTPUT, oid, 'Pressure_storm', 'units',     'hPa'																	;Units attribute
;
;vid = NCDF_VARDEF(oid, 'RH_storm', [tid], /FLOAT)												;Define the latitude variable
;NCDF_ATTPUT, oid, 'RH_storm', 'long_name', 'Relative Humidity storm 1980-2017'									;Name attribute
;NCDF_ATTPUT, oid, 'RH_storm', 'units',     '%'												;Units attribute
;
;vid = NCDF_VARDEF(oid, 'Dew_Point_storm', [tid], /FLOAT)												;Define the latitude variable
;NCDF_ATTPUT, oid, 'Dew_Point_storm', 'long_name', 'Dew Point Temperature storm 1980-2017'									;Name attribute
;NCDF_ATTPUT, oid, 'Dew_Point_storm', 'units',     'degrees F'												;Units attribute

dim = SIZE(o3_nstorm, /DIMENSIONS)																			;Get grid dimension sizes
tid = NCDF_DIMDEF(oid, 'NoStorm_Conc_Hours', dim[0])																		;Define output dimensions in netCDF file

vid = NCDF_VARDEF(oid, 'O3_nstorm', [tid], /FLOAT)												;Define the longitude variable
NCDF_ATTPUT, oid, 'O3_nstorm', 'long_name', 'O3 nstorm concentration 1980-2017'								;Name attribute
NCDF_ATTPUT, oid, 'O3_nstorm', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'O3DM_nstorm', [tid], /FLOAT)												;Define the longitude variable
NCDF_ATTPUT, oid, 'O3DM_nstorm', 'long_name', 'O3 DM nstorm concentration 2005-2017'								;Name attribute
NCDF_ATTPUT, oid, 'O3DM_nstorm', 'units',     'ppb'												;Units attribute

;vid = NCDF_VARDEF(oid, 'VOC_nstorm', [tid], /FLOAT)												;Define the longitude variable
;NCDF_ATTPUT, oid, 'VOC_nstorm', 'long_name', 'VOC nstorm concentration 1980-2017'								;Name attribute
;NCDF_ATTPUT, oid, 'VOC_nstorm', 'units',     'ppb'												;Units attribute

;vid = NCDF_VARDEF(oid, 'PM2.5_nstorm', [tid], /FLOAT)												;Define the latitude variable
;NCDF_ATTPUT, oid, 'PM2.5_nstorm', 'long_name', 'PM 2.5 nstorm concentration 1980-2017'									;Name attribute
;NCDF_ATTPUT, oid, 'PM2.5_nstorm', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'NOx_nstorm', [tid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'NOx_nstorm', 'long_name', 'NOy nstorm concentration 1980-2017'									;Name attribute
NCDF_ATTPUT, oid, 'NOx_nstorm', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'NO_nstorm', [tid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'NO_nstorm', 'long_name', 'NO nstorm concentration 1980-2017'									;Name attribute
NCDF_ATTPUT, oid, 'NO_nstorm', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'NO2_nstorm', [tid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'NO2_nstorm', 'long_name', 'NO2 nstorm concentration 1980-2017'									;Name attribute
NCDF_ATTPUT, oid, 'NO2_nstorm', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'CO_nstorm', [tid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'CO_nstorm', 'long_name', 'CO nstorm concentration 1980-2017'									;Name attribute
NCDF_ATTPUT, oid, 'CO_nstorm', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'Temperature_nstorm', [tid], /FLOAT)													;Define the pressure variable
NCDF_ATTPUT, oid, 'Temperature_nstorm', 'long_name', 'Temperature nstorm 1980-2017'														;Name attribute
NCDF_ATTPUT, oid, 'Temperature_nstorm', 'units',     'degrees F'																	;Units attribute

vid = NCDF_VARDEF(oid, 'Wind_Speed_nstorm', [tid], /FLOAT)													;Define the pressure variable
NCDF_ATTPUT, oid, 'Wind_Speed_nstorm', 'long_name', 'Wind Speed nstorm 1980-2017'														;Name attribute
NCDF_ATTPUT, oid, 'Wind_Speed_nstorm', 'units',     'Knots'																	;Units attribute

vid = NCDF_VARDEF(oid, 'Wind_Direction_nstorm', [tid], /FLOAT)													;Define the geopotential height variable
NCDF_ATTPUT, oid, 'Wind_Direction_nstorm', 'long_name', 'Wind Direction nstorm 1980-2017' 											;Name attribute
NCDF_ATTPUT, oid, 'Wind_Direction_nstorm', 'units',     'Degrees Compass'																	;Units attribute

;vid = NCDF_VARDEF(oid, 'Pressure_nstorm', [tid], /FLOAT)													;Define the geopotential height variable
;NCDF_ATTPUT, oid, 'Pressure_nstorm', 'long_name', 'Pressure nstorm 1980-2017'											;Name attribute
;NCDF_ATTPUT, oid, 'Pressure_nstorm', 'units',     'hPa'																	;Units attribute
;
;vid = NCDF_VARDEF(oid, 'RH_nstorm', [tid], /FLOAT)												;Define the latitude variable
;NCDF_ATTPUT, oid, 'RH_nstorm', 'long_name', 'Relative Humidity nstorm 1980-2017'									;Name attribute
;NCDF_ATTPUT, oid, 'RH_nstorm', 'units',     '%'												;Units attribute
;
;vid = NCDF_VARDEF(oid, 'Dew_Point_nstorm', [tid], /FLOAT)												;Define the latitude variable
;NCDF_ATTPUT, oid, 'Dew_Point_nstorm', 'long_name', 'Dew Point Temperature nstorm 1980-2017'									;Name attribute
;NCDF_ATTPUT, oid, 'Dew_Point_nstorm', 'units',     'degrees F'													;Units attribute

PRINT, 'done creating variables'
NCDF_CONTROL, oid, /ENDEF

PRINT, 'start assigning variables'
NCDF_VARPUT, oid, 'O3_anomaly'				, o3_array
;NCDF_VARPUT, oid, 'VOC_anomaly'   			, voc_array
;NCDF_VARPUT, oid, 'PM2.5_anomaly'			, pm_array
NCDF_VARPUT, oid, 'NOx_anomaly' 			, nox_array
NCDF_VARPUT, oid, 'CO_anomaly' 				, co_array
NCDF_VARPUT, oid, 'Temperature_anomaly'  	, temp_array
NCDF_VARPUT, oid, 'Wind_Speed_anomaly'   	, wind_spd_array
NCDF_VARPUT, oid, 'Wind_Direction_anomaly'	, wind_dir_array
;NCDF_VARPUT, oid, 'Pressure_anomaly'		, press_array
;NCDF_VARPUT, oid, 'RH_anomaly'        		, rh_array
;NCDF_VARPUT, oid, 'Dew_Point_anomaly'   	, dp_array

NCDF_VARPUT, oid, 'O3_nanomaly'				, o3_narray
;NCDF_VARPUT, oid, 'VOC_nanomaly'   			, voc_narray
;NCDF_VARPUT, oid, 'PM2.5_nanomaly'			, pm_narray
NCDF_VARPUT, oid, 'NOx_nanomaly' 			, nox_narray
NCDF_VARPUT, oid, 'CO_nanomaly' 			, co_narray
NCDF_VARPUT, oid, 'Temperature_nanomaly'  	, temp_narray
NCDF_VARPUT, oid, 'Wind_Speed_nanomaly'   	, wind_spd_narray
NCDF_VARPUT, oid, 'Wind_Direction_nanomaly'	, wind_dir_narray
;NCDF_VARPUT, oid, 'Pressure_nanomaly'		, press_narray
;NCDF_VARPUT, oid, 'RH_nanomaly'        		, rh_narray
;NCDF_VARPUT, oid, 'Dew_Point_nanomaly'   	, dp_narray

NCDF_VARPUT, oid, 'O3_storm'				, o3_storm
NCDF_VARPUT, oid, 'O3DM_storm'				, o3dm_storm
;NCDF_VARPUT, oid, 'VOC_storm'   			, voc_storm
;NCDF_VARPUT, oid, 'PM2.5_storm'				, pm_storm
NCDF_VARPUT, oid, 'NOx_storm' 				, nox_storm
NCDF_VARPUT, oid, 'NO_storm' 				, no_storm
NCDF_VARPUT, oid, 'NO2_storm' 				, no2_storm
NCDF_VARPUT, oid, 'CO_storm' 				, co_storm
NCDF_VARPUT, oid, 'Temperature_storm'  		, temp_storm
NCDF_VARPUT, oid, 'Wind_Speed_storm'   		, wind_spd_storm
NCDF_VARPUT, oid, 'Wind_Direction_storm'	, wind_dir_storm
;NCDF_VARPUT, oid, 'Pressure_storm'			, press_storm
;NCDF_VARPUT, oid, 'RH_storm'        		, rh_storm
;NCDF_VARPUT, oid, 'Dew_Point_storm'   		, dp_storm

NCDF_VARPUT, oid, 'O3_nstorm'				, o3_nstorm
NCDF_VARPUT, oid, 'O3DM_nstorm'				, o3dm_nstorm
;NCDF_VARPUT, oid, 'VOC_nstorm'   			, voc_nstorm
;NCDF_VARPUT, oid, 'PM2.5_nstorm'			, pm_nstorm
NCDF_VARPUT, oid, 'NOx_nstorm' 				, nox_nstorm
NCDF_VARPUT, oid, 'NO_nstorm' 				, no_nstorm
NCDF_VARPUT, oid, 'NO2_nstorm' 				, no2_nstorm
NCDF_VARPUT, oid, 'CO_nstorm' 				, co_nstorm
NCDF_VARPUT, oid, 'Temperature_nstorm'  	, temp_nstorm
NCDF_VARPUT, oid, 'Wind_Speed_nstorm'   	, wind_spd_nstorm
NCDF_VARPUT, oid, 'Wind_Direction_nstorm'	, wind_dir_nstorm
;NCDF_VARPUT, oid, 'Pressure_nstorm'			, press_nstorm
;NCDF_VARPUT, oid, 'RH_nstorm'        		, rh_nstorm
;NCDF_VARPUT, oid, 'Dew_Point_nstorm'   		, dp_nstorm
	
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
