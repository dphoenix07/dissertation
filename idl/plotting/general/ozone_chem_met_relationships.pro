PRO OZONE_CHEM_MET_RELATIONSHIPS, hour, $
	PNG		  = png, $
	EPS		  = eps, $
	CLOBBER   = clobber

;+
;NAME:
;     OZONE_CHEM_MET_RELATIONSHIPS
;PURPOSE:
;     This copies variables from ERA-Interim analysis into a single
;     file in pressure coordinates for use in TRAJ3D.
;     W at the surface is set to zero.  One pressure level is added at 
;     the top of the domain (p = 0), where w is also set to zero.
;CATEGORY:
;     Data handling utility.
;CALLING SEQUENCE:
;     OZONE_CHEM_MET_RELATIONSHIPS, date0, outfile
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

;+Read tropical storm file
;ts_data = READ_HURDAT2()
;
;;Truncate data to only look at years with 00,06,12,18Z hours
;imonth = WHERE(((ts_data.date.year GE '2001') AND (ts_data.date.year LT '2018')) AND $
;	((ts_data.date.month GE 7) AND (ts_data.date.month LT 12)) AND (ts_data.date.hour EQ hour))
;
;extr_storms = WHERE(((ts_data.class[imonth] EQ 'TD') OR (ts_data.class[imonth] EQ 'TS') OR $
;    (ts_data.class[imonth] EQ 'HU')), istorms)
;iperiod = imonth[extr_storms]
;
;nt = N_ELEMENTS(ts_data.date[iperiod])
;
;years  = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),0,4) 
;months = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),5,2) 
;days   = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),8,2) 
;hours  = STRMID(MAKE_ISO_DATE_STRING(ts_data.date[iperiod]),11,2)
;
;;From Reanalysis monthly mean files, create month mean for entire period
;;mth_arr=['08','09','10']
;;day_arr=[31,30,31]
;-end

;mth_arr = ['07','08','09','10','11']
;day_arr = [31  ,  31,  30,  31,  30]
;hr_arr = [hour]

yr_arr = ['1980','1981','1982','1983','1984','1985','1986','1987','1988','1989','1990', $
			'1991','1992','1993','1994','1995','1996','1997','1998','1999','2000', $
			'2001','2002','2003','2004','2005','2006','2007','2008','2009','2010', $
			'2011','2012','2013','2014','2015','2016','2017']

;for voc, co, noy, temp, wind
;yr_arr = ['1996','1997','1998','1999','2000','2001','2002','2003','2004','2005','2006',$
;		'2007','2008','2009','2010','2011','2012','2013','2014','2015','2016','2017']

;for voc, co, noy, temp, wind, press, rh, dp
;yr_arr = ['2001','2002','2003','2004','2005','2006','2007','2008','2009','2010', $
;			'2011','2012','2013','2014','2015','2016','2017']
			
;for voc, co, noy, temp, wind, press, rh, dp, pm
;yr_arr = ['2005','2006','2007','2008','2009','2010', $
;			'2011','2012','2013','2014','2015','2016','2017']

;test array
;yr_arr=['2011','2012','2013']

;m=0		
;ny = N_ELEMENTS(yr_arr)
;mt = N_ELEMENTS(mth_arr)
;-end tropical storm file 

o3_yr       = []
voc_yr      = []
pm_yr		= []
nox_yr      = []
co_yr		= []
temp_yr     = []
wind_spd_yr = []
wind_dir_yr = []
press_yr    = []
rh_yr       = []
dp_yr       = []

FOREACH year, yr_arr DO BEGIN
    PRINT, year
    time_arr = [ ]
    date1 = MAKE_DATE(year,07,01,06,00)
    date2 = MAKE_DATE(year,12,01,06,00)
    end_datestr = STRMID(MAKE_ISO_DATE_STRING(date2),0,4) + STRMID(MAKE_ISO_DATE_STRING(date2),5,2) + $
    			 	STRMID(MAKE_ISO_DATE_STRING(date2),8,2) + STRMID(MAKE_ISO_DATE_STRING(date1),11,2) + $
    		  	 STRMID(MAKE_ISO_DATE_STRING(date1),14,2) 
   
    nt_yr = 8760
    FOR i = 0, nt_yr - 1 DO BEGIN
    		datestr = STRMID(MAKE_ISO_DATE_STRING(date1),0,4) + STRMID(MAKE_ISO_DATE_STRING(date1),5,2) + $
    			 STRMID(MAKE_ISO_DATE_STRING(date1),8,2) + STRMID(MAKE_ISO_DATE_STRING(date1),11,2) + $
    		  	 STRMID(MAKE_ISO_DATE_STRING(date1),14,2)  
    		time_arr = [time_arr, datestr]
    		IF (datestr EQ end_datestr) THEN BREAK
    		date1 = TIME_INC(date1, 3600)
    ENDFOR 
	
	o3_data        = OZONE_DALLAS_NAAQS(year)
;	voc_data       =   VOC_DALLAS_NAAQS(year)
;	pm_data        =   PM_DALLAS_NAAQS(year)
	nox_data       =   NOX_DALLAS_NAAQS(year)
	co_data        =  CO_DALLAS_NAAQS(year)
	temp_data  	   =  TEMP_DALLAS_NAAQS(year)
	wind_spd_data  =  WIND_SPD_DALLAS_NAAQS(year)
	wind_dir_data  =  WIND_DIR_DALLAS_NAAQS(year)
;	press_data 	   = PRESS_DALLAS_NAAQS(year)
;	rh_data        = RH_DALLAS_NAAQS(year)
;	dp_data        = DP_DALLAS_NAAQS(year)

	o3_datestr =  STRMID(o3_data.date,0,4)+STRMID(o3_data.date,5,2)+STRMID(o3_data.date,8,2) + $
				  STRMID(o3_data.time,0,2)+STRMID(o3_data.time,3,2) 

;	voc_datestr =  STRMID(voc_data.date,0,4)+STRMID(voc_data.date,5,2)+STRMID(voc_data.date,8,2) + $
;				  STRMID(voc_data.time,0,2)+STRMID(voc_data.time,3,2) 

;	pm_datestr =  STRMID(pm_data.date,0,4)+STRMID(pm_data.date,5,2)+STRMID(pm_data.date,8,2) + $
;				  STRMID(pm_data.time,0,2)+STRMID(pm_data.time,3,2) 

	nox_datestr =  STRMID(nox_data.date,0,4)+STRMID(nox_data.date,5,2)+STRMID(nox_data.date,8,2) + $
				  STRMID(nox_data.time,0,2)+STRMID(nox_data.time,3,2) 

	co_datestr =  STRMID(co_data.date,0,4)+STRMID(co_data.date,5,2)+STRMID(co_data.date,8,2) + $
				  STRMID(co_data.time,0,2)+STRMID(co_data.time,3,2) 

	temp_datestr =  STRMID(temp_data.date,0,4)+STRMID(temp_data.date,5,2)+STRMID(temp_data.date,8,2) + $
				  STRMID(temp_data.time,0,2)+STRMID(temp_data.time,3,2) 

	wind_spd_datestr =  STRMID(wind_spd_data.date,0,4)+STRMID(wind_spd_data.date,5,2)+STRMID(wind_spd_data.date,8,2) + $
				  STRMID(wind_spd_data.time,0,2)+STRMID(wind_spd_data.time,3,2) 

	wind_dir_datestr =  STRMID(wind_dir_data.date,0,4)+STRMID(wind_dir_data.date,5,2)+STRMID(wind_dir_data.date,8,2) + $
				  STRMID(wind_dir_data.time,0,2)+STRMID(wind_dir_data.time,3,2) 

;	press_datestr =  STRMID(press_data.date,0,4)+STRMID(press_data.date,5,2)+STRMID(press_data.date,8,2) + $
;				  STRMID(press_data.time,0,2)+STRMID(press_data.time,3,2) 
;
;	rh_datestr =  STRMID(rh_data.date,0,4)+STRMID(rh_data.date,5,2)+STRMID(rh_data.date,8,2) + $
;				  STRMID(rh_data.time,0,2)+STRMID(rh_data.time,3,2) 
;
;	dp_datestr =  STRMID(dp_data.date,0,4)+STRMID(dp_data.date,5,2)+STRMID(dp_data.date,8,2) + $
;				  STRMID(dp_data.time,0,2)+STRMID(dp_data.time,3,2) 

	o3_hr	    = []
	voc_hr      = []
	pm_hr       = []
	nox_hr      = []
	co_hr       = []
	temp_hr     = []
	wind_spd_hr = []
	wind_dir_hr = []
	press_hr 	= []
	rh_hr 		= []
	dp_hr 		= []
	
	nt = N_ELEMENTS(time_arr)
	FOR tt = 0, nt-1 DO BEGIN

		io3 = WHERE(o3_datestr EQ time_arr[tt], o3_count)
		o3_hr = [o3_hr, MEAN(o3_data.ozone_hourly[io3],/NAN)]
		
;		ivoc = WHERE(voc_datestr EQ time_arr[tt], voc_count)
;		voc_hr = [voc_hr, MEAN(voc_data.voc_hourly[ivoc],/NAN)]

;		ipm = WHERE(pm_datestr EQ time_arr[tt], pm_count)
;		pm_hr = [pm_hr, MEAN(pm_data.pm_hourly[ipm],/NAN)]

		inox = WHERE(nox_datestr EQ time_arr[tt], nox_count)
		nox_hr = [nox_hr, MEAN(nox_data.nox_hourly[inox],/NAN)]

		ico = WHERE(co_datestr EQ time_arr[tt], co_count)
		co_hr = [co_hr, MEAN(co_data.co_hourly[ico],/NAN)]

		itemp = WHERE(temp_datestr EQ time_arr[tt], temp_count)
		temp_hr = [temp_hr, MEAN(temp_data.temp_hourly[itemp],/NAN)]

		iwind_spd = WHERE(wind_spd_datestr EQ time_arr[tt], wind_spd_count)
		wind_spd_hr = [wind_spd_hr, MEAN(wind_spd_data.wind_spd_hourly[iwind_spd],/NAN)]

		iwind_dir = WHERE(wind_dir_datestr EQ time_arr[tt], wind_dir_count)
		wind_dir_hr = [wind_dir_hr, MEAN(wind_dir_data.wind_dir_hourly[iwind_dir],/NAN)]

;		ipress = WHERE(press_datestr EQ time_arr[tt], press_count)
;		press_hr = [press_hr, MEAN(press_data.press_hourly[ipress],/NAN)]
;
;		irh = WHERE(rh_datestr EQ time_arr[tt], rh_count)
;		rh_hr = [rh_hr, MEAN(rh_data.rh_hourly[irh],/NAN)]
;
;		idp = WHERE(dp_datestr EQ time_arr[tt], dp_count)
;		dp_hr = [dp_hr, MEAN(dp_data.dp_hourly[idp],/NAN)]
	ENDFOR
	
	o3_yr       = [o3_yr       ,o3_hr      ]	
;	voc_yr      = [voc_yr      ,voc_hr     ]
;	pm_yr		= [pm_yr       ,pm_hr	   ]
	nox_yr      = [nox_yr      ,nox_hr     ]
	co_yr		= [co_yr	   ,co_hr	   ]
	temp_yr     = [temp_yr     ,temp_hr    ]
	wind_spd_yr = [wind_spd_yr ,wind_spd_hr]
	wind_dir_yr = [wind_dir_yr ,wind_dir_hr]
;	press_yr    = [press_yr    ,press_hr   ]
;	rh_yr 		= [rh_yr       ,rh_hr      ]
;	dp_yr 		= [dp_yr       ,dp_hr      ]

ENDFOREACH	 	

outfile = 'hourly_data_071980_122017.nc'
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
NCDF_ATTPUT, oid, 'O3', 'long_name', 'O3 concentration 7/1980-12/2017'								;Name attribute
NCDF_ATTPUT, oid, 'O3', 'units',     'ppb'												;Units attribute

;vid = NCDF_VARDEF(oid, 'VOC', [tid], /FLOAT)												;Define the longitude variable
;NCDF_ATTPUT, oid, 'VOC', 'long_name', 'VOC concentration 7/1980-12/2017'								;Name attribute
;NCDF_ATTPUT, oid, 'VOC', 'units',     'ppb'												;Units attribute

;vid = NCDF_VARDEF(oid, 'PM2.5', [tid], /FLOAT)												;Define the latitude variable
;NCDF_ATTPUT, oid, 'PM2.5', 'long_name', 'PM 2.5 concentration 7/1980-12/2017'									;Name attribute
;NCDF_ATTPUT, oid, 'PM2.5', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'NOx', [tid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'NOx', 'long_name', 'NOx concentration 7/1980-12/2017'									;Name attribute
NCDF_ATTPUT, oid, 'NOx', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'CO', [tid], /FLOAT)												;Define the latitude variable
NCDF_ATTPUT, oid, 'CO', 'long_name', 'CO concentration 7/1980-12/2017'									;Name attribute
NCDF_ATTPUT, oid, 'CO', 'units',     'ppb'												;Units attribute

vid = NCDF_VARDEF(oid, 'Temperature', [tid], /FLOAT)													;Define the pressure variable
NCDF_ATTPUT, oid, 'Temperature', 'long_name', 'Temperature 7/1980-12/2017'														;Name attribute
NCDF_ATTPUT, oid, 'Temperature', 'units',     'degrees F'																	;Units attribute

vid = NCDF_VARDEF(oid, 'Wind_Speed', [tid], /FLOAT)													;Define the pressure variable
NCDF_ATTPUT, oid, 'Wind_Speed', 'long_name', 'Wind Speed 7/1980-12/2017'														;Name attribute
NCDF_ATTPUT, oid, 'Wind_Speed', 'units',     'Knots'																	;Units attribute

vid = NCDF_VARDEF(oid, 'Wind_Direction', [tid], /FLOAT)													;Define the geopotential height variable
NCDF_ATTPUT, oid, 'Wind_Direction', 'long_name', 'Wind Direction 7/1980-12/2017' 											;Name attribute
NCDF_ATTPUT, oid, 'Wind_Direction', 'units',     'Degrees Compass'																	;Units attribute

;vid = NCDF_VARDEF(oid, 'Pressure', [tid], /FLOAT)													;Define the geopotential height variable
;NCDF_ATTPUT, oid, 'Pressure', 'long_name', 'Pressure 7/1980-12/2017'											;Name attribute
;NCDF_ATTPUT, oid, 'Pressure', 'units',     'hPa'																	;Units attribute
;
;vid = NCDF_VARDEF(oid, 'RH', [tid], /FLOAT)												;Define the latitude variable
;NCDF_ATTPUT, oid, 'RH', 'long_name', 'Relative Humidity 7/1980-12/2017'									;Name attribute
;NCDF_ATTPUT, oid, 'RH', 'units',     '%'												;Units attribute
;
;vid = NCDF_VARDEF(oid, 'Dew_Point', [tid], /FLOAT)												;Define the latitude variable
;NCDF_ATTPUT, oid, 'Dew_Point', 'long_name', 'Dew Point Temperature 7/1980-12/2017'									;Name attribute
;NCDF_ATTPUT, oid, 'Dew_Point', 'units',     'degrees F'												;Units attribute

PRINT, 'done creating variables'
NCDF_CONTROL, oid, /ENDEF

PRINT, 'start assigning variables'
NCDF_VARPUT, oid, 'O3'				, o3_yr
;NCDF_VARPUT, oid, 'VOC'   			, voc_yr
;NCDF_VARPUT, oid, 'PM2.5'			, pm_yr
NCDF_VARPUT, oid, 'NOx' 			, nox_yr
NCDF_VARPUT, oid, 'CO' 				, co_yr
NCDF_VARPUT, oid, 'Temperature'  	, temp_yr
NCDF_VARPUT, oid, 'Wind_Speed'   	, wind_spd_yr
NCDF_VARPUT, oid, 'Wind_Direction'	, wind_dir_yr
;NCDF_VARPUT, oid, 'Pressure'		, press_yr
;NCDF_VARPUT, oid, 'RH'        		, rh_yr
;NCDF_VARPUT, oid, 'Dew_Point'   	, dp_yr

PRINT, 'done assigning variables'

NCDF_CLOSE, oid																									;Close output file
;NCDF_CLOSE, iid																									;Close input file

PRINT, 'File ' + outfile + ' processed.' 



STOP

END
