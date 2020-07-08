PRO PLOT_DFW_OBS_RELATIONSHIPS2, $
	PNG		  = png, $
	EPS		  = eps, $
	CLOBBER   = clobber

;+
;NAME:
;     PLOT_DFW_OBS_RELATIONSHIPS2
;PURPOSE:
;     This copies variables from ERA-Interim analysis into a single
;     file in pressure coordinates for use in TRAJ3D.
;     W at the surface is set to zero.  One pressure level is added at 
;     the top of the domain (p = 0), where w is also set to zero.
;CATEGORY:
;     Data handling utility.
;CALLING SEQUENCE:
;     PLOT_DFW_OBS_RELATIONSHIPS2, date0, outfile
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

o3_storm_july = []
o3dm_storm_july = []
voc_storm_july = []
nox_storm_july = []
no_storm_july = []
no2_storm_july = []
pm_storm_july = []
co_storm_july = []
temp_storm_july = []
wind_spd_storm_july = []
wind_dir_storm_july = []
press_storm_july = []
rh_storm_july = []
dp_storm_july = []

o3_nstorm_july = []
o3dm_nstorm_july = []
voc_nstorm_july = []
nox_nstorm_july = []
no_nstorm_july = []
no2_nstorm_july = []
pm_nstorm_july = []
co_nstorm_july = []
temp_nstorm_july = []
wind_spd_nstorm_july = []
wind_dir_nstorm_july = []
press_nstorm_july = []
rh_nstorm_july = []
dp_nstorm_july = []

infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_071980_082017_JULY_18Z_new_area.nc'																;Set input file path

;Get monthly averaged data
id  = NCDF_OPEN(infile)																						;Open input file for reading	

NCDF_VARGET, id, 'O3_storm'					, o3_storm
NCDF_VARGET, id, 'O3DM_storm'				, o3dm_storm
NCDF_VARGET, id, 'NOx_storm' 				, nox_storm
NCDF_VARGET, id, 'NO_storm' 				, no_storm
NCDF_VARGET, id, 'NO2_storm' 				, no2_storm
NCDF_VARGET, id, 'CO_storm' 				, co_storm
NCDF_VARGET, id, 'Temperature_storm'  		, temp_storm
NCDF_VARGET, id, 'Wind_Speed_storm'   		, wind_spd_storm
NCDF_VARGET, id, 'Wind_Direction_storm'		, wind_dir_storm

NCDF_VARGET, id, 'O3_nstorm'				, o3_nstorm
NCDF_VARGET, id, 'O3DM_nstorm'				, o3dm_nstorm
NCDF_VARGET, id, 'NOx_nstorm' 				, nox_nstorm
NCDF_VARGET, id, 'NO_nstorm' 				, no_nstorm
NCDF_VARGET, id, 'NO2_nstorm' 				, no2_nstorm
NCDF_VARGET, id, 'CO_nstorm' 				, co_nstorm
NCDF_VARGET, id, 'Temperature_nstorm'  		, temp_nstorm
NCDF_VARGET, id, 'Wind_Speed_nstorm'   		, wind_spd_nstorm
NCDF_VARGET, id, 'Wind_Direction_nstorm'	, wind_dir_nstorm
NCDF_CLOSE, id

o3_storm_july = [o3_storm_july, o3_storm]
o3dm_storm_july = [o3dm_storm_july, o3dm_storm]
nox_storm_july = [nox_storm_july, nox_storm]
no_storm_july = [no_storm_july, no_storm]
no2_storm_july = [no2_storm_july, no2_storm]
co_storm_july = [co_storm_july, co_storm]
temp_storm_july = [temp_storm_july, temp_storm]
wind_spd_storm_july = [wind_spd_storm_july, wind_spd_storm]
wind_dir_storm_july = [wind_dir_storm_july, wind_dir_storm]

o3_nstorm_july = [o3_nstorm_july, o3_nstorm]
o3dm_nstorm_july = [o3dm_nstorm_july, o3dm_nstorm]
nox_nstorm_july = [nox_nstorm_july, nox_nstorm]
no_nstorm_july  = [no_nstorm_july, no_nstorm]
no2_nstorm_july = [no2_nstorm_july, no2_nstorm]
co_nstorm_july = [co_nstorm_july, co_nstorm]
temp_nstorm_july = [temp_nstorm_july, temp_nstorm]
wind_spd_nstorm_july = [wind_spd_nstorm_july, wind_spd_nstorm]
wind_dir_nstorm_july = [wind_dir_nstorm_july, wind_dir_nstorm]

;infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_071996_082000_JUL_18Z.nc'																;Set input file path
;
;;Get monthly averaged data
;id  = NCDF_OPEN(infile)																						;Open input file for reading	
;
;NCDF_VARGET, id, 'O3_storm'					, o3_storm
;;NCDF_VARGET, id, 'O3DM_storm'				, o3dm_storm
;NCDF_VARGET, id, 'VOC_storm'   				, voc_storm
;NCDF_VARGET, id, 'NOx_storm' 				, nox_storm
;NCDF_VARGET, id, 'NO_storm' 				, no_storm
;NCDF_VARGET, id, 'NO2_storm' 				, no2_storm
;NCDF_VARGET, id, 'CO_storm' 				, co_storm
;NCDF_VARGET, id, 'Temperature_storm'  		, temp_storm
;NCDF_VARGET, id, 'Wind_Speed_storm'   		, wind_spd_storm
;NCDF_VARGET, id, 'Wind_Direction_storm'		, wind_dir_storm
;
;NCDF_VARGET, id, 'O3_nstorm'				, o3_nstorm
;;NCDF_VARGET, id, 'O3DM_nstorm'				, o3dm_nstorm
;NCDF_VARGET, id, 'VOC_nstorm'   			, voc_nstorm
;NCDF_VARGET, id, 'NOx_nstorm' 				, nox_nstorm
;NCDF_VARGET, id, 'NO_nstorm' 				, no_nstorm
;NCDF_VARGET, id, 'NO2_nstorm' 				, no2_nstorm
;NCDF_VARGET, id, 'CO_nstorm' 				, co_nstorm
;NCDF_VARGET, id, 'Temperature_nstorm'  		, temp_nstorm
;NCDF_VARGET, id, 'Wind_Speed_nstorm'   		, wind_spd_nstorm
;NCDF_VARGET, id, 'Wind_Direction_nstorm'	, wind_dir_nstorm
;NCDF_CLOSE, id
;
;o3_storm_july = [o3_storm_july, o3_storm]
;;o3dm_storm_july = [o3dm_storm_july, o3dm_storm]
;voc_storm_july = [voc_storm_july, voc_storm]
;nox_storm_july = [nox_storm_july, nox_storm]
;no_storm_july = [no_storm_july, no_storm]
;no2_storm_july = [no2_storm_july, no2_storm]
;co_storm_july = [co_storm_july, co_storm]
;temp_storm_july = [temp_storm_july, temp_storm]
;wind_spd_storm_july = [wind_spd_storm_july, wind_spd_storm]
;wind_dir_storm_july = [wind_dir_storm_july, wind_dir_storm]
;
;o3_nstorm_july = [o3_nstorm_july, o3_nstorm]
;voc_nstorm_july = [voc_nstorm_july, voc_nstorm]
;nox_nstorm_july = [nox_nstorm_july, nox_nstorm]
;no_nstorm_july  = [no_nstorm_july, no_nstorm]
;no2_nstorm_july = [no2_nstorm_july, no2_nstorm]
;co_nstorm_july = [co_nstorm_july, co_nstorm]
;temp_nstorm_july = [temp_nstorm_july, temp_nstorm]
;wind_spd_nstorm_july = [wind_spd_nstorm_july, wind_spd_nstorm]
;wind_dir_nstorm_july = [wind_dir_nstorm_july, wind_dir_nstorm]
;
;
;infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_072001_082004_JUL_18Z.nc'																;Set input file path
;
;;Get monthly averaged data
;id  = NCDF_OPEN(infile)																						;Open input file for reading	
;
;NCDF_VARGET, id, 'O3_storm'					, o3_storm
;;NCDF_VARGET, id, 'O3DM_storm'				, o3dm_storm
;NCDF_VARGET, id, 'VOC_storm'   				, voc_storm
;NCDF_VARGET, id, 'NOx_storm' 				, nox_storm
;NCDF_VARGET, id, 'NO_storm' 				, no_storm
;NCDF_VARGET, id, 'NO2_storm' 				, no2_storm
;NCDF_VARGET, id, 'CO_storm' 				, co_storm
;NCDF_VARGET, id, 'Temperature_storm'  		, temp_storm
;NCDF_VARGET, id, 'Wind_Speed_storm'   		, wind_spd_storm
;NCDF_VARGET, id, 'Wind_Direction_storm'		, wind_dir_storm
;NCDF_VARGET, id, 'Pressure_storm'			, press_storm
;NCDF_VARGET, id, 'RH_storm'        			, rh_storm
;NCDF_VARGET, id, 'Dew_Point_storm'   		, dp_storm
;
;NCDF_VARGET, id, 'O3_nstorm'				, o3_nstorm
;;NCDF_VARGET, id, 'O3DM_nstorm'				, o3dm_nstorm
;NCDF_VARGET, id, 'VOC_nstorm'   			, voc_nstorm
;NCDF_VARGET, id, 'NOx_nstorm' 				, nox_nstorm
;NCDF_VARGET, id, 'NO_nstorm' 				, no_nstorm
;NCDF_VARGET, id, 'NO2_nstorm' 				, no2_nstorm
;NCDF_VARGET, id, 'CO_nstorm' 				, co_nstorm
;NCDF_VARGET, id, 'Temperature_nstorm'  		, temp_nstorm
;NCDF_VARGET, id, 'Wind_Speed_nstorm'   		, wind_spd_nstorm
;NCDF_VARGET, id, 'Wind_Direction_nstorm'	, wind_dir_nstorm
;NCDF_VARGET, id, 'Pressure_nstorm'			, press_nstorm
;NCDF_VARGET, id, 'RH_nstorm'        		, rh_nstorm
;NCDF_VARGET, id, 'Dew_Point_nstorm'   		, dp_nstorm
;NCDF_CLOSE, id
;
;o3_storm_july = [o3_storm_july, o3_storm]
;;o3dm_storm_july = [o3dm_storm_july, o3dm_storm]
;voc_storm_july = [voc_storm_july, voc_storm]
;nox_storm_july = [nox_storm_july, nox_storm]
;no_storm_july = [no_storm_july, no_storm]
;no2_storm_july = [no2_storm_july, no2_storm]
;co_storm_july = [co_storm_july, co_storm]
;temp_storm_july = [temp_storm_july, temp_storm]
;wind_spd_storm_july = [wind_spd_storm_july, wind_spd_storm]
;wind_dir_storm_july = [wind_dir_storm_july, wind_dir_storm]
;press_storm_july = [press_storm_july, press_storm]
;rh_storm_july = [rh_storm_july, rh_storm]
;dp_storm_july = [dp_storm_july, dp_storm]
;
;o3_nstorm_july = [o3_nstorm_july, o3_nstorm]
;;o3dm_nstorm_july = [o3dm_nstorm_july, o3dm_nstorm]
;voc_nstorm_july = [voc_nstorm_july, voc_nstorm]
;nox_nstorm_july = [nox_nstorm_july, nox_nstorm]
;no_nstorm_july  = [no_nstorm_july, no_nstorm]
;no2_nstorm_july = [no2_nstorm_july, no2_nstorm]
;co_nstorm_july = [co_nstorm_july, co_nstorm]
;temp_nstorm_july = [temp_nstorm_july, temp_nstorm]
;wind_spd_nstorm_july = [wind_spd_nstorm_july, wind_spd_nstorm]
;wind_dir_nstorm_july = [wind_dir_nstorm_july, wind_dir_nstorm]
;press_nstorm_july = [press_nstorm_july, press_nstorm]
;rh_nstorm_july = [rh_nstorm_july, rh_nstorm]
;dp_nstorm_july = [dp_nstorm_july, dp_nstorm]
;
;
;infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_072005_082017_JUL_18Z.nc'																;Set input file path
;
;;Get monthly averaged data
;id  = NCDF_OPEN(infile)																						;Open input file for reading	
;
;;NCDF_VARGET, id, 'O3_anomaly'				, o3_array_july
;;NCDF_VARGET, id, 'VOC_anomaly'   			, voc_array_july
;;NCDF_VARGET, id, 'PM2.5_anomaly'			, pm_array_july
;;NCDF_VARGET, id, 'NOx_anomaly' 				, nox_array_july
;;NCDF_VARGET, id, 'CO_anomaly' 				, co_array_july
;;NCDF_VARGET, id, 'Temperature_anomaly'  	, temp_array_july
;;NCDF_VARGET, id, 'Wind_Speed_anomaly'   	, wind_spd_array_july
;;NCDF_VARGET, id, 'Wind_Direction_anomaly'	, wind_dir_array_july
;;NCDF_VARGET, id, 'Pressure_anomaly'			, press_array_july
;;NCDF_VARGET, id, 'RH_anomaly'        		, rh_array_july
;;NCDF_VARGET, id, 'Dew_Point_anomaly'   		, dp_array_july
;;
;;NCDF_VARGET, id, 'O3_nanomaly'				, o3_narray_july
;;NCDF_VARGET, id, 'VOC_nanomaly'   			, voc_narray_july
;;NCDF_VARGET, id, 'PM2.5_nanomaly'			, pm_narray_july
;;NCDF_VARGET, id, 'NOx_nanomaly' 			, nox_narray_july
;;NCDF_VARGET, id, 'CO_nanomaly' 				, co_narray_july
;;NCDF_VARGET, id, 'Temperature_nanomaly'  	, temp_narray_july
;;NCDF_VARGET, id, 'Wind_Speed_nanomaly'   	, wind_spd_narray_july
;;NCDF_VARGET, id, 'Wind_Direction_nanomaly'	, wind_dir_narray_july
;;NCDF_VARGET, id, 'Pressure_nanomaly'		, press_narray_july
;;NCDF_VARGET, id, 'RH_nanomaly'        		, rh_narray_july
;;NCDF_VARGET, id, 'Dew_Point_nanomaly'   	, dp_narray_july
;
;NCDF_VARGET, id, 'O3_storm'					, o3_storm
;;NCDF_VARGET, id, 'O3DM_storm'				, o3dm_storm
;NCDF_VARGET, id, 'VOC_storm'   				, voc_storm
;NCDF_VARGET, id, 'PM2.5_storm'				, pm_storm
;NCDF_VARGET, id, 'NOx_storm' 				, nox_storm
;NCDF_VARGET, id, 'NO_storm' 				, no_storm
;NCDF_VARGET, id, 'NO2_storm' 				, no2_storm
;NCDF_VARGET, id, 'CO_storm' 				, co_storm
;NCDF_VARGET, id, 'Temperature_storm'  		, temp_storm
;NCDF_VARGET, id, 'Wind_Speed_storm'   		, wind_spd_storm
;NCDF_VARGET, id, 'Wind_Direction_storm'		, wind_dir_storm
;NCDF_VARGET, id, 'Pressure_storm'			, press_storm
;NCDF_VARGET, id, 'RH_storm'        			, rh_storm
;NCDF_VARGET, id, 'Dew_Point_storm'   		, dp_storm
;
;NCDF_VARGET, id, 'O3_nstorm'				, o3_nstorm
;;NCDF_VARGET, id, 'O3DM_nstorm'				, o3dm_nstorm
;NCDF_VARGET, id, 'VOC_nstorm'   			, voc_nstorm
;NCDF_VARGET, id, 'PM2.5_nstorm'				, pm_nstorm
;NCDF_VARGET, id, 'NOx_nstorm' 				, nox_nstorm
;NCDF_VARGET, id, 'NO_nstorm' 				, no_nstorm
;NCDF_VARGET, id, 'NO2_nstorm' 				, no2_nstorm
;NCDF_VARGET, id, 'CO_nstorm' 				, co_nstorm
;NCDF_VARGET, id, 'Temperature_nstorm'  		, temp_nstorm
;NCDF_VARGET, id, 'Wind_Speed_nstorm'   		, wind_spd_nstorm
;NCDF_VARGET, id, 'Wind_Direction_nstorm'	, wind_dir_nstorm
;NCDF_VARGET, id, 'Pressure_nstorm'			, press_nstorm
;NCDF_VARGET, id, 'RH_nstorm'        		, rh_nstorm
;NCDF_VARGET, id, 'Dew_Point_nstorm'   		, dp_nstorm
;NCDF_CLOSE, id
;
;o3_storm_july = [o3_storm_july, o3_storm]
;;o3dm_storm_july = [o3dm_storm_july, o3dm_storm]
;voc_storm_july = [voc_storm_july, voc_storm]
;nox_storm_july = [nox_storm_july, nox_storm]
;no_storm_july = [no_storm_july, no_storm]
;no2_storm_july = [no2_storm_july, no2_storm]
;pm_storm_july = [pm_storm_july, pm_storm]
;co_storm_july = [co_storm_july, co_storm]
;temp_storm_july = [temp_storm_july, temp_storm]
;wind_spd_storm_july = [wind_spd_storm_july, wind_spd_storm]
;wind_dir_storm_july = [wind_dir_storm_july, wind_dir_storm]
;press_storm_july = [press_storm_july, press_storm]
;rh_storm_july = [rh_storm_july, rh_storm]
;dp_storm_july = [dp_storm_july, dp_storm]
;
;o3_nstorm_july = [o3_nstorm_july, o3_nstorm]
;;o3dm_nstorm_july = [o3dm_nstorm_july, o3dm_nstorm]
;voc_nstorm_july = [voc_nstorm_july, voc_nstorm]
;nox_nstorm_july = [nox_nstorm_july, nox_nstorm]
;no_nstorm_july  = [no_nstorm_july, no_nstorm]
;no2_nstorm_july = [no2_nstorm_july, no2_nstorm]
;pm_nstorm_july = [pm_nstorm_july, pm_nstorm]
;co_nstorm_july = [co_nstorm_july, co_nstorm]
;temp_nstorm_july = [temp_nstorm_july, temp_nstorm]
;wind_spd_nstorm_july = [wind_spd_nstorm_july, wind_spd_nstorm]
;wind_dir_nstorm_july = [wind_dir_nstorm_july, wind_dir_nstorm]
;press_nstorm_july = [press_nstorm_july, press_nstorm]
;rh_nstorm_july = [rh_nstorm_july, rh_nstorm]
;dp_nstorm_july = [dp_nstorm_july, dp_nstorm]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

o3_storm_oct = []
o3dm_storm_oct = []
voc_storm_oct = []
nox_storm_oct = []
no_storm_oct = []
no2_storm_oct = []
pm_storm_oct = []
co_storm_oct = []
temp_storm_oct = []
wind_spd_storm_oct = []
wind_dir_storm_oct = []
press_storm_oct = []
rh_storm_oct = []
dp_storm_oct = []

o3_nstorm_oct = []
o3dm_nstorm_oct = []
voc_nstorm_oct = []
nox_nstorm_oct = []
no_nstorm_oct = []
no2_nstorm_oct = []
pm_nstorm_oct = []
co_nstorm_oct = []
temp_nstorm_oct = []
wind_spd_nstorm_oct = []
wind_dir_nstorm_oct = []
press_nstorm_oct = []
rh_nstorm_oct = []
dp_nstorm_oct = []

infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_101980_112017_OCT_18Z_new_area.nc'																;Set input file path

;Get monthly averaged data
id  = NCDF_OPEN(infile)																						;Open input file for reading	

NCDF_VARGET, id, 'O3_storm'					, o3_storm
NCDF_VARGET, id, 'O3DM_storm'				, o3dm_storm
NCDF_VARGET, id, 'NOx_storm' 				, nox_storm
NCDF_VARGET, id, 'NO_storm' 				, no_storm
NCDF_VARGET, id, 'NO2_storm' 				, no2_storm
NCDF_VARGET, id, 'CO_storm' 				, co_storm
NCDF_VARGET, id, 'Temperature_storm'  		, temp_storm
NCDF_VARGET, id, 'Wind_Speed_storm'   		, wind_spd_storm
NCDF_VARGET, id, 'Wind_Direction_storm'		, wind_dir_storm

NCDF_VARGET, id, 'O3_nstorm'				, o3_nstorm
;NCDF_VARGET, id, 'O3DM_nstorm'				, o3dm_nstorm
NCDF_VARGET, id, 'NOx_nstorm' 				, nox_nstorm
NCDF_VARGET, id, 'NO_nstorm' 				, no_nstorm
NCDF_VARGET, id, 'NO2_nstorm' 				, no2_nstorm
NCDF_VARGET, id, 'CO_nstorm' 				, co_nstorm
NCDF_VARGET, id, 'Temperature_nstorm'  		, temp_nstorm
NCDF_VARGET, id, 'Wind_Speed_nstorm'   		, wind_spd_nstorm
NCDF_VARGET, id, 'Wind_Direction_nstorm'	, wind_dir_nstorm
NCDF_CLOSE, id

o3_storm_oct = [o3_storm_oct, o3_storm]
o3dm_storm_oct = [o3dm_storm_oct, o3dm_storm]
nox_storm_oct = [nox_storm_oct, nox_storm]
no_storm_oct = [no_storm_oct, no_storm]
no2_storm_oct = [no2_storm_oct, no2_storm]
co_storm_oct = [co_storm_oct, co_storm]
temp_storm_oct = [temp_storm_oct, temp_storm]
wind_spd_storm_oct = [wind_spd_storm_oct, wind_spd_storm]
wind_dir_storm_oct = [wind_dir_storm_oct, wind_dir_storm]

o3_nstorm_oct = [o3_nstorm_oct, o3_nstorm]
o3dm_nstorm_oct = [o3dm_nstorm_oct, o3dm_nstorm]
nox_nstorm_oct = [nox_nstorm_oct, nox_nstorm]
no_nstorm_oct = [no_nstorm_oct, no_nstorm]
no2_nstorm_oct = [no2_nstorm_oct, no2_nstorm]
co_nstorm_oct = [co_nstorm_oct, co_nstorm]
temp_nstorm_oct = [temp_nstorm_oct, temp_nstorm]
wind_spd_nstorm_oct = [wind_spd_nstorm_oct, wind_spd_nstorm]
wind_dir_nstorm_oct = [wind_dir_nstorm_oct, wind_dir_nstorm]

;infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_101996_112000_OCT_18Z.nc'																;Set input file path
;
;;Get monthly averaged data
;id  = NCDF_OPEN(infile)																						;Open input file for reading	
;
;NCDF_VARGET, id, 'O3_storm'					, o3_storm
;;NCDF_VARGET, id, 'O3DM_storm'				, o3dm_storm
;NCDF_VARGET, id, 'VOC_storm'   				, voc_storm
;NCDF_VARGET, id, 'NOx_storm' 				, nox_storm
;NCDF_VARGET, id, 'NO_storm' 				, no_storm
;NCDF_VARGET, id, 'NO2_storm' 				, no2_storm
;NCDF_VARGET, id, 'CO_storm' 				, co_storm
;NCDF_VARGET, id, 'Temperature_storm'  		, temp_storm
;NCDF_VARGET, id, 'Wind_Speed_storm'   		, wind_spd_storm
;NCDF_VARGET, id, 'Wind_Direction_storm'		, wind_dir_storm
;
;NCDF_VARGET, id, 'O3_nstorm'				, o3_nstorm
;;NCDF_VARGET, id, 'O3DM_nstorm'				, o3dm_nstorm
;NCDF_VARGET, id, 'VOC_nstorm'   			, voc_nstorm
;NCDF_VARGET, id, 'NOx_nstorm' 				, nox_nstorm
;NCDF_VARGET, id, 'NO_nstorm' 				, no_nstorm
;NCDF_VARGET, id, 'NO2_nstorm' 				, no2_nstorm
;NCDF_VARGET, id, 'CO_nstorm' 				, co_nstorm
;NCDF_VARGET, id, 'Temperature_nstorm'  		, temp_nstorm
;NCDF_VARGET, id, 'Wind_Speed_nstorm'   		, wind_spd_nstorm
;NCDF_VARGET, id, 'Wind_Direction_nstorm'	, wind_dir_nstorm
;NCDF_CLOSE, id
;
;o3_storm_oct = [o3_storm_oct, o3_storm]
;;o3dm_storm_oct = [o3dm_storm_oct, o3dm_storm]
;voc_storm_oct = [voc_storm_oct, voc_storm]
;nox_storm_oct = [nox_storm_oct, nox_storm]
;no_storm_oct = [no_storm_oct, no_storm]
;no2_storm_oct = [no2_storm_oct, no2_storm]
;co_storm_oct = [co_storm_oct, co_storm]
;temp_storm_oct = [temp_storm_oct, temp_storm]
;wind_spd_storm_oct = [wind_spd_storm_oct, wind_spd_storm]
;wind_dir_storm_oct = [wind_dir_storm_oct, wind_dir_storm]
;
;o3_nstorm_oct = [o3_nstorm_oct, o3_nstorm]
;;o3dm_nstorm_oct = [o3dm_nstorm_oct, o3dm_nstorm]
;voc_nstorm_oct = [voc_nstorm_oct, voc_nstorm]
;nox_nstorm_oct = [nox_nstorm_oct, nox_nstorm]
;no_nstorm_oct = [no_nstorm_oct, no_nstorm]
;no2_nstorm_oct = [no2_nstorm_oct, no2_nstorm]
;co_nstorm_oct = [co_nstorm_oct, co_nstorm]
;temp_nstorm_oct = [temp_nstorm_oct, temp_nstorm]
;wind_spd_nstorm_oct = [wind_spd_nstorm_oct, wind_spd_nstorm]
;wind_dir_nstorm_oct = [wind_dir_nstorm_oct, wind_dir_nstorm]
;
;
;infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_102001_112004_OCT_18Z.nc'																;Set input file path
;
;;Get monthly averaged data
;id  = NCDF_OPEN(infile)																						;Open input file for reading	
;
;NCDF_VARGET, id, 'O3_storm'					, o3_storm
;;NCDF_VARGET, id, 'O3DM_storm'				, o3dm_storm
;NCDF_VARGET, id, 'VOC_storm'   				, voc_storm
;NCDF_VARGET, id, 'NOx_storm' 				, nox_storm
;NCDF_VARGET, id, 'NO_storm' 				, no_storm
;NCDF_VARGET, id, 'NO2_storm' 				, no2_storm
;NCDF_VARGET, id, 'CO_storm' 				, co_storm
;NCDF_VARGET, id, 'Temperature_storm'  		, temp_storm
;NCDF_VARGET, id, 'Wind_Speed_storm'   		, wind_spd_storm
;NCDF_VARGET, id, 'Wind_Direction_storm'		, wind_dir_storm
;NCDF_VARGET, id, 'Pressure_storm'			, press_storm
;NCDF_VARGET, id, 'RH_storm'        			, rh_storm
;NCDF_VARGET, id, 'Dew_Point_storm'   		, dp_storm
;
;NCDF_VARGET, id, 'O3_nstorm'				, o3_nstorm
;;NCDF_VARGET, id, 'O3DM_nstorm'				, o3dm_nstorm
;NCDF_VARGET, id, 'VOC_nstorm'   			, voc_nstorm
;NCDF_VARGET, id, 'NOx_nstorm' 				, nox_nstorm
;NCDF_VARGET, id, 'NO_nstorm' 				, no_nstorm
;NCDF_VARGET, id, 'NO2_nstorm' 				, no2_nstorm
;NCDF_VARGET, id, 'CO_nstorm' 				, co_nstorm
;NCDF_VARGET, id, 'Temperature_nstorm'  		, temp_nstorm
;NCDF_VARGET, id, 'Wind_Speed_nstorm'   		, wind_spd_nstorm
;NCDF_VARGET, id, 'Wind_Direction_nstorm'	, wind_dir_nstorm
;NCDF_VARGET, id, 'Pressure_nstorm'			, press_nstorm
;NCDF_VARGET, id, 'RH_nstorm'        		, rh_nstorm
;NCDF_VARGET, id, 'Dew_Point_nstorm'   		, dp_nstorm
;NCDF_CLOSE, id
;
;o3_storm_oct = [o3_storm_oct, o3_storm]
;;o3dm_storm_oct = [o3dm_storm_oct, o3dm_storm]
;voc_storm_oct = [voc_storm_oct, voc_storm]
;nox_storm_oct = [nox_storm_oct, nox_storm]
;no_storm_oct = [no_storm_oct, no_storm]
;no2_storm_oct = [no2_storm_oct, no2_storm]
;co_storm_oct = [co_storm_oct, co_storm]
;temp_storm_oct = [temp_storm_oct, temp_storm]
;wind_spd_storm_oct = [wind_spd_storm_oct, wind_spd_storm]
;wind_dir_storm_oct = [wind_dir_storm_oct, wind_dir_storm]
;press_storm_oct = [press_storm_oct, press_storm]
;rh_storm_oct = [rh_storm_oct, rh_storm]
;dp_storm_oct = [dp_storm_oct, dp_storm]
;
;o3_nstorm_oct = [o3_nstorm_oct, o3_nstorm]
;;o3dm_nstorm_oct = [o3dm_nstorm_oct, o3dm_nstorm]
;voc_nstorm_oct = [voc_nstorm_oct, voc_nstorm]
;nox_nstorm_oct = [nox_nstorm_oct, nox_nstorm]
;no_nstorm_oct = [no_nstorm_oct, no_nstorm]
;no2_nstorm_oct = [no2_nstorm_oct, no2_nstorm]
;co_nstorm_oct = [co_nstorm_oct, co_nstorm]
;temp_nstorm_oct = [temp_nstorm_oct, temp_nstorm]
;wind_spd_nstorm_oct = [wind_spd_nstorm_oct, wind_spd_nstorm]
;wind_dir_nstorm_oct = [wind_dir_nstorm_oct, wind_dir_nstorm]
;press_nstorm_oct = [press_nstorm_oct, press_nstorm]
;rh_nstorm_oct = [rh_nstorm_oct, rh_nstorm]
;dp_nstorm_oct = [dp_nstorm_oct, dp_nstorm]
;
;
;infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_102005_112017_OCT_18Z.nc'																;Set input file path
;
;;Get monthly averaged data
;id  = NCDF_OPEN(infile)																						;Open input file for reading	
;
;;NCDF_VARGET, id, 'O3_anomaly'				, o3_array_oct
;;NCDF_VARGET, id, 'VOC_anomaly'   			, voc_array_oct
;;NCDF_VARGET, id, 'PM2.5_anomaly'			, pm_array_oct
;;NCDF_VARGET, id, 'NOx_anomaly' 				, nox_array_oct
;;NCDF_VARGET, id, 'CO_anomaly' 				, co_array_oct
;;NCDF_VARGET, id, 'Temperature_anomaly'  	, temp_array_oct
;;NCDF_VARGET, id, 'Wind_Speed_anomaly'   	, wind_spd_array_oct
;;NCDF_VARGET, id, 'Wind_Direction_anomaly'	, wind_dir_array_oct
;;NCDF_VARGET, id, 'Pressure_anomaly'			, press_array_oct
;;NCDF_VARGET, id, 'RH_anomaly'        		, rh_array_oct
;;NCDF_VARGET, id, 'Dew_Point_anomaly'   		, dp_array_oct
;;
;;NCDF_VARGET, id, 'O3_nanomaly'				, o3_narray_oct
;;NCDF_VARGET, id, 'VOC_nanomaly'   			, voc_narray_oct
;;NCDF_VARGET, id, 'PM2.5_nanomaly'			, pm_narray_oct
;;NCDF_VARGET, id, 'NOx_nanomaly' 			, nox_narray_oct
;;NCDF_VARGET, id, 'CO_nanomaly' 				, co_narray_oct
;;NCDF_VARGET, id, 'Temperature_nanomaly'  	, temp_narray_oct
;;NCDF_VARGET, id, 'Wind_Speed_nanomaly'   	, wind_spd_narray_oct
;;NCDF_VARGET, id, 'Wind_Direction_nanomaly'	, wind_dir_narray_oct
;;NCDF_VARGET, id, 'Pressure_nanomaly'		, press_narray_oct
;;NCDF_VARGET, id, 'RH_nanomaly'        		, rh_narray_oct
;;NCDF_VARGET, id, 'Dew_Point_nanomaly'   	, dp_narray_oct
;
;NCDF_VARGET, id, 'O3_storm'					, o3_storm
;;NCDF_VARGET, id, 'O3DM_storm'				, o3dm_storm
;NCDF_VARGET, id, 'VOC_storm'   				, voc_storm
;NCDF_VARGET, id, 'PM2.5_storm'				, pm_storm
;NCDF_VARGET, id, 'NOx_storm' 				, nox_storm
;NCDF_VARGET, id, 'NO_storm' 				, no_storm
;NCDF_VARGET, id, 'NO2_storm' 				, no2_storm
;NCDF_VARGET, id, 'CO_storm' 				, co_storm
;NCDF_VARGET, id, 'Temperature_storm'  		, temp_storm
;NCDF_VARGET, id, 'Wind_Speed_storm'   		, wind_spd_storm
;NCDF_VARGET, id, 'Wind_Direction_storm'		, wind_dir_storm
;NCDF_VARGET, id, 'Pressure_storm'			, press_storm
;NCDF_VARGET, id, 'RH_storm'        			, rh_storm
;NCDF_VARGET, id, 'Dew_Point_storm'   		, dp_storm
;
;NCDF_VARGET, id, 'O3_nstorm'				, o3_nstorm
;;NCDF_VARGET, id, 'O3DM_nstorm'				, o3dm_nstorm
;NCDF_VARGET, id, 'VOC_nstorm'   			, voc_nstorm
;NCDF_VARGET, id, 'PM2.5_nstorm'				, pm_nstorm
;NCDF_VARGET, id, 'NOx_nstorm' 				, nox_nstorm
;NCDF_VARGET, id, 'NO_nstorm' 				, no_nstorm
;NCDF_VARGET, id, 'NO2_nstorm' 				, no2_nstorm
;NCDF_VARGET, id, 'CO_nstorm' 				, co_nstorm
;NCDF_VARGET, id, 'Temperature_nstorm'  		, temp_nstorm
;NCDF_VARGET, id, 'Wind_Speed_nstorm'   		, wind_spd_nstorm
;NCDF_VARGET, id, 'Wind_Direction_nstorm'	, wind_dir_nstorm
;NCDF_VARGET, id, 'Pressure_nstorm'			, press_nstorm
;NCDF_VARGET, id, 'RH_nstorm'        		, rh_nstorm
;NCDF_VARGET, id, 'Dew_Point_nstorm'   		, dp_nstorm
;NCDF_CLOSE, id
;
;o3_storm_oct = [o3_storm_oct, o3_storm]
;;o3dm_storm_oct = [o3dm_storm_oct, o3dm_storm]
;nox_storm_oct = [nox_storm_oct, nox_storm]
;no_storm_oct = [no_storm_oct, no_storm]
;no2_storm_oct = [no2_storm_oct, no2_storm]
;pm_storm_oct = [pm_storm_oct, pm_storm]
;co_storm_oct = [co_storm_oct, co_storm]
;temp_storm_oct = [temp_storm_oct, temp_storm]
;wind_spd_storm_oct = [wind_spd_storm_oct, wind_spd_storm]
;wind_dir_storm_oct = [wind_dir_storm_oct, wind_dir_storm]
;press_storm_oct = [press_storm_oct, press_storm]
;rh_storm_oct = [rh_storm_oct, rh_storm]
;dp_storm_oct = [dp_storm_oct, dp_storm]
;
;o3_nstorm_oct = [o3_nstorm_oct, o3_nstorm]
;;o3dm_nstorm_oct = [o3dm_nstorm_oct, o3dm_nstorm]
;voc_nstorm_oct = [voc_nstorm_oct, voc_nstorm]
;nox_nstorm_oct = [nox_nstorm_oct, nox_nstorm]
;no_nstorm_oct = [no_nstorm_oct, no_nstorm]
;no2_nstorm_oct = [no2_nstorm_oct, no2_nstorm]
;pm_nstorm_oct = [pm_nstorm_oct, pm_nstorm]
;co_nstorm_oct = [co_nstorm_oct, co_nstorm]
;temp_nstorm_oct = [temp_nstorm_oct, temp_nstorm]
;wind_spd_nstorm_oct = [wind_spd_nstorm_oct, wind_spd_nstorm]
;wind_dir_nstorm_oct = [wind_dir_nstorm_oct, wind_dir_nstorm]
;press_nstorm_oct = [press_nstorm_oct, press_nstorm]
;rh_nstorm_oct = [rh_nstorm_oct, rh_nstorm]
;dp_nstorm_oct = [dp_nstorm_oct, dp_nstorm]
	 	
fname = '/data3/dphoenix/wrf/general/met_chem_boxplots/jul_oct_mean_18Z_new_area.txt'
OPENW, lun, fname, /GET_LUN     

PRINTF, lun, FORMAT = '("  Variable", 5X, "oct Storm", 4X, "oct No Storm", 4X, "july Storm", 4X, "july No Storm")'								;Print table header information
PRINTF, lun, '==================================================================='
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'O3', MEAN(o3_storm_oct), MEAN(o3_nstorm_oct), MEAN(o3_storm_july), MEAN(o3_nstorm_july)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'o3dm', MEAN(o3dm_storm_oct), MEAN(o3dm_nstorm_oct), MEAN(o3dm_storm_july), MEAN(o3dm_nstorm_july)		
;PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'voc', MEAN(voc_storm_oct), MEAN(voc_nstorm_oct), MEAN(voc_storm_july), MEAN(voc_nstorm_july)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'nox', MEAN(nox_storm_oct), MEAN(nox_nstorm_oct), MEAN(nox_storm_july), MEAN(nox_nstorm_july)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'no', MEAN(no_storm_oct), MEAN(no_nstorm_oct), MEAN(no_storm_july), MEAN(no_nstorm_july)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'no2', MEAN(no2_storm_oct), MEAN(no2_nstorm_oct), MEAN(no2_storm_july), MEAN(no2_nstorm_july)		
;PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'pm', MEAN(pm_storm_oct), MEAN(pm_nstorm_oct), MEAN(pm_storm_july), MEAN(pm_nstorm_july)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'co', MEAN(co_storm_oct), MEAN(co_nstorm_oct), MEAN(co_storm_july), MEAN(co_nstorm_july)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'temp', MEAN(temp_storm_oct), MEAN(temp_nstorm_oct), MEAN(temp_storm_july), MEAN(temp_nstorm_july)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'wind_spd', MEAN(wind_spd_storm_oct), MEAN(wind_spd_nstorm_oct), MEAN(wind_spd_storm_july), MEAN(wind_spd_nstorm_july)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'wind_dir', MEAN(wind_dir_storm_oct), MEAN(wind_dir_nstorm_oct), MEAN(wind_dir_storm_july), MEAN(wind_dir_nstorm_july)		
;PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'press', MEAN(press_storm_oct), MEAN(press_nstorm_oct), MEAN(press_storm_july), MEAN(press_nstorm_july)		
;PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'rh', MEAN(rh_storm_oct), MEAN(rh_nstorm_oct), MEAN(rh_storm_july), MEAN(rh_nstorm_july)		
;PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'dp', MEAN(dp_storm_oct), MEAN(dp_nstorm_oct), MEAN(dp_storm_july), MEAN(dp_nstorm_july)		

PRINTF, lun, ' '
PRINTF, lun,'For Days exceeding the EPA O3 standard'
iepa_oct_s  = WHERE(o3_storm_oct  GE 70.0, COMPLEMENT = nepa_oct_s )
iepa_july_s = WHERE(o3dm_storm_july GE 70.0, COMPLEMENT = nepa_july_s)
iepa_oct_ns  = WHERE(o3_nstorm_oct  GE 70.0, COMPLEMENT = nepa_oct_ns )
iepa_july_ns = WHERE(o3dm_nstorm_july GE 70.0, COMPLEMENT = nepa_july_ns)


PRINTF, lun, FORMAT = '("  Variable", 5X, "oct Storm", 4X, "oct No Storm", 4X, "july Storm", 4X, "july No Storm")'								;Print table header information
PRINTF, lun, '==================================================================='
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'O3', MEAN(o3_storm_oct[iepa_oct_s]), MEAN(o3_nstorm_oct[iepa_oct_ns]), MEAN(o3_storm_july[iepa_july_s]), MEAN(o3_nstorm_july[iepa_july_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'o3dm', MEAN(o3dm_storm_oct[iepa_oct_s]), MEAN(o3dm_nstorm_oct[iepa_oct_ns]), MEAN(o3dm_storm_july[iepa_july_s]), MEAN(o3dm_nstorm_july[iepa_july_ns])		
;PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'voc', MEAN(voc_storm_oct[iepa_oct_s]), MEAN(voc_nstorm_oct[iepa_oct_ns]), MEAN(voc_storm_july[iepa_july_s]), MEAN(voc_nstorm_july[iepa_july_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'nox', MEAN(nox_storm_oct[iepa_oct_s]), MEAN(nox_nstorm_oct[iepa_oct_ns]), MEAN(nox_storm_july[iepa_july_s]), MEAN(nox_nstorm_july[iepa_july_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'no', MEAN(no_storm_oct[iepa_oct_s]), MEAN(no_nstorm_oct[iepa_oct_ns]), MEAN(no_storm_july[iepa_july_s]), MEAN(no_nstorm_july[iepa_july_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'no2', MEAN(no2_storm_oct[iepa_oct_s]), MEAN(no2_nstorm_oct[iepa_oct_ns]), MEAN(no2_storm_july[iepa_july_s]), MEAN(no2_nstorm_july[iepa_july_ns])		
;PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'pm', MEAN(pm_storm_oct[iepa_oct_s]), MEAN(pm_nstorm_oct[iepa_oct_ns]), MEAN(pm_storm_july[iepa_july_s]), MEAN(pm_nstorm_july[iepa_july_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'co', MEAN(co_storm_oct[iepa_oct_s]), MEAN(co_nstorm_oct[iepa_oct_ns]), MEAN(co_storm_july[iepa_july_s]), MEAN(co_nstorm_july[iepa_july_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'temp', MEAN(temp_storm_oct[iepa_oct_s]), MEAN(temp_nstorm_oct[iepa_oct_ns]), MEAN(temp_storm_july[iepa_july_s]), MEAN(temp_nstorm_july[iepa_july_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'wind_spd', MEAN(wind_spd_storm_oct[iepa_oct_s]), MEAN(wind_spd_nstorm_oct[iepa_oct_ns]), MEAN(wind_spd_storm_july[iepa_july_s]), MEAN(wind_spd_nstorm_july[iepa_july_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'wind_dir', MEAN(wind_dir_storm_oct[iepa_oct_s]), MEAN(wind_dir_nstorm_oct[iepa_oct_ns]), MEAN(wind_dir_storm_july[iepa_july_s]), MEAN(wind_dir_nstorm_july[iepa_july_ns])		
;PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'press', MEAN(press_storm_oct[iepa_oct_s]), MEAN(press_nstorm_oct[iepa_oct_ns]), MEAN(press_storm_july[iepa_july_s]), MEAN(press_nstorm_july[iepa_july_ns])		
;PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'rh', MEAN(rh_storm_oct[iepa_oct_s]), MEAN(rh_nstorm_oct[iepa_oct_ns]), MEAN(rh_storm_july[iepa_july_s]), MEAN(rh_nstorm_july[iepa_july_ns])		
;PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'dp', MEAN(dp_storm_oct[iepa_oct_s]), MEAN(dp_nstorm_oct[iepa_oct_ns]), MEAN(dp_storm_july[iepa_july_s]), MEAN(dp_nstorm_july[iepa_july_ns])		

PRINTF, lun, ' '
PRINTF, lun,'For days NOT exceeding the EPA O3 standard'

PRINTF, lun, FORMAT = '("  Variable", 5X, "oct Storm", 4X, "oct No Storm", 4X, "july Storm", 4X, "july No Storm")'								;Print table header information
PRINTF, lun, '==================================================================='
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'O3', MEAN(o3_storm_oct[nepa_oct_s]), MEAN(o3_nstorm_oct[nepa_oct_ns]), MEAN(o3_storm_july[nepa_july_s]), MEAN(o3_nstorm_july[nepa_july_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'o3dm', MEAN(o3dm_storm_oct[nepa_oct_s]), MEAN(o3dm_nstorm_oct[nepa_oct_ns]), MEAN(o3dm_storm_july[nepa_july_s]), MEAN(o3dm_nstorm_july[nepa_july_ns])		
;PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'voc', MEAN(voc_storm_oct[nepa_oct_s]), MEAN(voc_nstorm_oct[nepa_oct_ns]), MEAN(voc_storm_july[nepa_july_s]), MEAN(voc_nstorm_july[nepa_july_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'nox', MEAN(nox_storm_oct[nepa_oct_s]), MEAN(nox_nstorm_oct[nepa_oct_ns]), MEAN(nox_storm_july[nepa_july_s]), MEAN(nox_nstorm_july[nepa_july_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'no', MEAN(no_storm_oct[nepa_oct_s]), MEAN(no_nstorm_oct[nepa_oct_ns]), MEAN(no_storm_july[nepa_july_s]), MEAN(no_nstorm_july[nepa_july_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'no2', MEAN(no2_storm_oct[nepa_oct_s]), MEAN(no2_nstorm_oct[nepa_oct_ns]), MEAN(no2_storm_july[nepa_july_s]), MEAN(no2_nstorm_july[nepa_july_ns])		
;PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'pm', MEAN(pm_storm_oct[nepa_oct_s]), MEAN(pm_nstorm_oct[nepa_oct_ns]), MEAN(pm_storm_july[nepa_july_s]), MEAN(pm_nstorm_july[nepa_july_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'co', MEAN(co_storm_oct[nepa_oct_s]), MEAN(co_nstorm_oct[nepa_oct_ns]), MEAN(co_storm_july[nepa_july_s]), MEAN(co_nstorm_july[nepa_july_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'temp', MEAN(temp_storm_oct[nepa_oct_s]), MEAN(temp_nstorm_oct[nepa_oct_ns]), MEAN(temp_storm_july[nepa_july_s]), MEAN(temp_nstorm_july[nepa_july_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'wind_spd', MEAN(wind_spd_storm_oct[nepa_oct_s]), MEAN(wind_spd_nstorm_oct[nepa_oct_ns]), MEAN(wind_spd_storm_july[nepa_july_s]), MEAN(wind_spd_nstorm_july[nepa_july_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'wind_dir', MEAN(wind_dir_storm_oct[nepa_oct_s]), MEAN(wind_dir_nstorm_oct[nepa_oct_ns]), MEAN(wind_dir_storm_july[nepa_july_s]), MEAN(wind_dir_nstorm_july[nepa_july_ns])		
;PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'press', MEAN(press_storm_oct[nepa_oct_s]), MEAN(press_nstorm_oct[nepa_oct_ns]), MEAN(press_storm_july[nepa_july_s]), MEAN(press_nstorm_july[nepa_july_ns])		
;PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'rh', MEAN(rh_storm_oct[nepa_oct_s]), MEAN(rh_nstorm_oct[nepa_oct_ns]), MEAN(rh_storm_july[nepa_july_s]), MEAN(rh_nstorm_july[nepa_july_ns])		
;PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'dp', MEAN(dp_storm_oct[nepa_oct_s]), MEAN(dp_nstorm_oct[nepa_oct_ns]), MEAN(dp_storm_july[nepa_july_s]), MEAN(dp_nstorm_july[nepa_july_ns])		

PRINTF, lun, ' '
PRINTF, lun, 'Number of days exceeding the EPA standard with storms july    = ', N_ELEMENTS(iepa_july_s)
PRINTF, lun, 'Number of days exceeding the EPA standard without storms july = ', N_ELEMENTS(iepa_july_ns)
PRINTF, lun, 'Number of days exceeding the EPA standard with storms oct     = ', N_ELEMENTS(iepa_oct_s)
PRINTF, lun, 'Number of days exceeding the EPA standard without storms oct  = ', N_ELEMENTS(iepa_oct_ns)

PRINTF, lun, ' '

PRINTF, lun, 'Number of days not exceeding the EPA standard with storms july    = ', N_ELEMENTS(nepa_july_s)
PRINTF, lun, 'Number of days not exceeding the EPA standard without storms july = ', N_ELEMENTS(nepa_july_ns)
PRINTF, lun, 'Number of days not exceeding the EPA standard with storms oct     = ', N_ELEMENTS(nepa_oct_s)
PRINTF, lun, 'Number of days not exceeding the EPA standard without storms oct  = ', N_ELEMENTS(nepa_oct_ns)

FREE_LUN, lun

;boxplot code 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;JULY

;;;EPA exceedances without storms: OZONE
o3dm_epa_storm7 = o3dm_storm_july[iepa_july_s]
o3dm_epa_storm7_sort  = o3dm_epa_storm7[SORT(o3dm_epa_storm7)]
IF (N_ELEMENTS(o3dm_epa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
	No3dm_epa_storm7_sort 	= N_ELEMENTS(o3dm_epa_storm7_sort)
	o3dm_med = (o3dm_epa_storm7_sort[(No3dm_epa_storm7_sort/2)-1] + o3dm_epa_storm7_sort[(No3dm_epa_storm7_sort/2)]) / 2.0
	lower_half = o3dm_epa_storm7_sort[0:(No3dm_epa_storm7_sort/2)-1]
	upper_half = o3dm_epa_storm7_sort[(No3dm_epa_storm7_sort/2):(No3dm_epa_storm7_sort-1)]
ENDIF ELSE BEGIN
	No3dm_epa_storm7_sort 	= N_ELEMENTS(o3dm_epa_storm7_sort)
	o3dm_med = o3dm_epa_storm7_sort[(No3dm_epa_storm7_sort/2)] 
	lower_half = o3dm_epa_storm7_sort[0:(No3dm_epa_storm7_sort/2)-1]
	upper_half = o3dm_epa_storm7_sort[(No3dm_epa_storm7_sort/2):(No3dm_epa_storm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3dm_epa_storm7_sort[0.05*No3dm_epa_storm7_sort]
quartile_95 = o3dm_epa_storm7_sort[0.95*No3dm_epa_storm7_sort]

o3dm_epa_storm7_ptile = [quartile_05, quartile_25, o3dm_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: OZONE
o3dm_epa_nstorm7 = o3dm_nstorm_july[iepa_july_ns]
o3dm_epa_nstorm7_sort  = o3dm_epa_nstorm7[SORT(o3dm_epa_nstorm7)]
IF (N_ELEMENTS(o3dm_epa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
	No3dm_epa_nstorm7_sort 	= N_ELEMENTS(o3dm_epa_nstorm7_sort)
	o3dm_med = (o3dm_epa_nstorm7_sort[(No3dm_epa_nstorm7_sort/2)-1] + o3dm_epa_nstorm7_sort[(No3dm_epa_nstorm7_sort/2)]) / 2.0
	lower_half = o3dm_epa_nstorm7_sort[0:(No3dm_epa_nstorm7_sort/2)-1]
	upper_half = o3dm_epa_nstorm7_sort[(No3dm_epa_nstorm7_sort/2):(No3dm_epa_nstorm7_sort-1)]
ENDIF ELSE BEGIN
	No3dm_epa_nstorm7_sort 	= N_ELEMENTS(o3dm_epa_nstorm7_sort)
	o3dm_med = o3dm_epa_nstorm7_sort[(No3dm_epa_nstorm7_sort/2)] 
	lower_half = o3dm_epa_nstorm7_sort[0:(No3dm_epa_nstorm7_sort/2)-1]
	upper_half = o3dm_epa_nstorm7_sort[(No3dm_epa_nstorm7_sort/2):(No3dm_epa_nstorm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3dm_epa_nstorm7_sort[0.05*No3dm_epa_nstorm7_sort]
quartile_95 = o3dm_epa_nstorm7_sort[0.95*No3dm_epa_nstorm7_sort]

o3dm_epa_nstorm7_ptile = [quartile_05, quartile_25, o3dm_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: OZONE
o3dm_nepa_storm7 = o3dm_storm_july[nepa_july_s]
o3dm_nepa_storm7_sort  = o3dm_nepa_storm7[SORT(o3dm_nepa_storm7)]
IF (N_ELEMENTS(o3dm_nepa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
	No3dm_nepa_storm7_sort 	= N_ELEMENTS(o3dm_nepa_storm7_sort)
	o3dm_med = (o3dm_nepa_storm7_sort[(No3dm_nepa_storm7_sort/2)-1] + o3dm_nepa_storm7_sort[(No3dm_nepa_storm7_sort/2)]) / 2.0
	lower_half = o3dm_nepa_storm7_sort[0:(No3dm_nepa_storm7_sort/2)-1]
	upper_half = o3dm_nepa_storm7_sort[(No3dm_nepa_storm7_sort/2):(No3dm_nepa_storm7_sort-1)]
ENDIF ELSE BEGIN
	No3dm_nepa_storm7_sort 	= N_ELEMENTS(o3dm_nepa_storm7_sort)
	o3dm_med = o3dm_nepa_storm7_sort[(No3dm_nepa_storm7_sort/2)] 
	lower_half = o3dm_nepa_storm7_sort[0:(No3dm_nepa_storm7_sort/2)-1]
	upper_half = o3dm_nepa_storm7_sort[(No3dm_nepa_storm7_sort/2):(No3dm_nepa_storm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3dm_nepa_storm7_sort[0.05*No3dm_nepa_storm7_sort]
quartile_95 = o3dm_nepa_storm7_sort[0.95*No3dm_nepa_storm7_sort]

o3dm_nepa_storm7_ptile = [quartile_05, quartile_25, o3dm_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: OZONE
o3dm_nepa_nstorm7 = o3dm_nstorm_july[nepa_july_ns]
o3dm_nepa_nstorm7_sort  = o3dm_nepa_nstorm7[SORT(o3dm_nepa_nstorm7)]
IF (N_ELEMENTS(o3dm_nepa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
	No3dm_nepa_nstorm7_sort 	= N_ELEMENTS(o3dm_nepa_nstorm7_sort)
	o3dm_med = (o3dm_nepa_nstorm7_sort[(No3dm_nepa_nstorm7_sort/2)-1] + o3dm_nepa_nstorm7_sort[(No3dm_nepa_nstorm7_sort/2)]) / 2.0
	lower_half = o3dm_nepa_nstorm7_sort[0:(No3dm_nepa_nstorm7_sort/2)-1]
	upper_half = o3dm_nepa_nstorm7_sort[(No3dm_nepa_nstorm7_sort/2):(No3dm_nepa_nstorm7_sort-1)]
ENDIF ELSE BEGIN
	No3dm_nepa_nstorm7_sort 	= N_ELEMENTS(o3dm_nepa_nstorm7_sort)
	o3dm_med = o3dm_nepa_nstorm7_sort[(No3dm_nepa_nstorm7_sort/2)] 
	lower_half = o3dm_nepa_nstorm7_sort[0:(No3dm_nepa_nstorm7_sort/2)-1]
	upper_half = o3dm_nepa_nstorm7_sort[(No3dm_nepa_nstorm7_sort/2):(No3dm_nepa_nstorm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3dm_nepa_nstorm7_sort[0.05*No3dm_nepa_nstorm7_sort]
quartile_95 = o3dm_nepa_nstorm7_sort[0.95*No3dm_nepa_nstorm7_sort]

o3dm_nepa_nstorm7_ptile = [quartile_05, quartile_25, o3dm_med, quartile_75,quartile_95]

data = [[o3dm_epa_storm7_ptile], [o3dm_epa_nstorm7_ptile], [o3dm_nepa_storm7_ptile], [o3dm_nepa_nstorm7_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','non-EPA w/ Storms','non-EPA w/o Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'July: Ozone Daily Max', $
		XRANGE 		= [10,140], $
		YRANGE 		= [-1, 4], $
		XTITLE 		= 'O3 Concentration (ppb)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances without storms: OZONE
o3_epa_storm7 = o3_storm_july[iepa_july_s]
o3_epa_storm7_sort  = o3_epa_storm7[SORT(o3_epa_storm7)]
IF (N_ELEMENTS(o3_epa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
	No3_epa_storm7_sort 	= N_ELEMENTS(o3_epa_storm7_sort)
	o3_med = (o3_epa_storm7_sort[(No3_epa_storm7_sort/2)-1] + o3_epa_storm7_sort[(No3_epa_storm7_sort/2)]) / 2.0
	lower_half = o3_epa_storm7_sort[0:(No3_epa_storm7_sort/2)-1]
	upper_half = o3_epa_storm7_sort[(No3_epa_storm7_sort/2):(No3_epa_storm7_sort-1)]
ENDIF ELSE BEGIN
	No3_epa_storm7_sort 	= N_ELEMENTS(o3_epa_storm7_sort)
	o3_med = o3_epa_storm7_sort[(No3_epa_storm7_sort/2)] 
	lower_half = o3_epa_storm7_sort[0:(No3_epa_storm7_sort/2)-1]
	upper_half = o3_epa_storm7_sort[(No3_epa_storm7_sort/2):(No3_epa_storm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_epa_storm7_sort[0.05*No3_epa_storm7_sort]
quartile_95 = o3_epa_storm7_sort[0.95*No3_epa_storm7_sort]

o3_epa_storm7_ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: OZONE
o3_epa_nstorm7 = o3_nstorm_july[iepa_july_ns]
o3_epa_nstorm7_sort  = o3_epa_nstorm7[SORT(o3_epa_nstorm7)]
IF (N_ELEMENTS(o3_epa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
	No3_epa_nstorm7_sort 	= N_ELEMENTS(o3_epa_nstorm7_sort)
	o3_med = (o3_epa_nstorm7_sort[(No3_epa_nstorm7_sort/2)-1] + o3_epa_nstorm7_sort[(No3_epa_nstorm7_sort/2)]) / 2.0
	lower_half = o3_epa_nstorm7_sort[0:(No3_epa_nstorm7_sort/2)-1]
	upper_half = o3_epa_nstorm7_sort[(No3_epa_nstorm7_sort/2):(No3_epa_nstorm7_sort-1)]
ENDIF ELSE BEGIN
	No3_epa_nstorm7_sort 	= N_ELEMENTS(o3_epa_nstorm7_sort)
	o3_med = o3_epa_nstorm7_sort[(No3_epa_nstorm7_sort/2)] 
	lower_half = o3_epa_nstorm7_sort[0:(No3_epa_nstorm7_sort/2)-1]
	upper_half = o3_epa_nstorm7_sort[(No3_epa_nstorm7_sort/2):(No3_epa_nstorm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_epa_nstorm7_sort[0.05*No3_epa_nstorm7_sort]
quartile_95 = o3_epa_nstorm7_sort[0.95*No3_epa_nstorm7_sort]

o3_epa_nstorm7_ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: OZONE
o3_nepa_storm7 = o3_storm_july[nepa_july_s]
o3_nepa_storm7_sort  = o3_nepa_storm7[SORT(o3_nepa_storm7)]
IF (N_ELEMENTS(o3_nepa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
	No3_nepa_storm7_sort 	= N_ELEMENTS(o3_nepa_storm7_sort)
	o3_med = (o3_nepa_storm7_sort[(No3_nepa_storm7_sort/2)-1] + o3_nepa_storm7_sort[(No3_nepa_storm7_sort/2)]) / 2.0
	lower_half = o3_nepa_storm7_sort[0:(No3_nepa_storm7_sort/2)-1]
	upper_half = o3_nepa_storm7_sort[(No3_nepa_storm7_sort/2):(No3_nepa_storm7_sort-1)]
ENDIF ELSE BEGIN
	No3_nepa_storm7_sort 	= N_ELEMENTS(o3_nepa_storm7_sort)
	o3_med = o3_nepa_storm7_sort[(No3_nepa_storm7_sort/2)] 
	lower_half = o3_nepa_storm7_sort[0:(No3_nepa_storm7_sort/2)-1]
	upper_half = o3_nepa_storm7_sort[(No3_nepa_storm7_sort/2):(No3_nepa_storm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_nepa_storm7_sort[0.05*No3_nepa_storm7_sort]
quartile_95 = o3_nepa_storm7_sort[0.95*No3_nepa_storm7_sort]

o3_nepa_storm7_ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: OZONE
o3_nepa_nstorm7 = o3_nstorm_july[nepa_july_ns]
o3_nepa_nstorm7_sort  = o3_nepa_nstorm7[SORT(o3_nepa_nstorm7)]
IF (N_ELEMENTS(o3_nepa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
	No3_nepa_nstorm7_sort 	= N_ELEMENTS(o3_nepa_nstorm7_sort)
	o3_med = (o3_nepa_nstorm7_sort[(No3_nepa_nstorm7_sort/2)-1] + o3_nepa_nstorm7_sort[(No3_nepa_nstorm7_sort/2)]) / 2.0
	lower_half = o3_nepa_nstorm7_sort[0:(No3_nepa_nstorm7_sort/2)-1]
	upper_half = o3_nepa_nstorm7_sort[(No3_nepa_nstorm7_sort/2):(No3_nepa_nstorm7_sort-1)]
ENDIF ELSE BEGIN
	No3_nepa_nstorm7_sort 	= N_ELEMENTS(o3_nepa_nstorm7_sort)
	o3_med = o3_nepa_nstorm7_sort[(No3_nepa_nstorm7_sort/2)] 
	lower_half = o3_nepa_nstorm7_sort[0:(No3_nepa_nstorm7_sort/2)-1]
	upper_half = o3_nepa_nstorm7_sort[(No3_nepa_nstorm7_sort/2):(No3_nepa_nstorm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_nepa_nstorm7_sort[0.05*No3_nepa_nstorm7_sort]
quartile_95 = o3_nepa_nstorm7_sort[0.95*No3_nepa_nstorm7_sort]

o3_nepa_nstorm7_ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]

data = [[o3_epa_storm7_ptile], [o3_epa_nstorm7_ptile], [o3_nepa_storm7_ptile], [o3_nepa_nstorm7_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','non-EPA w/ Storms','non-EPA w/o Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'July: Ozone', $
		XRANGE 		= [0,80], $
		YRANGE 		= [-1, 4], $
		XTITLE 		= 'O3 Concentration (ppb)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: voc
;voc_epa_storm7 = voc_storm_july[iepa_july_s]
;voc_epa_storm7_sort  = voc_epa_storm7[SORT(voc_epa_storm7)]
;IF (N_ELEMENTS(voc_epa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
;	Nvoc_epa_storm7_sort 	= N_ELEMENTS(voc_epa_storm7_sort)
;	voc_med = (voc_epa_storm7_sort[(Nvoc_epa_storm7_sort/2)-1] + voc_epa_storm7_sort[(Nvoc_epa_storm7_sort/2)]) / 2.0
;	lower_half = voc_epa_storm7_sort[0:(Nvoc_epa_storm7_sort/2)-1]
;	upper_half = voc_epa_storm7_sort[(Nvoc_epa_storm7_sort/2):(Nvoc_epa_storm7_sort-1)]
;ENDIF ELSE BEGIN
;	Nvoc_epa_storm7_sort 	= N_ELEMENTS(voc_epa_storm7_sort)
;	voc_med = voc_epa_storm7_sort[(Nvoc_epa_storm7_sort/2)] 
;	lower_half = voc_epa_storm7_sort[0:(Nvoc_epa_storm7_sort/2)-1]
;	upper_half = voc_epa_storm7_sort[(Nvoc_epa_storm7_sort/2):(Nvoc_epa_storm7_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = voc_epa_storm7_sort[0.05*Nvoc_epa_storm7_sort]
;quartile_95 = voc_epa_storm7_sort[0.95*Nvoc_epa_storm7_sort]
;
;voc_epa_storm7_ptile = [quartile_05, quartile_25, voc_med, quartile_75,quartile_95]
;
;
;;;;EPA exceedances without storms: voc
;voc_epa_nstorm7 = voc_nstorm_july[iepa_july_ns]
;voc_epa_nstorm7_sort  = voc_epa_nstorm7[SORT(voc_epa_nstorm7)]
;IF (N_ELEMENTS(voc_epa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
;	Nvoc_epa_nstorm7_sort 	= N_ELEMENTS(voc_epa_nstorm7_sort)
;	voc_med = (voc_epa_nstorm7_sort[(Nvoc_epa_nstorm7_sort/2)-1] + voc_epa_nstorm7_sort[(Nvoc_epa_nstorm7_sort/2)]) / 2.0
;	lower_half = voc_epa_nstorm7_sort[0:(Nvoc_epa_nstorm7_sort/2)-1]
;	upper_half = voc_epa_nstorm7_sort[(Nvoc_epa_nstorm7_sort/2):(Nvoc_epa_nstorm7_sort-1)]
;ENDIF ELSE BEGIN
;	Nvoc_epa_nstorm7_sort 	= N_ELEMENTS(voc_epa_nstorm7_sort)
;	voc_med = voc_epa_nstorm7_sort[(Nvoc_epa_nstorm7_sort/2)] 
;	lower_half = voc_epa_nstorm7_sort[0:(Nvoc_epa_nstorm7_sort/2)-1]
;	upper_half = voc_epa_nstorm7_sort[(Nvoc_epa_nstorm7_sort/2):(Nvoc_epa_nstorm7_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = voc_epa_nstorm7_sort[0.05*Nvoc_epa_nstorm7_sort]
;quartile_95 = voc_epa_nstorm7_sort[0.95*Nvoc_epa_nstorm7_sort]
;
;voc_epa_nstorm7_ptile = [quartile_05, quartile_25, voc_med, quartile_75,quartile_95]
;
;
;;;;non-EPA exceedances with storms: voc
;voc_nepa_storm7 = voc_storm_july[nepa_july_s]
;voc_nepa_storm7_sort  = voc_nepa_storm7[SORT(voc_nepa_storm7)]
;IF (N_ELEMENTS(voc_nepa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
;	Nvoc_nepa_storm7_sort 	= N_ELEMENTS(voc_nepa_storm7_sort)
;	voc_med = (voc_nepa_storm7_sort[(Nvoc_nepa_storm7_sort/2)-1] + voc_nepa_storm7_sort[(Nvoc_nepa_storm7_sort/2)]) / 2.0
;	lower_half = voc_nepa_storm7_sort[0:(Nvoc_nepa_storm7_sort/2)-1]
;	upper_half = voc_nepa_storm7_sort[(Nvoc_nepa_storm7_sort/2):(Nvoc_nepa_storm7_sort-1)]
;ENDIF ELSE BEGIN
;	Nvoc_nepa_storm7_sort 	= N_ELEMENTS(voc_nepa_storm7_sort)
;	voc_med = voc_nepa_storm7_sort[(Nvoc_nepa_storm7_sort/2)] 
;	lower_half = voc_nepa_storm7_sort[0:(Nvoc_nepa_storm7_sort/2)-1]
;	upper_half = voc_nepa_storm7_sort[(Nvoc_nepa_storm7_sort/2):(Nvoc_nepa_storm7_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = voc_nepa_storm7_sort[0.05*Nvoc_nepa_storm7_sort]
;quartile_95 = voc_nepa_storm7_sort[0.95*Nvoc_nepa_storm7_sort]
;
;voc_nepa_storm7_ptile = [quartile_05, quartile_25, voc_med, quartile_75,quartile_95]
;
;
;;;;non-EPA exceedances with storms: voc
;voc_nepa_nstorm7 = voc_nstorm_july[nepa_july_ns]
;voc_nepa_nstorm7_sort  = voc_nepa_nstorm7[SORT(voc_nepa_nstorm7)]
;IF (N_ELEMENTS(voc_nepa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
;	Nvoc_nepa_nstorm7_sort 	= N_ELEMENTS(voc_nepa_nstorm7_sort)
;	voc_med = (voc_nepa_nstorm7_sort[(Nvoc_nepa_nstorm7_sort/2)-1] + voc_nepa_nstorm7_sort[(Nvoc_nepa_nstorm7_sort/2)]) / 2.0
;	lower_half = voc_nepa_nstorm7_sort[0:(Nvoc_nepa_nstorm7_sort/2)-1]
;	upper_half = voc_nepa_nstorm7_sort[(Nvoc_nepa_nstorm7_sort/2):(Nvoc_nepa_nstorm7_sort-1)]
;ENDIF ELSE BEGIN
;	Nvoc_nepa_nstorm7_sort 	= N_ELEMENTS(voc_nepa_nstorm7_sort)
;	voc_med = voc_nepa_nstorm7_sort[(Nvoc_nepa_nstorm7_sort/2)] 
;	lower_half = voc_nepa_nstorm7_sort[0:(Nvoc_nepa_nstorm7_sort/2)-1]
;	upper_half = voc_nepa_nstorm7_sort[(Nvoc_nepa_nstorm7_sort/2):(Nvoc_nepa_nstorm7_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = voc_nepa_nstorm7_sort[0.05*Nvoc_nepa_nstorm7_sort]
;quartile_95 = voc_nepa_nstorm7_sort[0.95*Nvoc_nepa_nstorm7_sort]
;
;voc_nepa_nstorm7_ptile = [quartile_05, quartile_25, voc_med, quartile_75,quartile_95]
;
;data = [[voc_epa_storm7_ptile], [voc_epa_nstorm7_ptile], [voc_nepa_storm7_ptile], [voc_nepa_nstorm7_ptile]]
;ytitle = ['EPA w/ Storms', 'EPA w/o Storms','non-EPA w/ Storms','non-EPA w/o Storms']
;boxes = BOXPLOT(data, $
;		TITLE		= 'July: VOC', $
;		XRANGE 		= [0,600], $
;		YRANGE 		= [-1, 4], $
;		XTITLE 		= 'VOC Concentration (ppb)', $
;		YTICKNAME 	= ytitle, $
;		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
;		FONT_SIZE   = 20, $
;		HORIZONTAL	= 1)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances without storms: nox
nox_epa_storm7 = nox_storm_july[iepa_july_s]
nox_epa_storm7_sort  = nox_epa_storm7[SORT(nox_epa_storm7)]
IF (N_ELEMENTS(nox_epa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
	Nnox_epa_storm7_sort 	= N_ELEMENTS(nox_epa_storm7_sort)
	nox_med = (nox_epa_storm7_sort[(Nnox_epa_storm7_sort/2)-1] + nox_epa_storm7_sort[(Nnox_epa_storm7_sort/2)]) / 2.0
	lower_half = nox_epa_storm7_sort[0:(Nnox_epa_storm7_sort/2)-1]
	upper_half = nox_epa_storm7_sort[(Nnox_epa_storm7_sort/2):(Nnox_epa_storm7_sort-1)]
ENDIF ELSE BEGIN
	Nnox_epa_storm7_sort 	= N_ELEMENTS(nox_epa_storm7_sort)
	nox_med = nox_epa_storm7_sort[(Nnox_epa_storm7_sort/2)] 
	lower_half = nox_epa_storm7_sort[0:(Nnox_epa_storm7_sort/2)-1]
	upper_half = nox_epa_storm7_sort[(Nnox_epa_storm7_sort/2):(Nnox_epa_storm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = nox_epa_storm7_sort[0.05*Nnox_epa_storm7_sort]
quartile_95 = nox_epa_storm7_sort[0.95*Nnox_epa_storm7_sort]

nox_epa_storm7_ptile = [quartile_05, quartile_25, nox_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: nox
nox_epa_nstorm7 = nox_nstorm_july[iepa_july_ns]
nox_epa_nstorm7_sort  = nox_epa_nstorm7[SORT(nox_epa_nstorm7)]
IF (N_ELEMENTS(nox_epa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
	Nnox_epa_nstorm7_sort 	= N_ELEMENTS(nox_epa_nstorm7_sort)
	nox_med = (nox_epa_nstorm7_sort[(Nnox_epa_nstorm7_sort/2)-1] + nox_epa_nstorm7_sort[(Nnox_epa_nstorm7_sort/2)]) / 2.0
	lower_half = nox_epa_nstorm7_sort[0:(Nnox_epa_nstorm7_sort/2)-1]
	upper_half = nox_epa_nstorm7_sort[(Nnox_epa_nstorm7_sort/2):(Nnox_epa_nstorm7_sort-1)]
ENDIF ELSE BEGIN
	Nnox_epa_nstorm7_sort 	= N_ELEMENTS(nox_epa_nstorm7_sort)
	nox_med = nox_epa_nstorm7_sort[(Nnox_epa_nstorm7_sort/2)] 
	lower_half = nox_epa_nstorm7_sort[0:(Nnox_epa_nstorm7_sort/2)-1]
	upper_half = nox_epa_nstorm7_sort[(Nnox_epa_nstorm7_sort/2):(Nnox_epa_nstorm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = nox_epa_nstorm7_sort[0.05*Nnox_epa_nstorm7_sort]
quartile_95 = nox_epa_nstorm7_sort[0.95*Nnox_epa_nstorm7_sort]

nox_epa_nstorm7_ptile = [quartile_05, quartile_25, nox_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: nox
nox_nepa_storm7 = nox_storm_july[nepa_july_s]
nox_nepa_storm7_sort  = nox_nepa_storm7[SORT(nox_nepa_storm7)]
IF (N_ELEMENTS(nox_nepa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
	Nnox_nepa_storm7_sort 	= N_ELEMENTS(nox_nepa_storm7_sort)
	nox_med = (nox_nepa_storm7_sort[(Nnox_nepa_storm7_sort/2)-1] + nox_nepa_storm7_sort[(Nnox_nepa_storm7_sort/2)]) / 2.0
	lower_half = nox_nepa_storm7_sort[0:(Nnox_nepa_storm7_sort/2)-1]
	upper_half = nox_nepa_storm7_sort[(Nnox_nepa_storm7_sort/2):(Nnox_nepa_storm7_sort-1)]
ENDIF ELSE BEGIN
	Nnox_nepa_storm7_sort 	= N_ELEMENTS(nox_nepa_storm7_sort)
	nox_med = nox_nepa_storm7_sort[(Nnox_nepa_storm7_sort/2)] 
	lower_half = nox_nepa_storm7_sort[0:(Nnox_nepa_storm7_sort/2)-1]
	upper_half = nox_nepa_storm7_sort[(Nnox_nepa_storm7_sort/2):(Nnox_nepa_storm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = nox_nepa_storm7_sort[0.05*Nnox_nepa_storm7_sort]
quartile_95 = nox_nepa_storm7_sort[0.95*Nnox_nepa_storm7_sort]

nox_nepa_storm7_ptile = [quartile_05, quartile_25, nox_med, quartile_75,quartile_95]


;;;non-EPA exceedances without storms: nox
nox_nepa_nstorm7 = nox_nstorm_july[nepa_july_ns]
nox_nepa_nstorm7_sort  = nox_nepa_nstorm7[SORT(nox_nepa_nstorm7)]
IF (N_ELEMENTS(nox_nepa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
	Nnox_nepa_nstorm7_sort 	= N_ELEMENTS(nox_nepa_nstorm7_sort)
	nox_med = (nox_nepa_nstorm7_sort[(Nnox_nepa_nstorm7_sort/2)-1] + nox_nepa_nstorm7_sort[(Nnox_nepa_nstorm7_sort/2)]) / 2.0
	lower_half = nox_nepa_nstorm7_sort[0:(Nnox_nepa_nstorm7_sort/2)-1]
	upper_half = nox_nepa_nstorm7_sort[(Nnox_nepa_nstorm7_sort/2):(Nnox_nepa_nstorm7_sort-1)]
ENDIF ELSE BEGIN
	Nnox_nepa_nstorm7_sort 	= N_ELEMENTS(nox_nepa_nstorm7_sort)
	nox_med = nox_nepa_nstorm7_sort[(Nnox_nepa_nstorm7_sort/2)] 
	lower_half = nox_nepa_nstorm7_sort[0:(Nnox_nepa_nstorm7_sort/2)-1]
	upper_half = nox_nepa_nstorm7_sort[(Nnox_nepa_nstorm7_sort/2):(Nnox_nepa_nstorm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = nox_nepa_nstorm7_sort[0.05*Nnox_nepa_nstorm7_sort]
quartile_95 = nox_nepa_nstorm7_sort[0.95*Nnox_nepa_nstorm7_sort]

nox_nepa_nstorm7_ptile = [quartile_05, quartile_25, nox_med, quartile_75,quartile_95]

data = [[nox_epa_storm7_ptile], [nox_epa_nstorm7_ptile], [nox_nepa_storm7_ptile], [nox_nepa_nstorm7_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','non-EPA w/ Storms','non-EPA w/o Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'July: NOx', $
		XRANGE 		= [0,150], $
		YRANGE 		= [-1, 4], $
		XTITLE 		= 'NOx Concentration (ppb)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: no
no_epa_storm7 = no_storm_july[iepa_july_s]
no_epa_storm7_sort  = no_epa_storm7[SORT(no_epa_storm7)]
IF (N_ELEMENTS(no_epa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
	Nno_epa_storm7_sort 	= N_ELEMENTS(no_epa_storm7_sort)
	no_med = (no_epa_storm7_sort[(Nno_epa_storm7_sort/2)-1] + no_epa_storm7_sort[(Nno_epa_storm7_sort/2)]) / 2.0
	lower_half = no_epa_storm7_sort[0:(Nno_epa_storm7_sort/2)-1]
	upper_half = no_epa_storm7_sort[(Nno_epa_storm7_sort/2):(Nno_epa_storm7_sort-1)]
ENDIF ELSE BEGIN
	Nno_epa_storm7_sort 	= N_ELEMENTS(no_epa_storm7_sort)
	no_med = no_epa_storm7_sort[(Nno_epa_storm7_sort/2)] 
	lower_half = no_epa_storm7_sort[0:(Nno_epa_storm7_sort/2)-1]
	upper_half = no_epa_storm7_sort[(Nno_epa_storm7_sort/2):(Nno_epa_storm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = no_epa_storm7_sort[0.05*Nno_epa_storm7_sort]
quartile_95 = no_epa_storm7_sort[0.95*Nno_epa_storm7_sort]

no_epa_storm7_ptile = [quartile_05, quartile_25, no_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: no
no_epa_nstorm7 = no_nstorm_july[iepa_july_ns]
no_epa_nstorm7_sort  = no_epa_nstorm7[SORT(no_epa_nstorm7)]
IF (N_ELEMENTS(no_epa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
	Nno_epa_nstorm7_sort 	= N_ELEMENTS(no_epa_nstorm7_sort)
	no_med = (no_epa_nstorm7_sort[(Nno_epa_nstorm7_sort/2)-1] + no_epa_nstorm7_sort[(Nno_epa_nstorm7_sort/2)]) / 2.0
	lower_half = no_epa_nstorm7_sort[0:(Nno_epa_nstorm7_sort/2)-1]
	upper_half = no_epa_nstorm7_sort[(Nno_epa_nstorm7_sort/2):(Nno_epa_nstorm7_sort-1)]
ENDIF ELSE BEGIN
	Nno_epa_nstorm7_sort 	= N_ELEMENTS(no_epa_nstorm7_sort)
	no_med = no_epa_nstorm7_sort[(Nno_epa_nstorm7_sort/2)] 
	lower_half = no_epa_nstorm7_sort[0:(Nno_epa_nstorm7_sort/2)-1]
	upper_half = no_epa_nstorm7_sort[(Nno_epa_nstorm7_sort/2):(Nno_epa_nstorm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = no_epa_nstorm7_sort[0.05*Nno_epa_nstorm7_sort]
quartile_95 = no_epa_nstorm7_sort[0.95*Nno_epa_nstorm7_sort]

no_epa_nstorm7_ptile = [quartile_05, quartile_25, no_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: no
no_nepa_storm7 = no_storm_july[nepa_july_s]
no_nepa_storm7_sort  = no_nepa_storm7[SORT(no_nepa_storm7)]
IF (N_ELEMENTS(no_nepa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
	Nno_nepa_storm7_sort 	= N_ELEMENTS(no_nepa_storm7_sort)
	no_med = (no_nepa_storm7_sort[(Nno_nepa_storm7_sort/2)-1] + no_nepa_storm7_sort[(Nno_nepa_storm7_sort/2)]) / 2.0
	lower_half = no_nepa_storm7_sort[0:(Nno_nepa_storm7_sort/2)-1]
	upper_half = no_nepa_storm7_sort[(Nno_nepa_storm7_sort/2):(Nno_nepa_storm7_sort-1)]
ENDIF ELSE BEGIN
	Nno_nepa_storm7_sort 	= N_ELEMENTS(no_nepa_storm7_sort)
	no_med = no_nepa_storm7_sort[(Nno_nepa_storm7_sort/2)] 
	lower_half = no_nepa_storm7_sort[0:(Nno_nepa_storm7_sort/2)-1]
	upper_half = no_nepa_storm7_sort[(Nno_nepa_storm7_sort/2):(Nno_nepa_storm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = no_nepa_storm7_sort[0.05*Nno_nepa_storm7_sort]
quartile_95 = no_nepa_storm7_sort[0.95*Nno_nepa_storm7_sort]

no_nepa_storm7_ptile = [quartile_05, quartile_25, no_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: no
no_nepa_nstorm7 = no_nstorm_july[nepa_july_ns]
no_nepa_nstorm7_sort  = no_nepa_nstorm7[SORT(no_nepa_nstorm7)]
IF (N_ELEMENTS(no_nepa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
	Nno_nepa_nstorm7_sort 	= N_ELEMENTS(no_nepa_nstorm7_sort)
	no_med = (no_nepa_nstorm7_sort[(Nno_nepa_nstorm7_sort/2)-1] + no_nepa_nstorm7_sort[(Nno_nepa_nstorm7_sort/2)]) / 2.0
	lower_half = no_nepa_nstorm7_sort[0:(Nno_nepa_nstorm7_sort/2)-1]
	upper_half = no_nepa_nstorm7_sort[(Nno_nepa_nstorm7_sort/2):(Nno_nepa_nstorm7_sort-1)]
ENDIF ELSE BEGIN
	Nno_nepa_nstorm7_sort 	= N_ELEMENTS(no_nepa_nstorm7_sort)
	no_med = no_nepa_nstorm7_sort[(Nno_nepa_nstorm7_sort/2)] 
	lower_half = no_nepa_nstorm7_sort[0:(Nno_nepa_nstorm7_sort/2)-1]
	upper_half = no_nepa_nstorm7_sort[(Nno_nepa_nstorm7_sort/2):(Nno_nepa_nstorm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = no_nepa_nstorm7_sort[0.05*Nno_nepa_nstorm7_sort]
quartile_95 = no_nepa_nstorm7_sort[0.95*Nno_nepa_nstorm7_sort]

no_nepa_nstorm7_ptile = [quartile_05, quartile_25, no_med, quartile_75,quartile_95]

data = [[no_epa_storm7_ptile], [no_epa_nstorm7_ptile], [no_nepa_storm7_ptile], [no_nepa_nstorm7_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','non-EPA w/ Storms','non-EPA w/o Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'July: NO', $
		XRANGE 		= [0,90], $
		YRANGE 		= [-1, 4], $
		XTITLE 		= 'NO Concentration (ppb)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: no2
no2_epa_storm7 = no2_storm_july[iepa_july_s]
no2_epa_storm7_sort  = no2_epa_storm7[SORT(no2_epa_storm7)]
IF (N_ELEMENTS(no2_epa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
	Nno2_epa_storm7_sort 	= N_ELEMENTS(no2_epa_storm7_sort)
	no2_med = (no2_epa_storm7_sort[(Nno2_epa_storm7_sort/2)-1] + no2_epa_storm7_sort[(Nno2_epa_storm7_sort/2)]) / 2.0
	lower_half = no2_epa_storm7_sort[0:(Nno2_epa_storm7_sort/2)-1]
	upper_half = no2_epa_storm7_sort[(Nno2_epa_storm7_sort/2):(Nno2_epa_storm7_sort-1)]
ENDIF ELSE BEGIN
	Nno2_epa_storm7_sort 	= N_ELEMENTS(no2_epa_storm7_sort)
	no2_med = no2_epa_storm7_sort[(Nno2_epa_storm7_sort/2)] 
	lower_half = no2_epa_storm7_sort[0:(Nno2_epa_storm7_sort/2)-1]
	upper_half = no2_epa_storm7_sort[(Nno2_epa_storm7_sort/2):(Nno2_epa_storm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = no2_epa_storm7_sort[0.05*Nno2_epa_storm7_sort]
quartile_95 = no2_epa_storm7_sort[0.95*Nno2_epa_storm7_sort]

no2_epa_storm7_ptile = [quartile_05, quartile_25, no2_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: no2
no2_epa_nstorm7 = no2_nstorm_july[iepa_july_ns]
no2_epa_nstorm7_sort  = no2_epa_nstorm7[SORT(no2_epa_nstorm7)]
IF (N_ELEMENTS(no2_epa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
	Nno2_epa_nstorm7_sort 	= N_ELEMENTS(no2_epa_nstorm7_sort)
	no2_med = (no2_epa_nstorm7_sort[(Nno2_epa_nstorm7_sort/2)-1] + no2_epa_nstorm7_sort[(Nno2_epa_nstorm7_sort/2)]) / 2.0
	lower_half = no2_epa_nstorm7_sort[0:(Nno2_epa_nstorm7_sort/2)-1]
	upper_half = no2_epa_nstorm7_sort[(Nno2_epa_nstorm7_sort/2):(Nno2_epa_nstorm7_sort-1)]
ENDIF ELSE BEGIN
	Nno2_epa_nstorm7_sort 	= N_ELEMENTS(no2_epa_nstorm7_sort)
	no2_med = no2_epa_nstorm7_sort[(Nno2_epa_nstorm7_sort/2)] 
	lower_half = no2_epa_nstorm7_sort[0:(Nno2_epa_nstorm7_sort/2)-1]
	upper_half = no2_epa_nstorm7_sort[(Nno2_epa_nstorm7_sort/2):(Nno2_epa_nstorm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = no2_epa_nstorm7_sort[0.05*Nno2_epa_nstorm7_sort]
quartile_95 = no2_epa_nstorm7_sort[0.95*Nno2_epa_nstorm7_sort]

no2_epa_nstorm7_ptile = [quartile_05, quartile_25, no2_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: no2
no2_nepa_storm7 = no2_storm_july[nepa_july_s]
no2_nepa_storm7_sort  = no2_nepa_storm7[SORT(no2_nepa_storm7)]
IF (N_ELEMENTS(no2_nepa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
	Nno2_nepa_storm7_sort 	= N_ELEMENTS(no2_nepa_storm7_sort)
	no2_med = (no2_nepa_storm7_sort[(Nno2_nepa_storm7_sort/2)-1] + no2_nepa_storm7_sort[(Nno2_nepa_storm7_sort/2)]) / 2.0
	lower_half = no2_nepa_storm7_sort[0:(Nno2_nepa_storm7_sort/2)-1]
	upper_half = no2_nepa_storm7_sort[(Nno2_nepa_storm7_sort/2):(Nno2_nepa_storm7_sort-1)]
ENDIF ELSE BEGIN
	Nno2_nepa_storm7_sort 	= N_ELEMENTS(no2_nepa_storm7_sort)
	no2_med = no2_nepa_storm7_sort[(Nno2_nepa_storm7_sort/2)] 
	lower_half = no2_nepa_storm7_sort[0:(Nno2_nepa_storm7_sort/2)-1]
	upper_half = no2_nepa_storm7_sort[(Nno2_nepa_storm7_sort/2):(Nno2_nepa_storm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = no2_nepa_storm7_sort[0.05*Nno2_nepa_storm7_sort]
quartile_95 = no2_nepa_storm7_sort[0.95*Nno2_nepa_storm7_sort]

no2_nepa_storm7_ptile = [quartile_05, quartile_25, no2_med, quartile_75,quartile_95]


;;;non-EPA exceedances without storms: no2
no2_nepa_nstorm7 = no2_nstorm_july[nepa_july_ns]
no2_nepa_nstorm7_sort  = no2_nepa_nstorm7[SORT(no2_nepa_nstorm7)]
IF (N_ELEMENTS(no2_nepa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
	Nno2_nepa_nstorm7_sort 	= N_ELEMENTS(no2_nepa_nstorm7_sort)
	no2_med = (no2_nepa_nstorm7_sort[(Nno2_nepa_nstorm7_sort/2)-1] + no2_nepa_nstorm7_sort[(Nno2_nepa_nstorm7_sort/2)]) / 2.0
	lower_half = no2_nepa_nstorm7_sort[0:(Nno2_nepa_nstorm7_sort/2)-1]
	upper_half = no2_nepa_nstorm7_sort[(Nno2_nepa_nstorm7_sort/2):(Nno2_nepa_nstorm7_sort-1)]
ENDIF ELSE BEGIN
	Nno2_nepa_nstorm7_sort 	= N_ELEMENTS(no2_nepa_nstorm7_sort)
	no2_med = no2_nepa_nstorm7_sort[(Nno2_nepa_nstorm7_sort/2)] 
	lower_half = no2_nepa_nstorm7_sort[0:(Nno2_nepa_nstorm7_sort/2)-1]
	upper_half = no2_nepa_nstorm7_sort[(Nno2_nepa_nstorm7_sort/2):(Nno2_nepa_nstorm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = no2_nepa_nstorm7_sort[0.05*Nno2_nepa_nstorm7_sort]
quartile_95 = no2_nepa_nstorm7_sort[0.95*Nno2_nepa_nstorm7_sort]

no2_nepa_nstorm7_ptile = [quartile_05, quartile_25, no2_med, quartile_75,quartile_95]

data = [[no2_epa_storm7_ptile], [no2_epa_nstorm7_ptile], [no2_nepa_storm7_ptile], [no2_nepa_nstorm7_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','no2n-EPA w/ Storms','no2n-EPA w/o Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'July: NO2', $
		XRANGE 		= [0,50], $
		YRANGE 		= [-1, 4], $
		XTITLE 		= 'NO2 Concentration (ppb)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: co
co_epa_storm7 = co_storm_july[iepa_july_s]
co_epa_storm7_sort  = co_epa_storm7[SORT(co_epa_storm7)]
IF (N_ELEMENTS(co_epa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
	Nco_epa_storm7_sort 	= N_ELEMENTS(co_epa_storm7_sort)
	co_med = (co_epa_storm7_sort[(Nco_epa_storm7_sort/2)-1] + co_epa_storm7_sort[(Nco_epa_storm7_sort/2)]) / 2.0
	lower_half = co_epa_storm7_sort[0:(Nco_epa_storm7_sort/2)-1]
	upper_half = co_epa_storm7_sort[(Nco_epa_storm7_sort/2):(Nco_epa_storm7_sort-1)]
ENDIF ELSE BEGIN
	Nco_epa_storm7_sort 	= N_ELEMENTS(co_epa_storm7_sort)
	co_med = co_epa_storm7_sort[(Nco_epa_storm7_sort/2)] 
	lower_half = co_epa_storm7_sort[0:(Nco_epa_storm7_sort/2)-1]
	upper_half = co_epa_storm7_sort[(Nco_epa_storm7_sort/2):(Nco_epa_storm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = co_epa_storm7_sort[0.05*Nco_epa_storm7_sort]
quartile_95 = co_epa_storm7_sort[0.95*Nco_epa_storm7_sort]

co_epa_storm7_ptile = [quartile_05, quartile_25, co_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: co
co_epa_nstorm7 = co_nstorm_july[iepa_july_ns]
co_epa_nstorm7_sort  = co_epa_nstorm7[SORT(co_epa_nstorm7)]
IF (N_ELEMENTS(co_epa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
	Nco_epa_nstorm7_sort 	= N_ELEMENTS(co_epa_nstorm7_sort)
	co_med = (co_epa_nstorm7_sort[(Nco_epa_nstorm7_sort/2)-1] + co_epa_nstorm7_sort[(Nco_epa_nstorm7_sort/2)]) / 2.0
	lower_half = co_epa_nstorm7_sort[0:(Nco_epa_nstorm7_sort/2)-1]
	upper_half = co_epa_nstorm7_sort[(Nco_epa_nstorm7_sort/2):(Nco_epa_nstorm7_sort-1)]
ENDIF ELSE BEGIN
	Nco_epa_nstorm7_sort 	= N_ELEMENTS(co_epa_nstorm7_sort)
	co_med = co_epa_nstorm7_sort[(Nco_epa_nstorm7_sort/2)] 
	lower_half = co_epa_nstorm7_sort[0:(Nco_epa_nstorm7_sort/2)-1]
	upper_half = co_epa_nstorm7_sort[(Nco_epa_nstorm7_sort/2):(Nco_epa_nstorm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = co_epa_nstorm7_sort[0.05*Nco_epa_nstorm7_sort]
quartile_95 = co_epa_nstorm7_sort[0.95*Nco_epa_nstorm7_sort]

co_epa_nstorm7_ptile = [quartile_05, quartile_25, co_med, quartile_75,quartile_95]


;;;con-EPA exceedances with storms: co
co_nepa_storm7 = co_storm_july[nepa_july_s]
co_nepa_storm7_sort  = co_nepa_storm7[SORT(co_nepa_storm7)]
IF (N_ELEMENTS(co_nepa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
	Nco_nepa_storm7_sort 	= N_ELEMENTS(co_nepa_storm7_sort)
	co_med = (co_nepa_storm7_sort[(Nco_nepa_storm7_sort/2)-1] + co_nepa_storm7_sort[(Nco_nepa_storm7_sort/2)]) / 2.0
	lower_half = co_nepa_storm7_sort[0:(Nco_nepa_storm7_sort/2)-1]
	upper_half = co_nepa_storm7_sort[(Nco_nepa_storm7_sort/2):(Nco_nepa_storm7_sort-1)]
ENDIF ELSE BEGIN
	Nco_nepa_storm7_sort 	= N_ELEMENTS(co_nepa_storm7_sort)
	co_med = co_nepa_storm7_sort[(Nco_nepa_storm7_sort/2)] 
	lower_half = co_nepa_storm7_sort[0:(Nco_nepa_storm7_sort/2)-1]
	upper_half = co_nepa_storm7_sort[(Nco_nepa_storm7_sort/2):(Nco_nepa_storm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = co_nepa_storm7_sort[0.05*Nco_nepa_storm7_sort]
quartile_95 = co_nepa_storm7_sort[0.95*Nco_nepa_storm7_sort]

co_nepa_storm7_ptile = [quartile_05, quartile_25, co_med, quartile_75,quartile_95]


;;;con-EPA exceedances with storms: co
co_nepa_nstorm7 = co_nstorm_july[nepa_july_ns]
co_nepa_nstorm7_sort  = co_nepa_nstorm7[SORT(co_nepa_nstorm7)]
IF (N_ELEMENTS(co_nepa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
	Nco_nepa_nstorm7_sort 	= N_ELEMENTS(co_nepa_nstorm7_sort)
	co_med = (co_nepa_nstorm7_sort[(Nco_nepa_nstorm7_sort/2)-1] + co_nepa_nstorm7_sort[(Nco_nepa_nstorm7_sort/2)]) / 2.0
	lower_half = co_nepa_nstorm7_sort[0:(Nco_nepa_nstorm7_sort/2)-1]
	upper_half = co_nepa_nstorm7_sort[(Nco_nepa_nstorm7_sort/2):(Nco_nepa_nstorm7_sort-1)]
ENDIF ELSE BEGIN
	Nco_nepa_nstorm7_sort 	= N_ELEMENTS(co_nepa_nstorm7_sort)
	co_med = co_nepa_nstorm7_sort[(Nco_nepa_nstorm7_sort/2)] 
	lower_half = co_nepa_nstorm7_sort[0:(Nco_nepa_nstorm7_sort/2)-1]
	upper_half = co_nepa_nstorm7_sort[(Nco_nepa_nstorm7_sort/2):(Nco_nepa_nstorm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = co_nepa_nstorm7_sort[0.05*Nco_nepa_nstorm7_sort]
quartile_95 = co_nepa_nstorm7_sort[0.95*Nco_nepa_nstorm7_sort]

co_nepa_nstorm7_ptile = [quartile_05, quartile_25, co_med, quartile_75,quartile_95]

data = [[co_epa_storm7_ptile], [co_epa_nstorm7_ptile], [co_nepa_storm7_ptile], [co_nepa_nstorm7_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','con-EPA w/ Storms','con-EPA w/o Storms']
boxes = BOXPLOT(data*1.0E3, $
		TITLE		= 'July: CO', $
		XRANGE 		= [0,3000], $
		YRANGE 		= [-1, 4], $
		XTITLE 		= 'CO Concentration (ppb)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: pm
;pm_epa_storm7 = pm_storm_july[iepa_july_s]
;pm_epa_storm7_sort  = pm_epa_storm7[SORT(pm_epa_storm7)]
;IF (N_ELEMENTS(pm_epa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
;	Npm_epa_storm7_sort 	= N_ELEMENTS(pm_epa_storm7_sort)
;	pm_med = (pm_epa_storm7_sort[(Npm_epa_storm7_sort/2)-1] + pm_epa_storm7_sort[(Npm_epa_storm7_sort/2)]) / 2.0
;	lower_half = pm_epa_storm7_sort[0:(Npm_epa_storm7_sort/2)-1]
;	upper_half = pm_epa_storm7_sort[(Npm_epa_storm7_sort/2):(Npm_epa_storm7_sort-1)]
;ENDIF ELSE BEGIN
;	Npm_epa_storm7_sort 	= N_ELEMENTS(pm_epa_storm7_sort)
;	pm_med = pm_epa_storm7_sort[(Npm_epa_storm7_sort/2)] 
;	lower_half = pm_epa_storm7_sort[0:(Npm_epa_storm7_sort/2)-1]
;	upper_half = pm_epa_storm7_sort[(Npm_epa_storm7_sort/2):(Npm_epa_storm7_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = pm_epa_storm7_sort[0.05*Npm_epa_storm7_sort]
;quartile_95 = pm_epa_storm7_sort[0.95*Npm_epa_storm7_sort]
;
;pm_epa_storm7_ptile = [quartile_05, quartile_25, pm_med, quartile_75,quartile_95]
;
;
;;;;EPA exceedances without storms: pm
;pm_epa_nstorm7 = pm_nstorm_july[iepa_july_ns]
;pm_epa_nstorm7_sort  = pm_epa_nstorm7[SORT(pm_epa_nstorm7)]
;IF (N_ELEMENTS(pm_epa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
;	Npm_epa_nstorm7_sort 	= N_ELEMENTS(pm_epa_nstorm7_sort)
;	pm_med = (pm_epa_nstorm7_sort[(Npm_epa_nstorm7_sort/2)-1] + pm_epa_nstorm7_sort[(Npm_epa_nstorm7_sort/2)]) / 2.0
;	lower_half = pm_epa_nstorm7_sort[0:(Npm_epa_nstorm7_sort/2)-1]
;	upper_half = pm_epa_nstorm7_sort[(Npm_epa_nstorm7_sort/2):(Npm_epa_nstorm7_sort-1)]
;ENDIF ELSE BEGIN
;	Npm_epa_nstorm7_sort 	= N_ELEMENTS(pm_epa_nstorm7_sort)
;	pm_med = pm_epa_nstorm7_sort[(Npm_epa_nstorm7_sort/2)] 
;	lower_half = pm_epa_nstorm7_sort[0:(Npm_epa_nstorm7_sort/2)-1]
;	upper_half = pm_epa_nstorm7_sort[(Npm_epa_nstorm7_sort/2):(Npm_epa_nstorm7_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = pm_epa_nstorm7_sort[0.05*Npm_epa_nstorm7_sort]
;quartile_95 = pm_epa_nstorm7_sort[0.95*Npm_epa_nstorm7_sort]
;
;pm_epa_nstorm7_ptile = [quartile_05, quartile_25, pm_med, quartile_75,quartile_95]
;
;
;;;;pmn-EPA exceedances with storms: pm
;pm_nepa_storm7 = pm_storm_july[nepa_july_s]
;pm_nepa_storm7_sort  = pm_nepa_storm7[SORT(pm_nepa_storm7)]
;IF (N_ELEMENTS(pm_nepa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
;	Npm_nepa_storm7_sort 	= N_ELEMENTS(pm_nepa_storm7_sort)
;	pm_med = (pm_nepa_storm7_sort[(Npm_nepa_storm7_sort/2)-1] + pm_nepa_storm7_sort[(Npm_nepa_storm7_sort/2)]) / 2.0
;	lower_half = pm_nepa_storm7_sort[0:(Npm_nepa_storm7_sort/2)-1]
;	upper_half = pm_nepa_storm7_sort[(Npm_nepa_storm7_sort/2):(Npm_nepa_storm7_sort-1)]
;ENDIF ELSE BEGIN
;	Npm_nepa_storm7_sort 	= N_ELEMENTS(pm_nepa_storm7_sort)
;	pm_med = pm_nepa_storm7_sort[(Npm_nepa_storm7_sort/2)] 
;	lower_half = pm_nepa_storm7_sort[0:(Npm_nepa_storm7_sort/2)-1]
;	upper_half = pm_nepa_storm7_sort[(Npm_nepa_storm7_sort/2):(Npm_nepa_storm7_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = pm_nepa_storm7_sort[0.05*Npm_nepa_storm7_sort]
;quartile_95 = pm_nepa_storm7_sort[0.95*Npm_nepa_storm7_sort]
;
;pm_nepa_storm7_ptile = [quartile_05, quartile_25, pm_med, quartile_75,quartile_95]
;
;
;;;;pmn-EPA exceedances with storms: pm
;pm_nepa_nstorm7 = pm_nstorm_july[nepa_july_ns]
;pm_nepa_nstorm7_sort  = pm_nepa_nstorm7[SORT(pm_nepa_nstorm7)]
;IF (N_ELEMENTS(pm_nepa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
;	Npm_nepa_nstorm7_sort 	= N_ELEMENTS(pm_nepa_nstorm7_sort)
;	pm_med = (pm_nepa_nstorm7_sort[(Npm_nepa_nstorm7_sort/2)-1] + pm_nepa_nstorm7_sort[(Npm_nepa_nstorm7_sort/2)]) / 2.0
;	lower_half = pm_nepa_nstorm7_sort[0:(Npm_nepa_nstorm7_sort/2)-1]
;	upper_half = pm_nepa_nstorm7_sort[(Npm_nepa_nstorm7_sort/2):(Npm_nepa_nstorm7_sort-1)]
;ENDIF ELSE BEGIN
;	Npm_nepa_nstorm7_sort 	= N_ELEMENTS(pm_nepa_nstorm7_sort)
;	pm_med = pm_nepa_nstorm7_sort[(Npm_nepa_nstorm7_sort/2)] 
;	lower_half = pm_nepa_nstorm7_sort[0:(Npm_nepa_nstorm7_sort/2)-1]
;	upper_half = pm_nepa_nstorm7_sort[(Npm_nepa_nstorm7_sort/2):(Npm_nepa_nstorm7_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = pm_nepa_nstorm7_sort[0.05*Npm_nepa_nstorm7_sort]
;quartile_95 = pm_nepa_nstorm7_sort[0.95*Npm_nepa_nstorm7_sort]
;
;pm_nepa_nstorm7_ptile = [quartile_05, quartile_25, pm_med, quartile_75,quartile_95]
;
;data = [[pm_epa_storm7_ptile], [pm_epa_nstorm7_ptile], [pm_nepa_storm7_ptile], [pm_nepa_nstorm7_ptile]]
;ytitle = ['EPA w/ Storms', 'EPA w/o Storms','pmn-EPA w/ Storms','pmn-EPA w/o Storms']
;boxes = BOXPLOT(data, $
;		TITLE		= 'July: PM2.5', $
;		XRANGE 		= [0, 25], $
;		YRANGE 		= [-1, 4], $
;		XTITLE 		= 'PM2.5 concentration (ug/m3)', $
;		YTICKNAME 	= ytitle, $
;		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
;		FONT_SIZE   = 20, $
;		HORIZONTAL	= 1)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: temp
temp_epa_storm7 = temp_storm_july[iepa_july_s]
temp_epa_storm7_sort  = temp_epa_storm7[SORT(temp_epa_storm7)]
IF (N_ELEMENTS(temp_epa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
	Ntemp_epa_storm7_sort 	= N_ELEMENTS(temp_epa_storm7_sort)
	temp_med = (temp_epa_storm7_sort[(Ntemp_epa_storm7_sort/2)-1] + temp_epa_storm7_sort[(Ntemp_epa_storm7_sort/2)]) / 2.0
	lower_half = temp_epa_storm7_sort[0:(Ntemp_epa_storm7_sort/2)-1]
	upper_half = temp_epa_storm7_sort[(Ntemp_epa_storm7_sort/2):(Ntemp_epa_storm7_sort-1)]
ENDIF ELSE BEGIN
	Ntemp_epa_storm7_sort 	= N_ELEMENTS(temp_epa_storm7_sort)
	temp_med = temp_epa_storm7_sort[(Ntemp_epa_storm7_sort/2)] 
	lower_half = temp_epa_storm7_sort[0:(Ntemp_epa_storm7_sort/2)-1]
	upper_half = temp_epa_storm7_sort[(Ntemp_epa_storm7_sort/2):(Ntemp_epa_storm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = temp_epa_storm7_sort[0.05*Ntemp_epa_storm7_sort]
quartile_95 = temp_epa_storm7_sort[0.95*Ntemp_epa_storm7_sort]

temp_epa_storm7_ptile = [quartile_05, quartile_25, temp_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: temp
temp_epa_nstorm7 = temp_nstorm_july[iepa_july_ns]
temp_epa_nstorm7_sort  = temp_epa_nstorm7[SORT(temp_epa_nstorm7)]
IF (N_ELEMENTS(temp_epa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
	Ntemp_epa_nstorm7_sort 	= N_ELEMENTS(temp_epa_nstorm7_sort)
	temp_med = (temp_epa_nstorm7_sort[(Ntemp_epa_nstorm7_sort/2)-1] + temp_epa_nstorm7_sort[(Ntemp_epa_nstorm7_sort/2)]) / 2.0
	lower_half = temp_epa_nstorm7_sort[0:(Ntemp_epa_nstorm7_sort/2)-1]
	upper_half = temp_epa_nstorm7_sort[(Ntemp_epa_nstorm7_sort/2):(Ntemp_epa_nstorm7_sort-1)]
ENDIF ELSE BEGIN
	Ntemp_epa_nstorm7_sort 	= N_ELEMENTS(temp_epa_nstorm7_sort)
	temp_med = temp_epa_nstorm7_sort[(Ntemp_epa_nstorm7_sort/2)] 
	lower_half = temp_epa_nstorm7_sort[0:(Ntemp_epa_nstorm7_sort/2)-1]
	upper_half = temp_epa_nstorm7_sort[(Ntemp_epa_nstorm7_sort/2):(Ntemp_epa_nstorm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = temp_epa_nstorm7_sort[0.05*Ntemp_epa_nstorm7_sort]
quartile_95 = temp_epa_nstorm7_sort[0.95*Ntemp_epa_nstorm7_sort]

temp_epa_nstorm7_ptile = [quartile_05, quartile_25, temp_med, quartile_75,quartile_95]


;;;tempn-EPA exceedances with storms: temp
temp_nepa_storm7 = temp_storm_july[nepa_july_s]
temp_nepa_storm7_sort  = temp_nepa_storm7[SORT(temp_nepa_storm7)]
IF (N_ELEMENTS(temp_nepa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
	Ntemp_nepa_storm7_sort 	= N_ELEMENTS(temp_nepa_storm7_sort)
	temp_med = (temp_nepa_storm7_sort[(Ntemp_nepa_storm7_sort/2)-1] + temp_nepa_storm7_sort[(Ntemp_nepa_storm7_sort/2)]) / 2.0
	lower_half = temp_nepa_storm7_sort[0:(Ntemp_nepa_storm7_sort/2)-1]
	upper_half = temp_nepa_storm7_sort[(Ntemp_nepa_storm7_sort/2):(Ntemp_nepa_storm7_sort-1)]
ENDIF ELSE BEGIN
	Ntemp_nepa_storm7_sort 	= N_ELEMENTS(temp_nepa_storm7_sort)
	temp_med = temp_nepa_storm7_sort[(Ntemp_nepa_storm7_sort/2)] 
	lower_half = temp_nepa_storm7_sort[0:(Ntemp_nepa_storm7_sort/2)-1]
	upper_half = temp_nepa_storm7_sort[(Ntemp_nepa_storm7_sort/2):(Ntemp_nepa_storm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = temp_nepa_storm7_sort[0.05*Ntemp_nepa_storm7_sort]
quartile_95 = temp_nepa_storm7_sort[0.95*Ntemp_nepa_storm7_sort]

temp_nepa_storm7_ptile = [quartile_05, quartile_25, temp_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: temp
temp_nepa_nstorm7 = temp_nstorm_july[nepa_july_ns]
temp_nepa_nstorm7_sort  = temp_nepa_nstorm7[SORT(temp_nepa_nstorm7)]
IF (N_ELEMENTS(temp_nepa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
	Ntemp_nepa_nstorm7_sort 	= N_ELEMENTS(temp_nepa_nstorm7_sort)
	temp_med = (temp_nepa_nstorm7_sort[(Ntemp_nepa_nstorm7_sort/2)-1] + temp_nepa_nstorm7_sort[(Ntemp_nepa_nstorm7_sort/2)]) / 2.0
	lower_half = temp_nepa_nstorm7_sort[0:(Ntemp_nepa_nstorm7_sort/2)-1]
	upper_half = temp_nepa_nstorm7_sort[(Ntemp_nepa_nstorm7_sort/2):(Ntemp_nepa_nstorm7_sort-1)]
ENDIF ELSE BEGIN
	Ntemp_nepa_nstorm7_sort 	= N_ELEMENTS(temp_nepa_nstorm7_sort)
	temp_med = temp_nepa_nstorm7_sort[(Ntemp_nepa_nstorm7_sort/2)] 
	lower_half = temp_nepa_nstorm7_sort[0:(Ntemp_nepa_nstorm7_sort/2)-1]
	upper_half = temp_nepa_nstorm7_sort[(Ntemp_nepa_nstorm7_sort/2):(Ntemp_nepa_nstorm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = temp_nepa_nstorm7_sort[0.05*Ntemp_nepa_nstorm7_sort]
quartile_95 = temp_nepa_nstorm7_sort[0.95*Ntemp_nepa_nstorm7_sort]

temp_nepa_nstorm7_ptile = [quartile_05, quartile_25, temp_med, quartile_75,quartile_95]

data = [[temp_epa_storm7_ptile], [temp_epa_nstorm7_ptile], [temp_nepa_storm7_ptile], [temp_nepa_nstorm7_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','tempn-EPA w/ Storms','tempn-EPA w/o Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'july: Temperature', $
		XRANGE 		= [45, 95], $
		YRANGE 		= [-1, 4], $
		XTITLE 		= 'Temperature (deg F)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: press
;press_epa_storm7 = press_storm_july[iepa_july_s]
;press_epa_storm7_sort  = press_epa_storm7[SORT(press_epa_storm7)]
;IF (N_ELEMENTS(press_epa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
;	Npress_epa_storm7_sort 	= N_ELEMENTS(press_epa_storm7_sort)
;	press_med = (press_epa_storm7_sort[(Npress_epa_storm7_sort/2)-1] + press_epa_storm7_sort[(Npress_epa_storm7_sort/2)]) / 2.0
;	lower_half = press_epa_storm7_sort[0:(Npress_epa_storm7_sort/2)-1]
;	upper_half = press_epa_storm7_sort[(Npress_epa_storm7_sort/2):(Npress_epa_storm7_sort-1)]
;ENDIF ELSE BEGIN
;	Npress_epa_storm7_sort 	= N_ELEMENTS(press_epa_storm7_sort)
;	press_med = press_epa_storm7_sort[(Npress_epa_storm7_sort/2)] 
;	lower_half = press_epa_storm7_sort[0:(Npress_epa_storm7_sort/2)-1]
;	upper_half = press_epa_storm7_sort[(Npress_epa_storm7_sort/2):(Npress_epa_storm7_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = press_epa_storm7_sort[0.05*Npress_epa_storm7_sort]
;quartile_95 = press_epa_storm7_sort[0.95*Npress_epa_storm7_sort]
;
;press_epa_storm7_ptile = [quartile_05, quartile_25, press_med, quartile_75,quartile_95]
;
;
;;;;EPA exceedances without storms: press
;press_epa_nstorm7 = press_nstorm_july[iepa_july_ns]
;press_epa_nstorm7_sort  = press_epa_nstorm7[SORT(press_epa_nstorm7)]
;IF (N_ELEMENTS(press_epa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
;	Npress_epa_nstorm7_sort 	= N_ELEMENTS(press_epa_nstorm7_sort)
;	press_med = (press_epa_nstorm7_sort[(Npress_epa_nstorm7_sort/2)-1] + press_epa_nstorm7_sort[(Npress_epa_nstorm7_sort/2)]) / 2.0
;	lower_half = press_epa_nstorm7_sort[0:(Npress_epa_nstorm7_sort/2)-1]
;	upper_half = press_epa_nstorm7_sort[(Npress_epa_nstorm7_sort/2):(Npress_epa_nstorm7_sort-1)]
;ENDIF ELSE BEGIN
;	Npress_epa_nstorm7_sort 	= N_ELEMENTS(press_epa_nstorm7_sort)
;	press_med = press_epa_nstorm7_sort[(Npress_epa_nstorm7_sort/2)] 
;	lower_half = press_epa_nstorm7_sort[0:(Npress_epa_nstorm7_sort/2)-1]
;	upper_half = press_epa_nstorm7_sort[(Npress_epa_nstorm7_sort/2):(Npress_epa_nstorm7_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = press_epa_nstorm7_sort[0.05*Npress_epa_nstorm7_sort]
;quartile_95 = press_epa_nstorm7_sort[0.95*Npress_epa_nstorm7_sort]
;
;press_epa_nstorm7_ptile = [quartile_05, quartile_25, press_med, quartile_75,quartile_95]
;
;
;;;;pressn-EPA exceedances with storms: press
;press_nepa_storm7 = press_storm_july[nepa_july_s]
;press_nepa_storm7_sort  = press_nepa_storm7[SORT(press_nepa_storm7)]
;IF (N_ELEMENTS(press_nepa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
;	Npress_nepa_storm7_sort 	= N_ELEMENTS(press_nepa_storm7_sort)
;	press_med = (press_nepa_storm7_sort[(Npress_nepa_storm7_sort/2)-1] + press_nepa_storm7_sort[(Npress_nepa_storm7_sort/2)]) / 2.0
;	lower_half = press_nepa_storm7_sort[0:(Npress_nepa_storm7_sort/2)-1]
;	upper_half = press_nepa_storm7_sort[(Npress_nepa_storm7_sort/2):(Npress_nepa_storm7_sort-1)]
;ENDIF ELSE BEGIN
;	Npress_nepa_storm7_sort 	= N_ELEMENTS(press_nepa_storm7_sort)
;	press_med = press_nepa_storm7_sort[(Npress_nepa_storm7_sort/2)] 
;	lower_half = press_nepa_storm7_sort[0:(Npress_nepa_storm7_sort/2)-1]
;	upper_half = press_nepa_storm7_sort[(Npress_nepa_storm7_sort/2):(Npress_nepa_storm7_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = press_nepa_storm7_sort[0.05*Npress_nepa_storm7_sort]
;quartile_95 = press_nepa_storm7_sort[0.95*Npress_nepa_storm7_sort]
;
;press_nepa_storm7_ptile = [quartile_05, quartile_25, press_med, quartile_75,quartile_95]
;
;
;;;;non-EPA exceedances with storms: press
;press_nepa_nstorm7 = press_nstorm_july[nepa_july_ns]
;press_nepa_nstorm7_sort  = press_nepa_nstorm7[SORT(press_nepa_nstorm7)]
;IF (N_ELEMENTS(press_nepa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
;	Npress_nepa_nstorm7_sort 	= N_ELEMENTS(press_nepa_nstorm7_sort)
;	press_med = (press_nepa_nstorm7_sort[(Npress_nepa_nstorm7_sort/2)-1] + press_nepa_nstorm7_sort[(Npress_nepa_nstorm7_sort/2)]) / 2.0
;	lower_half = press_nepa_nstorm7_sort[0:(Npress_nepa_nstorm7_sort/2)-1]
;	upper_half = press_nepa_nstorm7_sort[(Npress_nepa_nstorm7_sort/2):(Npress_nepa_nstorm7_sort-1)]
;ENDIF ELSE BEGIN
;	Npress_nepa_nstorm7_sort 	= N_ELEMENTS(press_nepa_nstorm7_sort)
;	press_med = press_nepa_nstorm7_sort[(Npress_nepa_nstorm7_sort/2)] 
;	lower_half = press_nepa_nstorm7_sort[0:(Npress_nepa_nstorm7_sort/2)-1]
;	upper_half = press_nepa_nstorm7_sort[(Npress_nepa_nstorm7_sort/2):(Npress_nepa_nstorm7_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = press_nepa_nstorm7_sort[0.05*Npress_nepa_nstorm7_sort]
;quartile_95 = press_nepa_nstorm7_sort[0.95*Npress_nepa_nstorm7_sort]
;
;press_nepa_nstorm7_ptile = [quartile_05, quartile_25, press_med, quartile_75,quartile_95]
;
;data = [[press_epa_storm7_ptile], [press_epa_nstorm7_ptile], [press_nepa_storm7_ptile], [press_nepa_nstorm7_ptile]]
;ytitle = ['EPA w/ Storms', 'EPA w/o Storms','pressn-EPA w/ Storms','pressn-EPA w/o Storms']
;boxes = BOXPLOT(data, $
;		TITLE		= 'july: Pressure', $
;		XRANGE 		= [990, 1005], $
;		YRANGE 		= [-1, 4], $
;		XTITLE 		= 'Pressure (hPa)', $
;		YTICKNAME 	= ytitle, $
;		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
;		FONT_SIZE   = 20, $
;		HORIZONTAL	= 1)
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: wind_spd
wind_spd_epa_storm7 = wind_spd_storm_july[iepa_july_s]
wind_spd_epa_storm7_sort  = wind_spd_epa_storm7[SORT(wind_spd_epa_storm7)]
IF (N_ELEMENTS(wind_spd_epa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_spd_epa_storm7_sort 	= N_ELEMENTS(wind_spd_epa_storm7_sort)
	wind_spd_med = (wind_spd_epa_storm7_sort[(Nwind_spd_epa_storm7_sort/2)-1] + wind_spd_epa_storm7_sort[(Nwind_spd_epa_storm7_sort/2)]) / 2.0
	lower_half = wind_spd_epa_storm7_sort[0:(Nwind_spd_epa_storm7_sort/2)-1]
	upper_half = wind_spd_epa_storm7_sort[(Nwind_spd_epa_storm7_sort/2):(Nwind_spd_epa_storm7_sort-1)]
ENDIF ELSE BEGIN
	Nwind_spd_epa_storm7_sort 	= N_ELEMENTS(wind_spd_epa_storm7_sort)
	wind_spd_med = wind_spd_epa_storm7_sort[(Nwind_spd_epa_storm7_sort/2)] 
	lower_half = wind_spd_epa_storm7_sort[0:(Nwind_spd_epa_storm7_sort/2)-1]
	upper_half = wind_spd_epa_storm7_sort[(Nwind_spd_epa_storm7_sort/2):(Nwind_spd_epa_storm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_spd_epa_storm7_sort[0.05*Nwind_spd_epa_storm7_sort]
quartile_95 = wind_spd_epa_storm7_sort[0.95*Nwind_spd_epa_storm7_sort]

wind_spd_epa_storm7_ptile = [quartile_05, quartile_25, wind_spd_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: wind_spd
wind_spd_epa_nstorm7 = wind_spd_nstorm_july[iepa_july_ns]
wind_spd_epa_nstorm7_sort  = wind_spd_epa_nstorm7[SORT(wind_spd_epa_nstorm7)]
IF (N_ELEMENTS(wind_spd_epa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_spd_epa_nstorm7_sort 	= N_ELEMENTS(wind_spd_epa_nstorm7_sort)
	wind_spd_med = (wind_spd_epa_nstorm7_sort[(Nwind_spd_epa_nstorm7_sort/2)-1] + wind_spd_epa_nstorm7_sort[(Nwind_spd_epa_nstorm7_sort/2)]) / 2.0
	lower_half = wind_spd_epa_nstorm7_sort[0:(Nwind_spd_epa_nstorm7_sort/2)-1]
	upper_half = wind_spd_epa_nstorm7_sort[(Nwind_spd_epa_nstorm7_sort/2):(Nwind_spd_epa_nstorm7_sort-1)]
ENDIF ELSE BEGIN
	Nwind_spd_epa_nstorm7_sort 	= N_ELEMENTS(wind_spd_epa_nstorm7_sort)
	wind_spd_med = wind_spd_epa_nstorm7_sort[(Nwind_spd_epa_nstorm7_sort/2)] 
	lower_half = wind_spd_epa_nstorm7_sort[0:(Nwind_spd_epa_nstorm7_sort/2)-1]
	upper_half = wind_spd_epa_nstorm7_sort[(Nwind_spd_epa_nstorm7_sort/2):(Nwind_spd_epa_nstorm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_spd_epa_nstorm7_sort[0.05*Nwind_spd_epa_nstorm7_sort]
quartile_95 = wind_spd_epa_nstorm7_sort[0.95*Nwind_spd_epa_nstorm7_sort]

wind_spd_epa_nstorm7_ptile = [quartile_05, quartile_25, wind_spd_med, quartile_75,quartile_95]


;;;wind_spdn-EPA exceedances with storms: wind_spd
wind_spd_nepa_storm7 = wind_spd_storm_july[nepa_july_s]
wind_spd_nepa_storm7_sort  = wind_spd_nepa_storm7[SORT(wind_spd_nepa_storm7)]
IF (N_ELEMENTS(wind_spd_nepa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_spd_nepa_storm7_sort 	= N_ELEMENTS(wind_spd_nepa_storm7_sort)
	wind_spd_med = (wind_spd_nepa_storm7_sort[(Nwind_spd_nepa_storm7_sort/2)-1] + wind_spd_nepa_storm7_sort[(Nwind_spd_nepa_storm7_sort/2)]) / 2.0
	lower_half = wind_spd_nepa_storm7_sort[0:(Nwind_spd_nepa_storm7_sort/2)-1]
	upper_half = wind_spd_nepa_storm7_sort[(Nwind_spd_nepa_storm7_sort/2):(Nwind_spd_nepa_storm7_sort-1)]
ENDIF ELSE BEGIN
	Nwind_spd_nepa_storm7_sort 	= N_ELEMENTS(wind_spd_nepa_storm7_sort)
	wind_spd_med = wind_spd_nepa_storm7_sort[(Nwind_spd_nepa_storm7_sort/2)] 
	lower_half = wind_spd_nepa_storm7_sort[0:(Nwind_spd_nepa_storm7_sort/2)-1]
	upper_half = wind_spd_nepa_storm7_sort[(Nwind_spd_nepa_storm7_sort/2):(Nwind_spd_nepa_storm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_spd_nepa_storm7_sort[0.05*Nwind_spd_nepa_storm7_sort]
quartile_95 = wind_spd_nepa_storm7_sort[0.95*Nwind_spd_nepa_storm7_sort]

wind_spd_nepa_storm7_ptile = [quartile_05, quartile_25, wind_spd_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: wind_spd
wind_spd_nepa_nstorm7 = wind_spd_nstorm_july[nepa_july_ns]
wind_spd_nepa_nstorm7_sort  = wind_spd_nepa_nstorm7[SORT(wind_spd_nepa_nstorm7)]
IF (N_ELEMENTS(wind_spd_nepa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_spd_nepa_nstorm7_sort 	= N_ELEMENTS(wind_spd_nepa_nstorm7_sort)
	wind_spd_med = (wind_spd_nepa_nstorm7_sort[(Nwind_spd_nepa_nstorm7_sort/2)-1] + wind_spd_nepa_nstorm7_sort[(Nwind_spd_nepa_nstorm7_sort/2)]) / 2.0
	lower_half = wind_spd_nepa_nstorm7_sort[0:(Nwind_spd_nepa_nstorm7_sort/2)-1]
	upper_half = wind_spd_nepa_nstorm7_sort[(Nwind_spd_nepa_nstorm7_sort/2):(Nwind_spd_nepa_nstorm7_sort-1)]
ENDIF ELSE BEGIN
	Nwind_spd_nepa_nstorm7_sort 	= N_ELEMENTS(wind_spd_nepa_nstorm7_sort)
	wind_spd_med = wind_spd_nepa_nstorm7_sort[(Nwind_spd_nepa_nstorm7_sort/2)] 
	lower_half = wind_spd_nepa_nstorm7_sort[0:(Nwind_spd_nepa_nstorm7_sort/2)-1]
	upper_half = wind_spd_nepa_nstorm7_sort[(Nwind_spd_nepa_nstorm7_sort/2):(Nwind_spd_nepa_nstorm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_spd_nepa_nstorm7_sort[0.05*Nwind_spd_nepa_nstorm7_sort]
quartile_95 = wind_spd_nepa_nstorm7_sort[0.95*Nwind_spd_nepa_nstorm7_sort]

wind_spd_nepa_nstorm7_ptile = [quartile_05, quartile_25, wind_spd_med, quartile_75,quartile_95]

data = [[wind_spd_epa_storm7_ptile], [wind_spd_epa_nstorm7_ptile], [wind_spd_nepa_storm7_ptile], [wind_spd_nepa_nstorm7_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','non-EPA w/ Storms','non-EPA w/o Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'july: Wind Speed', $
		XRANGE 		= [0, 10], $
		YRANGE 		= [-1, 4], $
		XTITLE 		= 'Wind Speed (m/s)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: wind_dir
wind_dir_epa_storm7 = wind_dir_storm_july[iepa_july_s]
wind_dir_epa_storm7_sort  = wind_dir_epa_storm7[SORT(wind_dir_epa_storm7)]
IF (N_ELEMENTS(wind_dir_epa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_dir_epa_storm7_sort 	= N_ELEMENTS(wind_dir_epa_storm7_sort)
	wind_dir_med = (wind_dir_epa_storm7_sort[(Nwind_dir_epa_storm7_sort/2)-1] + wind_dir_epa_storm7_sort[(Nwind_dir_epa_storm7_sort/2)]) / 2.0
	lower_half = wind_dir_epa_storm7_sort[0:(Nwind_dir_epa_storm7_sort/2)-1]
	upper_half = wind_dir_epa_storm7_sort[(Nwind_dir_epa_storm7_sort/2):(Nwind_dir_epa_storm7_sort-1)]
ENDIF ELSE BEGIN
	Nwind_dir_epa_storm7_sort 	= N_ELEMENTS(wind_dir_epa_storm7_sort)
	wind_dir_med = wind_dir_epa_storm7_sort[(Nwind_dir_epa_storm7_sort/2)] 
	lower_half = wind_dir_epa_storm7_sort[0:(Nwind_dir_epa_storm7_sort/2)-1]
	upper_half = wind_dir_epa_storm7_sort[(Nwind_dir_epa_storm7_sort/2):(Nwind_dir_epa_storm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_dir_epa_storm7_sort[0.05*Nwind_dir_epa_storm7_sort]
quartile_95 = wind_dir_epa_storm7_sort[0.95*Nwind_dir_epa_storm7_sort]

wind_dir_epa_storm7_ptile = [quartile_05, quartile_25, wind_dir_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: wind_dir
wind_dir_epa_nstorm7 = wind_dir_nstorm_july[iepa_july_ns]
wind_dir_epa_nstorm7_sort  = wind_dir_epa_nstorm7[SORT(wind_dir_epa_nstorm7)]
IF (N_ELEMENTS(wind_dir_epa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_dir_epa_nstorm7_sort 	= N_ELEMENTS(wind_dir_epa_nstorm7_sort)
	wind_dir_med = (wind_dir_epa_nstorm7_sort[(Nwind_dir_epa_nstorm7_sort/2)-1] + wind_dir_epa_nstorm7_sort[(Nwind_dir_epa_nstorm7_sort/2)]) / 2.0
	lower_half = wind_dir_epa_nstorm7_sort[0:(Nwind_dir_epa_nstorm7_sort/2)-1]
	upper_half = wind_dir_epa_nstorm7_sort[(Nwind_dir_epa_nstorm7_sort/2):(Nwind_dir_epa_nstorm7_sort-1)]
ENDIF ELSE BEGIN
	Nwind_dir_epa_nstorm7_sort 	= N_ELEMENTS(wind_dir_epa_nstorm7_sort)
	wind_dir_med = wind_dir_epa_nstorm7_sort[(Nwind_dir_epa_nstorm7_sort/2)] 
	lower_half = wind_dir_epa_nstorm7_sort[0:(Nwind_dir_epa_nstorm7_sort/2)-1]
	upper_half = wind_dir_epa_nstorm7_sort[(Nwind_dir_epa_nstorm7_sort/2):(Nwind_dir_epa_nstorm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_dir_epa_nstorm7_sort[0.05*Nwind_dir_epa_nstorm7_sort]
quartile_95 = wind_dir_epa_nstorm7_sort[0.95*Nwind_dir_epa_nstorm7_sort]

wind_dir_epa_nstorm7_ptile = [quartile_05, quartile_25, wind_dir_med, quartile_75,quartile_95]


;;;wind_dirn-EPA exceedances with storms: wind_dir
wind_dir_nepa_storm7 = wind_dir_storm_july[nepa_july_s]
wind_dir_nepa_storm7_sort  = wind_dir_nepa_storm7[SORT(wind_dir_nepa_storm7)]
IF (N_ELEMENTS(wind_dir_nepa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_dir_nepa_storm7_sort 	= N_ELEMENTS(wind_dir_nepa_storm7_sort)
	wind_dir_med = (wind_dir_nepa_storm7_sort[(Nwind_dir_nepa_storm7_sort/2)-1] + wind_dir_nepa_storm7_sort[(Nwind_dir_nepa_storm7_sort/2)]) / 2.0
	lower_half = wind_dir_nepa_storm7_sort[0:(Nwind_dir_nepa_storm7_sort/2)-1]
	upper_half = wind_dir_nepa_storm7_sort[(Nwind_dir_nepa_storm7_sort/2):(Nwind_dir_nepa_storm7_sort-1)]
ENDIF ELSE BEGIN
	Nwind_dir_nepa_storm7_sort 	= N_ELEMENTS(wind_dir_nepa_storm7_sort)
	wind_dir_med = wind_dir_nepa_storm7_sort[(Nwind_dir_nepa_storm7_sort/2)] 
	lower_half = wind_dir_nepa_storm7_sort[0:(Nwind_dir_nepa_storm7_sort/2)-1]
	upper_half = wind_dir_nepa_storm7_sort[(Nwind_dir_nepa_storm7_sort/2):(Nwind_dir_nepa_storm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_dir_nepa_storm7_sort[0.05*Nwind_dir_nepa_storm7_sort]
quartile_95 = wind_dir_nepa_storm7_sort[0.95*Nwind_dir_nepa_storm7_sort]

wind_dir_nepa_storm7_ptile = [quartile_05, quartile_25, wind_dir_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: wind_dir
wind_dir_nepa_nstorm7 = wind_dir_nstorm_july[nepa_july_ns]
wind_dir_nepa_nstorm7_sort  = wind_dir_nepa_nstorm7[SORT(wind_dir_nepa_nstorm7)]
IF (N_ELEMENTS(wind_dir_nepa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_dir_nepa_nstorm7_sort 	= N_ELEMENTS(wind_dir_nepa_nstorm7_sort)
	wind_dir_med = (wind_dir_nepa_nstorm7_sort[(Nwind_dir_nepa_nstorm7_sort/2)-1] + wind_dir_nepa_nstorm7_sort[(Nwind_dir_nepa_nstorm7_sort/2)]) / 2.0
	lower_half = wind_dir_nepa_nstorm7_sort[0:(Nwind_dir_nepa_nstorm7_sort/2)-1]
	upper_half = wind_dir_nepa_nstorm7_sort[(Nwind_dir_nepa_nstorm7_sort/2):(Nwind_dir_nepa_nstorm7_sort-1)]
ENDIF ELSE BEGIN
	Nwind_dir_nepa_nstorm7_sort 	= N_ELEMENTS(wind_dir_nepa_nstorm7_sort)
	wind_dir_med = wind_dir_nepa_nstorm7_sort[(Nwind_dir_nepa_nstorm7_sort/2)] 
	lower_half = wind_dir_nepa_nstorm7_sort[0:(Nwind_dir_nepa_nstorm7_sort/2)-1]
	upper_half = wind_dir_nepa_nstorm7_sort[(Nwind_dir_nepa_nstorm7_sort/2):(Nwind_dir_nepa_nstorm7_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_dir_nepa_nstorm7_sort[0.05*Nwind_dir_nepa_nstorm7_sort]
quartile_95 = wind_dir_nepa_nstorm7_sort[0.95*Nwind_dir_nepa_nstorm7_sort]

wind_dir_nepa_nstorm7_ptile = [quartile_05, quartile_25, wind_dir_med, quartile_75,quartile_95]

data = [[wind_dir_epa_storm7_ptile], [wind_dir_epa_nstorm7_ptile], [wind_dir_nepa_storm7_ptile], [wind_dir_nepa_nstorm7_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','non-EPA w/ Storms','non-EPA w/o Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'July: Wind Direction', $
		XRANGE 		= [0, 360], $
		YRANGE 		= [-1, 4], $
		XTITLE 		= 'Wind Dir. (deg)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: rh
;rh_epa_storm7 = rh_storm_july[iepa_july_s]
;rh_epa_storm7_sort  = rh_epa_storm7[SORT(rh_epa_storm7)]
;IF (N_ELEMENTS(rh_epa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
;	Nrh_epa_storm7_sort 	= N_ELEMENTS(rh_epa_storm7_sort)
;	rh_med = (rh_epa_storm7_sort[(Nrh_epa_storm7_sort/2)-1] + rh_epa_storm7_sort[(Nrh_epa_storm7_sort/2)]) / 2.0
;	lower_half = rh_epa_storm7_sort[0:(Nrh_epa_storm7_sort/2)-1]
;	upper_half = rh_epa_storm7_sort[(Nrh_epa_storm7_sort/2):(Nrh_epa_storm7_sort-1)]
;ENDIF ELSE BEGIN
;	Nrh_epa_storm7_sort 	= N_ELEMENTS(rh_epa_storm7_sort)
;	rh_med = rh_epa_storm7_sort[(Nrh_epa_storm7_sort/2)] 
;	lower_half = rh_epa_storm7_sort[0:(Nrh_epa_storm7_sort/2)-1]
;	upper_half = rh_epa_storm7_sort[(Nrh_epa_storm7_sort/2):(Nrh_epa_storm7_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = rh_epa_storm7_sort[0.05*Nrh_epa_storm7_sort]
;quartile_95 = rh_epa_storm7_sort[0.95*Nrh_epa_storm7_sort]
;
;rh_epa_storm7_ptile = [quartile_05, quartile_25, rh_med, quartile_75,quartile_95]
;
;
;;;;EPA exceedances without storms: rh
;rh_epa_nstorm7 = rh_nstorm_july[iepa_july_ns]
;rh_epa_nstorm7_sort  = rh_epa_nstorm7[SORT(rh_epa_nstorm7)]
;IF (N_ELEMENTS(rh_epa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
;	Nrh_epa_nstorm7_sort 	= N_ELEMENTS(rh_epa_nstorm7_sort)
;	rh_med = (rh_epa_nstorm7_sort[(Nrh_epa_nstorm7_sort/2)-1] + rh_epa_nstorm7_sort[(Nrh_epa_nstorm7_sort/2)]) / 2.0
;	lower_half = rh_epa_nstorm7_sort[0:(Nrh_epa_nstorm7_sort/2)-1]
;	upper_half = rh_epa_nstorm7_sort[(Nrh_epa_nstorm7_sort/2):(Nrh_epa_nstorm7_sort-1)]
;ENDIF ELSE BEGIN
;	Nrh_epa_nstorm7_sort 	= N_ELEMENTS(rh_epa_nstorm7_sort)
;	rh_med = rh_epa_nstorm7_sort[(Nrh_epa_nstorm7_sort/2)] 
;	lower_half = rh_epa_nstorm7_sort[0:(Nrh_epa_nstorm7_sort/2)-1]
;	upper_half = rh_epa_nstorm7_sort[(Nrh_epa_nstorm7_sort/2):(Nrh_epa_nstorm7_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = rh_epa_nstorm7_sort[0.05*Nrh_epa_nstorm7_sort]
;quartile_95 = rh_epa_nstorm7_sort[0.95*Nrh_epa_nstorm7_sort]
;
;rh_epa_nstorm7_ptile = [quartile_05, quartile_25, rh_med, quartile_75,quartile_95]
;
;
;;;;rhn-EPA exceedances with storms: rh
;rh_nepa_storm7 = rh_storm_july[nepa_july_s]
;rh_nepa_storm7_sort  = rh_nepa_storm7[SORT(rh_nepa_storm7)]
;IF (N_ELEMENTS(rh_nepa_storm7_sort) MOD 2 EQ 0) THEN BEGIN
;	Nrh_nepa_storm7_sort 	= N_ELEMENTS(rh_nepa_storm7_sort)
;	rh_med = (rh_nepa_storm7_sort[(Nrh_nepa_storm7_sort/2)-1] + rh_nepa_storm7_sort[(Nrh_nepa_storm7_sort/2)]) / 2.0
;	lower_half = rh_nepa_storm7_sort[0:(Nrh_nepa_storm7_sort/2)-1]
;	upper_half = rh_nepa_storm7_sort[(Nrh_nepa_storm7_sort/2):(Nrh_nepa_storm7_sort-1)]
;ENDIF ELSE BEGIN
;	Nrh_nepa_storm7_sort 	= N_ELEMENTS(rh_nepa_storm7_sort)
;	rh_med = rh_nepa_storm7_sort[(Nrh_nepa_storm7_sort/2)] 
;	lower_half = rh_nepa_storm7_sort[0:(Nrh_nepa_storm7_sort/2)-1]
;	upper_half = rh_nepa_storm7_sort[(Nrh_nepa_storm7_sort/2):(Nrh_nepa_storm7_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = rh_nepa_storm7_sort[0.05*Nrh_nepa_storm7_sort]
;quartile_95 = rh_nepa_storm7_sort[0.95*Nrh_nepa_storm7_sort]
;
;rh_nepa_storm7_ptile = [quartile_05, quartile_25, rh_med, quartile_75,quartile_95]
;
;
;;;;non-EPA exceedances with storms: rh
;rh_nepa_nstorm7 = rh_nstorm_july[nepa_july_ns]
;rh_nepa_nstorm7_sort  = rh_nepa_nstorm7[SORT(rh_nepa_nstorm7)]
;IF (N_ELEMENTS(rh_nepa_nstorm7_sort) MOD 2 EQ 0) THEN BEGIN
;	Nrh_nepa_nstorm7_sort 	= N_ELEMENTS(rh_nepa_nstorm7_sort)
;	rh_med = (rh_nepa_nstorm7_sort[(Nrh_nepa_nstorm7_sort/2)-1] + rh_nepa_nstorm7_sort[(Nrh_nepa_nstorm7_sort/2)]) / 2.0
;	lower_half = rh_nepa_nstorm7_sort[0:(Nrh_nepa_nstorm7_sort/2)-1]
;	upper_half = rh_nepa_nstorm7_sort[(Nrh_nepa_nstorm7_sort/2):(Nrh_nepa_nstorm7_sort-1)]
;ENDIF ELSE BEGIN
;	Nrh_nepa_nstorm7_sort 	= N_ELEMENTS(rh_nepa_nstorm7_sort)
;	rh_med = rh_nepa_nstorm7_sort[(Nrh_nepa_nstorm7_sort/2)] 
;	lower_half = rh_nepa_nstorm7_sort[0:(Nrh_nepa_nstorm7_sort/2)-1]
;	upper_half = rh_nepa_nstorm7_sort[(Nrh_nepa_nstorm7_sort/2):(Nrh_nepa_nstorm7_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = rh_nepa_nstorm7_sort[0.05*Nrh_nepa_nstorm7_sort]
;quartile_95 = rh_nepa_nstorm7_sort[0.95*Nrh_nepa_nstorm7_sort]
;
;rh_nepa_nstorm7_ptile = [quartile_05, quartile_25, rh_med, quartile_75,quartile_95]
;
;data = [[rh_epa_storm7_ptile], [rh_epa_nstorm7_ptile], [rh_nepa_storm7_ptile], [rh_nepa_nstorm7_ptile]]
;ytitle = ['EPA w/ Storms', 'EPA w/o Storms','non-EPA w/ Storms','non-EPA w/o Storms']
;boxes = BOXPLOT(data, $
;		TITLE		= 'July: RH', $
;		XRANGE 		= [50, 90], $
;		YRANGE 		= [-1, 4], $
;		XTITLE 		= 'RH (%)', $
;		YTICKNAME 	= ytitle, $
;		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
;		FONT_SIZE   = 20, $
;		HORIZONTAL	= 1)
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;OCTOBER
o3dm_epa_storm10 = o3dm_storm_oct[iepa_oct_s]
o3dm_epa_storm10_sort  = o3dm_epa_storm10[SORT(o3dm_epa_storm10)]
IF (N_ELEMENTS(o3dm_epa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
	No3dm_epa_storm10_sort 	= N_ELEMENTS(o3dm_epa_storm10_sort)
	o3dm_med = (o3dm_epa_storm10_sort[(No3dm_epa_storm10_sort/2)-1] + o3dm_epa_storm10_sort[(No3dm_epa_storm10_sort/2)]) / 2.0
	lower_half = o3dm_epa_storm10_sort[0:(No3dm_epa_storm10_sort/2)-1]
	upper_half = o3dm_epa_storm10_sort[(No3dm_epa_storm10_sort/2):(No3dm_epa_storm10_sort-1)]
ENDIF ELSE BEGIN
	No3dm_epa_storm10_sort 	= N_ELEMENTS(o3dm_epa_storm10_sort)
	o3dm_med = o3dm_epa_storm10_sort[(No3dm_epa_storm10_sort/2)] 
	lower_half = o3dm_epa_storm10_sort[0:(No3dm_epa_storm10_sort/2)-1]
	upper_half = o3dm_epa_storm10_sort[(No3dm_epa_storm10_sort/2):(No3dm_epa_storm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3dm_epa_storm10_sort[0.05*No3dm_epa_storm10_sort]
quartile_95 = o3dm_epa_storm10_sort[0.95*No3dm_epa_storm10_sort]

o3dm_epa_storm10_ptile = [quartile_05, quartile_25, o3dm_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: OZONE
o3dm_epa_nstorm10 = o3dm_nstorm_oct[iepa_oct_ns]
o3dm_epa_nstorm10_sort  = o3dm_epa_nstorm10[SORT(o3dm_epa_nstorm10)]
IF (N_ELEMENTS(o3dm_epa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
	No3dm_epa_nstorm10_sort 	= N_ELEMENTS(o3dm_epa_nstorm10_sort)
	o3dm_med = (o3dm_epa_nstorm10_sort[(No3dm_epa_nstorm10_sort/2)-1] + o3dm_epa_nstorm10_sort[(No3dm_epa_nstorm10_sort/2)]) / 2.0
	lower_half = o3dm_epa_nstorm10_sort[0:(No3dm_epa_nstorm10_sort/2)-1]
	upper_half = o3dm_epa_nstorm10_sort[(No3dm_epa_nstorm10_sort/2):(No3dm_epa_nstorm10_sort-1)]
ENDIF ELSE BEGIN
	No3dm_epa_nstorm10_sort 	= N_ELEMENTS(o3dm_epa_nstorm10_sort)
	o3dm_med = o3dm_epa_nstorm10_sort[(No3dm_epa_nstorm10_sort/2)] 
	lower_half = o3dm_epa_nstorm10_sort[0:(No3dm_epa_nstorm10_sort/2)-1]
	upper_half = o3dm_epa_nstorm10_sort[(No3dm_epa_nstorm10_sort/2):(No3dm_epa_nstorm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3dm_epa_nstorm10_sort[0.05*No3dm_epa_nstorm10_sort]
quartile_95 = o3dm_epa_nstorm10_sort[0.95*No3dm_epa_nstorm10_sort]

o3dm_epa_nstorm10_ptile = [quartile_05, quartile_25, o3dm_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: OZONE
o3dm_nepa_storm10 = o3dm_storm_oct[nepa_oct_s]
o3dm_nepa_storm10_sort  = o3dm_nepa_storm10[SORT(o3dm_nepa_storm10)]
IF (N_ELEMENTS(o3dm_nepa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
	No3dm_nepa_storm10_sort 	= N_ELEMENTS(o3dm_nepa_storm10_sort)
	o3dm_med = (o3dm_nepa_storm10_sort[(No3dm_nepa_storm10_sort/2)-1] + o3dm_nepa_storm10_sort[(No3dm_nepa_storm10_sort/2)]) / 2.0
	lower_half = o3dm_nepa_storm10_sort[0:(No3dm_nepa_storm10_sort/2)-1]
	upper_half = o3dm_nepa_storm10_sort[(No3dm_nepa_storm10_sort/2):(No3dm_nepa_storm10_sort-1)]
ENDIF ELSE BEGIN
	No3dm_nepa_storm10_sort 	= N_ELEMENTS(o3dm_nepa_storm10_sort)
	o3dm_med = o3dm_nepa_storm10_sort[(No3dm_nepa_storm10_sort/2)] 
	lower_half = o3dm_nepa_storm10_sort[0:(No3dm_nepa_storm10_sort/2)-1]
	upper_half = o3dm_nepa_storm10_sort[(No3dm_nepa_storm10_sort/2):(No3dm_nepa_storm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3dm_nepa_storm10_sort[0.05*No3dm_nepa_storm10_sort]
quartile_95 = o3dm_nepa_storm10_sort[0.95*No3dm_nepa_storm10_sort]

o3dm_nepa_storm10_ptile = [quartile_05, quartile_25, o3dm_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: OZONE
o3dm_nepa_nstorm10 = o3dm_nstorm_oct[nepa_oct_ns]
o3dm_nepa_nstorm10_sort  = o3dm_nepa_nstorm10[SORT(o3dm_nepa_nstorm10)]
IF (N_ELEMENTS(o3dm_nepa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
	No3dm_nepa_nstorm10_sort 	= N_ELEMENTS(o3dm_nepa_nstorm10_sort)
	o3dm_med = (o3dm_nepa_nstorm10_sort[(No3dm_nepa_nstorm10_sort/2)-1] + o3dm_nepa_nstorm10_sort[(No3dm_nepa_nstorm10_sort/2)]) / 2.0
	lower_half = o3dm_nepa_nstorm10_sort[0:(No3dm_nepa_nstorm10_sort/2)-1]
	upper_half = o3dm_nepa_nstorm10_sort[(No3dm_nepa_nstorm10_sort/2):(No3dm_nepa_nstorm10_sort-1)]
ENDIF ELSE BEGIN
	No3dm_nepa_nstorm10_sort 	= N_ELEMENTS(o3dm_nepa_nstorm10_sort)
	o3dm_med = o3dm_nepa_nstorm10_sort[(No3dm_nepa_nstorm10_sort/2)] 
	lower_half = o3dm_nepa_nstorm10_sort[0:(No3dm_nepa_nstorm10_sort/2)-1]
	upper_half = o3dm_nepa_nstorm10_sort[(No3dm_nepa_nstorm10_sort/2):(No3dm_nepa_nstorm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3dm_nepa_nstorm10_sort[0.05*No3dm_nepa_nstorm10_sort]
quartile_95 = o3dm_nepa_nstorm10_sort[0.95*No3dm_nepa_nstorm10_sort]

o3dm_nepa_nstorm10_ptile = [quartile_05, quartile_25, o3dm_med, quartile_75,quartile_95]

data = [[o3dm_epa_storm10_ptile], [o3dm_epa_nstorm10_ptile], [o3dm_nepa_storm10_ptile], [o3dm_nepa_nstorm10_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','non-EPA w/ Storms','non-EPA w/o Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'oct: Ozone Daily Max', $
		XRANGE 		= [10,140], $
		YRANGE 		= [-1, 4], $
		XTITLE 		= 'O3 Concentration (ppb)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
o3_epa_storm10 = o3_storm_oct[iepa_oct_s]
o3_epa_storm10_sort  = o3_epa_storm10[SORT(o3_epa_storm10)]
IF (N_ELEMENTS(o3_epa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
	No3_epa_storm10_sort 	= N_ELEMENTS(o3_epa_storm10_sort)
	o3_med = (o3_epa_storm10_sort[(No3_epa_storm10_sort/2)-1] + o3_epa_storm10_sort[(No3_epa_storm10_sort/2)]) / 2.0
	lower_half = o3_epa_storm10_sort[0:(No3_epa_storm10_sort/2)-1]
	upper_half = o3_epa_storm10_sort[(No3_epa_storm10_sort/2):(No3_epa_storm10_sort-1)]
ENDIF ELSE BEGIN
	No3_epa_storm10_sort 	= N_ELEMENTS(o3_epa_storm10_sort)
	o3_med = o3_epa_storm10_sort[(No3_epa_storm10_sort/2)] 
	lower_half = o3_epa_storm10_sort[0:(No3_epa_storm10_sort/2)-1]
	upper_half = o3_epa_storm10_sort[(No3_epa_storm10_sort/2):(No3_epa_storm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_epa_storm10_sort[0.05*No3_epa_storm10_sort]
quartile_95 = o3_epa_storm10_sort[0.95*No3_epa_storm10_sort]

o3_epa_storm10_ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]

;;;EPA exceedances without storms: OZONE
o3_epa_nstorm10 = o3_nstorm_oct[iepa_oct_ns]
o3_epa_nstorm10_sort  = o3_epa_nstorm10[SORT(o3_epa_nstorm10)]
IF (N_ELEMENTS(o3_epa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
	No3_epa_nstorm10_sort 	= N_ELEMENTS(o3_epa_nstorm10_sort)
	o3_med = (o3_epa_nstorm10_sort[(No3_epa_nstorm10_sort/2)-1] + o3_epa_nstorm10_sort[(No3_epa_nstorm10_sort/2)]) / 2.0
	lower_half = o3_epa_nstorm10_sort[0:(No3_epa_nstorm10_sort/2)-1]
	upper_half = o3_epa_nstorm10_sort[(No3_epa_nstorm10_sort/2):(No3_epa_nstorm10_sort-1)]
ENDIF ELSE BEGIN
	No3_epa_nstorm10_sort 	= N_ELEMENTS(o3_epa_nstorm10_sort)
	o3_med = o3_epa_nstorm10_sort[(No3_epa_nstorm10_sort/2)] 
	lower_half = o3_epa_nstorm10_sort[0:(No3_epa_nstorm10_sort/2)-1]
	upper_half = o3_epa_nstorm10_sort[(No3_epa_nstorm10_sort/2):(No3_epa_nstorm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_epa_nstorm10_sort[0.05*No3_epa_nstorm10_sort]
quartile_95 = o3_epa_nstorm10_sort[0.95*No3_epa_nstorm10_sort]

o3_epa_nstorm10_ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: OZONE
o3_nepa_storm10 = o3_storm_oct[nepa_oct_s]
o3_nepa_storm10_sort  = o3_nepa_storm10[SORT(o3_nepa_storm10)]
IF (N_ELEMENTS(o3_nepa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
	No3_nepa_storm10_sort 	= N_ELEMENTS(o3_nepa_storm10_sort)
	o3_med = (o3_nepa_storm10_sort[(No3_nepa_storm10_sort/2)-1] + o3_nepa_storm10_sort[(No3_nepa_storm10_sort/2)]) / 2.0
	lower_half = o3_nepa_storm10_sort[0:(No3_nepa_storm10_sort/2)-1]
	upper_half = o3_nepa_storm10_sort[(No3_nepa_storm10_sort/2):(No3_nepa_storm10_sort-1)]
ENDIF ELSE BEGIN
	No3_nepa_storm10_sort 	= N_ELEMENTS(o3_nepa_storm10_sort)
	o3_med = o3_nepa_storm10_sort[(No3_nepa_storm10_sort/2)] 
	lower_half = o3_nepa_storm10_sort[0:(No3_nepa_storm10_sort/2)-1]
	upper_half = o3_nepa_storm10_sort[(No3_nepa_storm10_sort/2):(No3_nepa_storm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_nepa_storm10_sort[0.05*No3_nepa_storm10_sort]
quartile_95 = o3_nepa_storm10_sort[0.95*No3_nepa_storm10_sort]

o3_nepa_storm10_ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: OZONE
o3_nepa_nstorm10 = o3_nstorm_oct[nepa_oct_ns]
o3_nepa_nstorm10_sort  = o3_nepa_nstorm10[SORT(o3_nepa_nstorm10)]
IF (N_ELEMENTS(o3_nepa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
	No3_nepa_nstorm10_sort 	= N_ELEMENTS(o3_nepa_nstorm10_sort)
	o3_med = (o3_nepa_nstorm10_sort[(No3_nepa_nstorm10_sort/2)-1] + o3_nepa_nstorm10_sort[(No3_nepa_nstorm10_sort/2)]) / 2.0
	lower_half = o3_nepa_nstorm10_sort[0:(No3_nepa_nstorm10_sort/2)-1]
	upper_half = o3_nepa_nstorm10_sort[(No3_nepa_nstorm10_sort/2):(No3_nepa_nstorm10_sort-1)]
ENDIF ELSE BEGIN
	No3_nepa_nstorm10_sort 	= N_ELEMENTS(o3_nepa_nstorm10_sort)
	o3_med = o3_nepa_nstorm10_sort[(No3_nepa_nstorm10_sort/2)] 
	lower_half = o3_nepa_nstorm10_sort[0:(No3_nepa_nstorm10_sort/2)-1]
	upper_half = o3_nepa_nstorm10_sort[(No3_nepa_nstorm10_sort/2):(No3_nepa_nstorm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_nepa_nstorm10_sort[0.05*No3_nepa_nstorm10_sort]
quartile_95 = o3_nepa_nstorm10_sort[0.95*No3_nepa_nstorm10_sort]

o3_nepa_nstorm10_ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]

data = [[o3_epa_storm10_ptile], [o3_epa_nstorm10_ptile], [o3_nepa_storm10_ptile], [o3_nepa_nstorm10_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','non-EPA w/ Storms','non-EPA w/o Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'oct: Ozone', $
		XRANGE 		= [0,80], $
		YRANGE 		= [-1, 4], $
		XTITLE 		= 'O3 Concentration (ppb)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: voc
;voc_epa_storm10 = voc_storm_oct[iepa_oct_s]
;voc_epa_storm10_sort  = voc_epa_storm10[SORT(voc_epa_storm10)]
;IF (N_ELEMENTS(voc_epa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
;	Nvoc_epa_storm10_sort 	= N_ELEMENTS(voc_epa_storm10_sort)
;	voc_med = (voc_epa_storm10_sort[(Nvoc_epa_storm10_sort/2)-1] + voc_epa_storm10_sort[(Nvoc_epa_storm10_sort/2)]) / 2.0
;	lower_half = voc_epa_storm10_sort[0:(Nvoc_epa_storm10_sort/2)-1]
;	upper_half = voc_epa_storm10_sort[(Nvoc_epa_storm10_sort/2):(Nvoc_epa_storm10_sort-1)]
;ENDIF ELSE BEGIN
;	Nvoc_epa_storm10_sort 	= N_ELEMENTS(voc_epa_storm10_sort)
;	voc_med = voc_epa_storm10_sort[(Nvoc_epa_storm10_sort/2)] 
;	lower_half = voc_epa_storm10_sort[0:(Nvoc_epa_storm10_sort/2)-1]
;	upper_half = voc_epa_storm10_sort[(Nvoc_epa_storm10_sort/2):(Nvoc_epa_storm10_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = voc_epa_storm10_sort[0.05*Nvoc_epa_storm10_sort]
;quartile_95 = voc_epa_storm10_sort[0.95*Nvoc_epa_storm10_sort]
;
;voc_epa_storm10_ptile = [quartile_05, quartile_25, voc_med, quartile_75,quartile_95]
;
;
;;;;EPA exceedances without storms: voc
;voc_epa_nstorm10 = voc_nstorm_oct[iepa_oct_ns]
;voc_epa_nstorm10_sort  = voc_epa_nstorm10[SORT(voc_epa_nstorm10)]
;IF (N_ELEMENTS(voc_epa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
;	Nvoc_epa_nstorm10_sort 	= N_ELEMENTS(voc_epa_nstorm10_sort)
;	voc_med = (voc_epa_nstorm10_sort[(Nvoc_epa_nstorm10_sort/2)-1] + voc_epa_nstorm10_sort[(Nvoc_epa_nstorm10_sort/2)]) / 2.0
;	lower_half = voc_epa_nstorm10_sort[0:(Nvoc_epa_nstorm10_sort/2)-1]
;	upper_half = voc_epa_nstorm10_sort[(Nvoc_epa_nstorm10_sort/2):(Nvoc_epa_nstorm10_sort-1)]
;ENDIF ELSE BEGIN
;	Nvoc_epa_nstorm10_sort 	= N_ELEMENTS(voc_epa_nstorm10_sort)
;	voc_med = voc_epa_nstorm10_sort[(Nvoc_epa_nstorm10_sort/2)] 
;	lower_half = voc_epa_nstorm10_sort[0:(Nvoc_epa_nstorm10_sort/2)-1]
;	upper_half = voc_epa_nstorm10_sort[(Nvoc_epa_nstorm10_sort/2):(Nvoc_epa_nstorm10_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = voc_epa_nstorm10_sort[0.05*Nvoc_epa_nstorm10_sort]
;quartile_95 = voc_epa_nstorm10_sort[0.95*Nvoc_epa_nstorm10_sort]
;
;voc_epa_nstorm10_ptile = [quartile_05, quartile_25, voc_med, quartile_75,quartile_95]
;
;
;;;;non-EPA exceedances with storms: voc
;voc_nepa_storm10 = voc_storm_oct[nepa_oct_s]
;voc_nepa_storm10_sort  = voc_nepa_storm10[SORT(voc_nepa_storm10)]
;IF (N_ELEMENTS(voc_nepa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
;	Nvoc_nepa_storm10_sort 	= N_ELEMENTS(voc_nepa_storm10_sort)
;	voc_med = (voc_nepa_storm10_sort[(Nvoc_nepa_storm10_sort/2)-1] + voc_nepa_storm10_sort[(Nvoc_nepa_storm10_sort/2)]) / 2.0
;	lower_half = voc_nepa_storm10_sort[0:(Nvoc_nepa_storm10_sort/2)-1]
;	upper_half = voc_nepa_storm10_sort[(Nvoc_nepa_storm10_sort/2):(Nvoc_nepa_storm10_sort-1)]
;ENDIF ELSE BEGIN
;	Nvoc_nepa_storm10_sort 	= N_ELEMENTS(voc_nepa_storm10_sort)
;	voc_med = voc_nepa_storm10_sort[(Nvoc_nepa_storm10_sort/2)] 
;	lower_half = voc_nepa_storm10_sort[0:(Nvoc_nepa_storm10_sort/2)-1]
;	upper_half = voc_nepa_storm10_sort[(Nvoc_nepa_storm10_sort/2):(Nvoc_nepa_storm10_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = voc_nepa_storm10_sort[0.05*Nvoc_nepa_storm10_sort]
;quartile_95 = voc_nepa_storm10_sort[0.95*Nvoc_nepa_storm10_sort]
;
;voc_nepa_storm10_ptile = [quartile_05, quartile_25, voc_med, quartile_75,quartile_95]
;
;
;;;;non-EPA exceedances with storms: voc
;voc_nepa_nstorm10 = voc_nstorm_oct[nepa_oct_ns]
;voc_nepa_nstorm10_sort  = voc_nepa_nstorm10[SORT(voc_nepa_nstorm10)]
;IF (N_ELEMENTS(voc_nepa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
;	Nvoc_nepa_nstorm10_sort 	= N_ELEMENTS(voc_nepa_nstorm10_sort)
;	voc_med = (voc_nepa_nstorm10_sort[(Nvoc_nepa_nstorm10_sort/2)-1] + voc_nepa_nstorm10_sort[(Nvoc_nepa_nstorm10_sort/2)]) / 2.0
;	lower_half = voc_nepa_nstorm10_sort[0:(Nvoc_nepa_nstorm10_sort/2)-1]
;	upper_half = voc_nepa_nstorm10_sort[(Nvoc_nepa_nstorm10_sort/2):(Nvoc_nepa_nstorm10_sort-1)]
;ENDIF ELSE BEGIN
;	Nvoc_nepa_nstorm10_sort 	= N_ELEMENTS(voc_nepa_nstorm10_sort)
;	voc_med = voc_nepa_nstorm10_sort[(Nvoc_nepa_nstorm10_sort/2)] 
;	lower_half = voc_nepa_nstorm10_sort[0:(Nvoc_nepa_nstorm10_sort/2)-1]
;	upper_half = voc_nepa_nstorm10_sort[(Nvoc_nepa_nstorm10_sort/2):(Nvoc_nepa_nstorm10_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = voc_nepa_nstorm10_sort[0.05*Nvoc_nepa_nstorm10_sort]
;quartile_95 = voc_nepa_nstorm10_sort[0.95*Nvoc_nepa_nstorm10_sort]
;
;voc_nepa_nstorm10_ptile = [quartile_05, quartile_25, voc_med, quartile_75,quartile_95]
;
;data = [[voc_epa_storm10_ptile], [voc_epa_nstorm10_ptile], [voc_nepa_storm10_ptile], [voc_nepa_nstorm10_ptile]]
;ytitle = ['EPA w/ Storms', 'EPA w/o Storms','non-EPA w/ Storms','non-EPA w/o Storms']
;boxes = BOXPLOT(data, $
;		TITLE		= 'oct: VOC', $
;		XRANGE 		= [0,1500], $
;		YRANGE 		= [-1, 4], $
;		XTITLE 		= 'VOC Concentration (ppb)', $
;		YTICKNAME 	= ytitle, $
;		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
;		FONT_SIZE   = 20, $
;		HORIZONTAL	= 1)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances without storms: nox
nox_epa_storm10 = nox_storm_oct[iepa_oct_s]
nox_epa_storm10_sort  = nox_epa_storm10[SORT(nox_epa_storm10)]
IF (N_ELEMENTS(nox_epa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
	Nnox_epa_storm10_sort 	= N_ELEMENTS(nox_epa_storm10_sort)
	nox_med = (nox_epa_storm10_sort[(Nnox_epa_storm10_sort/2)-1] + nox_epa_storm10_sort[(Nnox_epa_storm10_sort/2)]) / 2.0
	lower_half = nox_epa_storm10_sort[0:(Nnox_epa_storm10_sort/2)-1]
	upper_half = nox_epa_storm10_sort[(Nnox_epa_storm10_sort/2):(Nnox_epa_storm10_sort-1)]
ENDIF ELSE BEGIN
	Nnox_epa_storm10_sort 	= N_ELEMENTS(nox_epa_storm10_sort)
	nox_med = nox_epa_storm10_sort[(Nnox_epa_storm10_sort/2)] 
	lower_half = nox_epa_storm10_sort[0:(Nnox_epa_storm10_sort/2)-1]
	upper_half = nox_epa_storm10_sort[(Nnox_epa_storm10_sort/2):(Nnox_epa_storm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = nox_epa_storm10_sort[0.05*Nnox_epa_storm10_sort]
quartile_95 = nox_epa_storm10_sort[0.95*Nnox_epa_storm10_sort]

nox_epa_storm10_ptile = [quartile_05, quartile_25, nox_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: nox
nox_epa_nstorm10 = nox_nstorm_oct[iepa_oct_ns]
nox_epa_nstorm10_sort  = nox_epa_nstorm10[SORT(nox_epa_nstorm10)]
IF (N_ELEMENTS(nox_epa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
	Nnox_epa_nstorm10_sort 	= N_ELEMENTS(nox_epa_nstorm10_sort)
	nox_med = (nox_epa_nstorm10_sort[(Nnox_epa_nstorm10_sort/2)-1] + nox_epa_nstorm10_sort[(Nnox_epa_nstorm10_sort/2)]) / 2.0
	lower_half = nox_epa_nstorm10_sort[0:(Nnox_epa_nstorm10_sort/2)-1]
	upper_half = nox_epa_nstorm10_sort[(Nnox_epa_nstorm10_sort/2):(Nnox_epa_nstorm10_sort-1)]
ENDIF ELSE BEGIN
	Nnox_epa_nstorm10_sort 	= N_ELEMENTS(nox_epa_nstorm10_sort)
	nox_med = nox_epa_nstorm10_sort[(Nnox_epa_nstorm10_sort/2)] 
	lower_half = nox_epa_nstorm10_sort[0:(Nnox_epa_nstorm10_sort/2)-1]
	upper_half = nox_epa_nstorm10_sort[(Nnox_epa_nstorm10_sort/2):(Nnox_epa_nstorm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = nox_epa_nstorm10_sort[0.05*Nnox_epa_nstorm10_sort]
quartile_95 = nox_epa_nstorm10_sort[0.95*Nnox_epa_nstorm10_sort]

nox_epa_nstorm10_ptile = [quartile_05, quartile_25, nox_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: nox
nox_nepa_storm10 = nox_storm_oct[nepa_oct_s]
nox_nepa_storm10_sort  = nox_nepa_storm10[SORT(nox_nepa_storm10)]
IF (N_ELEMENTS(nox_nepa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
	Nnox_nepa_storm10_sort 	= N_ELEMENTS(nox_nepa_storm10_sort)
	nox_med = (nox_nepa_storm10_sort[(Nnox_nepa_storm10_sort/2)-1] + nox_nepa_storm10_sort[(Nnox_nepa_storm10_sort/2)]) / 2.0
	lower_half = nox_nepa_storm10_sort[0:(Nnox_nepa_storm10_sort/2)-1]
	upper_half = nox_nepa_storm10_sort[(Nnox_nepa_storm10_sort/2):(Nnox_nepa_storm10_sort-1)]
ENDIF ELSE BEGIN
	Nnox_nepa_storm10_sort 	= N_ELEMENTS(nox_nepa_storm10_sort)
	nox_med = nox_nepa_storm10_sort[(Nnox_nepa_storm10_sort/2)] 
	lower_half = nox_nepa_storm10_sort[0:(Nnox_nepa_storm10_sort/2)-1]
	upper_half = nox_nepa_storm10_sort[(Nnox_nepa_storm10_sort/2):(Nnox_nepa_storm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = nox_nepa_storm10_sort[0.05*Nnox_nepa_storm10_sort]
quartile_95 = nox_nepa_storm10_sort[0.95*Nnox_nepa_storm10_sort]

nox_nepa_storm10_ptile = [quartile_05, quartile_25, nox_med, quartile_75,quartile_95]


;;;non-EPA exceedances without storms: nox
nox_nepa_nstorm10 = nox_nstorm_oct[nepa_oct_ns]
nox_nepa_nstorm10_sort  = nox_nepa_nstorm10[SORT(nox_nepa_nstorm10)]
IF (N_ELEMENTS(nox_nepa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
	Nnox_nepa_nstorm10_sort 	= N_ELEMENTS(nox_nepa_nstorm10_sort)
	nox_med = (nox_nepa_nstorm10_sort[(Nnox_nepa_nstorm10_sort/2)-1] + nox_nepa_nstorm10_sort[(Nnox_nepa_nstorm10_sort/2)]) / 2.0
	lower_half = nox_nepa_nstorm10_sort[0:(Nnox_nepa_nstorm10_sort/2)-1]
	upper_half = nox_nepa_nstorm10_sort[(Nnox_nepa_nstorm10_sort/2):(Nnox_nepa_nstorm10_sort-1)]
ENDIF ELSE BEGIN
	Nnox_nepa_nstorm10_sort 	= N_ELEMENTS(nox_nepa_nstorm10_sort)
	nox_med = nox_nepa_nstorm10_sort[(Nnox_nepa_nstorm10_sort/2)] 
	lower_half = nox_nepa_nstorm10_sort[0:(Nnox_nepa_nstorm10_sort/2)-1]
	upper_half = nox_nepa_nstorm10_sort[(Nnox_nepa_nstorm10_sort/2):(Nnox_nepa_nstorm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = nox_nepa_nstorm10_sort[0.05*Nnox_nepa_nstorm10_sort]
quartile_95 = nox_nepa_nstorm10_sort[0.95*Nnox_nepa_nstorm10_sort]

nox_nepa_nstorm10_ptile = [quartile_05, quartile_25, nox_med, quartile_75,quartile_95]

data = [[nox_epa_storm10_ptile], [nox_epa_nstorm10_ptile], [nox_nepa_storm10_ptile], [nox_nepa_nstorm10_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','non-EPA w/ Storms','non-EPA w/o Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'Oct: NOx', $
		XRANGE 		= [0,150], $
		YRANGE 		= [-1, 4], $
		XTITLE 		= 'NOx Concentration (ppb)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: no
no_epa_storm10 = no_storm_oct[iepa_oct_s]
no_epa_storm10_sort  = no_epa_storm10[SORT(no_epa_storm10)]
IF (N_ELEMENTS(no_epa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
	Nno_epa_storm10_sort 	= N_ELEMENTS(no_epa_storm10_sort)
	no_med = (no_epa_storm10_sort[(Nno_epa_storm10_sort/2)-1] + no_epa_storm10_sort[(Nno_epa_storm10_sort/2)]) / 2.0
	lower_half = no_epa_storm10_sort[0:(Nno_epa_storm10_sort/2)-1]
	upper_half = no_epa_storm10_sort[(Nno_epa_storm10_sort/2):(Nno_epa_storm10_sort-1)]
ENDIF ELSE BEGIN
	Nno_epa_storm10_sort 	= N_ELEMENTS(no_epa_storm10_sort)
	no_med = no_epa_storm10_sort[(Nno_epa_storm10_sort/2)] 
	lower_half = no_epa_storm10_sort[0:(Nno_epa_storm10_sort/2)-1]
	upper_half = no_epa_storm10_sort[(Nno_epa_storm10_sort/2):(Nno_epa_storm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = no_epa_storm10_sort[0.05*Nno_epa_storm10_sort]
quartile_95 = no_epa_storm10_sort[0.95*Nno_epa_storm10_sort]

no_epa_storm10_ptile = [quartile_05, quartile_25, no_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: no
no_epa_nstorm10 = no_nstorm_oct[iepa_oct_ns]
no_epa_nstorm10_sort  = no_epa_nstorm10[SORT(no_epa_nstorm10)]
IF (N_ELEMENTS(no_epa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
	Nno_epa_nstorm10_sort 	= N_ELEMENTS(no_epa_nstorm10_sort)
	no_med = (no_epa_nstorm10_sort[(Nno_epa_nstorm10_sort/2)-1] + no_epa_nstorm10_sort[(Nno_epa_nstorm10_sort/2)]) / 2.0
	lower_half = no_epa_nstorm10_sort[0:(Nno_epa_nstorm10_sort/2)-1]
	upper_half = no_epa_nstorm10_sort[(Nno_epa_nstorm10_sort/2):(Nno_epa_nstorm10_sort-1)]
ENDIF ELSE BEGIN
	Nno_epa_nstorm10_sort 	= N_ELEMENTS(no_epa_nstorm10_sort)
	no_med = no_epa_nstorm10_sort[(Nno_epa_nstorm10_sort/2)] 
	lower_half = no_epa_nstorm10_sort[0:(Nno_epa_nstorm10_sort/2)-1]
	upper_half = no_epa_nstorm10_sort[(Nno_epa_nstorm10_sort/2):(Nno_epa_nstorm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = no_epa_nstorm10_sort[0.05*Nno_epa_nstorm10_sort]
quartile_95 = no_epa_nstorm10_sort[0.95*Nno_epa_nstorm10_sort]

no_epa_nstorm10_ptile = [quartile_05, quartile_25, no_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: no
no_nepa_storm10 = no_storm_oct[nepa_oct_s]
no_nepa_storm10_sort  = no_nepa_storm10[SORT(no_nepa_storm10)]
IF (N_ELEMENTS(no_nepa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
	Nno_nepa_storm10_sort 	= N_ELEMENTS(no_nepa_storm10_sort)
	no_med = (no_nepa_storm10_sort[(Nno_nepa_storm10_sort/2)-1] + no_nepa_storm10_sort[(Nno_nepa_storm10_sort/2)]) / 2.0
	lower_half = no_nepa_storm10_sort[0:(Nno_nepa_storm10_sort/2)-1]
	upper_half = no_nepa_storm10_sort[(Nno_nepa_storm10_sort/2):(Nno_nepa_storm10_sort-1)]
ENDIF ELSE BEGIN
	Nno_nepa_storm10_sort 	= N_ELEMENTS(no_nepa_storm10_sort)
	no_med = no_nepa_storm10_sort[(Nno_nepa_storm10_sort/2)] 
	lower_half = no_nepa_storm10_sort[0:(Nno_nepa_storm10_sort/2)-1]
	upper_half = no_nepa_storm10_sort[(Nno_nepa_storm10_sort/2):(Nno_nepa_storm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = no_nepa_storm10_sort[0.05*Nno_nepa_storm10_sort]
quartile_95 = no_nepa_storm10_sort[0.95*Nno_nepa_storm10_sort]

no_nepa_storm10_ptile = [quartile_05, quartile_25, no_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: no
no_nepa_nstorm10 = no_nstorm_oct[nepa_oct_ns]
no_nepa_nstorm10_sort  = no_nepa_nstorm10[SORT(no_nepa_nstorm10)]
IF (N_ELEMENTS(no_nepa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
	Nno_nepa_nstorm10_sort 	= N_ELEMENTS(no_nepa_nstorm10_sort)
	no_med = (no_nepa_nstorm10_sort[(Nno_nepa_nstorm10_sort/2)-1] + no_nepa_nstorm10_sort[(Nno_nepa_nstorm10_sort/2)]) / 2.0
	lower_half = no_nepa_nstorm10_sort[0:(Nno_nepa_nstorm10_sort/2)-1]
	upper_half = no_nepa_nstorm10_sort[(Nno_nepa_nstorm10_sort/2):(Nno_nepa_nstorm10_sort-1)]
ENDIF ELSE BEGIN
	Nno_nepa_nstorm10_sort 	= N_ELEMENTS(no_nepa_nstorm10_sort)
	no_med = no_nepa_nstorm10_sort[(Nno_nepa_nstorm10_sort/2)] 
	lower_half = no_nepa_nstorm10_sort[0:(Nno_nepa_nstorm10_sort/2)-1]
	upper_half = no_nepa_nstorm10_sort[(Nno_nepa_nstorm10_sort/2):(Nno_nepa_nstorm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = no_nepa_nstorm10_sort[0.05*Nno_nepa_nstorm10_sort]
quartile_95 = no_nepa_nstorm10_sort[0.95*Nno_nepa_nstorm10_sort]

no_nepa_nstorm10_ptile = [quartile_05, quartile_25, no_med, quartile_75,quartile_95]

data = [[no_epa_storm10_ptile], [no_epa_nstorm10_ptile], [no_nepa_storm10_ptile], [no_nepa_nstorm10_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','non-EPA w/ Storms','non-EPA w/o Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'Oct: NO', $
		XRANGE 		= [0,200], $
		YRANGE 		= [-1, 4], $
		XTITLE 		= 'NO Concentration (ppb)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: no2
no2_epa_storm10 = no2_storm_oct[iepa_oct_s]
no2_epa_storm10_sort  = no2_epa_storm10[SORT(no2_epa_storm10)]
IF (N_ELEMENTS(no2_epa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
	Nno2_epa_storm10_sort 	= N_ELEMENTS(no2_epa_storm10_sort)
	no2_med = (no2_epa_storm10_sort[(Nno2_epa_storm10_sort/2)-1] + no2_epa_storm10_sort[(Nno2_epa_storm10_sort/2)]) / 2.0
	lower_half = no2_epa_storm10_sort[0:(Nno2_epa_storm10_sort/2)-1]
	upper_half = no2_epa_storm10_sort[(Nno2_epa_storm10_sort/2):(Nno2_epa_storm10_sort-1)]
ENDIF ELSE BEGIN
	Nno2_epa_storm10_sort 	= N_ELEMENTS(no2_epa_storm10_sort)
	no2_med = no2_epa_storm10_sort[(Nno2_epa_storm10_sort/2)] 
	lower_half = no2_epa_storm10_sort[0:(Nno2_epa_storm10_sort/2)-1]
	upper_half = no2_epa_storm10_sort[(Nno2_epa_storm10_sort/2):(Nno2_epa_storm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = no2_epa_storm10_sort[0.05*Nno2_epa_storm10_sort]
quartile_95 = no2_epa_storm10_sort[0.95*Nno2_epa_storm10_sort]

no2_epa_storm10_ptile = [quartile_05, quartile_25, no2_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: no2
no2_epa_nstorm10 = no2_nstorm_oct[iepa_oct_ns]
no2_epa_nstorm10_sort  = no2_epa_nstorm10[SORT(no2_epa_nstorm10)]
IF (N_ELEMENTS(no2_epa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
	Nno2_epa_nstorm10_sort 	= N_ELEMENTS(no2_epa_nstorm10_sort)
	no2_med = (no2_epa_nstorm10_sort[(Nno2_epa_nstorm10_sort/2)-1] + no2_epa_nstorm10_sort[(Nno2_epa_nstorm10_sort/2)]) / 2.0
	lower_half = no2_epa_nstorm10_sort[0:(Nno2_epa_nstorm10_sort/2)-1]
	upper_half = no2_epa_nstorm10_sort[(Nno2_epa_nstorm10_sort/2):(Nno2_epa_nstorm10_sort-1)]
ENDIF ELSE BEGIN
	Nno2_epa_nstorm10_sort 	= N_ELEMENTS(no2_epa_nstorm10_sort)
	no2_med = no2_epa_nstorm10_sort[(Nno2_epa_nstorm10_sort/2)] 
	lower_half = no2_epa_nstorm10_sort[0:(Nno2_epa_nstorm10_sort/2)-1]
	upper_half = no2_epa_nstorm10_sort[(Nno2_epa_nstorm10_sort/2):(Nno2_epa_nstorm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = no2_epa_nstorm10_sort[0.05*Nno2_epa_nstorm10_sort]
quartile_95 = no2_epa_nstorm10_sort[0.95*Nno2_epa_nstorm10_sort]

no2_epa_nstorm10_ptile = [quartile_05, quartile_25, no2_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: no2
no2_nepa_storm10 = no2_storm_oct[nepa_oct_s]
no2_nepa_storm10_sort  = no2_nepa_storm10[SORT(no2_nepa_storm10)]
IF (N_ELEMENTS(no2_nepa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
	Nno2_nepa_storm10_sort 	= N_ELEMENTS(no2_nepa_storm10_sort)
	no2_med = (no2_nepa_storm10_sort[(Nno2_nepa_storm10_sort/2)-1] + no2_nepa_storm10_sort[(Nno2_nepa_storm10_sort/2)]) / 2.0
	lower_half = no2_nepa_storm10_sort[0:(Nno2_nepa_storm10_sort/2)-1]
	upper_half = no2_nepa_storm10_sort[(Nno2_nepa_storm10_sort/2):(Nno2_nepa_storm10_sort-1)]
ENDIF ELSE BEGIN
	Nno2_nepa_storm10_sort 	= N_ELEMENTS(no2_nepa_storm10_sort)
	no2_med = no2_nepa_storm10_sort[(Nno2_nepa_storm10_sort/2)] 
	lower_half = no2_nepa_storm10_sort[0:(Nno2_nepa_storm10_sort/2)-1]
	upper_half = no2_nepa_storm10_sort[(Nno2_nepa_storm10_sort/2):(Nno2_nepa_storm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = no2_nepa_storm10_sort[0.05*Nno2_nepa_storm10_sort]
quartile_95 = no2_nepa_storm10_sort[0.95*Nno2_nepa_storm10_sort]

no2_nepa_storm10_ptile = [quartile_05, quartile_25, no2_med, quartile_75,quartile_95]


;;;non-EPA exceedances without storms: no2
no2_nepa_nstorm10 = no2_nstorm_oct[nepa_oct_ns]
no2_nepa_nstorm10_sort  = no2_nepa_nstorm10[SORT(no2_nepa_nstorm10)]
IF (N_ELEMENTS(no2_nepa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
	Nno2_nepa_nstorm10_sort 	= N_ELEMENTS(no2_nepa_nstorm10_sort)
	no2_med = (no2_nepa_nstorm10_sort[(Nno2_nepa_nstorm10_sort/2)-1] + no2_nepa_nstorm10_sort[(Nno2_nepa_nstorm10_sort/2)]) / 2.0
	lower_half = no2_nepa_nstorm10_sort[0:(Nno2_nepa_nstorm10_sort/2)-1]
	upper_half = no2_nepa_nstorm10_sort[(Nno2_nepa_nstorm10_sort/2):(Nno2_nepa_nstorm10_sort-1)]
ENDIF ELSE BEGIN
	Nno2_nepa_nstorm10_sort 	= N_ELEMENTS(no2_nepa_nstorm10_sort)
	no2_med = no2_nepa_nstorm10_sort[(Nno2_nepa_nstorm10_sort/2)] 
	lower_half = no2_nepa_nstorm10_sort[0:(Nno2_nepa_nstorm10_sort/2)-1]
	upper_half = no2_nepa_nstorm10_sort[(Nno2_nepa_nstorm10_sort/2):(Nno2_nepa_nstorm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = no2_nepa_nstorm10_sort[0.05*Nno2_nepa_nstorm10_sort]
quartile_95 = no2_nepa_nstorm10_sort[0.95*Nno2_nepa_nstorm10_sort]

no2_nepa_nstorm10_ptile = [quartile_05, quartile_25, no2_med, quartile_75,quartile_95]

data = [[no2_epa_storm10_ptile], [no2_epa_nstorm10_ptile], [no2_nepa_storm10_ptile], [no2_nepa_nstorm10_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','no2n-EPA w/ Storms','no2n-EPA w/o Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'Oct: NO2', $
		XRANGE 		= [0,60], $
		YRANGE 		= [-1, 4], $
		XTITLE 		= 'NO2 Concentration (ppb)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: co
co_epa_storm10 = co_storm_oct[iepa_oct_s]
co_epa_storm10_sort  = co_epa_storm10[SORT(co_epa_storm10)]
IF (N_ELEMENTS(co_epa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
	Nco_epa_storm10_sort 	= N_ELEMENTS(co_epa_storm10_sort)
	co_med = (co_epa_storm10_sort[(Nco_epa_storm10_sort/2)-1] + co_epa_storm10_sort[(Nco_epa_storm10_sort/2)]) / 2.0
	lower_half = co_epa_storm10_sort[0:(Nco_epa_storm10_sort/2)-1]
	upper_half = co_epa_storm10_sort[(Nco_epa_storm10_sort/2):(Nco_epa_storm10_sort-1)]
ENDIF ELSE BEGIN
	Nco_epa_storm10_sort 	= N_ELEMENTS(co_epa_storm10_sort)
	co_med = co_epa_storm10_sort[(Nco_epa_storm10_sort/2)] 
	lower_half = co_epa_storm10_sort[0:(Nco_epa_storm10_sort/2)-1]
	upper_half = co_epa_storm10_sort[(Nco_epa_storm10_sort/2):(Nco_epa_storm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = co_epa_storm10_sort[0.05*Nco_epa_storm10_sort]
quartile_95 = co_epa_storm10_sort[0.95*Nco_epa_storm10_sort]

co_epa_storm10_ptile = [quartile_05, quartile_25, co_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: co
co_epa_nstorm10 = co_nstorm_oct[iepa_oct_ns]
co_epa_nstorm10_sort  = co_epa_nstorm10[SORT(co_epa_nstorm10)]
IF (N_ELEMENTS(co_epa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
	Nco_epa_nstorm10_sort 	= N_ELEMENTS(co_epa_nstorm10_sort)
	co_med = (co_epa_nstorm10_sort[(Nco_epa_nstorm10_sort/2)-1] + co_epa_nstorm10_sort[(Nco_epa_nstorm10_sort/2)]) / 2.0
	lower_half = co_epa_nstorm10_sort[0:(Nco_epa_nstorm10_sort/2)-1]
	upper_half = co_epa_nstorm10_sort[(Nco_epa_nstorm10_sort/2):(Nco_epa_nstorm10_sort-1)]
ENDIF ELSE BEGIN
	Nco_epa_nstorm10_sort 	= N_ELEMENTS(co_epa_nstorm10_sort)
	co_med = co_epa_nstorm10_sort[(Nco_epa_nstorm10_sort/2)] 
	lower_half = co_epa_nstorm10_sort[0:(Nco_epa_nstorm10_sort/2)-1]
	upper_half = co_epa_nstorm10_sort[(Nco_epa_nstorm10_sort/2):(Nco_epa_nstorm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = co_epa_nstorm10_sort[0.05*Nco_epa_nstorm10_sort]
quartile_95 = co_epa_nstorm10_sort[0.95*Nco_epa_nstorm10_sort]

co_epa_nstorm10_ptile = [quartile_05, quartile_25, co_med, quartile_75,quartile_95]


;;;con-EPA exceedances with storms: co
co_nepa_storm10 = co_storm_oct[nepa_oct_s]
co_nepa_storm10_sort  = co_nepa_storm10[SORT(co_nepa_storm10)]
IF (N_ELEMENTS(co_nepa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
	Nco_nepa_storm10_sort 	= N_ELEMENTS(co_nepa_storm10_sort)
	co_med = (co_nepa_storm10_sort[(Nco_nepa_storm10_sort/2)-1] + co_nepa_storm10_sort[(Nco_nepa_storm10_sort/2)]) / 2.0
	lower_half = co_nepa_storm10_sort[0:(Nco_nepa_storm10_sort/2)-1]
	upper_half = co_nepa_storm10_sort[(Nco_nepa_storm10_sort/2):(Nco_nepa_storm10_sort-1)]
ENDIF ELSE BEGIN
	Nco_nepa_storm10_sort 	= N_ELEMENTS(co_nepa_storm10_sort)
	co_med = co_nepa_storm10_sort[(Nco_nepa_storm10_sort/2)] 
	lower_half = co_nepa_storm10_sort[0:(Nco_nepa_storm10_sort/2)-1]
	upper_half = co_nepa_storm10_sort[(Nco_nepa_storm10_sort/2):(Nco_nepa_storm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = co_nepa_storm10_sort[0.05*Nco_nepa_storm10_sort]
quartile_95 = co_nepa_storm10_sort[0.95*Nco_nepa_storm10_sort]

co_nepa_storm10_ptile = [quartile_05, quartile_25, co_med, quartile_75,quartile_95]


;;;con-EPA exceedances with storms: co
co_nepa_nstorm10 = co_nstorm_oct[nepa_oct_ns]
co_nepa_nstorm10_sort  = co_nepa_nstorm10[SORT(co_nepa_nstorm10)]
IF (N_ELEMENTS(co_nepa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
	Nco_nepa_nstorm10_sort 	= N_ELEMENTS(co_nepa_nstorm10_sort)
	co_med = (co_nepa_nstorm10_sort[(Nco_nepa_nstorm10_sort/2)-1] + co_nepa_nstorm10_sort[(Nco_nepa_nstorm10_sort/2)]) / 2.0
	lower_half = co_nepa_nstorm10_sort[0:(Nco_nepa_nstorm10_sort/2)-1]
	upper_half = co_nepa_nstorm10_sort[(Nco_nepa_nstorm10_sort/2):(Nco_nepa_nstorm10_sort-1)]
ENDIF ELSE BEGIN
	Nco_nepa_nstorm10_sort 	= N_ELEMENTS(co_nepa_nstorm10_sort)
	co_med = co_nepa_nstorm10_sort[(Nco_nepa_nstorm10_sort/2)] 
	lower_half = co_nepa_nstorm10_sort[0:(Nco_nepa_nstorm10_sort/2)-1]
	upper_half = co_nepa_nstorm10_sort[(Nco_nepa_nstorm10_sort/2):(Nco_nepa_nstorm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = co_nepa_nstorm10_sort[0.05*Nco_nepa_nstorm10_sort]
quartile_95 = co_nepa_nstorm10_sort[0.95*Nco_nepa_nstorm10_sort]

co_nepa_nstorm10_ptile = [quartile_05, quartile_25, co_med, quartile_75,quartile_95]

data = [[co_epa_storm10_ptile], [co_epa_nstorm10_ptile], [co_nepa_storm10_ptile], [co_nepa_nstorm10_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','con-EPA w/ Storms','con-EPA w/o Storms']
boxes = BOXPLOT(data*1.0E3, $
		TITLE		= 'Oct: CO', $
		XRANGE 		= [0,3000], $
		YRANGE 		= [-1, 4], $
		XTITLE 		= 'CO Concentration (ppb)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: pm
;pm_epa_storm10 = pm_storm_oct[iepa_oct_s]
;pm_epa_storm10_sort  = pm_epa_storm10[SORT(pm_epa_storm10)]
;IF (N_ELEMENTS(pm_epa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
;	Npm_epa_storm10_sort 	= N_ELEMENTS(pm_epa_storm10_sort)
;	pm_med = (pm_epa_storm10_sort[(Npm_epa_storm10_sort/2)-1] + pm_epa_storm10_sort[(Npm_epa_storm10_sort/2)]) / 2.0
;	lower_half = pm_epa_storm10_sort[0:(Npm_epa_storm10_sort/2)-1]
;	upper_half = pm_epa_storm10_sort[(Npm_epa_storm10_sort/2):(Npm_epa_storm10_sort-1)]
;ENDIF ELSE BEGIN
;	Npm_epa_storm10_sort 	= N_ELEMENTS(pm_epa_storm10_sort)
;	pm_med = pm_epa_storm10_sort[(Npm_epa_storm10_sort/2)] 
;	lower_half = pm_epa_storm10_sort[0:(Npm_epa_storm10_sort/2)-1]
;	upper_half = pm_epa_storm10_sort[(Npm_epa_storm10_sort/2):(Npm_epa_storm10_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = pm_epa_storm10_sort[0.05*Npm_epa_storm10_sort]
;quartile_95 = pm_epa_storm10_sort[0.95*Npm_epa_storm10_sort]
;
;pm_epa_storm10_ptile = [quartile_05, quartile_25, pm_med, quartile_75,quartile_95]
;
;
;;;;EPA exceedances without storms: pm
;pm_epa_nstorm10 = pm_nstorm_oct[iepa_oct_ns]
;pm_epa_nstorm10_sort  = pm_epa_nstorm10[SORT(pm_epa_nstorm10)]
;IF (N_ELEMENTS(pm_epa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
;	Npm_epa_nstorm10_sort 	= N_ELEMENTS(pm_epa_nstorm10_sort)
;	pm_med = (pm_epa_nstorm10_sort[(Npm_epa_nstorm10_sort/2)-1] + pm_epa_nstorm10_sort[(Npm_epa_nstorm10_sort/2)]) / 2.0
;	lower_half = pm_epa_nstorm10_sort[0:(Npm_epa_nstorm10_sort/2)-1]
;	upper_half = pm_epa_nstorm10_sort[(Npm_epa_nstorm10_sort/2):(Npm_epa_nstorm10_sort-1)]
;ENDIF ELSE BEGIN
;	Npm_epa_nstorm10_sort 	= N_ELEMENTS(pm_epa_nstorm10_sort)
;	pm_med = pm_epa_nstorm10_sort[(Npm_epa_nstorm10_sort/2)] 
;	lower_half = pm_epa_nstorm10_sort[0:(Npm_epa_nstorm10_sort/2)-1]
;	upper_half = pm_epa_nstorm10_sort[(Npm_epa_nstorm10_sort/2):(Npm_epa_nstorm10_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = pm_epa_nstorm10_sort[0.05*Npm_epa_nstorm10_sort]
;quartile_95 = pm_epa_nstorm10_sort[0.95*Npm_epa_nstorm10_sort]
;
;pm_epa_nstorm10_ptile = [quartile_05, quartile_25, pm_med, quartile_75,quartile_95]
;
;
;;;;pmn-EPA exceedances with storms: pm
;pm_nepa_storm10 = pm_storm_oct[nepa_oct_s]
;pm_nepa_storm10_sort  = pm_nepa_storm10[SORT(pm_nepa_storm10)]
;IF (N_ELEMENTS(pm_nepa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
;	Npm_nepa_storm10_sort 	= N_ELEMENTS(pm_nepa_storm10_sort)
;	pm_med = (pm_nepa_storm10_sort[(Npm_nepa_storm10_sort/2)-1] + pm_nepa_storm10_sort[(Npm_nepa_storm10_sort/2)]) / 2.0
;	lower_half = pm_nepa_storm10_sort[0:(Npm_nepa_storm10_sort/2)-1]
;	upper_half = pm_nepa_storm10_sort[(Npm_nepa_storm10_sort/2):(Npm_nepa_storm10_sort-1)]
;ENDIF ELSE BEGIN
;	Npm_nepa_storm10_sort 	= N_ELEMENTS(pm_nepa_storm10_sort)
;	pm_med = pm_nepa_storm10_sort[(Npm_nepa_storm10_sort/2)] 
;	lower_half = pm_nepa_storm10_sort[0:(Npm_nepa_storm10_sort/2)-1]
;	upper_half = pm_nepa_storm10_sort[(Npm_nepa_storm10_sort/2):(Npm_nepa_storm10_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = pm_nepa_storm10_sort[0.05*Npm_nepa_storm10_sort]
;quartile_95 = pm_nepa_storm10_sort[0.95*Npm_nepa_storm10_sort]
;
;pm_nepa_storm10_ptile = [quartile_05, quartile_25, pm_med, quartile_75,quartile_95]
;
;
;;;;pmn-EPA exceedances with storms: pm
;pm_nepa_nstorm10 = pm_nstorm_oct[nepa_oct_ns]
;pm_nepa_nstorm10_sort  = pm_nepa_nstorm10[SORT(pm_nepa_nstorm10)]
;IF (N_ELEMENTS(pm_nepa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
;	Npm_nepa_nstorm10_sort 	= N_ELEMENTS(pm_nepa_nstorm10_sort)
;	pm_med = (pm_nepa_nstorm10_sort[(Npm_nepa_nstorm10_sort/2)-1] + pm_nepa_nstorm10_sort[(Npm_nepa_nstorm10_sort/2)]) / 2.0
;	lower_half = pm_nepa_nstorm10_sort[0:(Npm_nepa_nstorm10_sort/2)-1]
;	upper_half = pm_nepa_nstorm10_sort[(Npm_nepa_nstorm10_sort/2):(Npm_nepa_nstorm10_sort-1)]
;ENDIF ELSE BEGIN
;	Npm_nepa_nstorm10_sort 	= N_ELEMENTS(pm_nepa_nstorm10_sort)
;	pm_med = pm_nepa_nstorm10_sort[(Npm_nepa_nstorm10_sort/2)] 
;	lower_half = pm_nepa_nstorm10_sort[0:(Npm_nepa_nstorm10_sort/2)-1]
;	upper_half = pm_nepa_nstorm10_sort[(Npm_nepa_nstorm10_sort/2):(Npm_nepa_nstorm10_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = pm_nepa_nstorm10_sort[0.05*Npm_nepa_nstorm10_sort]
;quartile_95 = pm_nepa_nstorm10_sort[0.95*Npm_nepa_nstorm10_sort]
;
;pm_nepa_nstorm10_ptile = [quartile_05, quartile_25, pm_med, quartile_75,quartile_95]
;
;data = [[pm_epa_storm10_ptile], [pm_epa_nstorm10_ptile], [pm_nepa_storm10_ptile], [pm_nepa_nstorm10_ptile]]
;ytitle = ['EPA w/ Storms', 'EPA w/o Storms','pmn-EPA w/ Storms','pmn-EPA w/o Storms']
;boxes = BOXPLOT(data, $
;		TITLE		= 'Oct: PM2.5', $
;		XRANGE 		= [0, 25], $
;		YRANGE 		= [-1, 4], $
;		XTITLE 		= 'PM2.5 concentration (ug/m3)', $
;		YTICKNAME 	= ytitle, $
;		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
;		FONT_SIZE   = 20, $
;		HORIZONTAL	= 1)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: temp
temp_epa_storm10 = temp_storm_oct[iepa_oct_s]
temp_epa_storm10_sort  = temp_epa_storm10[SORT(temp_epa_storm10)]
IF (N_ELEMENTS(temp_epa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
	Ntemp_epa_storm10_sort 	= N_ELEMENTS(temp_epa_storm10_sort)
	temp_med = (temp_epa_storm10_sort[(Ntemp_epa_storm10_sort/2)-1] + temp_epa_storm10_sort[(Ntemp_epa_storm10_sort/2)]) / 2.0
	lower_half = temp_epa_storm10_sort[0:(Ntemp_epa_storm10_sort/2)-1]
	upper_half = temp_epa_storm10_sort[(Ntemp_epa_storm10_sort/2):(Ntemp_epa_storm10_sort-1)]
ENDIF ELSE BEGIN
	Ntemp_epa_storm10_sort 	= N_ELEMENTS(temp_epa_storm10_sort)
	temp_med = temp_epa_storm10_sort[(Ntemp_epa_storm10_sort/2)] 
	lower_half = temp_epa_storm10_sort[0:(Ntemp_epa_storm10_sort/2)-1]
	upper_half = temp_epa_storm10_sort[(Ntemp_epa_storm10_sort/2):(Ntemp_epa_storm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = temp_epa_storm10_sort[0.05*Ntemp_epa_storm10_sort]
quartile_95 = temp_epa_storm10_sort[0.95*Ntemp_epa_storm10_sort]

temp_epa_storm10_ptile = [quartile_05, quartile_25, temp_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: temp
temp_epa_nstorm10 = temp_nstorm_oct[iepa_oct_ns]
temp_epa_nstorm10_sort  = temp_epa_nstorm10[SORT(temp_epa_nstorm10)]
IF (N_ELEMENTS(temp_epa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
	Ntemp_epa_nstorm10_sort 	= N_ELEMENTS(temp_epa_nstorm10_sort)
	temp_med = (temp_epa_nstorm10_sort[(Ntemp_epa_nstorm10_sort/2)-1] + temp_epa_nstorm10_sort[(Ntemp_epa_nstorm10_sort/2)]) / 2.0
	lower_half = temp_epa_nstorm10_sort[0:(Ntemp_epa_nstorm10_sort/2)-1]
	upper_half = temp_epa_nstorm10_sort[(Ntemp_epa_nstorm10_sort/2):(Ntemp_epa_nstorm10_sort-1)]
ENDIF ELSE BEGIN
	Ntemp_epa_nstorm10_sort 	= N_ELEMENTS(temp_epa_nstorm10_sort)
	temp_med = temp_epa_nstorm10_sort[(Ntemp_epa_nstorm10_sort/2)] 
	lower_half = temp_epa_nstorm10_sort[0:(Ntemp_epa_nstorm10_sort/2)-1]
	upper_half = temp_epa_nstorm10_sort[(Ntemp_epa_nstorm10_sort/2):(Ntemp_epa_nstorm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = temp_epa_nstorm10_sort[0.05*Ntemp_epa_nstorm10_sort]
quartile_95 = temp_epa_nstorm10_sort[0.95*Ntemp_epa_nstorm10_sort]

temp_epa_nstorm10_ptile = [quartile_05, quartile_25, temp_med, quartile_75,quartile_95]


;;;tempn-EPA exceedances with storms: temp
temp_nepa_storm10 = temp_storm_oct[nepa_oct_s]
temp_nepa_storm10_sort  = temp_nepa_storm10[SORT(temp_nepa_storm10)]
IF (N_ELEMENTS(temp_nepa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
	Ntemp_nepa_storm10_sort 	= N_ELEMENTS(temp_nepa_storm10_sort)
	temp_med = (temp_nepa_storm10_sort[(Ntemp_nepa_storm10_sort/2)-1] + temp_nepa_storm10_sort[(Ntemp_nepa_storm10_sort/2)]) / 2.0
	lower_half = temp_nepa_storm10_sort[0:(Ntemp_nepa_storm10_sort/2)-1]
	upper_half = temp_nepa_storm10_sort[(Ntemp_nepa_storm10_sort/2):(Ntemp_nepa_storm10_sort-1)]
ENDIF ELSE BEGIN
	Ntemp_nepa_storm10_sort 	= N_ELEMENTS(temp_nepa_storm10_sort)
	temp_med = temp_nepa_storm10_sort[(Ntemp_nepa_storm10_sort/2)] 
	lower_half = temp_nepa_storm10_sort[0:(Ntemp_nepa_storm10_sort/2)-1]
	upper_half = temp_nepa_storm10_sort[(Ntemp_nepa_storm10_sort/2):(Ntemp_nepa_storm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = temp_nepa_storm10_sort[0.05*Ntemp_nepa_storm10_sort]
quartile_95 = temp_nepa_storm10_sort[0.95*Ntemp_nepa_storm10_sort]

temp_nepa_storm10_ptile = [quartile_05, quartile_25, temp_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: temp
temp_nepa_nstorm10 = temp_nstorm_oct[nepa_oct_ns]
temp_nepa_nstorm10_sort  = temp_nepa_nstorm10[SORT(temp_nepa_nstorm10)]
IF (N_ELEMENTS(temp_nepa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
	Ntemp_nepa_nstorm10_sort 	= N_ELEMENTS(temp_nepa_nstorm10_sort)
	temp_med = (temp_nepa_nstorm10_sort[(Ntemp_nepa_nstorm10_sort/2)-1] + temp_nepa_nstorm10_sort[(Ntemp_nepa_nstorm10_sort/2)]) / 2.0
	lower_half = temp_nepa_nstorm10_sort[0:(Ntemp_nepa_nstorm10_sort/2)-1]
	upper_half = temp_nepa_nstorm10_sort[(Ntemp_nepa_nstorm10_sort/2):(Ntemp_nepa_nstorm10_sort-1)]
ENDIF ELSE BEGIN
	Ntemp_nepa_nstorm10_sort 	= N_ELEMENTS(temp_nepa_nstorm10_sort)
	temp_med = temp_nepa_nstorm10_sort[(Ntemp_nepa_nstorm10_sort/2)] 
	lower_half = temp_nepa_nstorm10_sort[0:(Ntemp_nepa_nstorm10_sort/2)-1]
	upper_half = temp_nepa_nstorm10_sort[(Ntemp_nepa_nstorm10_sort/2):(Ntemp_nepa_nstorm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = temp_nepa_nstorm10_sort[0.05*Ntemp_nepa_nstorm10_sort]
quartile_95 = temp_nepa_nstorm10_sort[0.95*Ntemp_nepa_nstorm10_sort]

temp_nepa_nstorm10_ptile = [quartile_05, quartile_25, temp_med, quartile_75,quartile_95]

data = [[temp_epa_storm10_ptile], [temp_epa_nstorm10_ptile], [temp_nepa_storm10_ptile], [temp_nepa_nstorm10_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','tempn-EPA w/ Storms','tempn-EPA w/o Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'oct: Temperature', $
		XRANGE 		= [45, 95], $
		YRANGE 		= [-1, 4], $
		XTITLE 		= 'Temperature (deg F)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: press
;press_epa_storm10 = press_storm_july[iepa_july_s]
;press_epa_storm10_sort  = press_epa_storm10[SORT(press_epa_storm10)]
;IF (N_ELEMENTS(press_epa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
;	Npress_epa_storm10_sort 	= N_ELEMENTS(press_epa_storm10_sort)
;	press_med = (press_epa_storm10_sort[(Npress_epa_storm10_sort/2)-1] + press_epa_storm10_sort[(Npress_epa_storm10_sort/2)]) / 2.0
;	lower_half = press_epa_storm10_sort[0:(Npress_epa_storm10_sort/2)-1]
;	upper_half = press_epa_storm10_sort[(Npress_epa_storm10_sort/2):(Npress_epa_storm10_sort-1)]
;ENDIF ELSE BEGIN
;	Npress_epa_storm10_sort 	= N_ELEMENTS(press_epa_storm10_sort)
;	press_med = press_epa_storm10_sort[(Npress_epa_storm10_sort/2)] 
;	lower_half = press_epa_storm10_sort[0:(Npress_epa_storm10_sort/2)-1]
;	upper_half = press_epa_storm10_sort[(Npress_epa_storm10_sort/2):(Npress_epa_storm10_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = press_epa_storm10_sort[0.05*Npress_epa_storm10_sort]
;quartile_95 = press_epa_storm10_sort[0.95*Npress_epa_storm10_sort]
;
;press_epa_storm10_ptile = [quartile_05, quartile_25, press_med, quartile_75,quartile_95]
;
;
;;;;EPA exceedances without storms: press
;press_epa_nstorm10 = press_nstorm_july[iepa_july_ns]
;press_epa_nstorm10_sort  = press_epa_nstorm10[SORT(press_epa_nstorm10)]
;IF (N_ELEMENTS(press_epa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
;	Npress_epa_nstorm10_sort 	= N_ELEMENTS(press_epa_nstorm10_sort)
;	press_med = (press_epa_nstorm10_sort[(Npress_epa_nstorm10_sort/2)-1] + press_epa_nstorm10_sort[(Npress_epa_nstorm10_sort/2)]) / 2.0
;	lower_half = press_epa_nstorm10_sort[0:(Npress_epa_nstorm10_sort/2)-1]
;	upper_half = press_epa_nstorm10_sort[(Npress_epa_nstorm10_sort/2):(Npress_epa_nstorm10_sort-1)]
;ENDIF ELSE BEGIN
;	Npress_epa_nstorm10_sort 	= N_ELEMENTS(press_epa_nstorm10_sort)
;	press_med = press_epa_nstorm10_sort[(Npress_epa_nstorm10_sort/2)] 
;	lower_half = press_epa_nstorm10_sort[0:(Npress_epa_nstorm10_sort/2)-1]
;	upper_half = press_epa_nstorm10_sort[(Npress_epa_nstorm10_sort/2):(Npress_epa_nstorm10_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = press_epa_nstorm10_sort[0.05*Npress_epa_nstorm10_sort]
;quartile_95 = press_epa_nstorm10_sort[0.95*Npress_epa_nstorm10_sort]
;
;press_epa_nstorm10_ptile = [quartile_05, quartile_25, press_med, quartile_75,quartile_95]
;
;
;;;;pressn-EPA exceedances with storms: press
;press_nepa_storm10 = press_storm_july[nepa_july_s]
;press_nepa_storm10_sort  = press_nepa_storm10[SORT(press_nepa_storm10)]
;IF (N_ELEMENTS(press_nepa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
;	Npress_nepa_storm10_sort 	= N_ELEMENTS(press_nepa_storm10_sort)
;	press_med = (press_nepa_storm10_sort[(Npress_nepa_storm10_sort/2)-1] + press_nepa_storm10_sort[(Npress_nepa_storm10_sort/2)]) / 2.0
;	lower_half = press_nepa_storm10_sort[0:(Npress_nepa_storm10_sort/2)-1]
;	upper_half = press_nepa_storm10_sort[(Npress_nepa_storm10_sort/2):(Npress_nepa_storm10_sort-1)]
;ENDIF ELSE BEGIN
;	Npress_nepa_storm10_sort 	= N_ELEMENTS(press_nepa_storm10_sort)
;	press_med = press_nepa_storm10_sort[(Npress_nepa_storm10_sort/2)] 
;	lower_half = press_nepa_storm10_sort[0:(Npress_nepa_storm10_sort/2)-1]
;	upper_half = press_nepa_storm10_sort[(Npress_nepa_storm10_sort/2):(Npress_nepa_storm10_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = press_nepa_storm10_sort[0.05*Npress_nepa_storm10_sort]
;quartile_95 = press_nepa_storm10_sort[0.95*Npress_nepa_storm10_sort]
;
;press_nepa_storm10_ptile = [quartile_05, quartile_25, press_med, quartile_75,quartile_95]
;
;
;;;;non-EPA exceedances with storms: press
;press_nepa_nstorm10 = press_nstorm_july[nepa_july_ns]
;press_nepa_nstorm10_sort  = press_nepa_nstorm10[SORT(press_nepa_nstorm10)]
;IF (N_ELEMENTS(press_nepa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
;	Npress_nepa_nstorm10_sort 	= N_ELEMENTS(press_nepa_nstorm10_sort)
;	press_med = (press_nepa_nstorm10_sort[(Npress_nepa_nstorm10_sort/2)-1] + press_nepa_nstorm10_sort[(Npress_nepa_nstorm10_sort/2)]) / 2.0
;	lower_half = press_nepa_nstorm10_sort[0:(Npress_nepa_nstorm10_sort/2)-1]
;	upper_half = press_nepa_nstorm10_sort[(Npress_nepa_nstorm10_sort/2):(Npress_nepa_nstorm10_sort-1)]
;ENDIF ELSE BEGIN
;	Npress_nepa_nstorm10_sort 	= N_ELEMENTS(press_nepa_nstorm10_sort)
;	press_med = press_nepa_nstorm10_sort[(Npress_nepa_nstorm10_sort/2)] 
;	lower_half = press_nepa_nstorm10_sort[0:(Npress_nepa_nstorm10_sort/2)-1]
;	upper_half = press_nepa_nstorm10_sort[(Npress_nepa_nstorm10_sort/2):(Npress_nepa_nstorm10_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = press_nepa_nstorm10_sort[0.05*Npress_nepa_nstorm10_sort]
;quartile_95 = press_nepa_nstorm10_sort[0.95*Npress_nepa_nstorm10_sort]
;
;press_nepa_nstorm10_ptile = [quartile_05, quartile_25, press_med, quartile_75,quartile_95]
;
;data = [[press_epa_storm10_ptile], [press_epa_nstorm10_ptile], [press_nepa_storm10_ptile], [press_nepa_nstorm10_ptile]]
;ytitle = ['EPA w/ Storms', 'EPA w/o Storms','pressn-EPA w/ Storms','pressn-EPA w/o Storms']
;boxes = BOXPLOT(data, $
;		TITLE		= 'Oct: Pressure', $
;		XRANGE 		= [990, 1005], $
;		YRANGE 		= [-1, 4], $
;		XTITLE 		= 'Pressure (hPa)', $
;		YTICKNAME 	= ytitle, $
;		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
;		FONT_SIZE   = 20, $
;		HORIZONTAL	= 1)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: wind_spd
wind_spd_epa_storm10 = wind_spd_storm_oct[iepa_oct_s]
wind_spd_epa_storm10_sort  = wind_spd_epa_storm10[SORT(wind_spd_epa_storm10)]
IF (N_ELEMENTS(wind_spd_epa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_spd_epa_storm10_sort 	= N_ELEMENTS(wind_spd_epa_storm10_sort)
	wind_spd_med = (wind_spd_epa_storm10_sort[(Nwind_spd_epa_storm10_sort/2)-1] + wind_spd_epa_storm10_sort[(Nwind_spd_epa_storm10_sort/2)]) / 2.0
	lower_half = wind_spd_epa_storm10_sort[0:(Nwind_spd_epa_storm10_sort/2)-1]
	upper_half = wind_spd_epa_storm10_sort[(Nwind_spd_epa_storm10_sort/2):(Nwind_spd_epa_storm10_sort-1)]
ENDIF ELSE BEGIN
	Nwind_spd_epa_storm10_sort 	= N_ELEMENTS(wind_spd_epa_storm10_sort)
	wind_spd_med = wind_spd_epa_storm10_sort[(Nwind_spd_epa_storm10_sort/2)] 
	lower_half = wind_spd_epa_storm10_sort[0:(Nwind_spd_epa_storm10_sort/2)-1]
	upper_half = wind_spd_epa_storm10_sort[(Nwind_spd_epa_storm10_sort/2):(Nwind_spd_epa_storm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_spd_epa_storm10_sort[0.05*Nwind_spd_epa_storm10_sort]
quartile_95 = wind_spd_epa_storm10_sort[0.95*Nwind_spd_epa_storm10_sort]

wind_spd_epa_storm10_ptile = [quartile_05, quartile_25, wind_spd_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: wind_spd
wind_spd_epa_nstorm10 = wind_spd_nstorm_oct[iepa_oct_ns]
wind_spd_epa_nstorm10_sort  = wind_spd_epa_nstorm10[SORT(wind_spd_epa_nstorm10)]
IF (N_ELEMENTS(wind_spd_epa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_spd_epa_nstorm10_sort 	= N_ELEMENTS(wind_spd_epa_nstorm10_sort)
	wind_spd_med = (wind_spd_epa_nstorm10_sort[(Nwind_spd_epa_nstorm10_sort/2)-1] + wind_spd_epa_nstorm10_sort[(Nwind_spd_epa_nstorm10_sort/2)]) / 2.0
	lower_half = wind_spd_epa_nstorm10_sort[0:(Nwind_spd_epa_nstorm10_sort/2)-1]
	upper_half = wind_spd_epa_nstorm10_sort[(Nwind_spd_epa_nstorm10_sort/2):(Nwind_spd_epa_nstorm10_sort-1)]
ENDIF ELSE BEGIN
	Nwind_spd_epa_nstorm10_sort 	= N_ELEMENTS(wind_spd_epa_nstorm10_sort)
	wind_spd_med = wind_spd_epa_nstorm10_sort[(Nwind_spd_epa_nstorm10_sort/2)] 
	lower_half = wind_spd_epa_nstorm10_sort[0:(Nwind_spd_epa_nstorm10_sort/2)-1]
	upper_half = wind_spd_epa_nstorm10_sort[(Nwind_spd_epa_nstorm10_sort/2):(Nwind_spd_epa_nstorm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_spd_epa_nstorm10_sort[0.05*Nwind_spd_epa_nstorm10_sort]
quartile_95 = wind_spd_epa_nstorm10_sort[0.95*Nwind_spd_epa_nstorm10_sort]

wind_spd_epa_nstorm10_ptile = [quartile_05, quartile_25, wind_spd_med, quartile_75,quartile_95]


;;;wind_spdn-EPA exceedances with storms: wind_spd
wind_spd_nepa_storm10 = wind_spd_storm_oct[nepa_oct_s]
wind_spd_nepa_storm10_sort  = wind_spd_nepa_storm10[SORT(wind_spd_nepa_storm10)]
IF (N_ELEMENTS(wind_spd_nepa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_spd_nepa_storm10_sort 	= N_ELEMENTS(wind_spd_nepa_storm10_sort)
	wind_spd_med = (wind_spd_nepa_storm10_sort[(Nwind_spd_nepa_storm10_sort/2)-1] + wind_spd_nepa_storm10_sort[(Nwind_spd_nepa_storm10_sort/2)]) / 2.0
	lower_half = wind_spd_nepa_storm10_sort[0:(Nwind_spd_nepa_storm10_sort/2)-1]
	upper_half = wind_spd_nepa_storm10_sort[(Nwind_spd_nepa_storm10_sort/2):(Nwind_spd_nepa_storm10_sort-1)]
ENDIF ELSE BEGIN
	Nwind_spd_nepa_storm10_sort 	= N_ELEMENTS(wind_spd_nepa_storm10_sort)
	wind_spd_med = wind_spd_nepa_storm10_sort[(Nwind_spd_nepa_storm10_sort/2)] 
	lower_half = wind_spd_nepa_storm10_sort[0:(Nwind_spd_nepa_storm10_sort/2)-1]
	upper_half = wind_spd_nepa_storm10_sort[(Nwind_spd_nepa_storm10_sort/2):(Nwind_spd_nepa_storm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_spd_nepa_storm10_sort[0.05*Nwind_spd_nepa_storm10_sort]
quartile_95 = wind_spd_nepa_storm10_sort[0.95*Nwind_spd_nepa_storm10_sort]

wind_spd_nepa_storm10_ptile = [quartile_05, quartile_25, wind_spd_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: wind_spd
wind_spd_nepa_nstorm10 = wind_spd_nstorm_oct[nepa_oct_ns]
wind_spd_nepa_nstorm10_sort  = wind_spd_nepa_nstorm10[SORT(wind_spd_nepa_nstorm10)]
IF (N_ELEMENTS(wind_spd_nepa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_spd_nepa_nstorm10_sort 	= N_ELEMENTS(wind_spd_nepa_nstorm10_sort)
	wind_spd_med = (wind_spd_nepa_nstorm10_sort[(Nwind_spd_nepa_nstorm10_sort/2)-1] + wind_spd_nepa_nstorm10_sort[(Nwind_spd_nepa_nstorm10_sort/2)]) / 2.0
	lower_half = wind_spd_nepa_nstorm10_sort[0:(Nwind_spd_nepa_nstorm10_sort/2)-1]
	upper_half = wind_spd_nepa_nstorm10_sort[(Nwind_spd_nepa_nstorm10_sort/2):(Nwind_spd_nepa_nstorm10_sort-1)]
ENDIF ELSE BEGIN
	Nwind_spd_nepa_nstorm10_sort 	= N_ELEMENTS(wind_spd_nepa_nstorm10_sort)
	wind_spd_med = wind_spd_nepa_nstorm10_sort[(Nwind_spd_nepa_nstorm10_sort/2)] 
	lower_half = wind_spd_nepa_nstorm10_sort[0:(Nwind_spd_nepa_nstorm10_sort/2)-1]
	upper_half = wind_spd_nepa_nstorm10_sort[(Nwind_spd_nepa_nstorm10_sort/2):(Nwind_spd_nepa_nstorm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_spd_nepa_nstorm10_sort[0.05*Nwind_spd_nepa_nstorm10_sort]
quartile_95 = wind_spd_nepa_nstorm10_sort[0.95*Nwind_spd_nepa_nstorm10_sort]

wind_spd_nepa_nstorm10_ptile = [quartile_05, quartile_25, wind_spd_med, quartile_75,quartile_95]

data = [[wind_spd_epa_storm10_ptile], [wind_spd_epa_nstorm10_ptile], [wind_spd_nepa_storm10_ptile], [wind_spd_nepa_nstorm10_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','non-EPA w/ Storms','non-EPA w/o Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'oct: Wind Speed', $
		XRANGE 		= [0, 10], $
		YRANGE 		= [-1, 4], $
		XTITLE 		= 'Wind Speed (m/s)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: wind_dir
wind_dir_epa_storm10 = wind_dir_storm_oct[iepa_oct_s]
wind_dir_epa_storm10_sort  = wind_dir_epa_storm10[SORT(wind_dir_epa_storm10)]
IF (N_ELEMENTS(wind_dir_epa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_dir_epa_storm10_sort 	= N_ELEMENTS(wind_dir_epa_storm10_sort)
	wind_dir_med = (wind_dir_epa_storm10_sort[(Nwind_dir_epa_storm10_sort/2)-1] + wind_dir_epa_storm10_sort[(Nwind_dir_epa_storm10_sort/2)]) / 2.0
	lower_half = wind_dir_epa_storm10_sort[0:(Nwind_dir_epa_storm10_sort/2)-1]
	upper_half = wind_dir_epa_storm10_sort[(Nwind_dir_epa_storm10_sort/2):(Nwind_dir_epa_storm10_sort-1)]
ENDIF ELSE BEGIN
	Nwind_dir_epa_storm10_sort 	= N_ELEMENTS(wind_dir_epa_storm10_sort)
	wind_dir_med = wind_dir_epa_storm10_sort[(Nwind_dir_epa_storm10_sort/2)] 
	lower_half = wind_dir_epa_storm10_sort[0:(Nwind_dir_epa_storm10_sort/2)-1]
	upper_half = wind_dir_epa_storm10_sort[(Nwind_dir_epa_storm10_sort/2):(Nwind_dir_epa_storm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_dir_epa_storm10_sort[0.05*Nwind_dir_epa_storm10_sort]
quartile_95 = wind_dir_epa_storm10_sort[0.95*Nwind_dir_epa_storm10_sort]

wind_dir_epa_storm10_ptile = [quartile_05, quartile_25, wind_dir_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: wind_dir
wind_dir_epa_nstorm10 = wind_dir_nstorm_oct[iepa_oct_ns]
wind_dir_epa_nstorm10_sort  = wind_dir_epa_nstorm10[SORT(wind_dir_epa_nstorm10)]
IF (N_ELEMENTS(wind_dir_epa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_dir_epa_nstorm10_sort 	= N_ELEMENTS(wind_dir_epa_nstorm10_sort)
	wind_dir_med = (wind_dir_epa_nstorm10_sort[(Nwind_dir_epa_nstorm10_sort/2)-1] + wind_dir_epa_nstorm10_sort[(Nwind_dir_epa_nstorm10_sort/2)]) / 2.0
	lower_half = wind_dir_epa_nstorm10_sort[0:(Nwind_dir_epa_nstorm10_sort/2)-1]
	upper_half = wind_dir_epa_nstorm10_sort[(Nwind_dir_epa_nstorm10_sort/2):(Nwind_dir_epa_nstorm10_sort-1)]
ENDIF ELSE BEGIN
	Nwind_dir_epa_nstorm10_sort 	= N_ELEMENTS(wind_dir_epa_nstorm10_sort)
	wind_dir_med = wind_dir_epa_nstorm10_sort[(Nwind_dir_epa_nstorm10_sort/2)] 
	lower_half = wind_dir_epa_nstorm10_sort[0:(Nwind_dir_epa_nstorm10_sort/2)-1]
	upper_half = wind_dir_epa_nstorm10_sort[(Nwind_dir_epa_nstorm10_sort/2):(Nwind_dir_epa_nstorm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_dir_epa_nstorm10_sort[0.05*Nwind_dir_epa_nstorm10_sort]
quartile_95 = wind_dir_epa_nstorm10_sort[0.95*Nwind_dir_epa_nstorm10_sort]

wind_dir_epa_nstorm10_ptile = [quartile_05, quartile_25, wind_dir_med, quartile_75,quartile_95]


;;;wind_dirn-EPA exceedances with storms: wind_dir
wind_dir_nepa_storm10 = wind_dir_storm_oct[nepa_oct_s]
wind_dir_nepa_storm10_sort  = wind_dir_nepa_storm10[SORT(wind_dir_nepa_storm10)]
IF (N_ELEMENTS(wind_dir_nepa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_dir_nepa_storm10_sort 	= N_ELEMENTS(wind_dir_nepa_storm10_sort)
	wind_dir_med = (wind_dir_nepa_storm10_sort[(Nwind_dir_nepa_storm10_sort/2)-1] + wind_dir_nepa_storm10_sort[(Nwind_dir_nepa_storm10_sort/2)]) / 2.0
	lower_half = wind_dir_nepa_storm10_sort[0:(Nwind_dir_nepa_storm10_sort/2)-1]
	upper_half = wind_dir_nepa_storm10_sort[(Nwind_dir_nepa_storm10_sort/2):(Nwind_dir_nepa_storm10_sort-1)]
ENDIF ELSE BEGIN
	Nwind_dir_nepa_storm10_sort 	= N_ELEMENTS(wind_dir_nepa_storm10_sort)
	wind_dir_med = wind_dir_nepa_storm10_sort[(Nwind_dir_nepa_storm10_sort/2)] 
	lower_half = wind_dir_nepa_storm10_sort[0:(Nwind_dir_nepa_storm10_sort/2)-1]
	upper_half = wind_dir_nepa_storm10_sort[(Nwind_dir_nepa_storm10_sort/2):(Nwind_dir_nepa_storm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_dir_nepa_storm10_sort[0.05*Nwind_dir_nepa_storm10_sort]
quartile_95 = wind_dir_nepa_storm10_sort[0.95*Nwind_dir_nepa_storm10_sort]

wind_dir_nepa_storm10_ptile = [quartile_05, quartile_25, wind_dir_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: wind_dir
wind_dir_nepa_nstorm10 = wind_dir_nstorm_oct[nepa_oct_ns]
wind_dir_nepa_nstorm10_sort  = wind_dir_nepa_nstorm10[SORT(wind_dir_nepa_nstorm10)]
IF (N_ELEMENTS(wind_dir_nepa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_dir_nepa_nstorm10_sort 	= N_ELEMENTS(wind_dir_nepa_nstorm10_sort)
	wind_dir_med = (wind_dir_nepa_nstorm10_sort[(Nwind_dir_nepa_nstorm10_sort/2)-1] + wind_dir_nepa_nstorm10_sort[(Nwind_dir_nepa_nstorm10_sort/2)]) / 2.0
	lower_half = wind_dir_nepa_nstorm10_sort[0:(Nwind_dir_nepa_nstorm10_sort/2)-1]
	upper_half = wind_dir_nepa_nstorm10_sort[(Nwind_dir_nepa_nstorm10_sort/2):(Nwind_dir_nepa_nstorm10_sort-1)]
ENDIF ELSE BEGIN
	Nwind_dir_nepa_nstorm10_sort 	= N_ELEMENTS(wind_dir_nepa_nstorm10_sort)
	wind_dir_med = wind_dir_nepa_nstorm10_sort[(Nwind_dir_nepa_nstorm10_sort/2)] 
	lower_half = wind_dir_nepa_nstorm10_sort[0:(Nwind_dir_nepa_nstorm10_sort/2)-1]
	upper_half = wind_dir_nepa_nstorm10_sort[(Nwind_dir_nepa_nstorm10_sort/2):(Nwind_dir_nepa_nstorm10_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_dir_nepa_nstorm10_sort[0.05*Nwind_dir_nepa_nstorm10_sort]
quartile_95 = wind_dir_nepa_nstorm10_sort[0.95*Nwind_dir_nepa_nstorm10_sort]

wind_dir_nepa_nstorm10_ptile = [quartile_05, quartile_25, wind_dir_med, quartile_75,quartile_95]

data = [[wind_dir_epa_storm10_ptile], [wind_dir_epa_nstorm10_ptile], [wind_dir_nepa_storm10_ptile], [wind_dir_nepa_nstorm10_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','non-EPA w/ Storms','non-EPA w/o Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'oct: Wind Direction', $
		XRANGE 		= [0, 360], $
		YRANGE 		= [-1, 4], $
		XTITLE 		= 'Wind Dir. (deg)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: rh
;rh_epa_storm10 = rh_storm_oct[iepa_oct_s]
;rh_epa_storm10_sort  = rh_epa_storm10[SORT(rh_epa_storm10)]
;IF (N_ELEMENTS(rh_epa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
;	Nrh_epa_storm10_sort 	= N_ELEMENTS(rh_epa_storm10_sort)
;	rh_med = (rh_epa_storm10_sort[(Nrh_epa_storm10_sort/2)-1] + rh_epa_storm10_sort[(Nrh_epa_storm10_sort/2)]) / 2.0
;	lower_half = rh_epa_storm10_sort[0:(Nrh_epa_storm10_sort/2)-1]
;	upper_half = rh_epa_storm10_sort[(Nrh_epa_storm10_sort/2):(Nrh_epa_storm10_sort-1)]
;ENDIF ELSE BEGIN
;	Nrh_epa_storm10_sort 	= N_ELEMENTS(rh_epa_storm10_sort)
;	rh_med = rh_epa_storm10_sort[(Nrh_epa_storm10_sort/2)] 
;	lower_half = rh_epa_storm10_sort[0:(Nrh_epa_storm10_sort/2)-1]
;	upper_half = rh_epa_storm10_sort[(Nrh_epa_storm10_sort/2):(Nrh_epa_storm10_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = rh_epa_storm10_sort[0.05*Nrh_epa_storm10_sort]
;quartile_95 = rh_epa_storm10_sort[0.95*Nrh_epa_storm10_sort]
;
;rh_epa_storm10_ptile = [quartile_05, quartile_25, rh_med, quartile_75,quartile_95]
;
;
;;;;EPA exceedances without storms: rh
;rh_epa_nstorm10 = rh_nstorm_oct[iepa_oct_ns]
;rh_epa_nstorm10_sort  = rh_epa_nstorm10[SORT(rh_epa_nstorm10)]
;IF (N_ELEMENTS(rh_epa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
;	Nrh_epa_nstorm10_sort 	= N_ELEMENTS(rh_epa_nstorm10_sort)
;	rh_med = (rh_epa_nstorm10_sort[(Nrh_epa_nstorm10_sort/2)-1] + rh_epa_nstorm10_sort[(Nrh_epa_nstorm10_sort/2)]) / 2.0
;	lower_half = rh_epa_nstorm10_sort[0:(Nrh_epa_nstorm10_sort/2)-1]
;	upper_half = rh_epa_nstorm10_sort[(Nrh_epa_nstorm10_sort/2):(Nrh_epa_nstorm10_sort-1)]
;ENDIF ELSE BEGIN
;	Nrh_epa_nstorm10_sort 	= N_ELEMENTS(rh_epa_nstorm10_sort)
;	rh_med = rh_epa_nstorm10_sort[(Nrh_epa_nstorm10_sort/2)] 
;	lower_half = rh_epa_nstorm10_sort[0:(Nrh_epa_nstorm10_sort/2)-1]
;	upper_half = rh_epa_nstorm10_sort[(Nrh_epa_nstorm10_sort/2):(Nrh_epa_nstorm10_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = rh_epa_nstorm10_sort[0.05*Nrh_epa_nstorm10_sort]
;quartile_95 = rh_epa_nstorm10_sort[0.95*Nrh_epa_nstorm10_sort]
;
;rh_epa_nstorm10_ptile = [quartile_05, quartile_25, rh_med, quartile_75,quartile_95]
;
;
;;;;rhn-EPA exceedances with storms: rh
;rh_nepa_storm10 = rh_storm_oct[nepa_oct_s]
;rh_nepa_storm10_sort  = rh_nepa_storm10[SORT(rh_nepa_storm10)]
;IF (N_ELEMENTS(rh_nepa_storm10_sort) MOD 2 EQ 0) THEN BEGIN
;	Nrh_nepa_storm10_sort 	= N_ELEMENTS(rh_nepa_storm10_sort)
;	rh_med = (rh_nepa_storm10_sort[(Nrh_nepa_storm10_sort/2)-1] + rh_nepa_storm10_sort[(Nrh_nepa_storm10_sort/2)]) / 2.0
;	lower_half = rh_nepa_storm10_sort[0:(Nrh_nepa_storm10_sort/2)-1]
;	upper_half = rh_nepa_storm10_sort[(Nrh_nepa_storm10_sort/2):(Nrh_nepa_storm10_sort-1)]
;ENDIF ELSE BEGIN
;	Nrh_nepa_storm10_sort 	= N_ELEMENTS(rh_nepa_storm10_sort)
;	rh_med = rh_nepa_storm10_sort[(Nrh_nepa_storm10_sort/2)] 
;	lower_half = rh_nepa_storm10_sort[0:(Nrh_nepa_storm10_sort/2)-1]
;	upper_half = rh_nepa_storm10_sort[(Nrh_nepa_storm10_sort/2):(Nrh_nepa_storm10_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = rh_nepa_storm10_sort[0.05*Nrh_nepa_storm10_sort]
;quartile_95 = rh_nepa_storm10_sort[0.95*Nrh_nepa_storm10_sort]
;
;rh_nepa_storm10_ptile = [quartile_05, quartile_25, rh_med, quartile_75,quartile_95]
;
;
;;;;non-EPA exceedances with storms: rh
;rh_nepa_nstorm10 = rh_nstorm_oct[nepa_oct_ns]
;rh_nepa_nstorm10_sort  = rh_nepa_nstorm10[SORT(rh_nepa_nstorm10)]
;IF (N_ELEMENTS(rh_nepa_nstorm10_sort) MOD 2 EQ 0) THEN BEGIN
;	Nrh_nepa_nstorm10_sort 	= N_ELEMENTS(rh_nepa_nstorm10_sort)
;	rh_med = (rh_nepa_nstorm10_sort[(Nrh_nepa_nstorm10_sort/2)-1] + rh_nepa_nstorm10_sort[(Nrh_nepa_nstorm10_sort/2)]) / 2.0
;	lower_half = rh_nepa_nstorm10_sort[0:(Nrh_nepa_nstorm10_sort/2)-1]
;	upper_half = rh_nepa_nstorm10_sort[(Nrh_nepa_nstorm10_sort/2):(Nrh_nepa_nstorm10_sort-1)]
;ENDIF ELSE BEGIN
;	Nrh_nepa_nstorm10_sort 	= N_ELEMENTS(rh_nepa_nstorm10_sort)
;	rh_med = rh_nepa_nstorm10_sort[(Nrh_nepa_nstorm10_sort/2)] 
;	lower_half = rh_nepa_nstorm10_sort[0:(Nrh_nepa_nstorm10_sort/2)-1]
;	upper_half = rh_nepa_nstorm10_sort[(Nrh_nepa_nstorm10_sort/2):(Nrh_nepa_nstorm10_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = rh_nepa_nstorm10_sort[0.05*Nrh_nepa_nstorm10_sort]
;quartile_95 = rh_nepa_nstorm10_sort[0.95*Nrh_nepa_nstorm10_sort]
;
;rh_nepa_nstorm10_ptile = [quartile_05, quartile_25, rh_med, quartile_75,quartile_95]
;
;data = [[rh_epa_storm10_ptile], [rh_epa_nstorm10_ptile], [rh_nepa_storm10_ptile], [rh_nepa_nstorm10_ptile]]
;ytitle = ['EPA w/ Storms', 'EPA w/o Storms','non-EPA w/ Storms','non-EPA w/o Storms']
;boxes = BOXPLOT(data, $
;		TITLE		= 'Oct: RH', $
;		XRANGE 		= [30, 100], $
;		YRANGE 		= [-1, 4], $
;		XTITLE 		= 'RH (%)', $
;		YTICKNAME 	= ytitle, $
;		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
;		FONT_SIZE   = 20, $
;		HORIZONTAL	= 1)
;


STOP



























p2 = SCATTERPLOT( o3_array, temp_array, $
	TITLE  = 'O3 vs Temperature Anomalies: ' + STRTRIM(hour,1) + 'Z', $
	YTITLE = 'Temperature Anomalies (K)', $ 
	XTITLE = 'O3 Anomalies (ppb)', $
	YRANGE = [-25,25], $
	XRANGE = [-20,40])

l2 = LINFIT(o3_array, temp_array)
xplot2 = FINDGEN(60)-20
yplot2 = l2[0] + (l2[1])*xplot2

trend = PLOT(xplot2, yplot2, $
		/OVERPLOT, $
		COLOR = COLOR_24('red'), $
		THICK=2, $
		LINESTYLE=3, $
		XRANGE = [-20,40])

zero = PLOT([-20,40],[0,0], /OVERPLOT, THICK=2)

PRINT, 'O3-Temp Correlation: ' + STRING(CORRELATE(o3_array, temp_array))
PRINT, 'O3-Temp Covariance: ' + STRING(CORRELATE(o3_array, temp_array,/COVARIANCE))

p3= SCATTERPLOT(o3_array, gph_array, $
	TITLE  = 'O3 vs GPH Anomalies: ' + STRTRIM(hour,1) + 'Z', $
	YTITLE = 'Geopotential Height Anomalies (m)', $ 
	XTITLE = 'O3 Anomalies (ppb)', $
	YRANGE = [-100,100], $
	XRANGE = [-20,40])

l3 = LINFIT(o3_array, gph_array)
xplot3 = FINDGEN(60)-20
yplot3 = l3[0] + (l3[1])*xplot3

trend3 = PLOT(xplot3, yplot3, $
		/OVERPLOT, $
		COLOR = COLOR_24('red'), $
		THICK=2, $
		LINESTYLE=3, $
		xrange = [-20,40])

zero = PLOT([-20,40],[0,0], /OVERPLOT, THICK=2)

PRINT, 'O3-GPH Correlation: ' + STRING(CORRELATE(o3_array, gph_array))
PRINT, 'O3-GPH Covariance: ' + STRING(CORRELATE(o3_array,  gph_array,/COVARIANCE))


;;Write anomalies to file in case you want to look at different relationships


STOP

END
