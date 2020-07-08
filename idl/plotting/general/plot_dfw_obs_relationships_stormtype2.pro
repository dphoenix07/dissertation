PRO PLOT_DFW_OBS_RELATIONSHIPS_STORMTYPE2, $
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
;						  2019-03-07.	Like *_stormtype.pro but does not separate into
;										POLLUTION/NONPOLLUTION days
;-

COMPILE_OPT IDL2																									;Set compile options

o3_storm_hu = []
o3dm_storm_hu = []
voc_storm_hu = []
nox_storm_hu = []
no_storm_hu = []
no2_storm_hu = []
pm_storm_hu = []
co_storm_hu = []
temp_storm_hu = []
wind_spd_storm_hu = []
wind_dir_storm_hu = []
press_storm_hu = []
rh_storm_hu = []
dp_storm_hu = []

o3_nstorm_hu = []
o3dm_nstorm_hu = []
voc_nstorm_hu = []
nox_nstorm_hu = []
no_nstorm_hu = []
no2_nstorm_hu = []
pm_nstorm_hu = []
co_nstorm_hu = []
temp_nstorm_hu = []
wind_spd_nstorm_hu = []
wind_dir_nstorm_hu = []
press_nstorm_hu = []
rh_nstorm_hu = []
dp_nstorm_hu = []

infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_091980_102017_SEPT_18Z_HU_new_area.nc'																;Set input file path

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

o3_storm_hu = [o3_storm_hu, o3_storm]
o3dm_storm_hu = [o3dm_storm_hu, o3dm_storm]
nox_storm_hu = [nox_storm_hu, nox_storm]
no_storm_hu = [no_storm_hu, no_storm]
no2_storm_hu = [no2_storm_hu, no2_storm]
co_storm_hu = [co_storm_hu, co_storm]
temp_storm_hu = [temp_storm_hu, temp_storm]
wind_spd_storm_hu = [wind_spd_storm_hu, wind_spd_storm]
wind_dir_storm_hu = [wind_dir_storm_hu, wind_dir_storm]

o3_nstorm_hu = [o3_nstorm_hu, o3_nstorm]
o3dm_nstorm_hu = [o3dm_nstorm_hu, o3dm_nstorm]
nox_nstorm_hu = [nox_nstorm_hu, nox_nstorm]
no_nstorm_hu  = [no_nstorm_hu, no_nstorm]
no2_nstorm_hu = [no2_nstorm_hu, no2_nstorm]
co_nstorm_hu = [co_nstorm_hu, co_nstorm]
temp_nstorm_hu = [temp_nstorm_hu, temp_nstorm]
wind_spd_nstorm_hu = [wind_spd_nstorm_hu, wind_spd_nstorm]
wind_dir_nstorm_hu = [wind_dir_nstorm_hu, wind_dir_nstorm]

;infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_071996_082000_JULY_18Z_HU.nc'																;Set input file path
;
;;Get monthly averaged data
;id  = NCDF_OPEN(infile)																						;Open input file for reading	
;
;NCDF_VARGET, id, 'O3_storm'					, o3_storm
;NCDF_VARGET, id, 'O3DM_storm'				, o3dm_storm
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
;NCDF_VARGET, id, 'O3DM_nstorm'				, o3dm_nstorm
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
;o3_storm_hu = [o3_storm_hu, o3_storm]
;o3dm_storm_hu = [o3dm_storm_hu, o3dm_storm]
;voc_storm_hu = [voc_storm_hu, voc_storm]
;nox_storm_hu = [nox_storm_hu, nox_storm]
;no_storm_hu = [no_storm_hu, no_storm]
;no2_storm_hu = [no2_storm_hu, no2_storm]
;co_storm_hu = [co_storm_hu, co_storm]
;temp_storm_hu = [temp_storm_hu, temp_storm]
;wind_spd_storm_hu = [wind_spd_storm_hu, wind_spd_storm]
;wind_dir_storm_hu = [wind_dir_storm_hu, wind_dir_storm]
;
;o3_nstorm_hu = [o3_nstorm_hu, o3_nstorm]
;o3dm_nstorm_hu = [o3dm_nstorm_hu, o3dm_nstorm]
;voc_nstorm_hu = [voc_nstorm_hu, voc_nstorm]
;nox_nstorm_hu = [nox_nstorm_hu, nox_nstorm]
;no_nstorm_hu  = [no_nstorm_hu, no_nstorm]
;no2_nstorm_hu = [no2_nstorm_hu, no2_nstorm]
;co_nstorm_hu = [co_nstorm_hu, co_nstorm]
;temp_nstorm_hu = [temp_nstorm_hu, temp_nstorm]
;wind_spd_nstorm_hu = [wind_spd_nstorm_hu, wind_spd_nstorm]
;wind_dir_nstorm_hu = [wind_dir_nstorm_hu, wind_dir_nstorm]
;
;
;infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_072001_082004_JULY_18Z_HU.nc'																;Set input file path
;
;;Get monthly averaged data
;id  = NCDF_OPEN(infile)																						;Open input file for reading	
;
;NCDF_VARGET, id, 'O3_storm'					, o3_storm
;NCDF_VARGET, id, 'O3DM_storm'				, o3dm_storm
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
;NCDF_VARGET, id, 'O3DM_nstorm'				, o3dm_nstorm
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
;o3_storm_hu = [o3_storm_hu, o3_storm]
;o3dm_storm_hu = [o3dm_storm_hu, o3dm_storm]
;voc_storm_hu = [voc_storm_hu, voc_storm]
;nox_storm_hu = [nox_storm_hu, nox_storm]
;no_storm_hu = [no_storm_hu, no_storm]
;no2_storm_hu = [no2_storm_hu, no2_storm]
;co_storm_hu = [co_storm_hu, co_storm]
;temp_storm_hu = [temp_storm_hu, temp_storm]
;wind_spd_storm_hu = [wind_spd_storm_hu, wind_spd_storm]
;wind_dir_storm_hu = [wind_dir_storm_hu, wind_dir_storm]
;press_storm_hu = [press_storm_hu, press_storm]
;rh_storm_hu = [rh_storm_hu, rh_storm]
;dp_storm_hu = [dp_storm_hu, dp_storm]
;
;o3_nstorm_hu = [o3_nstorm_hu, o3_nstorm]
;o3dm_nstorm_hu = [o3dm_nstorm_hu, o3dm_nstorm]
;voc_nstorm_hu = [voc_nstorm_hu, voc_nstorm]
;nox_nstorm_hu = [nox_nstorm_hu, nox_nstorm]
;no_nstorm_hu  = [no_nstorm_hu, no_nstorm]
;no2_nstorm_hu = [no2_nstorm_hu, no2_nstorm]
;co_nstorm_hu = [co_nstorm_hu, co_nstorm]
;temp_nstorm_hu = [temp_nstorm_hu, temp_nstorm]
;wind_spd_nstorm_hu = [wind_spd_nstorm_hu, wind_spd_nstorm]
;wind_dir_nstorm_hu = [wind_dir_nstorm_hu, wind_dir_nstorm]
;press_nstorm_hu = [press_nstorm_hu, press_nstorm]
;rh_nstorm_hu = [rh_nstorm_hu, rh_nstorm]
;dp_nstorm_hu = [dp_nstorm_hu, dp_nstorm]
;
;
;infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_072005_082017_JULY_18Z_HU.nc'																;Set input file path
;
;;Get monthly averaged data
;id  = NCDF_OPEN(infile)																						;Open input file for reading	
;
;NCDF_VARGET, id, 'O3_storm'					, o3_storm
;NCDF_VARGET, id, 'O3DM_storm'				, o3dm_storm
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
;NCDF_VARGET, id, 'O3DM_nstorm'				, o3dm_nstorm
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
;o3_storm_hu = [o3_storm_hu, o3_storm]
;o3dm_storm_hu = [o3dm_storm_hu, o3dm_storm]
;voc_storm_hu = [voc_storm_hu, voc_storm]
;nox_storm_hu = [nox_storm_hu, nox_storm]
;no_storm_hu = [no_storm_hu, no_storm]
;no2_storm_hu = [no2_storm_hu, no2_storm]
;pm_storm_hu = [pm_storm_hu, pm_storm]
;co_storm_hu = [co_storm_hu, co_storm]
;temp_storm_hu = [temp_storm_hu, temp_storm]
;wind_spd_storm_hu = [wind_spd_storm_hu, wind_spd_storm]
;wind_dir_storm_hu = [wind_dir_storm_hu, wind_dir_storm]
;press_storm_hu = [press_storm_hu, press_storm]
;rh_storm_hu = [rh_storm_hu, rh_storm]
;dp_storm_hu = [dp_storm_hu, dp_storm]
;
;o3_nstorm_hu = [o3_nstorm_hu, o3_nstorm]
;o3dm_nstorm_hu = [o3dm_nstorm_hu, o3dm_nstorm]
;voc_nstorm_hu = [voc_nstorm_hu, voc_nstorm]
;nox_nstorm_hu = [nox_nstorm_hu, nox_nstorm]
;no_nstorm_hu  = [no_nstorm_hu, no_nstorm]
;no2_nstorm_hu = [no2_nstorm_hu, no2_nstorm]
;pm_nstorm_hu = [pm_nstorm_hu, pm_nstorm]
;co_nstorm_hu = [co_nstorm_hu, co_nstorm]
;temp_nstorm_hu = [temp_nstorm_hu, temp_nstorm]
;wind_spd_nstorm_hu = [wind_spd_nstorm_hu, wind_spd_nstorm]
;wind_dir_nstorm_hu = [wind_dir_nstorm_hu, wind_dir_nstorm]
;press_nstorm_hu = [press_nstorm_hu, press_nstorm]
;rh_nstorm_hu = [rh_nstorm_hu, rh_nstorm]
;dp_nstorm_hu = [dp_nstorm_hu, dp_nstorm]
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

o3_storm_ts = []
o3dm_storm_ts = []
voc_storm_ts = []
nox_storm_ts = []
no_storm_ts = []
no2_storm_ts = []
pm_storm_ts = []
co_storm_ts = []
temp_storm_ts = []
wind_spd_storm_ts = []
wind_dir_storm_ts = []
press_storm_ts = []
rh_storm_ts = []
dp_storm_ts = []

o3_nstorm_ts = []
o3dm_nstorm_ts = []
voc_nstorm_ts = []
nox_nstorm_ts = []
no_nstorm_ts = []
no2_nstorm_ts = []
pm_nstorm_ts = []
co_nstorm_ts = []
temp_nstorm_ts = []
wind_spd_nstorm_ts = []
wind_dir_nstorm_ts = []
press_nstorm_ts = []
rh_nstorm_ts = []
dp_nstorm_ts = []

infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_091980_102017_SEPT_18Z_TS_new_area.nc'																;Set input file path

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

o3_storm_ts = [o3_storm_ts, o3_storm]
o3dm_storm_ts = [o3dm_storm_ts, o3dm_storm]
nox_storm_ts = [nox_storm_ts, nox_storm]
no_storm_ts = [no_storm_ts, no_storm]
no2_storm_ts = [no2_storm_ts, no2_storm]
co_storm_ts = [co_storm_ts, co_storm]
temp_storm_ts = [temp_storm_ts, temp_storm]
wind_spd_storm_ts = [wind_spd_storm_ts, wind_spd_storm]
wind_dir_storm_ts = [wind_dir_storm_ts, wind_dir_storm]

o3_nstorm_ts = [o3_nstorm_ts, o3_nstorm]
o3dm_nstorm_ts = [o3dm_nstorm_ts, o3dm_nstorm]
nox_nstorm_ts = [nox_nstorm_ts, nox_nstorm]
no_nstorm_ts = [no_nstorm_ts, no_nstorm]
no2_nstorm_ts = [no2_nstorm_ts, no2_nstorm]
co_nstorm_ts = [co_nstorm_ts, co_nstorm]
temp_nstorm_ts = [temp_nstorm_ts, temp_nstorm]
wind_spd_nstorm_ts = [wind_spd_nstorm_ts, wind_spd_nstorm]
wind_dir_nstorm_ts = [wind_dir_nstorm_ts, wind_dir_nstorm]

;infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_071996_082000_JULY_18Z_TS.nc'																;Set input file path
;
;;Get monthly averaged data
;id  = NCDF_OPEN(infile)																						;Open input file for reading	
;
;NCDF_VARGET, id, 'O3_storm'					, o3_storm
;NCDF_VARGET, id, 'O3DM_storm'				, o3dm_storm
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
;NCDF_VARGET, id, 'O3DM_nstorm'				, o3dm_nstorm
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
;o3_storm_ts = [o3_storm_ts, o3_storm]
;o3dm_storm_ts = [o3dm_storm_ts, o3dm_storm]
;voc_storm_ts = [voc_storm_ts, voc_storm]
;nox_storm_ts = [nox_storm_ts, nox_storm]
;no_storm_ts = [no_storm_ts, no_storm]
;no2_storm_ts = [no2_storm_ts, no2_storm]
;co_storm_ts = [co_storm_ts, co_storm]
;temp_storm_ts = [temp_storm_ts, temp_storm]
;wind_spd_storm_ts = [wind_spd_storm_ts, wind_spd_storm]
;wind_dir_storm_ts = [wind_dir_storm_ts, wind_dir_storm]
;
;o3_nstorm_ts = [o3_nstorm_ts, o3_nstorm]
;o3dm_nstorm_ts = [o3dm_nstorm_ts, o3dm_nstorm]
;voc_nstorm_ts = [voc_nstorm_ts, voc_nstorm]
;nox_nstorm_ts = [nox_nstorm_ts, nox_nstorm]
;no_nstorm_ts = [no_nstorm_ts, no_nstorm]
;no2_nstorm_ts = [no2_nstorm_ts, no2_nstorm]
;co_nstorm_ts = [co_nstorm_ts, co_nstorm]
;temp_nstorm_ts = [temp_nstorm_ts, temp_nstorm]
;wind_spd_nstorm_ts = [wind_spd_nstorm_ts, wind_spd_nstorm]
;wind_dir_nstorm_ts = [wind_dir_nstorm_ts, wind_dir_nstorm]
;
;
;infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_072001_082004_JULY_18Z_TS.nc'																;Set input file path
;
;;Get monthly averaged data
;id  = NCDF_OPEN(infile)																						;Open input file for reading	
;
;NCDF_VARGET, id, 'O3_storm'					, o3_storm
;NCDF_VARGET, id, 'O3DM_storm'				, o3dm_storm
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
;NCDF_VARGET, id, 'O3DM_nstorm'				, o3dm_nstorm
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
;o3_storm_ts = [o3_storm_ts, o3_storm]
;o3dm_storm_ts = [o3dm_storm_ts, o3dm_storm]
;voc_storm_ts = [voc_storm_ts, voc_storm]
;nox_storm_ts = [nox_storm_ts, nox_storm]
;no_storm_ts = [no_storm_ts, no_storm]
;no2_storm_ts = [no2_storm_ts, no2_storm]
;co_storm_ts = [co_storm_ts, co_storm]
;temp_storm_ts = [temp_storm_ts, temp_storm]
;wind_spd_storm_ts = [wind_spd_storm_ts, wind_spd_storm]
;wind_dir_storm_ts = [wind_dir_storm_ts, wind_dir_storm]
;press_storm_ts = [press_storm_ts, press_storm]
;rh_storm_ts = [rh_storm_ts, rh_storm]
;dp_storm_ts = [dp_storm_ts, dp_storm]
;
;o3_nstorm_ts = [o3_nstorm_ts, o3_nstorm]
;o3dm_nstorm_ts = [o3dm_nstorm_ts, o3dm_nstorm]
;voc_nstorm_ts = [voc_nstorm_ts, voc_nstorm]
;nox_nstorm_ts = [nox_nstorm_ts, nox_nstorm]
;no_nstorm_ts = [no_nstorm_ts, no_nstorm]
;no2_nstorm_ts = [no2_nstorm_ts, no2_nstorm]
;co_nstorm_ts = [co_nstorm_ts, co_nstorm]
;temp_nstorm_ts = [temp_nstorm_ts, temp_nstorm]
;wind_spd_nstorm_ts = [wind_spd_nstorm_ts, wind_spd_nstorm]
;wind_dir_nstorm_ts = [wind_dir_nstorm_ts, wind_dir_nstorm]
;press_nstorm_ts = [press_nstorm_ts, press_nstorm]
;rh_nstorm_ts = [rh_nstorm_ts, rh_nstorm]
;dp_nstorm_ts = [dp_nstorm_ts, dp_nstorm]
;
;
;infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_072005_082017_JULY_18Z_TS.nc'																;Set input file path
;
;;Get monthly averaged data
;id  = NCDF_OPEN(infile)																						;Open input file for reading	
;
;NCDF_VARGET, id, 'O3_storm'					, o3_storm
;NCDF_VARGET, id, 'O3DM_storm'				, o3dm_storm
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
;NCDF_VARGET, id, 'O3DM_nstorm'				, o3dm_nstorm
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
;o3_storm_ts = [o3_storm_ts, o3_storm]
;o3dm_storm_ts = [o3dm_storm_ts, o3dm_storm]
;voc_storm_ts = [voc_storm_ts, voc_storm]
;nox_storm_ts = [nox_storm_ts, nox_storm]
;no_storm_ts = [no_storm_ts, no_storm]
;no2_storm_ts = [no2_storm_ts, no2_storm]
;pm_storm_ts = [pm_storm_ts, pm_storm]
;co_storm_ts = [co_storm_ts, co_storm]
;temp_storm_ts = [temp_storm_ts, temp_storm]
;wind_spd_storm_ts = [wind_spd_storm_ts, wind_spd_storm]
;wind_dir_storm_ts = [wind_dir_storm_ts, wind_dir_storm]
;press_storm_ts = [press_storm_ts, press_storm]
;rh_storm_ts = [rh_storm_ts, rh_storm]
;dp_storm_ts = [dp_storm_ts, dp_storm]
;
;o3_nstorm_ts = [o3_nstorm_ts, o3_nstorm]
;o3dm_nstorm_ts = [o3dm_nstorm_ts, o3dm_nstorm]
;voc_nstorm_ts = [voc_nstorm_ts, voc_nstorm]
;nox_nstorm_ts = [nox_nstorm_ts, nox_nstorm]
;no_nstorm_ts = [no_nstorm_ts, no_nstorm]
;no2_nstorm_ts = [no2_nstorm_ts, no2_nstorm]
;pm_nstorm_ts = [pm_nstorm_ts, pm_nstorm]
;co_nstorm_ts = [co_nstorm_ts, co_nstorm]
;temp_nstorm_ts = [temp_nstorm_ts, temp_nstorm]
;wind_spd_nstorm_ts = [wind_spd_nstorm_ts, wind_spd_nstorm]
;wind_dir_nstorm_ts = [wind_dir_nstorm_ts, wind_dir_nstorm]
;press_nstorm_ts = [press_nstorm_ts, press_nstorm]
;rh_nstorm_ts = [rh_nstorm_ts, rh_nstorm]
;dp_nstorm_ts = [dp_nstorm_ts, dp_nstorm]
;	 	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

o3_storm_td = []
o3dm_storm_td = []
voc_storm_td = []
nox_storm_td = []
no_storm_td = []
no2_storm_td = []
pm_storm_td = []
co_storm_td = []
temp_storm_td = []
wind_spd_storm_td = []
wind_dir_storm_td = []
press_storm_td = []
rh_storm_td = []
dp_storm_td = []

o3_nstorm_td = []
o3dm_nstorm_td = []
voc_nstorm_td = []
nox_nstorm_td = []
no_nstorm_td = []
no2_nstorm_td = []
pm_nstorm_td = []
co_nstorm_td = []
temp_nstorm_td = []
wind_spd_nstorm_td = []
wind_dir_nstorm_td = []
press_nstorm_td = []
rh_nstorm_td = []
dp_nstorm_td = []

infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_091980_102017_SEPT_18Z_TD_new_area.nc'																;Set input file path

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

o3_storm_td = [o3_storm_td, o3_storm]
o3dm_storm_td = [o3dm_storm_td, o3dm_storm]
nox_storm_td = [nox_storm_td, nox_storm]
no_storm_td = [no_storm_td, no_storm]
no2_storm_td = [no2_storm_td, no2_storm]
co_storm_td = [co_storm_td, co_storm]
temp_storm_td = [temp_storm_td, temp_storm]
wind_spd_storm_td = [wind_spd_storm_td, wind_spd_storm]
wind_dir_storm_td = [wind_dir_storm_td, wind_dir_storm]

o3_nstorm_td = [o3_nstorm_td, o3_nstorm]
o3dm_nstorm_td = [o3dm_nstorm_td, o3dm_nstorm]
nox_nstorm_td = [nox_nstorm_td, nox_nstorm]
no_nstorm_td = [no_nstorm_td, no_nstorm]
no2_nstorm_td = [no2_nstorm_td, no2_nstorm]
co_nstorm_td = [co_nstorm_td, co_nstorm]
temp_nstorm_td = [temp_nstorm_td, temp_nstorm]
wind_spd_nstorm_td = [wind_spd_nstorm_td, wind_spd_nstorm]
wind_dir_nstorm_td = [wind_dir_nstorm_td, wind_dir_nstorm]

;infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_071996_082000_JULY_18Z_TD.nc'																;Set input file path
;
;;Get monthly averaged data
;id  = NCDF_OPEN(infile)																						;Open input file for reading	
;
;NCDF_VARGET, id, 'O3_storm'					, o3_storm
;NCDF_VARGET, id, 'O3DM_storm'				, o3dm_storm
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
;NCDF_VARGET, id, 'O3DM_nstorm'				, o3dm_nstorm
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
;o3_storm_td = [o3_storm_td, o3_storm]
;o3dm_storm_td = [o3dm_storm_td, o3dm_storm]
;voc_storm_td = [voc_storm_td, voc_storm]
;nox_storm_td = [nox_storm_td, nox_storm]
;no_storm_td = [no_storm_td, no_storm]
;no2_storm_td = [no2_storm_td, no2_storm]
;co_storm_td = [co_storm_td, co_storm]
;temp_storm_td = [temp_storm_td, temp_storm]
;wind_spd_storm_td = [wind_spd_storm_td, wind_spd_storm]
;wind_dir_storm_td = [wind_dir_storm_td, wind_dir_storm]
;
;o3_nstorm_td = [o3_nstorm_td, o3_nstorm]
;o3dm_nstorm_td = [o3dm_nstorm_td, o3dm_nstorm]
;voc_nstorm_td = [voc_nstorm_td, voc_nstorm]
;nox_nstorm_td = [nox_nstorm_td, nox_nstorm]
;no_nstorm_td = [no_nstorm_td, no_nstorm]
;no2_nstorm_td = [no2_nstorm_td, no2_nstorm]
;co_nstorm_td = [co_nstorm_td, co_nstorm]
;temp_nstorm_td = [temp_nstorm_td, temp_nstorm]
;wind_spd_nstorm_td = [wind_spd_nstorm_td, wind_spd_nstorm]
;wind_dir_nstorm_td = [wind_dir_nstorm_td, wind_dir_nstorm]
;
;
;infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_072001_082004_JULY_18Z_TD.nc'																;Set input file path
;
;;Get monthly averaged data
;id  = NCDF_OPEN(infile)																						;Open input file for reading	
;
;NCDF_VARGET, id, 'O3_storm'					, o3_storm
;NCDF_VARGET, id, 'O3DM_storm'				, o3dm_storm
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
;NCDF_VARGET, id, 'O3DM_nstorm'				, o3dm_nstorm
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
;o3_storm_td = [o3_storm_td, o3_storm]
;o3dm_storm_td = [o3dm_storm_td, o3dm_storm]
;voc_storm_td = [voc_storm_td, voc_storm]
;nox_storm_td = [nox_storm_td, nox_storm]
;no_storm_td = [no_storm_td, no_storm]
;no2_storm_td = [no2_storm_td, no2_storm]
;co_storm_td = [co_storm_td, co_storm]
;temp_storm_td = [temp_storm_td, temp_storm]
;wind_spd_storm_td = [wind_spd_storm_td, wind_spd_storm]
;wind_dir_storm_td = [wind_dir_storm_td, wind_dir_storm]
;press_storm_td = [press_storm_td, press_storm]
;rh_storm_td = [rh_storm_td, rh_storm]
;dp_storm_td = [dp_storm_td, dp_storm]
;
;o3_nstorm_td = [o3_nstorm_td, o3_nstorm]
;o3dm_nstorm_td = [o3dm_nstorm_td, o3dm_nstorm]
;voc_nstorm_td = [voc_nstorm_td, voc_nstorm]
;nox_nstorm_td = [nox_nstorm_td, nox_nstorm]
;no_nstorm_td = [no_nstorm_td, no_nstorm]
;no2_nstorm_td = [no2_nstorm_td, no2_nstorm]
;co_nstorm_td = [co_nstorm_td, co_nstorm]
;temp_nstorm_td = [temp_nstorm_td, temp_nstorm]
;wind_spd_nstorm_td = [wind_spd_nstorm_td, wind_spd_nstorm]
;wind_dir_nstorm_td = [wind_dir_nstorm_td, wind_dir_nstorm]
;press_nstorm_td = [press_nstorm_td, press_nstorm]
;rh_nstorm_td = [rh_nstorm_td, rh_nstorm]
;dp_nstorm_td = [dp_nstorm_td, dp_nstorm]
;
;
;infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_072005_082017_JULY_18Z_TD.nc'																;Set input file path
;
;;Get monthly averaged data
;id  = NCDF_OPEN(infile)																						;Open input file for reading	
;
;NCDF_VARGET, id, 'O3_storm'					, o3_storm
;NCDF_VARGET, id, 'O3DM_storm'				, o3dm_storm
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
;NCDF_VARGET, id, 'O3DM_nstorm'				, o3dm_nstorm
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
;o3_storm_td = [o3_storm_td, o3_storm]
;o3dm_storm_td = [o3dm_storm_td, o3dm_storm]
;voc_storm_td = [voc_storm_td, voc_storm]
;nox_storm_td = [nox_storm_td, nox_storm]
;no_storm_td = [no_storm_td, no_storm]
;no2_storm_td = [no2_storm_td, no2_storm]
;pm_storm_td = [pm_storm_td, pm_storm]
;co_storm_td = [co_storm_td, co_storm]
;temp_storm_td = [temp_storm_td, temp_storm]
;wind_spd_storm_td = [wind_spd_storm_td, wind_spd_storm]
;wind_dir_storm_td = [wind_dir_storm_td, wind_dir_storm]
;press_storm_td = [press_storm_td, press_storm]
;rh_storm_td = [rh_storm_td, rh_storm]
;dp_storm_td = [dp_storm_td, dp_storm]
;
;o3_nstorm_td = [o3_nstorm_td, o3_nstorm]
;o3dm_nstorm_td = [o3dm_nstorm_td, o3dm_nstorm]
;voc_nstorm_td = [voc_nstorm_td, voc_nstorm]
;nox_nstorm_td = [nox_nstorm_td, nox_nstorm]
;no_nstorm_td = [no_nstorm_td, no_nstorm]
;no2_nstorm_td = [no2_nstorm_td, no2_nstorm]
;pm_nstorm_td = [pm_nstorm_td, pm_nstorm]
;co_nstorm_td = [co_nstorm_td, co_nstorm]
;temp_nstorm_td = [temp_nstorm_td, temp_nstorm]
;wind_spd_nstorm_td = [wind_spd_nstorm_td, wind_spd_nstorm]
;wind_dir_nstorm_td = [wind_dir_nstorm_td, wind_dir_nstorm]
;press_nstorm_td = [press_nstorm_td, press_nstorm]
;rh_nstorm_td = [rh_nstorm_td, rh_nstorm]
;dp_nstorm_td = [dp_nstorm_td, dp_nstorm]

fname = '/data3/dphoenix/wrf/general/met_chem_boxplots/sept_mean_18Z_stormcomp2_new_area.txt'
OPENW, lun, fname, /GET_LUN     
PRINTF, lun, FORMAT = '("  Variable", 10X, "TD", 12X, "TS", 12X, "HU")'								
PRINTF, lun, '==================================================================='
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'O3', 		MEAN(o3_storm_td), 	        MEAN(o3_storm_ts),		 MEAN(o3_storm_hu)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'O3dm', 		MEAN(o3dm_storm_td),        MEAN(o3dm_storm_ts), 	 MEAN(o3dm_storm_hu)		
;PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'voc',        MEAN(voc_storm_td),         MEAN(voc_storm_ts), 	 MEAN(voc_storm_hu)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'nox',        MEAN(nox_storm_td),         MEAN(nox_storm_ts), 	 MEAN(nox_storm_hu)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'no', 		MEAN(no_storm_td),	        MEAN(no_storm_ts),		 MEAN(no_storm_hu)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'no2', 		MEAN(no2_storm_td),         MEAN(no2_storm_ts), 	 MEAN(no2_storm_hu)		
;PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'pm', 		MEAN(pm_storm_td),          MEAN(pm_storm_ts), 		 MEAN(pm_storm_hu)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'co', 		MEAN(co_storm_td),          MEAN(co_storm_ts), 		 MEAN(co_storm_hu)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'temp', 		MEAN(temp_storm_td),        MEAN(temp_storm_ts), 	 MEAN(temp_storm_hu)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'wind_spd', 	MEAN(wind_spd_storm_td), 	MEAN(wind_spd_storm_ts), MEAN(wind_spd_storm_hu)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'wind_dir', 	MEAN(wind_dir_storm_td), 	MEAN(wind_dir_storm_ts), MEAN(wind_dir_storm_hu)		
;PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'press', 		MEAN(press_storm_td), 		MEAN(press_storm_ts), 	 MEAN(press_storm_hu)		
;PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'rh', 		MEAN(rh_storm_td), 			MEAN(rh_storm_ts), 		 MEAN(rh_storm_hu)		
;PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'dp', 		MEAN(dp_storm_td), 			MEAN(dp_storm_ts), 		 MEAN(dp_storm_hu)		

PRINTF, lun, ' '

PRINTF, lun, 'Number of days with storms td = ', N_ELEMENTS(o3_storm_td)
PRINTF, lun, 'Number of days with storms ts = ', N_ELEMENTS(o3_storm_ts)
PRINTF, lun, 'Number of days with storms hu = ', N_ELEMENTS(o3_storm_hu)


FREE_LUN, lun

;correlations  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;p2 = SCATTERPLOT( wind_dir_storm_td[iepa_ts_s], co_storm_td[iepa_ts_s], $
;	TITLE  = 'Wind Dir vs CO: ' + STRTRIM(hour,1) + 'Z', $
;	YTITLE = 'Wind Direction (deg)', $ 
;	XTITLE = 'Co Concentration (ppb)');, $
;	;YRANGE = [-25,25], $
;	;XRANGE = [-20,40])
;
;l2 = LINFIT(o3_array, temp_array)
;xplot2 = FINDGEN(60)-20
;yplot2 = l2[0] + (l2[1])*xplot2
;
;trend = PLOT(xplot2, yplot2, $
;		/OVERPLOT, $
;		COLOR = COLOR_24('red'), $
;		THICK=2, $
;		LINESTYLE=3, $
;		XRANGE = [-20,40])
;
;zero = PLOT([-20,40],[0,0], /OVERPLOT, THICK=2)
;
;PRINT, 'O3-Temp Correlation: ' + STRING(CORRELATE(o3_array, temp_array))
;PRINT, 'O3-Temp Covariance: ' + STRING(CORRELATE(o3_array, temp_array,/COVARIANCE))
;


;boxplot code 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;EPA exceedances without storms: OZONE DAILY MAX
o3dm_epa_td = o3dm_storm_td
o3dm_epa_td_sort  = o3dm_epa_td[SORT(o3dm_epa_td)]
IF (N_ELEMENTS(o3dm_epa_td_sort) MOD 2 EQ 0) THEN BEGIN
	No3dm_epa_td_sort 	= N_ELEMENTS(o3dm_epa_td_sort)
	o3dm_med = (o3dm_epa_td_sort[(No3dm_epa_td_sort/2)-1] + o3dm_epa_td_sort[(No3dm_epa_td_sort/2)]) / 2.0
	lower_half = o3dm_epa_td_sort[0:(No3dm_epa_td_sort/2)-1]
	upper_half = o3dm_epa_td_sort[(No3dm_epa_td_sort/2):(No3dm_epa_td_sort-1)]
ENDIF ELSE BEGIN
	No3dm_epa_td_sort 	= N_ELEMENTS(o3dm_epa_td_sort)
	o3dm_med = o3dm_epa_td_sort[(No3dm_epa_td_sort/2)] 
	lower_half = o3dm_epa_td_sort[0:(No3dm_epa_td_sort/2)-1]
	upper_half = o3dm_epa_td_sort[(No3dm_epa_td_sort/2):(No3dm_epa_td_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3dm_epa_td_sort[0.05*No3dm_epa_td_sort]
quartile_95 = o3dm_epa_td_sort[0.95*No3dm_epa_td_sort]

o3dm_epa_td_ptile = [quartile_05, quartile_25, o3dm_med, quartile_75,quartile_95]


;;;EPA exceedances TS: OZONE DAILY MAX
o3dm_epa_ts = o3dm_storm_ts
o3dm_epa_ts_sort  = o3dm_epa_ts[SORT(o3dm_epa_ts)]
IF (N_ELEMENTS(o3dm_epa_ts_sort) MOD 2 EQ 0) THEN BEGIN
	No3dm_epa_ts_sort 	= N_ELEMENTS(o3dm_epa_ts_sort)
	o3dm_med = (o3dm_epa_ts_sort[(No3dm_epa_ts_sort/2)-1] + o3dm_epa_ts_sort[(No3dm_epa_ts_sort/2)]) / 2.0
	lower_half = o3dm_epa_ts_sort[0:(No3dm_epa_ts_sort/2)-1]
	upper_half = o3dm_epa_ts_sort[(No3dm_epa_ts_sort/2):(No3dm_epa_ts_sort-1)]
ENDIF ELSE BEGIN
	No3dm_epa_ts_sort 	= N_ELEMENTS(o3dm_epa_ts_sort)
	o3dm_med = o3dm_epa_ts_sort[(No3dm_epa_ts_sort/2)] 
	lower_half = o3dm_epa_ts_sort[0:(No3dm_epa_ts_sort/2)-1]
	upper_half = o3dm_epa_ts_sort[(No3dm_epa_ts_sort/2):(No3dm_epa_ts_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3dm_epa_ts_sort[0.05*No3dm_epa_ts_sort]
quartile_95 = o3dm_epa_ts_sort[0.95*No3dm_epa_ts_sort]

o3dm_epa_ts_ptile = [quartile_05, quartile_25, o3dm_med, quartile_75,quartile_95]

 
;;;EPA exceedances HU: OZONE DAILY MAX
o3dm_epa_hu = o3dm_storm_hu
o3dm_epa_hu_sort  = o3dm_epa_hu[SORT(o3dm_epa_hu)]
IF (N_ELEMENTS(o3dm_epa_hu_sort) MOD 2 EQ 0) THEN BEGIN
	No3dm_epa_hu_sort 	= N_ELEMENTS(o3dm_epa_hu_sort)
	o3dm_med = (o3dm_epa_hu_sort[(No3dm_epa_hu_sort/2)-1] + o3dm_epa_hu_sort[(No3dm_epa_hu_sort/2)]) / 2.0
	lower_half = o3dm_epa_hu_sort[0:(No3dm_epa_hu_sort/2)-1]
	upper_half = o3dm_epa_hu_sort[(No3dm_epa_hu_sort/2):(No3dm_epa_hu_sort-1)]
ENDIF ELSE BEGIN
	No3dm_epa_hu_sort 	= N_ELEMENTS(o3dm_epa_hu_sort)
	o3dm_med = o3dm_epa_hu_sort[(No3dm_epa_hu_sort/2)] 
	lower_half = o3dm_epa_hu_sort[0:(No3dm_epa_hu_sort/2)-1]
	upper_half = o3dm_epa_hu_sort[(No3dm_epa_hu_sort/2):(No3dm_epa_hu_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3dm_epa_hu_sort[0.05*No3dm_epa_hu_sort]
quartile_95 = o3dm_epa_hu_sort[0.95*No3dm_epa_hu_sort]

o3dm_epa_hu_ptile = [quartile_05, quartile_25, o3dm_med, quartile_75,quartile_95]



data = [[o3dm_epa_td_ptile], [o3dm_epa_ts_ptile], [o3dm_epa_hu_ptile]]
ytitle = ['EPA TD','EPA TS','EPA HU']
boxes = BOXPLOT(data, $
		TITLE		= 'Sept: Ozone Daily Max', $
		XRANGE 		= [10,120], $
		YRANGE 		= [-1, 3], $
		XTITLE 		= 'o3dm Concentration (ppb)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;EPA exceedances without storms: OZONE
o3_epa_td = o3_storm_td
o3_epa_td_sort  = o3_epa_td[SORT(o3_epa_td)]
IF (N_ELEMENTS(o3_epa_td_sort) MOD 2 EQ 0) THEN BEGIN
	No3_epa_td_sort 	= N_ELEMENTS(o3_epa_td_sort)
	o3_med = (o3_epa_td_sort[(No3_epa_td_sort/2)-1] + o3_epa_td_sort[(No3_epa_td_sort/2)]) / 2.0
	lower_half = o3_epa_td_sort[0:(No3_epa_td_sort/2)-1]
	upper_half = o3_epa_td_sort[(No3_epa_td_sort/2):(No3_epa_td_sort-1)]
ENDIF ELSE BEGIN
	No3_epa_td_sort 	= N_ELEMENTS(o3_epa_td_sort)
	o3_med = o3_epa_td_sort[(No3_epa_td_sort/2)] 
	lower_half = o3_epa_td_sort[0:(No3_epa_td_sort/2)-1]
	upper_half = o3_epa_td_sort[(No3_epa_td_sort/2):(No3_epa_td_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_epa_td_sort[0.05*No3_epa_td_sort]
quartile_95 = o3_epa_td_sort[0.95*No3_epa_td_sort]

o3_epa_td_ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]


;;;EPA exceedances TS: OZONE DAILY MAX
o3_epa_ts = o3_storm_ts
o3_epa_ts_sort  = o3_epa_ts[SORT(o3_epa_ts)]
IF (N_ELEMENTS(o3_epa_ts_sort) MOD 2 EQ 0) THEN BEGIN
	No3_epa_ts_sort 	= N_ELEMENTS(o3_epa_ts_sort)
	o3_med = (o3_epa_ts_sort[(No3_epa_ts_sort/2)-1] + o3_epa_ts_sort[(No3_epa_ts_sort/2)]) / 2.0
	lower_half = o3_epa_ts_sort[0:(No3_epa_ts_sort/2)-1]
	upper_half = o3_epa_ts_sort[(No3_epa_ts_sort/2):(No3_epa_ts_sort-1)]
ENDIF ELSE BEGIN
	No3_epa_ts_sort 	= N_ELEMENTS(o3_epa_ts_sort)
	o3_med = o3_epa_ts_sort[(No3_epa_ts_sort/2)] 
	lower_half = o3_epa_ts_sort[0:(No3_epa_ts_sort/2)-1]
	upper_half = o3_epa_ts_sort[(No3_epa_ts_sort/2):(No3_epa_ts_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_epa_ts_sort[0.05*No3_epa_ts_sort]
quartile_95 = o3_epa_ts_sort[0.95*No3_epa_ts_sort]

o3_epa_ts_ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]


;;;EPA exceedances HU: OZONE DAILY MAX
o3_epa_hu = o3_storm_hu
o3_epa_hu_sort  = o3_epa_hu[SORT(o3_epa_hu)]
IF (N_ELEMENTS(o3_epa_hu_sort) MOD 2 EQ 0) THEN BEGIN
	No3_epa_hu_sort 	= N_ELEMENTS(o3_epa_hu_sort)
	o3_med = (o3_epa_hu_sort[(No3_epa_hu_sort/2)-1] + o3_epa_hu_sort[(No3_epa_hu_sort/2)]) / 2.0
	lower_half = o3_epa_hu_sort[0:(No3_epa_hu_sort/2)-1]
	upper_half = o3_epa_hu_sort[(No3_epa_hu_sort/2):(No3_epa_hu_sort-1)]
ENDIF ELSE BEGIN
	No3_epa_hu_sort 	= N_ELEMENTS(o3_epa_hu_sort)
	o3_med = o3_epa_hu_sort[(No3_epa_hu_sort/2)] 
	lower_half = o3_epa_hu_sort[0:(No3_epa_hu_sort/2)-1]
	upper_half = o3_epa_hu_sort[(No3_epa_hu_sort/2):(No3_epa_hu_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_epa_hu_sort[0.05*No3_epa_hu_sort]
quartile_95 = o3_epa_hu_sort[0.95*No3_epa_hu_sort]

o3_epa_hu_ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]


data = [[o3_epa_td_ptile], [o3_epa_ts_ptile], [o3_epa_hu_ptile]]
ytitle = ['EPA TD','EPA TS','EPA HU']
boxes = BOXPLOT(data, $
		TITLE		= 'Sept: Ozone', $
		XRANGE 		= [10,130], $
		YRANGE 		= [-1, 3], $
		XTITLE 		= 'O3 Concentration (ppb)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;EPA exceedances with storms: voc
;voc_epa_td = voc_storm_td
;voc_epa_td_sort  = voc_epa_td[SORT(voc_epa_td)]
;IF (N_ELEMENTS(voc_epa_td_sort) MOD 2 EQ 0) THEN BEGIN
;	Nvoc_epa_td_sort 	= N_ELEMENTS(voc_epa_td_sort)
;	voc_med = (voc_epa_td_sort[(Nvoc_epa_td_sort/2)-1] + voc_epa_td_sort[(Nvoc_epa_td_sort/2)]) / 2.0
;	lower_half = voc_epa_td_sort[0:(Nvoc_epa_td_sort/2)-1]
;	upper_half = voc_epa_td_sort[(Nvoc_epa_td_sort/2):(Nvoc_epa_td_sort-1)]
;ENDIF ELSE BEGIN
;	Nvoc_epa_td_sort 	= N_ELEMENTS(voc_epa_td_sort)
;	voc_med = voc_epa_td_sort[(Nvoc_epa_td_sort/2)] 
;	lower_half = voc_epa_td_sort[0:(Nvoc_epa_td_sort/2)-1]
;	upper_half = voc_epa_td_sort[(Nvoc_epa_td_sort/2):(Nvoc_epa_td_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = voc_epa_td_sort[0.05*Nvoc_epa_td_sort]
;quartile_95 = voc_epa_td_sort[0.95*Nvoc_epa_td_sort]
;
;voc_epa_td_ptile = [quartile_05, quartile_25, voc_med, quartile_75,quartile_95]
;
;
;;;;EPA exceedances TS: VOC
;voc_epa_ts = voc_storm_ts
;voc_epa_ts_sort  = voc_epa_ts[SORT(voc_epa_ts)]
;IF (N_ELEMENTS(voc_epa_ts_sort) MOD 2 EQ 0) THEN BEGIN
;	Nvoc_epa_ts_sort 	= N_ELEMENTS(voc_epa_ts_sort)
;	voc_med = (voc_epa_ts_sort[(Nvoc_epa_ts_sort/2)-1] + voc_epa_ts_sort[(Nvoc_epa_ts_sort/2)]) / 2.0
;	lower_half = voc_epa_ts_sort[0:(Nvoc_epa_ts_sort/2)-1]
;	upper_half = voc_epa_ts_sort[(Nvoc_epa_ts_sort/2):(Nvoc_epa_ts_sort-1)]
;ENDIF ELSE BEGIN
;	Nvoc_epa_ts_sort 	= N_ELEMENTS(voc_epa_ts_sort)
;	voc_med = voc_epa_ts_sort[(Nvoc_epa_ts_sort/2)] 
;	lower_half = voc_epa_ts_sort[0:(Nvoc_epa_ts_sort/2)-1]
;	upper_half = voc_epa_ts_sort[(Nvoc_epa_ts_sort/2):(Nvoc_epa_ts_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = voc_epa_ts_sort[0.05*Nvoc_epa_ts_sort]
;quartile_95 = voc_epa_ts_sort[0.95*Nvoc_epa_ts_sort]
;
;voc_epa_ts_ptile = [quartile_05, quartile_25, voc_med, quartile_75,quartile_95]
;
; 
;;;;EPA exceedances HU: VOC 
;voc_epa_hu = voc_storm_hu
;voc_epa_hu_sort  = voc_epa_hu[SORT(voc_epa_hu)]
;IF (N_ELEMENTS(voc_epa_hu_sort) MOD 2 EQ 0) THEN BEGIN
;	Nvoc_epa_hu_sort 	= N_ELEMENTS(voc_epa_hu_sort)
;	voc_med = (voc_epa_hu_sort[(Nvoc_epa_hu_sort/2)-1] + voc_epa_hu_sort[(Nvoc_epa_hu_sort/2)]) / 2.0
;	lower_half = voc_epa_hu_sort[0:(Nvoc_epa_hu_sort/2)-1]
;	upper_half = voc_epa_hu_sort[(Nvoc_epa_hu_sort/2):(Nvoc_epa_hu_sort-1)]
;ENDIF ELSE BEGIN
;	Nvoc_epa_hu_sort 	= N_ELEMENTS(voc_epa_hu_sort)
;	voc_med = voc_epa_hu_sort[(Nvoc_epa_hu_sort/2)] 
;	lower_half = voc_epa_hu_sort[0:(Nvoc_epa_hu_sort/2)-1]
;	upper_half = voc_epa_hu_sort[(Nvoc_epa_hu_sort/2):(Nvoc_epa_hu_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = voc_epa_hu_sort[0.05*Nvoc_epa_hu_sort]
;quartile_95 = voc_epa_hu_sort[0.95*Nvoc_epa_hu_sort]
;
;voc_epa_hu_ptile = [quartile_05, quartile_25, voc_med, quartile_75,quartile_95]
;
;
;data = [[voc_epa_td_ptile], [voc_epa_ts_ptile], [voc_epa_hu_ptile]]
;ytitle = ['EPA TD','EPA TS','EPA HU']
;boxes = BOXPLOT(data, $
;		TITLE		= 'July: VOC', $
;		XRANGE 		= [0,450], $
;		YRANGE 		= [-1, 3], $
;		XTITLE 		= 'VOC Concentration (ppb)', $
;		YTICKNAME 	= ytitle, $
;		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
;		FONT_SIZE   = 20, $
;		HORIZONTAL	= 1)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances without storms: nox
nox_epa_td = nox_storm_td
nox_epa_td_sort  = nox_epa_td[SORT(nox_epa_td)]
IF (N_ELEMENTS(nox_epa_td_sort) MOD 2 EQ 0) THEN BEGIN
	Nnox_epa_td_sort 	= N_ELEMENTS(nox_epa_td_sort)
	nox_med = (nox_epa_td_sort[(Nnox_epa_td_sort/2)-1] + nox_epa_td_sort[(Nnox_epa_td_sort/2)]) / 2.0
	lower_half = nox_epa_td_sort[0:(Nnox_epa_td_sort/2)-1]
	upper_half = nox_epa_td_sort[(Nnox_epa_td_sort/2):(Nnox_epa_td_sort-1)]
ENDIF ELSE BEGIN
	Nnox_epa_td_sort 	= N_ELEMENTS(nox_epa_td_sort)
	nox_med = nox_epa_td_sort[(Nnox_epa_td_sort/2)] 
	lower_half = nox_epa_td_sort[0:(Nnox_epa_td_sort/2)-1]
	upper_half = nox_epa_td_sort[(Nnox_epa_td_sort/2):(Nnox_epa_td_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = nox_epa_td_sort[0.05*Nnox_epa_td_sort]
quartile_95 = nox_epa_td_sort[0.95*Nnox_epa_td_sort]

nox_epa_td_ptile = [quartile_05, quartile_25, nox_med, quartile_75,quartile_95]


;;;EPA exceedances TS: nox
nox_epa_ts = nox_storm_ts
nox_epa_ts_sort  = nox_epa_ts[SORT(nox_epa_ts)]
IF (N_ELEMENTS(nox_epa_ts_sort) MOD 2 EQ 0) THEN BEGIN
	Nnox_epa_ts_sort 	= N_ELEMENTS(nox_epa_ts_sort)
	nox_med = (nox_epa_ts_sort[(Nnox_epa_ts_sort/2)-1] + nox_epa_ts_sort[(Nnox_epa_ts_sort/2)]) / 2.0
	lower_half = nox_epa_ts_sort[0:(Nnox_epa_ts_sort/2)-1]
	upper_half = nox_epa_ts_sort[(Nnox_epa_ts_sort/2):(Nnox_epa_ts_sort-1)]
ENDIF ELSE BEGIN
	Nnox_epa_ts_sort 	= N_ELEMENTS(nox_epa_ts_sort)
	nox_med = nox_epa_ts_sort[(Nnox_epa_ts_sort/2)] 
	lower_half = nox_epa_ts_sort[0:(Nnox_epa_ts_sort/2)-1]
	upper_half = nox_epa_ts_sort[(Nnox_epa_ts_sort/2):(Nnox_epa_ts_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = nox_epa_ts_sort[0.05*Nnox_epa_ts_sort]
quartile_95 = nox_epa_ts_sort[0.95*Nnox_epa_ts_sort]

nox_epa_ts_ptile = [quartile_05, quartile_25, nox_med, quartile_75,quartile_95]

 
;;;EPA exceedances HU: nox
nox_epa_hu = nox_storm_hu
nox_epa_hu_sort  = nox_epa_hu[SORT(nox_epa_hu)]
IF (N_ELEMENTS(nox_epa_hu_sort) MOD 2 EQ 0) THEN BEGIN
	Nnox_epa_hu_sort 	= N_ELEMENTS(nox_epa_hu_sort)
	nox_med = (nox_epa_hu_sort[(Nnox_epa_hu_sort/2)-1] + nox_epa_hu_sort[(Nnox_epa_hu_sort/2)]) / 2.0
	lower_half = nox_epa_hu_sort[0:(Nnox_epa_hu_sort/2)-1]
	upper_half = nox_epa_hu_sort[(Nnox_epa_hu_sort/2):(Nnox_epa_hu_sort-1)]
ENDIF ELSE BEGIN
	Nnox_epa_hu_sort 	= N_ELEMENTS(nox_epa_hu_sort)
	nox_med = nox_epa_hu_sort[(Nnox_epa_hu_sort/2)] 
	lower_half = nox_epa_hu_sort[0:(Nnox_epa_hu_sort/2)-1]
	upper_half = nox_epa_hu_sort[(Nnox_epa_hu_sort/2):(Nnox_epa_hu_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = nox_epa_hu_sort[0.05*Nnox_epa_hu_sort]
quartile_95 = nox_epa_hu_sort[0.95*Nnox_epa_hu_sort]

nox_epa_hu_ptile = [quartile_05, quartile_25, nox_med, quartile_75,quartile_95]



data = [[nox_epa_td_ptile], [nox_epa_ts_ptile], [nox_epa_hu_ptile]]
ytitle = ['EPA TD','EPA TS','EPA HU']
PRINT, 'NOx', data
boxes = BOXPLOT(data, $
		TITLE		= 'Sept: NOx', $
		XRANGE 		= [0,50], $
		YRANGE 		= [-1, 3], $
		XTITLE 		= 'NOx Concentration (ppb)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;EPA exceedances with storms: no
;no_epa_td = no_storm_td
;no_epa_td_sort  = no_epa_td[SORT(no_epa_td)]
;IF (N_ELEMENTS(no_epa_td_sort) MOD 2 EQ 0) THEN BEGIN
;	Nno_epa_td_sort 	= N_ELEMENTS(no_epa_td_sort)
;	no_med = (no_epa_td_sort[(Nno_epa_td_sort/2)-1] + no_epa_td_sort[(Nno_epa_td_sort/2)]) / 2.0
;	lower_half = no_epa_td_sort[0:(Nno_epa_td_sort/2)-1]
;	upper_half = no_epa_td_sort[(Nno_epa_td_sort/2):(Nno_epa_td_sort-1)]
;ENDIF ELSE BEGIN
;	Nno_epa_td_sort 	= N_ELEMENTS(no_epa_td_sort)
;	no_med = no_epa_td_sort[(Nno_epa_td_sort/2)] 
;	lower_half = no_epa_td_sort[0:(Nno_epa_td_sort/2)-1]
;	upper_half = no_epa_td_sort[(Nno_epa_td_sort/2):(Nno_epa_td_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = no_epa_td_sort[0.05*Nno_epa_td_sort]
;quartile_95 = no_epa_td_sort[0.95*Nno_epa_td_sort]
;
;no_epa_td_ptile = [quartile_05, quartile_25, no_med, quartile_75,quartile_95]
;
;
;;;;EPA exceedances TS: nox
;no_epa_ts = no_storm_ts
;no_epa_ts_sort  = no_epa_ts[SORT(no_epa_ts)]
;IF (N_ELEMENTS(no_epa_ts_sort) MOD 2 EQ 0) THEN BEGIN
;	Nno_epa_ts_sort 	= N_ELEMENTS(no_epa_ts_sort)
;	no_med = (no_epa_ts_sort[(Nno_epa_ts_sort/2)-1] + no_epa_ts_sort[(Nno_epa_ts_sort/2)]) / 2.0
;	lower_half = no_epa_ts_sort[0:(Nno_epa_ts_sort/2)-1]
;	upper_half = no_epa_ts_sort[(Nno_epa_ts_sort/2):(Nno_epa_ts_sort-1)]
;ENDIF ELSE BEGIN
;	Nno_epa_ts_sort 	= N_ELEMENTS(no_epa_ts_sort)
;	no_med = no_epa_ts_sort[(Nno_epa_ts_sort/2)] 
;	lower_half = no_epa_ts_sort[0:(Nno_epa_ts_sort/2)-1]
;	upper_half = no_epa_ts_sort[(Nno_epa_ts_sort/2):(Nno_epa_ts_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = no_epa_ts_sort[0.05*Nno_epa_ts_sort]
;quartile_95 = no_epa_ts_sort[0.95*Nno_epa_ts_sort]
;
;no_epa_ts_ptile = [quartile_05, quartile_25, no_med, quartile_75,quartile_95]
;
;
;;;;EPA exceedances HU: nox
;no_epa_hu = no_storm_hu
;no_epa_hu_sort  = no_epa_hu[SORT(no_epa_hu)]
;IF (N_ELEMENTS(no_epa_hu_sort) MOD 2 EQ 0) THEN BEGIN
;	Nno_epa_hu_sort 	= N_ELEMENTS(no_epa_hu_sort)
;	no_med = (no_epa_hu_sort[(Nno_epa_hu_sort/2)-1] + no_epa_hu_sort[(Nno_epa_hu_sort/2)]) / 2.0
;	lower_half = no_epa_hu_sort[0:(Nno_epa_hu_sort/2)-1]
;	upper_half = no_epa_hu_sort[(Nno_epa_hu_sort/2):(Nno_epa_hu_sort-1)]
;ENDIF ELSE BEGIN
;	Nno_epa_hu_sort 	= N_ELEMENTS(no_epa_hu_sort)
;	no_med = no_epa_hu_sort[(Nno_epa_hu_sort/2)] 
;	lower_half = no_epa_hu_sort[0:(Nno_epa_hu_sort/2)-1]
;	upper_half = no_epa_hu_sort[(Nno_epa_hu_sort/2):(Nno_epa_hu_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = no_epa_hu_sort[0.05*Nno_epa_hu_sort]
;quartile_95 = no_epa_hu_sort[0.95*Nno_epa_hu_sort]
;
;no_epa_hu_ptile = [quartile_05, quartile_25, no_med, quartile_75,quartile_95]
;
;
;data = [[no_epa_td_ptile], [no_epa_ts_ptile], [no_epa_hu_ptile]]
;ytitle = ['EPA TD','EPA TS','EPA HU']
;PRINT, 'NO', data
;boxes = BOXPLOT(data, $
;		TITLE		= 'July: NO', $
;		XRANGE 		= [0,50], $
;		YRANGE 		= [-1, 3], $
;		XTITLE 		= 'NO Concentration (ppb)', $
;		YTICKNAME 	= ytitle, $
;		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
;		FONT_SIZE   = 20, $
;		HORIZONTAL	= 1)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;EPA exceedances with storms: no2
;no2_epa_td = no2_storm_td
;no2_epa_td_sort  = no2_epa_td[SORT(no2_epa_td)]
;IF (N_ELEMENTS(no2_epa_td_sort) MOD 2 EQ 0) THEN BEGIN
;	Nno2_epa_td_sort 	= N_ELEMENTS(no2_epa_td_sort)
;	no2_med = (no2_epa_td_sort[(Nno2_epa_td_sort/2)-1] + no2_epa_td_sort[(Nno2_epa_td_sort/2)]) / 2.0
;	lower_half = no2_epa_td_sort[0:(Nno2_epa_td_sort/2)-1]
;	upper_half = no2_epa_td_sort[(Nno2_epa_td_sort/2):(Nno2_epa_td_sort-1)]
;ENDIF ELSE BEGIN
;	Nno2_epa_td_sort 	= N_ELEMENTS(no2_epa_td_sort)
;	no2_med = no2_epa_td_sort[(Nno2_epa_td_sort/2)] 
;	lower_half = no2_epa_td_sort[0:(Nno2_epa_td_sort/2)-1]
;	upper_half = no2_epa_td_sort[(Nno2_epa_td_sort/2):(Nno2_epa_td_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = no2_epa_td_sort[0.05*Nno2_epa_td_sort]
;quartile_95 = no2_epa_td_sort[0.95*Nno2_epa_td_sort]
;
;no2_epa_td_ptile = [quartile_05, quartile_25, no2_med, quartile_75,quartile_95]
;
;
;;;;EPA exceedances TS: nox
;no2_epa_ts = no2_storm_ts
;no2_epa_ts_sort  = no2_epa_ts[SORT(no2_epa_ts)]
;IF (N_ELEMENTS(no2_epa_ts_sort) MOD 2 EQ 0) THEN BEGIN
;	Nno2_epa_ts_sort 	= N_ELEMENTS(no2_epa_ts_sort)
;	no2_med = (no2_epa_ts_sort[(Nno2_epa_ts_sort/2)-1] + no2_epa_ts_sort[(Nno2_epa_ts_sort/2)]) / 2.0
;	lower_half = no2_epa_ts_sort[0:(Nno2_epa_ts_sort/2)-1]
;	upper_half = no2_epa_ts_sort[(Nno2_epa_ts_sort/2):(Nno2_epa_ts_sort-1)]
;ENDIF ELSE BEGIN
;	Nno2_epa_ts_sort 	= N_ELEMENTS(no2_epa_ts_sort)
;	no2_med = no2_epa_ts_sort[(Nno2_epa_ts_sort/2)] 
;	lower_half = no2_epa_ts_sort[0:(Nno2_epa_ts_sort/2)-1]
;	upper_half = no2_epa_ts_sort[(Nno2_epa_ts_sort/2):(Nno2_epa_ts_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = no2_epa_ts_sort[0.05*Nno2_epa_ts_sort]
;quartile_95 = no2_epa_ts_sort[0.95*Nno2_epa_ts_sort]
;
;no2_epa_ts_ptile = [quartile_05, quartile_25, no2_med, quartile_75,quartile_95]
;
; 
;;;;EPA exceedances HU: nox
;no2_epa_hu = no2_storm_hu
;no2_epa_hu_sort  = no2_epa_hu[SORT(no2_epa_hu)]
;IF (N_ELEMENTS(no2_epa_hu_sort) MOD 2 EQ 0) THEN BEGIN
;	Nno2_epa_hu_sort 	= N_ELEMENTS(no2_epa_hu_sort)
;	no2_med = (no2_epa_hu_sort[(Nno2_epa_hu_sort/2)-1] + no2_epa_hu_sort[(Nno2_epa_hu_sort/2)]) / 2.0
;	lower_half = no2_epa_hu_sort[0:(Nno2_epa_hu_sort/2)-1]
;	upper_half = no2_epa_hu_sort[(Nno2_epa_hu_sort/2):(Nno2_epa_hu_sort-1)]
;ENDIF ELSE BEGIN
;	Nno2_epa_hu_sort 	= N_ELEMENTS(no2_epa_hu_sort)
;	no2_med = no2_epa_hu_sort[(Nno2_epa_hu_sort/2)] 
;	lower_half = no2_epa_hu_sort[0:(Nno2_epa_hu_sort/2)-1]
;	upper_half = no2_epa_hu_sort[(Nno2_epa_hu_sort/2):(Nno2_epa_hu_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = no2_epa_hu_sort[0.05*Nno2_epa_hu_sort]
;quartile_95 = no2_epa_hu_sort[0.95*Nno2_epa_hu_sort]
;
;no2_epa_hu_ptile = [quartile_05, quartile_25, no2_med, quartile_75,quartile_95]
;
;
;data = [[no2_epa_td_ptile], [no2_epa_ts_ptile], [no2_epa_hu_ptile]]
;ytitle = ['EPA TD','EPA TS','EPA HU']
;PRINT, 'NO2', data
;boxes = BOXPLOT(data, $
;		TITLE		= 'July: NO2', $
;		XRANGE 		= [0,25], $
;		YRANGE 		= [-1, 3], $
;		XTITLE 		= 'NO2 Concentration (ppb)', $
;		YTICKNAME 	= ytitle, $
;		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
;		FONT_SIZE   = 20, $
;		HORIZONTAL	= 1)
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: co
co_epa_td = co_storm_td
co_epa_td_sort  = co_epa_td[SORT(co_epa_td)]
IF (N_ELEMENTS(co_epa_td_sort) MOD 2 EQ 0) THEN BEGIN
	Nco_epa_td_sort 	= N_ELEMENTS(co_epa_td_sort)
	co_med = (co_epa_td_sort[(Nco_epa_td_sort/2)-1] + co_epa_td_sort[(Nco_epa_td_sort/2)]) / 2.0
	lower_half = co_epa_td_sort[0:(Nco_epa_td_sort/2)-1]
	upper_half = co_epa_td_sort[(Nco_epa_td_sort/2):(Nco_epa_td_sort-1)]
ENDIF ELSE BEGIN
	Nco_epa_td_sort 	= N_ELEMENTS(co_epa_td_sort)
	co_med = co_epa_td_sort[(Nco_epa_td_sort/2)] 
	lower_half = co_epa_td_sort[0:(Nco_epa_td_sort/2)-1]
	upper_half = co_epa_td_sort[(Nco_epa_td_sort/2):(Nco_epa_td_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = co_epa_td_sort[0.05*Nco_epa_td_sort]
quartile_95 = co_epa_td_sort[0.95*Nco_epa_td_sort]

co_epa_td_ptile = [quartile_05, quartile_25, co_med, quartile_75,quartile_95]


;;;EPA exceedances TS: co
co_epa_ts = co_storm_ts
co_epa_ts_sort  = co_epa_ts[SORT(co_epa_ts)]
IF (N_ELEMENTS(co_epa_ts_sort) MOD 2 EQ 0) THEN BEGIN
	Nco_epa_ts_sort 	= N_ELEMENTS(co_epa_ts_sort)
	co_med = (co_epa_ts_sort[(Nco_epa_ts_sort/2)-1] + co_epa_ts_sort[(Nco_epa_ts_sort/2)]) / 2.0
	lower_half = co_epa_ts_sort[0:(Nco_epa_ts_sort/2)-1]
	upper_half = co_epa_ts_sort[(Nco_epa_ts_sort/2):(Nco_epa_ts_sort-1)]
ENDIF ELSE BEGIN
	Nco_epa_ts_sort 	= N_ELEMENTS(co_epa_ts_sort)
	co_med = co_epa_ts_sort[(Nco_epa_ts_sort/2)] 
	lower_half = co_epa_ts_sort[0:(Nco_epa_ts_sort/2)-1]
	upper_half = co_epa_ts_sort[(Nco_epa_ts_sort/2):(Nco_epa_ts_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = co_epa_ts_sort[0.05*Nco_epa_ts_sort]
quartile_95 = co_epa_ts_sort[0.95*Nco_epa_ts_sort]

co_epa_ts_ptile = [quartile_05, quartile_25, co_med, quartile_75,quartile_95]

 
;;;EPA exceedances HU: co
co_epa_hu = co_storm_hu
co_epa_hu_sort  = co_epa_hu[SORT(co_epa_hu)]
IF (N_ELEMENTS(co_epa_hu_sort) MOD 2 EQ 0) THEN BEGIN
	Nco_epa_hu_sort 	= N_ELEMENTS(co_epa_hu_sort)
	co_med = (co_epa_hu_sort[(Nco_epa_hu_sort/2)-1] + co_epa_hu_sort[(Nco_epa_hu_sort/2)]) / 2.0
	lower_half = co_epa_hu_sort[0:(Nco_epa_hu_sort/2)-1]
	upper_half = co_epa_hu_sort[(Nco_epa_hu_sort/2):(Nco_epa_hu_sort-1)]
ENDIF ELSE BEGIN
	Nco_epa_hu_sort 	= N_ELEMENTS(co_epa_hu_sort)
	co_med = co_epa_hu_sort[(Nco_epa_hu_sort/2)] 
	lower_half = co_epa_hu_sort[0:(Nco_epa_hu_sort/2)-1]
	upper_half = co_epa_hu_sort[(Nco_epa_hu_sort/2):(Nco_epa_hu_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = co_epa_hu_sort[0.05*Nco_epa_hu_sort]
quartile_95 = co_epa_hu_sort[0.95*Nco_epa_hu_sort]

co_epa_hu_ptile = [quartile_05, quartile_25, co_med, quartile_75,quartile_95]


data = [[co_epa_td_ptile], [co_epa_ts_ptile], [co_epa_hu_ptile]]
ytitle = ['EPA TD','EPA TS','EPA HU']
PRINT, 'CO', data
boxes = BOXPLOT(data*1.0E3, $
		TITLE		= 'Sept: CO', $
		XRANGE 		= [0,1500], $
		YRANGE 		= [-1, 3], $
		XTITLE 		= 'CO Concentration (ppb)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;EPA exceedances with storms: pm
;pm_epa_td = pm_storm_td
;pm_epa_td_sort  = pm_epa_td[SORT(pm_epa_td)]
;IF (N_ELEMENTS(pm_epa_td_sort) MOD 2 EQ 0) THEN BEGIN
;	Npm_epa_td_sort 	= N_ELEMENTS(pm_epa_td_sort)
;	pm_med = (pm_epa_td_sort[(Npm_epa_td_sort/2)-1] + pm_epa_td_sort[(Npm_epa_td_sort/2)]) / 2.0
;	lower_half = pm_epa_td_sort[0:(Npm_epa_td_sort/2)-1]
;	upper_half = pm_epa_td_sort[(Npm_epa_td_sort/2):(Npm_epa_td_sort-1)]
;ENDIF ELSE BEGIN
;	Npm_epa_td_sort 	= N_ELEMENTS(pm_epa_td_sort)
;	pm_med = pm_epa_td_sort[(Npm_epa_td_sort/2)] 
;	lower_half = pm_epa_td_sort[0:(Npm_epa_td_sort/2)-1]
;	upper_half = pm_epa_td_sort[(Npm_epa_td_sort/2):(Npm_epa_td_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = pm_epa_td_sort[0.05*Npm_epa_td_sort]
;quartile_95 = pm_epa_td_sort[0.95*Npm_epa_td_sort]
;
;pm_epa_td_ptile = [quartile_05, quartile_25, pm_med, quartile_75,quartile_95]
;
;
;;;;EPA exceedances TS: pm
;pm_epa_ts = pm_storm_ts
;pm_epa_ts_sort  = pm_epa_ts[SORT(pm_epa_ts)]
;IF (N_ELEMENTS(pm_epa_ts_sort) MOD 2 EQ 0) THEN BEGIN
;	Npm_epa_ts_sort 	= N_ELEMENTS(pm_epa_ts_sort)
;	pm_med = (pm_epa_ts_sort[(Npm_epa_ts_sort/2)-1] + pm_epa_ts_sort[(Npm_epa_ts_sort/2)]) / 2.0
;	lower_half = pm_epa_ts_sort[0:(Npm_epa_ts_sort/2)-1]
;	upper_half = pm_epa_ts_sort[(Npm_epa_ts_sort/2):(Npm_epa_ts_sort-1)]
;ENDIF ELSE BEGIN
;	Npm_epa_ts_sort 	= N_ELEMENTS(pm_epa_ts_sort)
;	pm_med = pm_epa_ts_sort[(Npm_epa_ts_sort/2)] 
;	lower_half = pm_epa_ts_sort[0:(Npm_epa_ts_sort/2)-1]
;	upper_half = pm_epa_ts_sort[(Npm_epa_ts_sort/2):(Npm_epa_ts_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = pm_epa_ts_sort[0.05*Npm_epa_ts_sort]
;quartile_95 = pm_epa_ts_sort[0.95*Npm_epa_ts_sort]
;
;pm_epa_ts_ptile = [quartile_05, quartile_25, pm_med, quartile_75,quartile_95]
;
; 
;;;;EPA exceedances HU: pm
;pm_epa_hu = pm_storm_hu
;pm_epa_hu_sort  = pm_epa_hu[SORT(pm_epa_hu)]
;IF (N_ELEMENTS(pm_epa_hu_sort) MOD 2 EQ 0) THEN BEGIN
;	Npm_epa_hu_sort 	= N_ELEMENTS(pm_epa_hu_sort)
;	pm_med = (pm_epa_hu_sort[(Npm_epa_hu_sort/2)-1] + pm_epa_hu_sort[(Npm_epa_hu_sort/2)]) / 2.0
;	lower_half = pm_epa_hu_sort[0:(Npm_epa_hu_sort/2)-1]
;	upper_half = pm_epa_hu_sort[(Npm_epa_hu_sort/2):(Npm_epa_hu_sort-1)]
;ENDIF ELSE BEGIN
;	Npm_epa_hu_sort 	= N_ELEMENTS(pm_epa_hu_sort)
;	pm_med = pm_epa_hu_sort[(Npm_epa_hu_sort/2)] 
;	lower_half = pm_epa_hu_sort[0:(Npm_epa_hu_sort/2)-1]
;	upper_half = pm_epa_hu_sort[(Npm_epa_hu_sort/2):(Npm_epa_hu_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = pm_epa_hu_sort[0.05*Npm_epa_hu_sort]
;quartile_95 = pm_epa_hu_sort[0.95*Npm_epa_hu_sort]
;
;pm_epa_hu_ptile = [quartile_05, quartile_25, pm_med, quartile_75,quartile_95]
;
;
;data = [[pm_epa_td_ptile], [pm_epa_ts_ptile], [pm_epa_hu_ptile]]
;ytitle = ['EPA TD','EPA TS','EPA HU']
;PRINT, 'PM', data
;boxes = BOXPLOT(data, $
;		TITLE		= 'July: PM2.5', $
;		XRANGE 		= [0, 25], $
;		YRANGE 		= [-1, 3], $
;		XTITLE 		= 'PM2.5 concentration (ug/m3)', $
;		YTICKNAME 	= ytitle, $
;		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
;		FONT_SIZE   = 20, $
;		HORIZONTAL	= 1)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: temp
temp_epa_td = temp_storm_td
temp_epa_td_sort  = temp_epa_td[SORT(temp_epa_td)]
IF (N_ELEMENTS(temp_epa_td_sort) MOD 2 EQ 0) THEN BEGIN
	Ntemp_epa_td_sort 	= N_ELEMENTS(temp_epa_td_sort)
	temp_med = (temp_epa_td_sort[(Ntemp_epa_td_sort/2)-1] + temp_epa_td_sort[(Ntemp_epa_td_sort/2)]) / 2.0
	lower_half = temp_epa_td_sort[0:(Ntemp_epa_td_sort/2)-1]
	upper_half = temp_epa_td_sort[(Ntemp_epa_td_sort/2):(Ntemp_epa_td_sort-1)]
ENDIF ELSE BEGIN
	Ntemp_epa_td_sort 	= N_ELEMENTS(temp_epa_td_sort)
	temp_med = temp_epa_td_sort[(Ntemp_epa_td_sort/2)] 
	lower_half = temp_epa_td_sort[0:(Ntemp_epa_td_sort/2)-1]
	upper_half = temp_epa_td_sort[(Ntemp_epa_td_sort/2):(Ntemp_epa_td_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = temp_epa_td_sort[0.05*Ntemp_epa_td_sort]
quartile_95 = temp_epa_td_sort[0.95*Ntemp_epa_td_sort]

temp_epa_td_ptile = [quartile_05, quartile_25, temp_med, quartile_75,quartile_95]


;;;EPA exceedances TS: temp
temp_epa_ts = temp_storm_ts
temp_epa_ts_sort  = temp_epa_ts[SORT(temp_epa_ts)]
IF (N_ELEMENTS(temp_epa_ts_sort) MOD 2 EQ 0) THEN BEGIN
	Ntemp_epa_ts_sort 	= N_ELEMENTS(temp_epa_ts_sort)
	temp_med = (temp_epa_ts_sort[(Ntemp_epa_ts_sort/2)-1] + temp_epa_ts_sort[(Ntemp_epa_ts_sort/2)]) / 2.0
	lower_half = temp_epa_ts_sort[0:(Ntemp_epa_ts_sort/2)-1]
	upper_half = temp_epa_ts_sort[(Ntemp_epa_ts_sort/2):(Ntemp_epa_ts_sort-1)]
ENDIF ELSE BEGIN
	Ntemp_epa_ts_sort 	= N_ELEMENTS(temp_epa_ts_sort)
	temp_med = temp_epa_ts_sort[(Ntemp_epa_ts_sort/2)] 
	lower_half = temp_epa_ts_sort[0:(Ntemp_epa_ts_sort/2)-1]
	upper_half = temp_epa_ts_sort[(Ntemp_epa_ts_sort/2):(Ntemp_epa_ts_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = temp_epa_ts_sort[0.05*Ntemp_epa_ts_sort]
quartile_95 = temp_epa_ts_sort[0.95*Ntemp_epa_ts_sort]

temp_epa_ts_ptile = [quartile_05, quartile_25, temp_med, quartile_75,quartile_95]


;;;EPA exceedances HU: temp
temp_epa_hu = temp_storm_hu
temp_epa_hu_sort  = temp_epa_hu[SORT(temp_epa_hu)]
IF (N_ELEMENTS(temp_epa_hu_sort) MOD 2 EQ 0) THEN BEGIN
	Ntemp_epa_hu_sort 	= N_ELEMENTS(temp_epa_hu_sort)
	temp_med = (temp_epa_hu_sort[(Ntemp_epa_hu_sort/2)-1] + temp_epa_hu_sort[(Ntemp_epa_hu_sort/2)]) / 2.0
	lower_half = temp_epa_hu_sort[0:(Ntemp_epa_hu_sort/2)-1]
	upper_half = temp_epa_hu_sort[(Ntemp_epa_hu_sort/2):(Ntemp_epa_hu_sort-1)]
ENDIF ELSE BEGIN
	Ntemp_epa_hu_sort 	= N_ELEMENTS(temp_epa_hu_sort)
	temp_med = temp_epa_hu_sort[(Ntemp_epa_hu_sort/2)] 
	lower_half = temp_epa_hu_sort[0:(Ntemp_epa_hu_sort/2)-1]
	upper_half = temp_epa_hu_sort[(Ntemp_epa_hu_sort/2):(Ntemp_epa_hu_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = temp_epa_hu_sort[0.05*Ntemp_epa_hu_sort]
quartile_95 = temp_epa_hu_sort[0.95*Ntemp_epa_hu_sort]

temp_epa_hu_ptile = [quartile_05, quartile_25, temp_med, quartile_75,quartile_95]


data = [[temp_epa_td_ptile], [temp_epa_ts_ptile], [temp_epa_hu_ptile]]
ytitle = ['EPA TD','EPA TS','EPA HU']
PRINT, 'TEMP', data
boxes = BOXPLOT(data, $
		TITLE		= 'Sept: Temperature', $
		XRANGE 		= [75, 105], $
		YRANGE 		= [-1, 3], $
		XTITLE 		= 'Temperature (deg F)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;EPA exceedances with storms: press
;press_epa_td = press_storm_td
;press_epa_td_sort  = press_epa_td[SORT(press_epa_td)]
;IF (N_ELEMENTS(press_epa_td_sort) MOD 2 EQ 0) THEN BEGIN
;	Npress_epa_td_sort 	= N_ELEMENTS(press_epa_td_sort)
;	press_med = (press_epa_td_sort[(Npress_epa_td_sort/2)-1] + press_epa_td_sort[(Npress_epa_td_sort/2)]) / 2.0
;	lower_half = press_epa_td_sort[0:(Npress_epa_td_sort/2)-1]
;	upper_half = press_epa_td_sort[(Npress_epa_td_sort/2):(Npress_epa_td_sort-1)]
;ENDIF ELSE BEGIN
;	Npress_epa_td_sort 	= N_ELEMENTS(press_epa_td_sort)
;	press_med = press_epa_td_sort[(Npress_epa_td_sort/2)] 
;	lower_half = press_epa_td_sort[0:(Npress_epa_td_sort/2)-1]
;	upper_half = press_epa_td_sort[(Npress_epa_td_sort/2):(Npress_epa_td_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = press_epa_td_sort[0.05*Npress_epa_td_sort]
;quartile_95 = press_epa_td_sort[0.95*Npress_epa_td_sort]
;
;press_epa_td_ptile = [quartile_05, quartile_25, press_med, quartile_75,quartile_95]
;
;
;;;;EPA exceedances TS: press
;press_epa_ts = press_storm_ts
;press_epa_ts_sort  = press_epa_ts[SORT(press_epa_ts)]
;IF (N_ELEMENTS(press_epa_ts_sort) MOD 2 EQ 0) THEN BEGIN
;	Npress_epa_ts_sort 	= N_ELEMENTS(press_epa_ts_sort)
;	press_med = (press_epa_ts_sort[(Npress_epa_ts_sort/2)-1] + press_epa_ts_sort[(Npress_epa_ts_sort/2)]) / 2.0
;	lower_half = press_epa_ts_sort[0:(Npress_epa_ts_sort/2)-1]
;	upper_half = press_epa_ts_sort[(Npress_epa_ts_sort/2):(Npress_epa_ts_sort-1)]
;ENDIF ELSE BEGIN
;	Npress_epa_ts_sort 	= N_ELEMENTS(press_epa_ts_sort)
;	press_med = press_epa_ts_sort[(Npress_epa_ts_sort/2)] 
;	lower_half = press_epa_ts_sort[0:(Npress_epa_ts_sort/2)-1]
;	upper_half = press_epa_ts_sort[(Npress_epa_ts_sort/2):(Npress_epa_ts_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = press_epa_ts_sort[0.05*Npress_epa_ts_sort]
;quartile_95 = press_epa_ts_sort[0.95*Npress_epa_ts_sort]
;
;press_epa_ts_ptile = [quartile_05, quartile_25, press_med, quartile_75,quartile_95]
;
; 
;;;;EPA exceedances HU: press
;press_epa_hu = press_storm_hu
;press_epa_hu_sort  = press_epa_hu[SORT(press_epa_hu)]
;IF (N_ELEMENTS(press_epa_hu_sort) MOD 2 EQ 0) THEN BEGIN
;	Npress_epa_hu_sort 	= N_ELEMENTS(press_epa_hu_sort)
;	press_med = (press_epa_hu_sort[(Npress_epa_hu_sort/2)-1] + press_epa_hu_sort[(Npress_epa_hu_sort/2)]) / 2.0
;	lower_half = press_epa_hu_sort[0:(Npress_epa_hu_sort/2)-1]
;	upper_half = press_epa_hu_sort[(Npress_epa_hu_sort/2):(Npress_epa_hu_sort-1)]
;ENDIF ELSE BEGIN
;	Npress_epa_hu_sort 	= N_ELEMENTS(press_epa_hu_sort)
;	press_med = press_epa_hu_sort[(Npress_epa_hu_sort/2)] 
;	lower_half = press_epa_hu_sort[0:(Npress_epa_hu_sort/2)-1]
;	upper_half = press_epa_hu_sort[(Npress_epa_hu_sort/2):(Npress_epa_hu_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = press_epa_hu_sort[0.05*Npress_epa_hu_sort]
;quartile_95 = press_epa_hu_sort[0.95*Npress_epa_hu_sort]
;
;press_epa_hu_ptile = [quartile_05, quartile_25, press_med, quartile_75,quartile_95]
;
;data = [[press_epa_td_ptile], [press_epa_ts_ptile], [press_epa_hu_ptile]]
;ytitle = ['EPA TD','EPA TS','EPA HU']
;PRINT, 'Pres', data
;boxes = BOXPLOT(data, $
;		TITLE		= 'July: Pressure', $
;		XRANGE 		= [985, 1015], $
;		YRANGE 		= [-1, 3], $
;		XTITLE 		= 'Pressure (hPa)', $
;		YTICKNAME 	= ytitle, $
;		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
;		FONT_SIZE   = 20, $
;		HORIZONTAL	= 1)
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: wind_spd
wind_spd_epa_td = wind_spd_storm_td
wind_spd_epa_td_sort  = wind_spd_epa_td[SORT(wind_spd_epa_td)]
IF (N_ELEMENTS(wind_spd_epa_td_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_spd_epa_td_sort 	= N_ELEMENTS(wind_spd_epa_td_sort)
	wind_spd_med = (wind_spd_epa_td_sort[(Nwind_spd_epa_td_sort/2)-1] + wind_spd_epa_td_sort[(Nwind_spd_epa_td_sort/2)]) / 2.0
	lower_half = wind_spd_epa_td_sort[0:(Nwind_spd_epa_td_sort/2)-1]
	upper_half = wind_spd_epa_td_sort[(Nwind_spd_epa_td_sort/2):(Nwind_spd_epa_td_sort-1)]
ENDIF ELSE BEGIN
	Nwind_spd_epa_td_sort 	= N_ELEMENTS(wind_spd_epa_td_sort)
	wind_spd_med = wind_spd_epa_td_sort[(Nwind_spd_epa_td_sort/2)] 
	lower_half = wind_spd_epa_td_sort[0:(Nwind_spd_epa_td_sort/2)-1]
	upper_half = wind_spd_epa_td_sort[(Nwind_spd_epa_td_sort/2):(Nwind_spd_epa_td_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_spd_epa_td_sort[0.05*Nwind_spd_epa_td_sort]
quartile_95 = wind_spd_epa_td_sort[0.95*Nwind_spd_epa_td_sort]

wind_spd_epa_td_ptile = [quartile_05, quartile_25, wind_spd_med, quartile_75,quartile_95]


;;;EPA exceedances TS: wind_spd
wind_spd_epa_ts = wind_spd_storm_ts
wind_spd_epa_ts_sort  = wind_spd_epa_ts[SORT(wind_spd_epa_ts)]
IF (N_ELEMENTS(wind_spd_epa_ts_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_spd_epa_ts_sort 	= N_ELEMENTS(wind_spd_epa_ts_sort)
	wind_spd_med = (wind_spd_epa_ts_sort[(Nwind_spd_epa_ts_sort/2)-1] + wind_spd_epa_ts_sort[(Nwind_spd_epa_ts_sort/2)]) / 2.0
	lower_half = wind_spd_epa_ts_sort[0:(Nwind_spd_epa_ts_sort/2)-1]
	upper_half = wind_spd_epa_ts_sort[(Nwind_spd_epa_ts_sort/2):(Nwind_spd_epa_ts_sort-1)]
ENDIF ELSE BEGIN
	Nwind_spd_epa_ts_sort 	= N_ELEMENTS(wind_spd_epa_ts_sort)
	wind_spd_med = wind_spd_epa_ts_sort[(Nwind_spd_epa_ts_sort/2)] 
	lower_half = wind_spd_epa_ts_sort[0:(Nwind_spd_epa_ts_sort/2)-1]
	upper_half = wind_spd_epa_ts_sort[(Nwind_spd_epa_ts_sort/2):(Nwind_spd_epa_ts_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_spd_epa_ts_sort[0.05*Nwind_spd_epa_ts_sort]
quartile_95 = wind_spd_epa_ts_sort[0.95*Nwind_spd_epa_ts_sort]

wind_spd_epa_ts_ptile = [quartile_05, quartile_25, wind_spd_med, quartile_75,quartile_95]

;;;EPA exceedances HU: wind_spd
wind_spd_epa_hu = wind_spd_storm_hu
wind_spd_epa_hu_sort  = wind_spd_epa_hu[SORT(wind_spd_epa_hu)]
IF (N_ELEMENTS(wind_spd_epa_hu_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_spd_epa_hu_sort 	= N_ELEMENTS(wind_spd_epa_hu_sort)
	wind_spd_med = (wind_spd_epa_hu_sort[(Nwind_spd_epa_hu_sort/2)-1] + wind_spd_epa_hu_sort[(Nwind_spd_epa_hu_sort/2)]) / 2.0
	lower_half = wind_spd_epa_hu_sort[0:(Nwind_spd_epa_hu_sort/2)-1]
	upper_half = wind_spd_epa_hu_sort[(Nwind_spd_epa_hu_sort/2):(Nwind_spd_epa_hu_sort-1)]
ENDIF ELSE BEGIN
	Nwind_spd_epa_hu_sort 	= N_ELEMENTS(wind_spd_epa_hu_sort)
	wind_spd_med = wind_spd_epa_hu_sort[(Nwind_spd_epa_hu_sort/2)] 
	lower_half = wind_spd_epa_hu_sort[0:(Nwind_spd_epa_hu_sort/2)-1]
	upper_half = wind_spd_epa_hu_sort[(Nwind_spd_epa_hu_sort/2):(Nwind_spd_epa_hu_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_spd_epa_hu_sort[0.05*Nwind_spd_epa_hu_sort]
quartile_95 = wind_spd_epa_hu_sort[0.95*Nwind_spd_epa_hu_sort]

wind_spd_epa_hu_ptile = [quartile_05, quartile_25, wind_spd_med, quartile_75,quartile_95]


data = [[wind_spd_epa_td_ptile], [wind_spd_epa_ts_ptile], [wind_spd_epa_hu_ptile]]
ytitle = ['EPA TD','EPA TS','EPA HU']
PRINT, 'wind spd', data
boxes = BOXPLOT(data, $
		TITLE		= 'Sept: Wind Speed', $
		XRANGE 		= [0, 12], $
		YRANGE 		= [-1, 3], $
		XTITLE 		= 'Wind Speed (m/s)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: wind_dir
wind_dir_epa_td = wind_dir_storm_td
wind_dir_epa_td_sort  = wind_dir_epa_td[SORT(wind_dir_epa_td)]
IF (N_ELEMENTS(wind_dir_epa_td_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_dir_epa_td_sort 	= N_ELEMENTS(wind_dir_epa_td_sort)
	wind_dir_med = (wind_dir_epa_td_sort[(Nwind_dir_epa_td_sort/2)-1] + wind_dir_epa_td_sort[(Nwind_dir_epa_td_sort/2)]) / 2.0
	lower_half = wind_dir_epa_td_sort[0:(Nwind_dir_epa_td_sort/2)-1]
	upper_half = wind_dir_epa_td_sort[(Nwind_dir_epa_td_sort/2):(Nwind_dir_epa_td_sort-1)]
ENDIF ELSE BEGIN
	Nwind_dir_epa_td_sort 	= N_ELEMENTS(wind_dir_epa_td_sort)
	wind_dir_med = wind_dir_epa_td_sort[(Nwind_dir_epa_td_sort/2)] 
	lower_half = wind_dir_epa_td_sort[0:(Nwind_dir_epa_td_sort/2)-1]
	upper_half = wind_dir_epa_td_sort[(Nwind_dir_epa_td_sort/2):(Nwind_dir_epa_td_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_dir_epa_td_sort[0.05*Nwind_dir_epa_td_sort]
quartile_95 = wind_dir_epa_td_sort[0.95*Nwind_dir_epa_td_sort]

wind_dir_epa_td_ptile = [quartile_05, quartile_25, wind_dir_med, quartile_75,quartile_95]


;;;EPA exceedances TS: wind_dir
wind_dir_epa_ts = wind_dir_storm_ts
wind_dir_epa_ts_sort  = wind_dir_epa_ts[SORT(wind_dir_epa_ts)]
IF (N_ELEMENTS(wind_dir_epa_ts_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_dir_epa_ts_sort 	= N_ELEMENTS(wind_dir_epa_ts_sort)
	wind_dir_med = (wind_dir_epa_ts_sort[(Nwind_dir_epa_ts_sort/2)-1] + wind_dir_epa_ts_sort[(Nwind_dir_epa_ts_sort/2)]) / 2.0
	lower_half = wind_dir_epa_ts_sort[0:(Nwind_dir_epa_ts_sort/2)-1]
	upper_half = wind_dir_epa_ts_sort[(Nwind_dir_epa_ts_sort/2):(Nwind_dir_epa_ts_sort-1)]
ENDIF ELSE BEGIN
	Nwind_dir_epa_ts_sort 	= N_ELEMENTS(wind_dir_epa_ts_sort)
	wind_dir_med = wind_dir_epa_ts_sort[(Nwind_dir_epa_ts_sort/2)] 
	lower_half = wind_dir_epa_ts_sort[0:(Nwind_dir_epa_ts_sort/2)-1]
	upper_half = wind_dir_epa_ts_sort[(Nwind_dir_epa_ts_sort/2):(Nwind_dir_epa_ts_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_dir_epa_ts_sort[0.05*Nwind_dir_epa_ts_sort]
quartile_95 = wind_dir_epa_ts_sort[0.95*Nwind_dir_epa_ts_sort]

wind_dir_epa_ts_ptile = [quartile_05, quartile_25, wind_dir_med, quartile_75,quartile_95]

 
;;;EPA exceedances HU: wind_dir
wind_dir_epa_hu = wind_dir_storm_hu
wind_dir_epa_hu_sort  = wind_dir_epa_hu[SORT(wind_dir_epa_hu)]
IF (N_ELEMENTS(wind_dir_epa_hu_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_dir_epa_hu_sort 	= N_ELEMENTS(wind_dir_epa_hu_sort)
	wind_dir_med = (wind_dir_epa_hu_sort[(Nwind_dir_epa_hu_sort/2)-1] + wind_dir_epa_hu_sort[(Nwind_dir_epa_hu_sort/2)]) / 2.0
	lower_half = wind_dir_epa_hu_sort[0:(Nwind_dir_epa_hu_sort/2)-1]
	upper_half = wind_dir_epa_hu_sort[(Nwind_dir_epa_hu_sort/2):(Nwind_dir_epa_hu_sort-1)]
ENDIF ELSE BEGIN
	Nwind_dir_epa_hu_sort 	= N_ELEMENTS(wind_dir_epa_hu_sort)
	wind_dir_med = wind_dir_epa_hu_sort[(Nwind_dir_epa_hu_sort/2)] 
	lower_half = wind_dir_epa_hu_sort[0:(Nwind_dir_epa_hu_sort/2)-1]
	upper_half = wind_dir_epa_hu_sort[(Nwind_dir_epa_hu_sort/2):(Nwind_dir_epa_hu_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_dir_epa_hu_sort[0.05*Nwind_dir_epa_hu_sort]
quartile_95 = wind_dir_epa_hu_sort[0.95*Nwind_dir_epa_hu_sort]

wind_dir_epa_hu_ptile = [quartile_05, quartile_25, wind_dir_med, quartile_75,quartile_95]

data = [[wind_dir_epa_td_ptile], [wind_dir_epa_ts_ptile], [wind_dir_epa_hu_ptile]]
ytitle = ['EPA TD','EPA TS','EPA HU']
PRINT, 'wind dir', data
boxes = BOXPLOT(data, $
		TITLE		= 'Sept: Wind Direction', $
		XRANGE 		= [0, 360], $
		YRANGE 		= [-1, 3], $
		XTITLE 		= 'Wind Dir. (deg)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: rh
;rh_epa_td = rh_storm_td
;rh_epa_td_sort  = rh_epa_td[SORT(rh_epa_td)]
;IF (N_ELEMENTS(rh_epa_td_sort) MOD 2 EQ 0) THEN BEGIN
;	Nrh_epa_td_sort 	= N_ELEMENTS(rh_epa_td_sort)
;	rh_med = (rh_epa_td_sort[(Nrh_epa_td_sort/2)-1] + rh_epa_td_sort[(Nrh_epa_td_sort/2)]) / 2.0
;	lower_half = rh_epa_td_sort[0:(Nrh_epa_td_sort/2)-1]
;	upper_half = rh_epa_td_sort[(Nrh_epa_td_sort/2):(Nrh_epa_td_sort-1)]
;ENDIF ELSE BEGIN
;	Nrh_epa_td_sort 	= N_ELEMENTS(rh_epa_td_sort)
;	rh_med = rh_epa_td_sort[(Nrh_epa_td_sort/2)] 
;	lower_half = rh_epa_td_sort[0:(Nrh_epa_td_sort/2)-1]
;	upper_half = rh_epa_td_sort[(Nrh_epa_td_sort/2):(Nrh_epa_td_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = rh_epa_td_sort[0.05*Nrh_epa_td_sort]
;quartile_95 = rh_epa_td_sort[0.95*Nrh_epa_td_sort]
;
;rh_epa_td_ptile = [quartile_05, quartile_25, rh_med, quartile_75,quartile_95]
;
;;;;EPA exceedances TS: rh
;rh_epa_ts = rh_storm_ts
;rh_epa_ts_sort  = rh_epa_ts[SORT(rh_epa_ts)]
;IF (N_ELEMENTS(rh_epa_ts_sort) MOD 2 EQ 0) THEN BEGIN
;	Nrh_epa_ts_sort 	= N_ELEMENTS(rh_epa_ts_sort)
;	rh_med = (rh_epa_ts_sort[(Nrh_epa_ts_sort/2)-1] + rh_epa_ts_sort[(Nrh_epa_ts_sort/2)]) / 2.0
;	lower_half = rh_epa_ts_sort[0:(Nrh_epa_ts_sort/2)-1]
;	upper_half = rh_epa_ts_sort[(Nrh_epa_ts_sort/2):(Nrh_epa_ts_sort-1)]
;ENDIF ELSE BEGIN
;	Nrh_epa_ts_sort 	= N_ELEMENTS(rh_epa_ts_sort)
;	rh_med = rh_epa_ts_sort[(Nrh_epa_ts_sort/2)] 
;	lower_half = rh_epa_ts_sort[0:(Nrh_epa_ts_sort/2)-1]
;	upper_half = rh_epa_ts_sort[(Nrh_epa_ts_sort/2):(Nrh_epa_ts_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = rh_epa_ts_sort[0.05*Nrh_epa_ts_sort]
;quartile_95 = rh_epa_ts_sort[0.95*Nrh_epa_ts_sort]
;
;rh_epa_ts_ptile = [quartile_05, quartile_25, rh_med, quartile_75,quartile_95]
;
;;;;EPA exceedances HU: rh
;rh_epa_hu = rh_storm_hu
;rh_epa_hu_sort  = rh_epa_hu[SORT(rh_epa_hu)]
;IF (N_ELEMENTS(rh_epa_hu_sort) MOD 2 EQ 0) THEN BEGIN
;	Nrh_epa_hu_sort 	= N_ELEMENTS(rh_epa_hu_sort)
;	rh_med = (rh_epa_hu_sort[(Nrh_epa_hu_sort/2)-1] + rh_epa_hu_sort[(Nrh_epa_hu_sort/2)]) / 2.0
;	lower_half = rh_epa_hu_sort[0:(Nrh_epa_hu_sort/2)-1]
;	upper_half = rh_epa_hu_sort[(Nrh_epa_hu_sort/2):(Nrh_epa_hu_sort-1)]
;ENDIF ELSE BEGIN
;	Nrh_epa_hu_sort 	= N_ELEMENTS(rh_epa_hu_sort)
;	rh_med = rh_epa_hu_sort[(Nrh_epa_hu_sort/2)] 
;	lower_half = rh_epa_hu_sort[0:(Nrh_epa_hu_sort/2)-1]
;	upper_half = rh_epa_hu_sort[(Nrh_epa_hu_sort/2):(Nrh_epa_hu_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = rh_epa_hu_sort[0.05*Nrh_epa_hu_sort]
;quartile_95 = rh_epa_hu_sort[0.95*Nrh_epa_hu_sort]
;
;rh_epa_hu_ptile = [quartile_05, quartile_25, rh_med, quartile_75,quartile_95]
;
;
;data = [[rh_epa_td_ptile], [rh_epa_ts_ptile], [rh_epa_hu_ptile]]
;ytitle = ['EPA TD','EPA TS','EPA HU']
;PRINT, 'rh', data
;boxes = BOXPLOT(data, $
;		TITLE		= 'July: RH', $
;		XRANGE 		= [20, 90], $
;		YRANGE 		= [-1, 3], $
;		XTITLE 		= 'RH (%)', $
;		YTICKNAME 	= ytitle, $
;		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
;		FONT_SIZE   = 20, $
;		HORIZONTAL	= 1)




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
