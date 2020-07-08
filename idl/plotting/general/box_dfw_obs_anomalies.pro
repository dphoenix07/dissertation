PRO BOX_DFW_OBS_ANOMALIES, $
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

o3_storm_sept = []
o3dm_storm_sept = []
voc_storm_sept = []
nox_storm_sept = []
no_storm_sept = []
no2_storm_sept = []
pm_storm_sept = []
co_storm_sept = []
temp_storm_sept = []
wind_spd_storm_sept = []
wind_dir_storm_sept = []
press_storm_sept = []
rh_storm_sept = []
dp_storm_sept = []

o3_nstorm_sept = []
o3dm_nstorm_sept = []
voc_nstorm_sept = []
nox_nstorm_sept = []
no_nstorm_sept = []
no2_nstorm_sept = []
pm_nstorm_sept = []
co_nstorm_sept = []
temp_nstorm_sept = []
wind_spd_nstorm_sept = []
wind_dir_nstorm_sept = []
press_nstorm_sept = []
rh_nstorm_sept = []
dp_nstorm_sept = []

infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_091980_101995_SEPT_18Z.nc'																;Set input file path

;Get monthly averaged data
id  = NCDF_OPEN(infile)																						;Open input file for reading	

NCDF_VARGET, id, 'O3_storm'					, o3_storm
;NCDF_VARGET, id, 'O3DM_storm'				, o3dm_storm
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

o3_storm_sept = [o3_storm_sept, o3_storm]
;o3dm_storm_sept = [o3dm_storm_sept, o3dm_storm]
nox_storm_sept = [nox_storm_sept, nox_storm]
no_storm_sept = [no_storm_sept, no_storm]
no2_storm_sept = [no2_storm_sept, no2_storm]
co_storm_sept = [co_storm_sept, co_storm]
temp_storm_sept = [temp_storm_sept, temp_storm]
wind_spd_storm_sept = [wind_spd_storm_sept, wind_spd_storm]
wind_dir_storm_sept = [wind_dir_storm_sept, wind_dir_storm]

o3_nstorm_sept = [o3_nstorm_sept, o3_nstorm]
;o3dm_nstorm_sept = [o3dm_nstorm_sept, o3dm_nstorm]
nox_nstorm_sept = [nox_nstorm_sept, nox_nstorm]
no_nstorm_sept  = [no_nstorm_sept, no_nstorm]
no2_nstorm_sept = [no2_nstorm_sept, no2_nstorm]
co_nstorm_sept = [co_nstorm_sept, co_nstorm]
temp_nstorm_sept = [temp_nstorm_sept, temp_nstorm]
wind_spd_nstorm_sept = [wind_spd_nstorm_sept, wind_spd_nstorm]
wind_dir_nstorm_sept = [wind_dir_nstorm_sept, wind_dir_nstorm]

infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_091996_102000_SEPT_18Z.nc'																;Set input file path

;Get monthly averaged data
id  = NCDF_OPEN(infile)																						;Open input file for reading	

NCDF_VARGET, id, 'O3_storm'					, o3_storm
;NCDF_VARGET, id, 'O3DM_storm'				, o3dm_storm
NCDF_VARGET, id, 'VOC_storm'   				, voc_storm
NCDF_VARGET, id, 'NOx_storm' 				, nox_storm
NCDF_VARGET, id, 'NO_storm' 				, no_storm
NCDF_VARGET, id, 'NO2_storm' 				, no2_storm
NCDF_VARGET, id, 'CO_storm' 				, co_storm
NCDF_VARGET, id, 'Temperature_storm'  		, temp_storm
NCDF_VARGET, id, 'Wind_Speed_storm'   		, wind_spd_storm
NCDF_VARGET, id, 'Wind_Direction_storm'		, wind_dir_storm

NCDF_VARGET, id, 'O3_nstorm'				, o3_nstorm
;NCDF_VARGET, id, 'O3DM_nstorm'				, o3dm_nstorm
NCDF_VARGET, id, 'VOC_nstorm'   			, voc_nstorm
NCDF_VARGET, id, 'NOx_nstorm' 				, nox_nstorm
NCDF_VARGET, id, 'NO_nstorm' 				, no_nstorm
NCDF_VARGET, id, 'NO2_nstorm' 				, no2_nstorm
NCDF_VARGET, id, 'CO_nstorm' 				, co_nstorm
NCDF_VARGET, id, 'Temperature_nstorm'  		, temp_nstorm
NCDF_VARGET, id, 'Wind_Speed_nstorm'   		, wind_spd_nstorm
NCDF_VARGET, id, 'Wind_Direction_nstorm'	, wind_dir_nstorm
NCDF_CLOSE, id

o3_storm_sept = [o3_storm_sept, o3_storm]
;o3dm_storm_sept = [o3dm_storm_sept, o3dm_storm]
voc_storm_sept = [voc_storm_sept, voc_storm]
nox_storm_sept = [nox_storm_sept, nox_storm]
no_storm_sept = [no_storm_sept, no_storm]
no2_storm_sept = [no2_storm_sept, no2_storm]
co_storm_sept = [co_storm_sept, co_storm]
temp_storm_sept = [temp_storm_sept, temp_storm]
wind_spd_storm_sept = [wind_spd_storm_sept, wind_spd_storm]
wind_dir_storm_sept = [wind_dir_storm_sept, wind_dir_storm]

o3_nstorm_sept = [o3_nstorm_sept, o3_nstorm]
;o3dm_nstorm_sept = [o3dm_nstorm_sept, o3dm_nstorm]
voc_nstorm_sept = [voc_nstorm_sept, voc_nstorm]
nox_nstorm_sept = [nox_nstorm_sept, nox_nstorm]
no_nstorm_sept  = [no_nstorm_sept, no_nstorm]
no2_nstorm_sept = [no2_nstorm_sept, no2_nstorm]
co_nstorm_sept = [co_nstorm_sept, co_nstorm]
temp_nstorm_sept = [temp_nstorm_sept, temp_nstorm]
wind_spd_nstorm_sept = [wind_spd_nstorm_sept, wind_spd_nstorm]
wind_dir_nstorm_sept = [wind_dir_nstorm_sept, wind_dir_nstorm]


infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_092001_102004_SEPT_18Z.nc'																;Set input file path

;Get monthly averaged data
id  = NCDF_OPEN(infile)																						;Open input file for reading	

NCDF_VARGET, id, 'O3_storm'					, o3_storm
;NCDF_VARGET, id, 'O3DM_storm'				, o3dm_storm
NCDF_VARGET, id, 'VOC_storm'   				, voc_storm
NCDF_VARGET, id, 'NOx_storm' 				, nox_storm
NCDF_VARGET, id, 'NO_storm' 				, no_storm
NCDF_VARGET, id, 'NO2_storm' 				, no2_storm
NCDF_VARGET, id, 'CO_storm' 				, co_storm
NCDF_VARGET, id, 'Temperature_storm'  		, temp_storm
NCDF_VARGET, id, 'Wind_Speed_storm'   		, wind_spd_storm
NCDF_VARGET, id, 'Wind_Direction_storm'		, wind_dir_storm
NCDF_VARGET, id, 'Pressure_storm'			, press_storm
NCDF_VARGET, id, 'RH_storm'        			, rh_storm
NCDF_VARGET, id, 'Dew_Point_storm'   		, dp_storm

NCDF_VARGET, id, 'O3_nstorm'				, o3_nstorm
;NCDF_VARGET, id, 'O3DM_nstorm'				, o3dm_nstorm
NCDF_VARGET, id, 'VOC_nstorm'   			, voc_nstorm
NCDF_VARGET, id, 'NOx_nstorm' 				, nox_nstorm
NCDF_VARGET, id, 'NO_nstorm' 				, no_nstorm
NCDF_VARGET, id, 'NO2_nstorm' 				, no2_nstorm
NCDF_VARGET, id, 'CO_nstorm' 				, co_nstorm
NCDF_VARGET, id, 'Temperature_nstorm'  		, temp_nstorm
NCDF_VARGET, id, 'Wind_Speed_nstorm'   		, wind_spd_nstorm
NCDF_VARGET, id, 'Wind_Direction_nstorm'	, wind_dir_nstorm
NCDF_VARGET, id, 'Pressure_nstorm'			, press_nstorm
NCDF_VARGET, id, 'RH_nstorm'        		, rh_nstorm
NCDF_VARGET, id, 'Dew_Point_nstorm'   		, dp_nstorm
NCDF_CLOSE, id

o3_storm_sept = [o3_storm_sept, o3_storm]
;o3dm_storm_sept = [o3dm_storm_sept, o3dm_storm]
voc_storm_sept = [voc_storm_sept, voc_storm]
nox_storm_sept = [nox_storm_sept, nox_storm]
no_storm_sept = [no_storm_sept, no_storm]
no2_storm_sept = [no2_storm_sept, no2_storm]
co_storm_sept = [co_storm_sept, co_storm]
temp_storm_sept = [temp_storm_sept, temp_storm]
wind_spd_storm_sept = [wind_spd_storm_sept, wind_spd_storm]
wind_dir_storm_sept = [wind_dir_storm_sept, wind_dir_storm]
press_storm_sept = [press_storm_sept, press_storm]
rh_storm_sept = [rh_storm_sept, rh_storm]
dp_storm_sept = [dp_storm_sept, dp_storm]

o3_nstorm_sept = [o3_nstorm_sept, o3_nstorm]
;o3dm_nstorm_sept = [o3dm_nstorm_sept, o3dm_nstorm]
voc_nstorm_sept = [voc_nstorm_sept, voc_nstorm]
nox_nstorm_sept = [nox_nstorm_sept, nox_nstorm]
no_nstorm_sept  = [no_nstorm_sept, no_nstorm]
no2_nstorm_sept = [no2_nstorm_sept, no2_nstorm]
co_nstorm_sept = [co_nstorm_sept, co_nstorm]
temp_nstorm_sept = [temp_nstorm_sept, temp_nstorm]
wind_spd_nstorm_sept = [wind_spd_nstorm_sept, wind_spd_nstorm]
wind_dir_nstorm_sept = [wind_dir_nstorm_sept, wind_dir_nstorm]
press_nstorm_sept = [press_nstorm_sept, press_nstorm]
rh_nstorm_sept = [rh_nstorm_sept, rh_nstorm]
dp_nstorm_sept = [dp_nstorm_sept, dp_nstorm]


infile      = !WRF_DIRECTORY + 'general/o3_data/hourly_data/hourly_anomalies_092005_102017_SEPT_18Z.nc'																;Set input file path

;Get monthly averaged data
id  = NCDF_OPEN(infile)																						;Open input file for reading	

NCDF_VARGET, id, 'O3_storm'					, o3_storm
;NCDF_VARGET, id, 'O3DM_storm'				, o3dm_storm
NCDF_VARGET, id, 'VOC_storm'   				, voc_storm
NCDF_VARGET, id, 'PM2.5_storm'				, pm_storm
NCDF_VARGET, id, 'NOx_storm' 				, nox_storm
NCDF_VARGET, id, 'NO_storm' 				, no_storm
NCDF_VARGET, id, 'NO2_storm' 				, no2_storm
NCDF_VARGET, id, 'CO_storm' 				, co_storm
NCDF_VARGET, id, 'Temperature_storm'  		, temp_storm
NCDF_VARGET, id, 'Wind_Speed_storm'   		, wind_spd_storm
NCDF_VARGET, id, 'Wind_Direction_storm'		, wind_dir_storm
NCDF_VARGET, id, 'Pressure_storm'			, press_storm
NCDF_VARGET, id, 'RH_storm'        			, rh_storm
NCDF_VARGET, id, 'Dew_Point_storm'   		, dp_storm

NCDF_VARGET, id, 'O3_nstorm'				, o3_nstorm
;NCDF_VARGET, id, 'O3DM_nstorm'				, o3dm_nstorm
NCDF_VARGET, id, 'VOC_nstorm'   			, voc_nstorm
NCDF_VARGET, id, 'PM2.5_nstorm'				, pm_nstorm
NCDF_VARGET, id, 'NOx_nstorm' 				, nox_nstorm
NCDF_VARGET, id, 'NO_nstorm' 				, no_nstorm
NCDF_VARGET, id, 'NO2_nstorm' 				, no2_nstorm
NCDF_VARGET, id, 'CO_nstorm' 				, co_nstorm
NCDF_VARGET, id, 'Temperature_nstorm'  		, temp_nstorm
NCDF_VARGET, id, 'Wind_Speed_nstorm'   		, wind_spd_nstorm
NCDF_VARGET, id, 'Wind_Direction_nstorm'	, wind_dir_nstorm
NCDF_VARGET, id, 'Pressure_nstorm'			, press_nstorm
NCDF_VARGET, id, 'RH_nstorm'        		, rh_nstorm
NCDF_VARGET, id, 'Dew_Point_nstorm'   		, dp_nstorm
NCDF_CLOSE, id

o3_storm_sept = [o3_storm_sept, o3_storm]
;o3dm_storm_sept = [o3dm_storm_sept, o3dm_storm]
voc_storm_sept = [voc_storm_sept, voc_storm]
nox_storm_sept = [nox_storm_sept, nox_storm]
no_storm_sept = [no_storm_sept, no_storm]
no2_storm_sept = [no2_storm_sept, no2_storm]
pm_storm_sept = [pm_storm_sept, pm_storm]
co_storm_sept = [co_storm_sept, co_storm]
temp_storm_sept = [temp_storm_sept, temp_storm]
wind_spd_storm_sept = [wind_spd_storm_sept, wind_spd_storm]
wind_dir_storm_sept = [wind_dir_storm_sept, wind_dir_storm]
press_storm_sept = [press_storm_sept, press_storm]
rh_storm_sept = [rh_storm_sept, rh_storm]
dp_storm_sept = [dp_storm_sept, dp_storm]

o3_nstorm_sept = [o3_nstorm_sept, o3_nstorm]
;o3dm_nstorm_sept = [o3dm_nstorm_sept, o3dm_nstorm]
voc_nstorm_sept = [voc_nstorm_sept, voc_nstorm]
nox_nstorm_sept = [nox_nstorm_sept, nox_nstorm]
no_nstorm_sept  = [no_nstorm_sept, no_nstorm]
no2_nstorm_sept = [no2_nstorm_sept, no2_nstorm]
pm_nstorm_sept = [pm_nstorm_sept, pm_nstorm]
co_nstorm_sept = [co_nstorm_sept, co_nstorm]
temp_nstorm_sept = [temp_nstorm_sept, temp_nstorm]
wind_spd_nstorm_sept = [wind_spd_nstorm_sept, wind_spd_nstorm]
wind_dir_nstorm_sept = [wind_dir_nstorm_sept, wind_dir_nstorm]
press_nstorm_sept = [press_nstorm_sept, press_nstorm]
rh_nstorm_sept = [rh_nstorm_sept, rh_nstorm]
dp_nstorm_sept = [dp_nstorm_sept, dp_nstorm]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

fname = '/data3/dphoenix/wrf/general/met_chem_boxplots/sept_mean_anomalies_18Z.txt'
OPENW, lun, fname, /GET_LUN     
PRINTF, lun, FORMAT = '("  Variable", 5X, "Sept Storm", 4X, "Sept No Storm")'								;Print table header information
PRINTF, lun, '==================================================================='
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'O3', MEAN(o3_storm_sept), MEAN(o3_nstorm_sept)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'voc', MEAN(voc_storm_sept), MEAN(voc_nstorm_sept)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'nox', MEAN(nox_storm_sept), MEAN(nox_nstorm_sept)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'no', MEAN(no_storm_sept), MEAN(no_nstorm_sept)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'no2', MEAN(no2_storm_sept), MEAN(no2_nstorm_sept)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'pm', MEAN(pm_storm_sept), MEAN(pm_nstorm_sept)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'co', MEAN(co_storm_sept), MEAN(co_nstorm_sept)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'temp', MEAN(temp_storm_sept), MEAN(temp_nstorm_sept)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'wind_spd',  MEAN(wind_spd_storm_sept), MEAN(wind_spd_nstorm_sept)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'wind_dir',  MEAN(wind_dir_storm_sept), MEAN(wind_dir_nstorm_sept)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'press',   MEAN(press_storm_sept), MEAN(press_nstorm_sept)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'rh',  MEAN(rh_storm_sept), MEAN(rh_nstorm_sept)		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'dp',  MEAN(dp_storm_sept), MEAN(dp_nstorm_sept)		

PRINTF, lun, ' '
PRINTF, lun, 'For Days exceeding the EPA O3 standard'
iepa_sept_s = WHERE(o3_storm_sept GT 70.0, COMPLEMENT = nepa_sept_s)
iepa_sept_ns = WHERE(o3_nstorm_sept GT 70.0, COMPLEMENT = nepa_sept_ns)

PRINTF, lun, FORMAT = '("  Variable", 5X, "Sept Storm", 4X, "Sept No Storm")'								;Print table header information
PRINTF, lun, '==================================================================='
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'O3',   MEAN(o3_storm_sept[iepa_sept_s])   		- MEAN(o3_nstorm_sept[nepa_sept_ns])		,          MEAN(o3_nstorm_sept[iepa_sept_ns])	-MEAN(o3_nstorm_sept[nepa_sept_ns])			
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'voc',  MEAN(voc_storm_sept[iepa_sept_s])			- MEAN(voc_nstorm_sept[nepa_sept_ns])		,         MEAN(voc_nstorm_sept[iepa_sept_ns])	-MEAN(voc_nstorm_sept[nepa_sept_ns])			
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'nox',  MEAN(nox_storm_sept[iepa_sept_s])			- MEAN(nox_nstorm_sept[nepa_sept_ns])		,         MEAN(nox_nstorm_sept[iepa_sept_ns])	-MEAN(nox_nstorm_sept[nepa_sept_ns])			
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'no',   MEAN(no_storm_sept[iepa_sept_s])			- MEAN(no_nstorm_sept[nepa_sept_ns])		,          MEAN(no_nstorm_sept[iepa_sept_ns])	-MEAN(no_nstorm_sept[nepa_sept_ns])			
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'no2',  MEAN(no2_storm_sept[iepa_sept_s])			- MEAN(no2_nstorm_sept[nepa_sept_ns])		,         MEAN(no2_nstorm_sept[iepa_sept_ns])	-MEAN(no2_nstorm_sept[nepa_sept_ns])			
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'pm',   MEAN(pm_storm_sept[iepa_sept_s])          - MEAN(pm_nstorm_sept[nepa_sept_ns])		,          MEAN(pm_nstorm_sept[iepa_sept_ns])	-MEAN(pm_nstorm_sept[nepa_sept_ns])			
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'co',   MEAN(co_storm_sept[iepa_sept_s])          - MEAN(co_nstorm_sept[nepa_sept_ns])		,          MEAN(co_nstorm_sept[iepa_sept_ns])	-MEAN(co_nstorm_sept[nepa_sept_ns])			
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'temp', MEAN(temp_storm_sept[iepa_sept_s])		- MEAN(temp_nstorm_sept[nepa_sept_ns])	,        MEAN(temp_nstorm_sept[iepa_sept_ns])		-MEAN(temp_nstorm_sept[nepa_sept_ns])	
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'wind_spd',MEAN(wind_spd_storm_sept[iepa_sept_s]) - MEAN(wind_spd_nstorm_sept[nepa_sept_ns]), MEAN(wind_spd_nstorm_sept[iepa_sept_ns])		-MEAN(wind_spd_nstorm_sept[nepa_sept_ns])
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'wind_dir',MEAN(wind_dir_storm_sept[iepa_sept_s]) - MEAN(wind_dir_nstorm_sept[nepa_sept_ns]), MEAN(wind_dir_nstorm_sept[iepa_sept_ns])		-MEAN(wind_dir_nstorm_sept[nepa_sept_ns])
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'press', MEAN(press_storm_sept[iepa_sept_s])		- MEAN(press_nstorm_sept[nepa_sept_ns])	,      MEAN(press_nstorm_sept[iepa_sept_ns])		-MEAN(press_nstorm_sept[nepa_sept_ns])	
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'rh',  MEAN(rh_storm_sept[iepa_sept_s])           - MEAN(rh_nstorm_sept[nepa_sept_ns])		,           MEAN(rh_nstorm_sept[iepa_sept_ns])	-MEAN(rh_nstorm_sept[nepa_sept_ns])			
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4, 4X, F10.4, 4X, F10.4)', 'dp',  MEAN(dp_storm_sept[iepa_sept_s])           - MEAN(dp_nstorm_sept[nepa_sept_ns])		,           MEAN(dp_nstorm_sept[iepa_sept_ns])	-MEAN(dp_nstorm_sept[nepa_sept_ns])			

PRINTF, lun, ' '
PRINTF, lun,'For days NOT exceeding the EPA O3 standard'

PRINTF, lun, FORMAT = '("  Variable", 5X, "Sept Storm", 4X, "Sept No Storm")'								;Print table header information
PRINTF, lun, '==================================================================='
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'O3', 	     MEAN(o3_storm_sept[nepa_sept_s])		-MEAN(o3_nstorm_sept[nepa_sept_ns])		,	    	MEAN(o3_nstorm_sept[nepa_sept_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'voc', 	     MEAN(voc_storm_sept[nepa_sept_s])      -MEAN(voc_nstorm_sept[nepa_sept_ns])		,	    	MEAN(voc_nstorm_sept[nepa_sept_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'nox',	     MEAN(nox_storm_sept[nepa_sept_s])      -MEAN(nox_nstorm_sept[nepa_sept_ns])		,	    	MEAN(nox_nstorm_sept[nepa_sept_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'no', 	     MEAN(no_storm_sept[nepa_sept_s])		-MEAN(no_nstorm_sept[nepa_sept_ns])		, 			MEAN(no_nstorm_sept[nepa_sept_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'no2', 	     MEAN(no2_storm_sept[nepa_sept_s])		-MEAN(no2_nstorm_sept[nepa_sept_ns])		, 		MEAN(no2_nstorm_sept[nepa_sept_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'pm', 	     MEAN(pm_storm_sept[nepa_sept_s])		-MEAN(pm_nstorm_sept[nepa_sept_ns])		, 			MEAN(pm_nstorm_sept[nepa_sept_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'co', 	     MEAN(co_storm_sept[nepa_sept_s])		-MEAN(co_nstorm_sept[nepa_sept_ns])		, 			MEAN(co_nstorm_sept[nepa_sept_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'temp',      MEAN(temp_storm_sept[nepa_sept_s])		-MEAN(temp_nstorm_sept[nepa_sept_ns])	,		MEAN(temp_nstorm_sept[nepa_sept_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'wind_spd',  MEAN(wind_spd_storm_sept[nepa_sept_s])	-MEAN(wind_spd_nstorm_sept[nepa_sept_ns]),	MEAN(wind_spd_nstorm_sept[nepa_sept_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'wind_dir',  MEAN(wind_dir_storm_sept[nepa_sept_s])	-MEAN(wind_dir_nstorm_sept[nepa_sept_ns]),	MEAN(wind_dir_nstorm_sept[nepa_sept_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'press', 	 MEAN(press_storm_sept[nepa_sept_s])	-MEAN(press_nstorm_sept[nepa_sept_ns])	, 		MEAN(press_nstorm_sept[nepa_sept_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'rh', 		 MEAN(rh_storm_sept[nepa_sept_s])		-MEAN(rh_nstorm_sept[nepa_sept_ns])		, 			MEAN(rh_nstorm_sept[nepa_sept_ns])		
PRINTF, lun, FORMAT = '(A8, 7X, F10.4, 3X, F10.4)', 'dp', 		 MEAN(dp_storm_sept[nepa_sept_s])		-MEAN(dp_nstorm_sept[nepa_sept_ns])		, 			MEAN(dp_nstorm_sept[nepa_sept_ns])		

PRINTF, lun, ' '

PRINTF, lun, 'Number of days exceeding the EPA standard with storms Sept    = ', N_ELEMENTS(iepa_sept_s)
PRINTF, lun, 'Number of days exceeding the EPA standard without storms Sept = ', N_ELEMENTS(iepa_sept_ns)

PRINTF, lun, ' '

PRINTF, lun, 'Number of days not exceeding the EPA standard with storms Sept    = ', N_ELEMENTS(nepa_sept_s)
PRINTF, lun, 'Number of days not exceeding the EPA standard without storms Sept = ', N_ELEMENTS(nepa_sept_ns)

FREE_LUN, lun

;boxplot code 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;SEPTEMBER

;;;EPA exceedances without storms: OZONE DAILY MAX
;o3dm_epa_storm9 = o3dm_storm_sept[iepa_sept_s] - o3dm_nstorm_sept[nepa_sept_ns]
;o3dm_epa_storm9_sort  = o3dm_epa_storm9[SORT(o3dm_epa_storm9)]
;IF (N_ELEMENTS(o3dm_epa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
;	No3dm_epa_storm9_sort 	= N_ELEMENTS(o3dm_epa_storm9_sort)
;	o3dm_med = (o3dm_epa_storm9_sort[(No3dm_epa_storm9_sort/2)-1] + o3dm_epa_storm9_sort[(No3dm_epa_storm9_sort/2)]) / 2.0
;	lower_half = o3dm_epa_storm9_sort[0:(No3dm_epa_storm9_sort/2)-1]
;	upper_half = o3dm_epa_storm9_sort[(No3dm_epa_storm9_sort/2):(No3dm_epa_storm9_sort-1)]
;ENDIF ELSE BEGIN
;	No3dm_epa_storm9_sort 	= N_ELEMENTS(o3dm_epa_storm9_sort)
;	o3dm_med = o3dm_epa_storm9_sort[(No3dm_epa_storm9_sort/2)] 
;	lower_half = o3dm_epa_storm9_sort[0:(No3dm_epa_storm9_sort/2)-1]
;	upper_half = o3dm_epa_storm9_sort[(No3dm_epa_storm9_sort/2):(No3dm_epa_storm9_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = o3dm_epa_storm9_sort[0.05*No3dm_epa_storm9_sort]
;quartile_95 = o3dm_epa_storm9_sort[0.95*No3dm_epa_storm9_sort]
;
;o3dm_epa_storm9_ptile = [quartile_05, quartile_25, o3dm_med, quartile_75,quartile_95]
;
;
;;;;EPA exceedances without storms: OZONE DAILY MAX
;o3dm_epa_nstorm9 = o3dm_nstorm_sept[iepa_sept_ns] - o3dm_nstorm_sept[nepa_sept_ns]
;o3dm_epa_nstorm9_sort  = o3dm_epa_nstorm9[SORT(o3dm_epa_nstorm9)]
;IF (N_ELEMENTS(o3dm_epa_nstorm9_sort) MOD 2 EQ 0) THEN BEGIN
;	No3dm_epa_nstorm9_sort 	= N_ELEMENTS(o3dm_epa_nstorm9_sort)
;	o3dm_med = (o3dm_epa_nstorm9_sort[(No3dm_epa_nstorm9_sort/2)-1] + o3dm_epa_nstorm9_sort[(No3dm_epa_nstorm9_sort/2)]) / 2.0
;	lower_half = o3dm_epa_nstorm9_sort[0:(No3dm_epa_nstorm9_sort/2)-1]
;	upper_half = o3dm_epa_nstorm9_sort[(No3dm_epa_nstorm9_sort/2):(No3dm_epa_nstorm9_sort-1)]
;ENDIF ELSE BEGIN
;	No3dm_epa_nstorm9_sort 	= N_ELEMENTS(o3dm_epa_nstorm9_sort)
;	o3dm_med = o3dm_epa_nstorm9_sort[(No3dm_epa_nstorm9_sort/2)] 
;	lower_half = o3dm_epa_nstorm9_sort[0:(No3dm_epa_nstorm9_sort/2)-1]
;	upper_half = o3dm_epa_nstorm9_sort[(No3dm_epa_nstorm9_sort/2):(No3dm_epa_nstorm9_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = o3dm_epa_nstorm9_sort[0.05*No3dm_epa_nstorm9_sort]
;quartile_95 = o3dm_epa_nstorm9_sort[0.95*No3dm_epa_nstorm9_sort]
;
;o3dm_epa_nstorm9_ptile = [quartile_05, quartile_25, o3dm_med, quartile_75,quartile_95]
;
;
;;;;non-EPA exceedances with storms: OZONE DAILY MAX
;o3dm_nepa_storm9 = o3dm_storm_sept[nepa_sept_s] - o3dm_nstorm_sept[nepa_sept_ns]
;o3dm_nepa_storm9_sort  = o3dm_nepa_storm9[SORT(o3dm_nepa_storm9)]
;IF (N_ELEMENTS(o3dm_nepa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
;	No3dm_nepa_storm9_sort 	= N_ELEMENTS(o3dm_nepa_storm9_sort)
;	o3dm_med = (o3dm_nepa_storm9_sort[(No3dm_nepa_storm9_sort/2)-1] + o3dm_nepa_storm9_sort[(No3dm_nepa_storm9_sort/2)]) / 2.0
;	lower_half = o3dm_nepa_storm9_sort[0:(No3dm_nepa_storm9_sort/2)-1]
;	upper_half = o3dm_nepa_storm9_sort[(No3dm_nepa_storm9_sort/2):(No3dm_nepa_storm9_sort-1)]
;ENDIF ELSE BEGIN
;	No3dm_nepa_storm9_sort 	= N_ELEMENTS(o3dm_nepa_storm9_sort)
;	o3dm_med = o3dm_nepa_storm9_sort[(No3dm_nepa_storm9_sort/2)] 
;	lower_half = o3dm_nepa_storm9_sort[0:(No3dm_nepa_storm9_sort/2)-1]
;	upper_half = o3dm_nepa_storm9_sort[(No3dm_nepa_storm9_sort/2):(No3dm_nepa_storm9_sort-1)]
;ENDELSE
;
;quartile_25 = MEDIAN(lower_half, /EVEN)
;quartile_75 = MEDIAN(upper_half, /EVEN)
;quartile_05 = o3dm_nepa_storm9_sort[0.05*No3dm_nepa_storm9_sort]
;quartile_95 = o3dm_nepa_storm9_sort[0.95*No3dm_nepa_storm9_sort]
;
;o3dm_nepa_storm9_ptile = [quartile_05, quartile_25, o3dm_med, quartile_75,quartile_95]
;
;
;data = [[o3dm_epa_storm9_ptile], [o3dm_epa_nstorm9_ptile], [o3dm_nepa_storm9_ptile]]
;ytitle = ['EPA w/ Storms', 'EPA w/o Storms','non-EPA w/ Storms']
;boxes = BOXPLOT(data, $
;		TITLE		= 'Sept: Ozone Daily Max', $
;		XRANGE 		= [-40,70], $
;		YRANGE 		= [-1, 3], $
;		XTITLE 		= 'o3dm Concentration (ppb)', $
;		YTICKNAME 	= ytitle, $
;		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
;		FONT_SIZE   = 20, $
;		HORIZONTAL	= 1)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;EPA exceedances without storms: OZONE
o3_epa_storm9 = o3_storm_sept[iepa_sept_s]-MEAN(o3_nstorm_sept[nepa_sept_ns],/NAN)
o3_epa_storm9_sort  = o3_epa_storm9[SORT(o3_epa_storm9)]
IF (N_ELEMENTS(o3_epa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
	No3_epa_storm9_sort 	= N_ELEMENTS(o3_epa_storm9_sort)
	o3_med = (o3_epa_storm9_sort[(No3_epa_storm9_sort/2)-1] + o3_epa_storm9_sort[(No3_epa_storm9_sort/2)]) / 2.0
	lower_half = o3_epa_storm9_sort[0:(No3_epa_storm9_sort/2)-1]
	upper_half = o3_epa_storm9_sort[(No3_epa_storm9_sort/2):(No3_epa_storm9_sort-1)]
ENDIF ELSE BEGIN
	No3_epa_storm9_sort 	= N_ELEMENTS(o3_epa_storm9_sort)
	o3_med = o3_epa_storm9_sort[(No3_epa_storm9_sort/2)] 
	lower_half = o3_epa_storm9_sort[0:(No3_epa_storm9_sort/2)-1]
	upper_half = o3_epa_storm9_sort[(No3_epa_storm9_sort/2):(No3_epa_storm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_epa_storm9_sort[0.05*No3_epa_storm9_sort]
quartile_95 = o3_epa_storm9_sort[0.95*No3_epa_storm9_sort]

o3_epa_storm9_ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: OZONE
o3_epa_nstorm9 = o3_nstorm_sept[iepa_sept_ns]-MEAN(o3_nstorm_sept[nepa_sept_ns],/NAN)
o3_epa_nstorm9_sort  = o3_epa_nstorm9[SORT(o3_epa_nstorm9)]
IF (N_ELEMENTS(o3_epa_nstorm9_sort) MOD 2 EQ 0) THEN BEGIN
	No3_epa_nstorm9_sort 	= N_ELEMENTS(o3_epa_nstorm9_sort)
	o3_med = (o3_epa_nstorm9_sort[(No3_epa_nstorm9_sort/2)-1] + o3_epa_nstorm9_sort[(No3_epa_nstorm9_sort/2)]) / 2.0
	lower_half = o3_epa_nstorm9_sort[0:(No3_epa_nstorm9_sort/2)-1]
	upper_half = o3_epa_nstorm9_sort[(No3_epa_nstorm9_sort/2):(No3_epa_nstorm9_sort-1)]
ENDIF ELSE BEGIN
	No3_epa_nstorm9_sort 	= N_ELEMENTS(o3_epa_nstorm9_sort)
	o3_med = o3_epa_nstorm9_sort[(No3_epa_nstorm9_sort/2)] 
	lower_half = o3_epa_nstorm9_sort[0:(No3_epa_nstorm9_sort/2)-1]
	upper_half = o3_epa_nstorm9_sort[(No3_epa_nstorm9_sort/2):(No3_epa_nstorm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_epa_nstorm9_sort[0.05*No3_epa_nstorm9_sort]
quartile_95 = o3_epa_nstorm9_sort[0.95*No3_epa_nstorm9_sort]

o3_epa_nstorm9_ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: OZONE
o3_nepa_storm9 = o3_storm_sept[nepa_sept_s]-MEAN(o3_nstorm_sept[nepa_sept_ns],/NAN)
o3_nepa_storm9_sort  = o3_nepa_storm9[SORT(o3_nepa_storm9)]
IF (N_ELEMENTS(o3_nepa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
	No3_nepa_storm9_sort 	= N_ELEMENTS(o3_nepa_storm9_sort)
	o3_med = (o3_nepa_storm9_sort[(No3_nepa_storm9_sort/2)-1] + o3_nepa_storm9_sort[(No3_nepa_storm9_sort/2)]) / 2.0
	lower_half = o3_nepa_storm9_sort[0:(No3_nepa_storm9_sort/2)-1]
	upper_half = o3_nepa_storm9_sort[(No3_nepa_storm9_sort/2):(No3_nepa_storm9_sort-1)]
ENDIF ELSE BEGIN
	No3_nepa_storm9_sort 	= N_ELEMENTS(o3_nepa_storm9_sort)
	o3_med = o3_nepa_storm9_sort[(No3_nepa_storm9_sort/2)] 
	lower_half = o3_nepa_storm9_sort[0:(No3_nepa_storm9_sort/2)-1]
	upper_half = o3_nepa_storm9_sort[(No3_nepa_storm9_sort/2):(No3_nepa_storm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = o3_nepa_storm9_sort[0.05*No3_nepa_storm9_sort]
quartile_95 = o3_nepa_storm9_sort[0.95*No3_nepa_storm9_sort]

o3_nepa_storm9_ptile = [quartile_05, quartile_25, o3_med, quartile_75,quartile_95]

data = [[o3_epa_storm9_ptile], [o3_epa_nstorm9_ptile], [o3_nepa_storm9_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','non-EPA w/ Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'Sept: Ozone', $
		XRANGE 		= [-50,80], $
		YRANGE 		= [-1, 3], $
		XTITLE 		= 'O3 Concentration (ppb)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: voc
voc_epa_storm9 = voc_storm_sept[iepa_sept_s]-MEAN(voc_nstorm_sept[nepa_sept_ns],/NAN)
voc_epa_storm9_sort  = voc_epa_storm9[SORT(voc_epa_storm9)]
IF (N_ELEMENTS(voc_epa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
	Nvoc_epa_storm9_sort 	= N_ELEMENTS(voc_epa_storm9_sort)
	voc_med = (voc_epa_storm9_sort[(Nvoc_epa_storm9_sort/2)-1] + voc_epa_storm9_sort[(Nvoc_epa_storm9_sort/2)]) / 2.0
	lower_half = voc_epa_storm9_sort[0:(Nvoc_epa_storm9_sort/2)-1]
	upper_half = voc_epa_storm9_sort[(Nvoc_epa_storm9_sort/2):(Nvoc_epa_storm9_sort-1)]
ENDIF ELSE BEGIN
	Nvoc_epa_storm9_sort 	= N_ELEMENTS(voc_epa_storm9_sort)
	voc_med = voc_epa_storm9_sort[(Nvoc_epa_storm9_sort/2)] 
	lower_half = voc_epa_storm9_sort[0:(Nvoc_epa_storm9_sort/2)-1]
	upper_half = voc_epa_storm9_sort[(Nvoc_epa_storm9_sort/2):(Nvoc_epa_storm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = voc_epa_storm9_sort[0.05*Nvoc_epa_storm9_sort]
quartile_95 = voc_epa_storm9_sort[0.95*Nvoc_epa_storm9_sort]

voc_epa_storm9_ptile = [quartile_05, quartile_25, voc_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: voc
voc_epa_nstorm9 = voc_nstorm_sept[iepa_sept_ns]-MEAN(voc_nstorm_sept[nepa_sept_ns],/NAN)
voc_epa_nstorm9_sort  = voc_epa_nstorm9[SORT(voc_epa_nstorm9)]
IF (N_ELEMENTS(voc_epa_nstorm9_sort) MOD 2 EQ 0) THEN BEGIN
	Nvoc_epa_nstorm9_sort 	= N_ELEMENTS(voc_epa_nstorm9_sort)
	voc_med = (voc_epa_nstorm9_sort[(Nvoc_epa_nstorm9_sort/2)-1] + voc_epa_nstorm9_sort[(Nvoc_epa_nstorm9_sort/2)]) / 2.0
	lower_half = voc_epa_nstorm9_sort[0:(Nvoc_epa_nstorm9_sort/2)-1]
	upper_half = voc_epa_nstorm9_sort[(Nvoc_epa_nstorm9_sort/2):(Nvoc_epa_nstorm9_sort-1)]
ENDIF ELSE BEGIN
	Nvoc_epa_nstorm9_sort 	= N_ELEMENTS(voc_epa_nstorm9_sort)
	voc_med = voc_epa_nstorm9_sort[(Nvoc_epa_nstorm9_sort/2)] 
	lower_half = voc_epa_nstorm9_sort[0:(Nvoc_epa_nstorm9_sort/2)-1]
	upper_half = voc_epa_nstorm9_sort[(Nvoc_epa_nstorm9_sort/2):(Nvoc_epa_nstorm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = voc_epa_nstorm9_sort[0.05*Nvoc_epa_nstorm9_sort]
quartile_95 = voc_epa_nstorm9_sort[0.95*Nvoc_epa_nstorm9_sort]

voc_epa_nstorm9_ptile = [quartile_05, quartile_25, voc_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: voc
voc_nepa_storm9 = voc_storm_sept[nepa_sept_s]-MEAN(voc_nstorm_sept[nepa_sept_ns],/NAN)
voc_nepa_storm9_sort  = voc_nepa_storm9[SORT(voc_nepa_storm9)]
IF (N_ELEMENTS(voc_nepa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
	Nvoc_nepa_storm9_sort 	= N_ELEMENTS(voc_nepa_storm9_sort)
	voc_med = (voc_nepa_storm9_sort[(Nvoc_nepa_storm9_sort/2)-1] + voc_nepa_storm9_sort[(Nvoc_nepa_storm9_sort/2)]) / 2.0
	lower_half = voc_nepa_storm9_sort[0:(Nvoc_nepa_storm9_sort/2)-1]
	upper_half = voc_nepa_storm9_sort[(Nvoc_nepa_storm9_sort/2):(Nvoc_nepa_storm9_sort-1)]
ENDIF ELSE BEGIN
	Nvoc_nepa_storm9_sort 	= N_ELEMENTS(voc_nepa_storm9_sort)
	voc_med = voc_nepa_storm9_sort[(Nvoc_nepa_storm9_sort/2)] 
	lower_half = voc_nepa_storm9_sort[0:(Nvoc_nepa_storm9_sort/2)-1]
	upper_half = voc_nepa_storm9_sort[(Nvoc_nepa_storm9_sort/2):(Nvoc_nepa_storm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = voc_nepa_storm9_sort[0.05*Nvoc_nepa_storm9_sort]
quartile_95 = voc_nepa_storm9_sort[0.95*Nvoc_nepa_storm9_sort]

voc_nepa_storm9_ptile = [quartile_05, quartile_25, voc_med, quartile_75,quartile_95]


data = [[voc_epa_storm9_ptile], [voc_epa_nstorm9_ptile], [voc_nepa_storm9_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','non-EPA w/ Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'Sept: VOC', $
		XRANGE 		= [-20,20], $
		YRANGE 		= [-1, 3], $
		XTITLE 		= 'VOC Concentration (ppb)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

STOP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances without storms: nox
nox_epa_storm9 = nox_storm_sept[iepa_sept_s]-MEAN(nox_nstorm_sept[nepa_sept_ns],/NAN)
nox_epa_storm9_sort  = nox_epa_storm9[SORT(nox_epa_storm9)]
IF (N_ELEMENTS(nox_epa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
	Nnox_epa_storm9_sort 	= N_ELEMENTS(nox_epa_storm9_sort)
	nox_med = (nox_epa_storm9_sort[(Nnox_epa_storm9_sort/2)-1] + nox_epa_storm9_sort[(Nnox_epa_storm9_sort/2)]) / 2.0
	lower_half = nox_epa_storm9_sort[0:(Nnox_epa_storm9_sort/2)-1]
	upper_half = nox_epa_storm9_sort[(Nnox_epa_storm9_sort/2):(Nnox_epa_storm9_sort-1)]
ENDIF ELSE BEGIN
	Nnox_epa_storm9_sort 	= N_ELEMENTS(nox_epa_storm9_sort)
	nox_med = nox_epa_storm9_sort[(Nnox_epa_storm9_sort/2)] 
	lower_half = nox_epa_storm9_sort[0:(Nnox_epa_storm9_sort/2)-1]
	upper_half = nox_epa_storm9_sort[(Nnox_epa_storm9_sort/2):(Nnox_epa_storm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = nox_epa_storm9_sort[0.05*Nnox_epa_storm9_sort]
quartile_95 = nox_epa_storm9_sort[0.95*Nnox_epa_storm9_sort]

nox_epa_storm9_ptile = [quartile_05, quartile_25, nox_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: nox
nox_epa_nstorm9 = nox_nstorm_sept[iepa_sept_ns]-MEAN(nox_nstorm_sept[nepa_sept_ns],/NAN)
nox_epa_nstorm9_sort  = nox_epa_nstorm9[SORT(nox_epa_nstorm9)]
IF (N_ELEMENTS(nox_epa_nstorm9_sort) MOD 2 EQ 0) THEN BEGIN
	Nnox_epa_nstorm9_sort 	= N_ELEMENTS(nox_epa_nstorm9_sort)
	nox_med = (nox_epa_nstorm9_sort[(Nnox_epa_nstorm9_sort/2)-1] + nox_epa_nstorm9_sort[(Nnox_epa_nstorm9_sort/2)]) / 2.0
	lower_half = nox_epa_nstorm9_sort[0:(Nnox_epa_nstorm9_sort/2)-1]
	upper_half = nox_epa_nstorm9_sort[(Nnox_epa_nstorm9_sort/2):(Nnox_epa_nstorm9_sort-1)]
ENDIF ELSE BEGIN
	Nnox_epa_nstorm9_sort 	= N_ELEMENTS(nox_epa_nstorm9_sort)
	nox_med = nox_epa_nstorm9_sort[(Nnox_epa_nstorm9_sort/2)] 
	lower_half = nox_epa_nstorm9_sort[0:(Nnox_epa_nstorm9_sort/2)-1]
	upper_half = nox_epa_nstorm9_sort[(Nnox_epa_nstorm9_sort/2):(Nnox_epa_nstorm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = nox_epa_nstorm9_sort[0.05*Nnox_epa_nstorm9_sort]
quartile_95 = nox_epa_nstorm9_sort[0.95*Nnox_epa_nstorm9_sort]

nox_epa_nstorm9_ptile = [quartile_05, quartile_25, nox_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: nox
nox_nepa_storm9 = nox_storm_sept[nepa_sept_s]-MEAN(nox_nstorm_sept[nepa_sept_ns],/NAN)
nox_nepa_storm9_sort  = nox_nepa_storm9[SORT(nox_nepa_storm9)]
IF (N_ELEMENTS(nox_nepa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
	Nnox_nepa_storm9_sort 	= N_ELEMENTS(nox_nepa_storm9_sort)
	nox_med = (nox_nepa_storm9_sort[(Nnox_nepa_storm9_sort/2)-1] + nox_nepa_storm9_sort[(Nnox_nepa_storm9_sort/2)]) / 2.0
	lower_half = nox_nepa_storm9_sort[0:(Nnox_nepa_storm9_sort/2)-1]
	upper_half = nox_nepa_storm9_sort[(Nnox_nepa_storm9_sort/2):(Nnox_nepa_storm9_sort-1)]
ENDIF ELSE BEGIN
	Nnox_nepa_storm9_sort 	= N_ELEMENTS(nox_nepa_storm9_sort)
	nox_med = nox_nepa_storm9_sort[(Nnox_nepa_storm9_sort/2)] 
	lower_half = nox_nepa_storm9_sort[0:(Nnox_nepa_storm9_sort/2)-1]
	upper_half = nox_nepa_storm9_sort[(Nnox_nepa_storm9_sort/2):(Nnox_nepa_storm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = nox_nepa_storm9_sort[0.05*Nnox_nepa_storm9_sort]
quartile_95 = nox_nepa_storm9_sort[0.95*Nnox_nepa_storm9_sort]

nox_nepa_storm9_ptile = [quartile_05, quartile_25, nox_med, quartile_75,quartile_95]


data = [[nox_epa_storm9_ptile], [nox_epa_nstorm9_ptile], [nox_nepa_storm9_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','non-EPA w/ Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'Sept: NOx', $
		XRANGE 		= [-15,20], $
		YRANGE 		= [-1, 3], $
		XTITLE 		= 'NOx Concentration (ppb)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: no
no_epa_storm9 = no_storm_sept[iepa_sept_s]-MEAN(no_nstorm_sept[nepa_sept_ns],/NAN)
no_epa_storm9_sort  = no_epa_storm9[SORT(no_epa_storm9)]
IF (N_ELEMENTS(no_epa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
	Nno_epa_storm9_sort 	= N_ELEMENTS(no_epa_storm9_sort)
	no_med = (no_epa_storm9_sort[(Nno_epa_storm9_sort/2)-1] + no_epa_storm9_sort[(Nno_epa_storm9_sort/2)]) / 2.0
	lower_half = no_epa_storm9_sort[0:(Nno_epa_storm9_sort/2)-1]
	upper_half = no_epa_storm9_sort[(Nno_epa_storm9_sort/2):(Nno_epa_storm9_sort-1)]
ENDIF ELSE BEGIN
	Nno_epa_storm9_sort 	= N_ELEMENTS(no_epa_storm9_sort)
	no_med = no_epa_storm9_sort[(Nno_epa_storm9_sort/2)] 
	lower_half = no_epa_storm9_sort[0:(Nno_epa_storm9_sort/2)-1]
	upper_half = no_epa_storm9_sort[(Nno_epa_storm9_sort/2):(Nno_epa_storm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = no_epa_storm9_sort[0.05*Nno_epa_storm9_sort]
quartile_95 = no_epa_storm9_sort[0.95*Nno_epa_storm9_sort]

no_epa_storm9_ptile = [quartile_05, quartile_25, no_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: no
no_epa_nstorm9 = no_nstorm_sept[iepa_sept_ns]-MEAN(no_nstorm_sept[nepa_sept_ns],/NAN)
no_epa_nstorm9_sort  = no_epa_nstorm9[SORT(no_epa_nstorm9)]
IF (N_ELEMENTS(no_epa_nstorm9_sort) MOD 2 EQ 0) THEN BEGIN
	Nno_epa_nstorm9_sort 	= N_ELEMENTS(no_epa_nstorm9_sort)
	no_med = (no_epa_nstorm9_sort[(Nno_epa_nstorm9_sort/2)-1] + no_epa_nstorm9_sort[(Nno_epa_nstorm9_sort/2)]) / 2.0
	lower_half = no_epa_nstorm9_sort[0:(Nno_epa_nstorm9_sort/2)-1]
	upper_half = no_epa_nstorm9_sort[(Nno_epa_nstorm9_sort/2):(Nno_epa_nstorm9_sort-1)]
ENDIF ELSE BEGIN
	Nno_epa_nstorm9_sort 	= N_ELEMENTS(no_epa_nstorm9_sort)
	no_med = no_epa_nstorm9_sort[(Nno_epa_nstorm9_sort/2)] 
	lower_half = no_epa_nstorm9_sort[0:(Nno_epa_nstorm9_sort/2)-1]
	upper_half = no_epa_nstorm9_sort[(Nno_epa_nstorm9_sort/2):(Nno_epa_nstorm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = no_epa_nstorm9_sort[0.05*Nno_epa_nstorm9_sort]
quartile_95 = no_epa_nstorm9_sort[0.95*Nno_epa_nstorm9_sort]

no_epa_nstorm9_ptile = [quartile_05, quartile_25, no_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: no
no_nepa_storm9 = no_storm_sept[nepa_sept_s]-MEAN(no_nstorm_sept[nepa_sept_ns],/NAN)
no_nepa_storm9_sort  = no_nepa_storm9[SORT(no_nepa_storm9)]
IF (N_ELEMENTS(no_nepa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
	Nno_nepa_storm9_sort 	= N_ELEMENTS(no_nepa_storm9_sort)
	no_med = (no_nepa_storm9_sort[(Nno_nepa_storm9_sort/2)-1] + no_nepa_storm9_sort[(Nno_nepa_storm9_sort/2)]) / 2.0
	lower_half = no_nepa_storm9_sort[0:(Nno_nepa_storm9_sort/2)-1]
	upper_half = no_nepa_storm9_sort[(Nno_nepa_storm9_sort/2):(Nno_nepa_storm9_sort-1)]
ENDIF ELSE BEGIN
	Nno_nepa_storm9_sort 	= N_ELEMENTS(no_nepa_storm9_sort)
	no_med = no_nepa_storm9_sort[(Nno_nepa_storm9_sort/2)] 
	lower_half = no_nepa_storm9_sort[0:(Nno_nepa_storm9_sort/2)-1]
	upper_half = no_nepa_storm9_sort[(Nno_nepa_storm9_sort/2):(Nno_nepa_storm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = no_nepa_storm9_sort[0.05*Nno_nepa_storm9_sort]
quartile_95 = no_nepa_storm9_sort[0.95*Nno_nepa_storm9_sort]

no_nepa_storm9_ptile = [quartile_05, quartile_25, no_med, quartile_75,quartile_95]


data = [[no_epa_storm9_ptile], [no_epa_nstorm9_ptile], [no_nepa_storm9_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','non-EPA w/ Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'Sept: NO', $
		XRANGE 		= [-10,10], $
		YRANGE 		= [-1, 3], $
		XTITLE 		= 'NO Concentration (ppb)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: no2
no2_epa_storm9 = no2_storm_sept[iepa_sept_s]-MEAN(no2_nstorm_sept[nepa_sept_ns],/NAN)
no2_epa_storm9_sort  = no2_epa_storm9[SORT(no2_epa_storm9)]
IF (N_ELEMENTS(no2_epa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
	Nno2_epa_storm9_sort 	= N_ELEMENTS(no2_epa_storm9_sort)
	no2_med = (no2_epa_storm9_sort[(Nno2_epa_storm9_sort/2)-1] + no2_epa_storm9_sort[(Nno2_epa_storm9_sort/2)]) / 2.0
	lower_half = no2_epa_storm9_sort[0:(Nno2_epa_storm9_sort/2)-1]
	upper_half = no2_epa_storm9_sort[(Nno2_epa_storm9_sort/2):(Nno2_epa_storm9_sort-1)]
ENDIF ELSE BEGIN
	Nno2_epa_storm9_sort 	= N_ELEMENTS(no2_epa_storm9_sort)
	no2_med = no2_epa_storm9_sort[(Nno2_epa_storm9_sort/2)] 
	lower_half = no2_epa_storm9_sort[0:(Nno2_epa_storm9_sort/2)-1]
	upper_half = no2_epa_storm9_sort[(Nno2_epa_storm9_sort/2):(Nno2_epa_storm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = no2_epa_storm9_sort[0.05*Nno2_epa_storm9_sort]
quartile_95 = no2_epa_storm9_sort[0.95*Nno2_epa_storm9_sort]

no2_epa_storm9_ptile = [quartile_05, quartile_25, no2_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: no2
no2_epa_nstorm9 = no2_nstorm_sept[iepa_sept_ns]-MEAN(no2_nstorm_sept[nepa_sept_ns],/NAN)
no2_epa_nstorm9_sort  = no2_epa_nstorm9[SORT(no2_epa_nstorm9)]
IF (N_ELEMENTS(no2_epa_nstorm9_sort) MOD 2 EQ 0) THEN BEGIN
	Nno2_epa_nstorm9_sort 	= N_ELEMENTS(no2_epa_nstorm9_sort)
	no2_med = (no2_epa_nstorm9_sort[(Nno2_epa_nstorm9_sort/2)-1] + no2_epa_nstorm9_sort[(Nno2_epa_nstorm9_sort/2)]) / 2.0
	lower_half = no2_epa_nstorm9_sort[0:(Nno2_epa_nstorm9_sort/2)-1]
	upper_half = no2_epa_nstorm9_sort[(Nno2_epa_nstorm9_sort/2):(Nno2_epa_nstorm9_sort-1)]
ENDIF ELSE BEGIN
	Nno2_epa_nstorm9_sort 	= N_ELEMENTS(no2_epa_nstorm9_sort)
	no2_med = no2_epa_nstorm9_sort[(Nno2_epa_nstorm9_sort/2)] 
	lower_half = no2_epa_nstorm9_sort[0:(Nno2_epa_nstorm9_sort/2)-1]
	upper_half = no2_epa_nstorm9_sort[(Nno2_epa_nstorm9_sort/2):(Nno2_epa_nstorm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = no2_epa_nstorm9_sort[0.05*Nno2_epa_nstorm9_sort]
quartile_95 = no2_epa_nstorm9_sort[0.95*Nno2_epa_nstorm9_sort]

no2_epa_nstorm9_ptile = [quartile_05, quartile_25, no2_med, quartile_75,quartile_95]


;;;non-EPA exceedances with storms: no2
no2_nepa_storm9 = no2_storm_sept[nepa_sept_s]-MEAN(no2_nstorm_sept[nepa_sept_ns],/NAN)
no2_nepa_storm9_sort  = no2_nepa_storm9[SORT(no2_nepa_storm9)]
IF (N_ELEMENTS(no2_nepa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
	Nno2_nepa_storm9_sort 	= N_ELEMENTS(no2_nepa_storm9_sort)
	no2_med = (no2_nepa_storm9_sort[(Nno2_nepa_storm9_sort/2)-1] + no2_nepa_storm9_sort[(Nno2_nepa_storm9_sort/2)]) / 2.0
	lower_half = no2_nepa_storm9_sort[0:(Nno2_nepa_storm9_sort/2)-1]
	upper_half = no2_nepa_storm9_sort[(Nno2_nepa_storm9_sort/2):(Nno2_nepa_storm9_sort-1)]
ENDIF ELSE BEGIN
	Nno2_nepa_storm9_sort 	= N_ELEMENTS(no2_nepa_storm9_sort)
	no2_med = no2_nepa_storm9_sort[(Nno2_nepa_storm9_sort/2)] 
	lower_half = no2_nepa_storm9_sort[0:(Nno2_nepa_storm9_sort/2)-1]
	upper_half = no2_nepa_storm9_sort[(Nno2_nepa_storm9_sort/2):(Nno2_nepa_storm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = no2_nepa_storm9_sort[0.05*Nno2_nepa_storm9_sort]
quartile_95 = no2_nepa_storm9_sort[0.95*Nno2_nepa_storm9_sort]

no2_nepa_storm9_ptile = [quartile_05, quartile_25, no2_med, quartile_75,quartile_95]


data = [[no2_epa_storm9_ptile], [no2_epa_nstorm9_ptile], [no2_nepa_storm9_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','no2n-EPA w/ Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'Sept: NO2', $
		XRANGE 		= [-10,20], $
		YRANGE 		= [-1, 3], $
		XTITLE 		= 'NO2 Concentration (ppb)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: co
co_epa_storm9 = co_storm_sept[iepa_sept_s]-MEAN(co_nstorm_sept[nepa_sept_ns],/NAN)
co_epa_storm9_sort  = co_epa_storm9[SORT(co_epa_storm9)]
IF (N_ELEMENTS(co_epa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
	Nco_epa_storm9_sort 	= N_ELEMENTS(co_epa_storm9_sort)
	co_med = (co_epa_storm9_sort[(Nco_epa_storm9_sort/2)-1] + co_epa_storm9_sort[(Nco_epa_storm9_sort/2)]) / 2.0
	lower_half = co_epa_storm9_sort[0:(Nco_epa_storm9_sort/2)-1]
	upper_half = co_epa_storm9_sort[(Nco_epa_storm9_sort/2):(Nco_epa_storm9_sort-1)]
ENDIF ELSE BEGIN
	Nco_epa_storm9_sort 	= N_ELEMENTS(co_epa_storm9_sort)
	co_med = co_epa_storm9_sort[(Nco_epa_storm9_sort/2)] 
	lower_half = co_epa_storm9_sort[0:(Nco_epa_storm9_sort/2)-1]
	upper_half = co_epa_storm9_sort[(Nco_epa_storm9_sort/2):(Nco_epa_storm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = co_epa_storm9_sort[0.05*Nco_epa_storm9_sort]
quartile_95 = co_epa_storm9_sort[0.95*Nco_epa_storm9_sort]

co_epa_storm9_ptile = [quartile_05, quartile_25, co_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: co
co_epa_nstorm9 = co_nstorm_sept[iepa_sept_ns]-MEAN(co_nstorm_sept[nepa_sept_ns],/NAN)
co_epa_nstorm9_sort  = co_epa_nstorm9[SORT(co_epa_nstorm9)]
IF (N_ELEMENTS(co_epa_nstorm9_sort) MOD 2 EQ 0) THEN BEGIN
	Nco_epa_nstorm9_sort 	= N_ELEMENTS(co_epa_nstorm9_sort)
	co_med = (co_epa_nstorm9_sort[(Nco_epa_nstorm9_sort/2)-1] + co_epa_nstorm9_sort[(Nco_epa_nstorm9_sort/2)]) / 2.0
	lower_half = co_epa_nstorm9_sort[0:(Nco_epa_nstorm9_sort/2)-1]
	upper_half = co_epa_nstorm9_sort[(Nco_epa_nstorm9_sort/2):(Nco_epa_nstorm9_sort-1)]
ENDIF ELSE BEGIN
	Nco_epa_nstorm9_sort 	= N_ELEMENTS(co_epa_nstorm9_sort)
	co_med = co_epa_nstorm9_sort[(Nco_epa_nstorm9_sort/2)] 
	lower_half = co_epa_nstorm9_sort[0:(Nco_epa_nstorm9_sort/2)-1]
	upper_half = co_epa_nstorm9_sort[(Nco_epa_nstorm9_sort/2):(Nco_epa_nstorm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = co_epa_nstorm9_sort[0.05*Nco_epa_nstorm9_sort]
quartile_95 = co_epa_nstorm9_sort[0.95*Nco_epa_nstorm9_sort]

co_epa_nstorm9_ptile = [quartile_05, quartile_25, co_med, quartile_75,quartile_95]


;;;con-EPA exceedances with storms: co
co_nepa_storm9 = co_storm_sept[nepa_sept_s]-MEAN(co_nstorm_sept[nepa_sept_ns],/NAN)
co_nepa_storm9_sort  = co_nepa_storm9[SORT(co_nepa_storm9)]
IF (N_ELEMENTS(co_nepa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
	Nco_nepa_storm9_sort 	= N_ELEMENTS(co_nepa_storm9_sort)
	co_med = (co_nepa_storm9_sort[(Nco_nepa_storm9_sort/2)-1] + co_nepa_storm9_sort[(Nco_nepa_storm9_sort/2)]) / 2.0
	lower_half = co_nepa_storm9_sort[0:(Nco_nepa_storm9_sort/2)-1]
	upper_half = co_nepa_storm9_sort[(Nco_nepa_storm9_sort/2):(Nco_nepa_storm9_sort-1)]
ENDIF ELSE BEGIN
	Nco_nepa_storm9_sort 	= N_ELEMENTS(co_nepa_storm9_sort)
	co_med = co_nepa_storm9_sort[(Nco_nepa_storm9_sort/2)] 
	lower_half = co_nepa_storm9_sort[0:(Nco_nepa_storm9_sort/2)-1]
	upper_half = co_nepa_storm9_sort[(Nco_nepa_storm9_sort/2):(Nco_nepa_storm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = co_nepa_storm9_sort[0.05*Nco_nepa_storm9_sort]
quartile_95 = co_nepa_storm9_sort[0.95*Nco_nepa_storm9_sort]

co_nepa_storm9_ptile = [quartile_05, quartile_25, co_med, quartile_75,quartile_95]


data = [[co_epa_storm9_ptile], [co_epa_nstorm9_ptile], [co_nepa_storm9_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','con-EPA w/ Storms']
boxes = BOXPLOT(data*1.0E3, $
		TITLE		= 'Sept: CO', $
		XRANGE 		= [-1000,1500], $
		YRANGE 		= [-1, 3], $
		XTITLE 		= 'CO Concentration (ppb)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: pm
pm_epa_storm9 = pm_storm_sept[iepa_sept_s]-MEAN(pm_nstorm_sept[nepa_sept_ns],/NAN)
pm_epa_storm9_sort  = pm_epa_storm9[SORT(pm_epa_storm9)]
IF (N_ELEMENTS(pm_epa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
	Npm_epa_storm9_sort 	= N_ELEMENTS(pm_epa_storm9_sort)
	pm_med = (pm_epa_storm9_sort[(Npm_epa_storm9_sort/2)-1] + pm_epa_storm9_sort[(Npm_epa_storm9_sort/2)]) / 2.0
	lower_half = pm_epa_storm9_sort[0:(Npm_epa_storm9_sort/2)-1]
	upper_half = pm_epa_storm9_sort[(Npm_epa_storm9_sort/2):(Npm_epa_storm9_sort-1)]
ENDIF ELSE BEGIN
	Npm_epa_storm9_sort 	= N_ELEMENTS(pm_epa_storm9_sort)
	pm_med = pm_epa_storm9_sort[(Npm_epa_storm9_sort/2)] 
	lower_half = pm_epa_storm9_sort[0:(Npm_epa_storm9_sort/2)-1]
	upper_half = pm_epa_storm9_sort[(Npm_epa_storm9_sort/2):(Npm_epa_storm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = pm_epa_storm9_sort[0.05*Npm_epa_storm9_sort]
quartile_95 = pm_epa_storm9_sort[0.95*Npm_epa_storm9_sort]

pm_epa_storm9_ptile = [quartile_05, quartile_25, pm_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: pm
pm_epa_nstorm9 = pm_nstorm_sept[iepa_sept_ns]-MEAN(pm_nstorm_sept[nepa_sept_ns],/NAN)
pm_epa_nstorm9_sort  = pm_epa_nstorm9[SORT(pm_epa_nstorm9)]
IF (N_ELEMENTS(pm_epa_nstorm9_sort) MOD 2 EQ 0) THEN BEGIN
	Npm_epa_nstorm9_sort 	= N_ELEMENTS(pm_epa_nstorm9_sort)
	pm_med = (pm_epa_nstorm9_sort[(Npm_epa_nstorm9_sort/2)-1] + pm_epa_nstorm9_sort[(Npm_epa_nstorm9_sort/2)]) / 2.0
	lower_half = pm_epa_nstorm9_sort[0:(Npm_epa_nstorm9_sort/2)-1]
	upper_half = pm_epa_nstorm9_sort[(Npm_epa_nstorm9_sort/2):(Npm_epa_nstorm9_sort-1)]
ENDIF ELSE BEGIN
	Npm_epa_nstorm9_sort 	= N_ELEMENTS(pm_epa_nstorm9_sort)
	pm_med = pm_epa_nstorm9_sort[(Npm_epa_nstorm9_sort/2)] 
	lower_half = pm_epa_nstorm9_sort[0:(Npm_epa_nstorm9_sort/2)-1]
	upper_half = pm_epa_nstorm9_sort[(Npm_epa_nstorm9_sort/2):(Npm_epa_nstorm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = pm_epa_nstorm9_sort[0.05*Npm_epa_nstorm9_sort]
quartile_95 = pm_epa_nstorm9_sort[0.95*Npm_epa_nstorm9_sort]

pm_epa_nstorm9_ptile = [quartile_05, quartile_25, pm_med, quartile_75,quartile_95]


;;;pmn-EPA exceedances with storms: pm
pm_nepa_storm9 = pm_storm_sept[nepa_sept_s]-MEAN(pm_nstorm_sept[nepa_sept_ns],/NAN)
pm_nepa_storm9_sort  = pm_nepa_storm9[SORT(pm_nepa_storm9)]
IF (N_ELEMENTS(pm_nepa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
	Npm_nepa_storm9_sort 	= N_ELEMENTS(pm_nepa_storm9_sort)
	pm_med = (pm_nepa_storm9_sort[(Npm_nepa_storm9_sort/2)-1] + pm_nepa_storm9_sort[(Npm_nepa_storm9_sort/2)]) / 2.0
	lower_half = pm_nepa_storm9_sort[0:(Npm_nepa_storm9_sort/2)-1]
	upper_half = pm_nepa_storm9_sort[(Npm_nepa_storm9_sort/2):(Npm_nepa_storm9_sort-1)]
ENDIF ELSE BEGIN
	Npm_nepa_storm9_sort 	= N_ELEMENTS(pm_nepa_storm9_sort)
	pm_med = pm_nepa_storm9_sort[(Npm_nepa_storm9_sort/2)] 
	lower_half = pm_nepa_storm9_sort[0:(Npm_nepa_storm9_sort/2)-1]
	upper_half = pm_nepa_storm9_sort[(Npm_nepa_storm9_sort/2):(Npm_nepa_storm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = pm_nepa_storm9_sort[0.05*Npm_nepa_storm9_sort]
quartile_95 = pm_nepa_storm9_sort[0.95*Npm_nepa_storm9_sort]

pm_nepa_storm9_ptile = [quartile_05, quartile_25, pm_med, quartile_75,quartile_95]

data = [[pm_epa_storm9_ptile], [pm_epa_nstorm9_ptile], [pm_nepa_storm9_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','pmn-EPA w/ Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'Sept: PM2.5', $
		XRANGE 		= [-5, 15], $
		YRANGE 		= [-1, 3], $
		XTITLE 		= 'PM2.5 concentration (ug/m3)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: temp
temp_epa_storm9 = temp_storm_sept[iepa_sept_s]-MEAN(temp_nstorm_sept[nepa_sept_ns],/NAN)
temp_epa_storm9_sort  = temp_epa_storm9[SORT(temp_epa_storm9)]
IF (N_ELEMENTS(temp_epa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
	Ntemp_epa_storm9_sort 	= N_ELEMENTS(temp_epa_storm9_sort)
	temp_med = (temp_epa_storm9_sort[(Ntemp_epa_storm9_sort/2)-1] + temp_epa_storm9_sort[(Ntemp_epa_storm9_sort/2)]) / 2.0
	lower_half = temp_epa_storm9_sort[0:(Ntemp_epa_storm9_sort/2)-1]
	upper_half = temp_epa_storm9_sort[(Ntemp_epa_storm9_sort/2):(Ntemp_epa_storm9_sort-1)]
ENDIF ELSE BEGIN
	Ntemp_epa_storm9_sort 	= N_ELEMENTS(temp_epa_storm9_sort)
	temp_med = temp_epa_storm9_sort[(Ntemp_epa_storm9_sort/2)] 
	lower_half = temp_epa_storm9_sort[0:(Ntemp_epa_storm9_sort/2)-1]
	upper_half = temp_epa_storm9_sort[(Ntemp_epa_storm9_sort/2):(Ntemp_epa_storm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = temp_epa_storm9_sort[0.05*Ntemp_epa_storm9_sort]
quartile_95 = temp_epa_storm9_sort[0.95*Ntemp_epa_storm9_sort]

temp_epa_storm9_ptile = [quartile_05, quartile_25, temp_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: temp
temp_epa_nstorm9 = temp_nstorm_sept[iepa_sept_ns]-MEAN(temp_nstorm_sept[nepa_sept_ns],/NAN)
temp_epa_nstorm9_sort  = temp_epa_nstorm9[SORT(temp_epa_nstorm9)]
IF (N_ELEMENTS(temp_epa_nstorm9_sort) MOD 2 EQ 0) THEN BEGIN
	Ntemp_epa_nstorm9_sort 	= N_ELEMENTS(temp_epa_nstorm9_sort)
	temp_med = (temp_epa_nstorm9_sort[(Ntemp_epa_nstorm9_sort/2)-1] + temp_epa_nstorm9_sort[(Ntemp_epa_nstorm9_sort/2)]) / 2.0
	lower_half = temp_epa_nstorm9_sort[0:(Ntemp_epa_nstorm9_sort/2)-1]
	upper_half = temp_epa_nstorm9_sort[(Ntemp_epa_nstorm9_sort/2):(Ntemp_epa_nstorm9_sort-1)]
ENDIF ELSE BEGIN
	Ntemp_epa_nstorm9_sort 	= N_ELEMENTS(temp_epa_nstorm9_sort)
	temp_med = temp_epa_nstorm9_sort[(Ntemp_epa_nstorm9_sort/2)] 
	lower_half = temp_epa_nstorm9_sort[0:(Ntemp_epa_nstorm9_sort/2)-1]
	upper_half = temp_epa_nstorm9_sort[(Ntemp_epa_nstorm9_sort/2):(Ntemp_epa_nstorm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = temp_epa_nstorm9_sort[0.05*Ntemp_epa_nstorm9_sort]
quartile_95 = temp_epa_nstorm9_sort[0.95*Ntemp_epa_nstorm9_sort]

temp_epa_nstorm9_ptile = [quartile_05, quartile_25, temp_med, quartile_75,quartile_95]


;;;tempn-EPA exceedances with storms: temp
temp_nepa_storm9 = temp_storm_sept[nepa_sept_s]-MEAN(temp_nstorm_sept[nepa_sept_ns],/NAN)
temp_nepa_storm9_sort  = temp_nepa_storm9[SORT(temp_nepa_storm9)]
IF (N_ELEMENTS(temp_nepa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
	Ntemp_nepa_storm9_sort 	= N_ELEMENTS(temp_nepa_storm9_sort)
	temp_med = (temp_nepa_storm9_sort[(Ntemp_nepa_storm9_sort/2)-1] + temp_nepa_storm9_sort[(Ntemp_nepa_storm9_sort/2)]) / 2.0
	lower_half = temp_nepa_storm9_sort[0:(Ntemp_nepa_storm9_sort/2)-1]
	upper_half = temp_nepa_storm9_sort[(Ntemp_nepa_storm9_sort/2):(Ntemp_nepa_storm9_sort-1)]
ENDIF ELSE BEGIN
	Ntemp_nepa_storm9_sort 	= N_ELEMENTS(temp_nepa_storm9_sort)
	temp_med = temp_nepa_storm9_sort[(Ntemp_nepa_storm9_sort/2)] 
	lower_half = temp_nepa_storm9_sort[0:(Ntemp_nepa_storm9_sort/2)-1]
	upper_half = temp_nepa_storm9_sort[(Ntemp_nepa_storm9_sort/2):(Ntemp_nepa_storm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = temp_nepa_storm9_sort[0.05*Ntemp_nepa_storm9_sort]
quartile_95 = temp_nepa_storm9_sort[0.95*Ntemp_nepa_storm9_sort]

temp_nepa_storm9_ptile = [quartile_05, quartile_25, temp_med, quartile_75,quartile_95]


data = [[temp_epa_storm9_ptile], [temp_epa_nstorm9_ptile], [temp_nepa_storm9_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','non-EPA w/ Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'Sept: Temperature', $
		XRANGE 		= [-20, 35], $
		YRANGE 		= [-1, 3], $
		XTITLE 		= 'Temperature (deg F)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: press
press_epa_storm9 = press_storm_sept[iepa_sept_s]-MEAN(press_nstorm_sept[nepa_sept_ns],/NAN)
press_epa_storm9_sort  = press_epa_storm9[SORT(press_epa_storm9)]
IF (N_ELEMENTS(press_epa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
	Npress_epa_storm9_sort 	= N_ELEMENTS(press_epa_storm9_sort)
	press_med = (press_epa_storm9_sort[(Npress_epa_storm9_sort/2)-1] + press_epa_storm9_sort[(Npress_epa_storm9_sort/2)]) / 2.0
	lower_half = press_epa_storm9_sort[0:(Npress_epa_storm9_sort/2)-1]
	upper_half = press_epa_storm9_sort[(Npress_epa_storm9_sort/2):(Npress_epa_storm9_sort-1)]
ENDIF ELSE BEGIN
	Npress_epa_storm9_sort 	= N_ELEMENTS(press_epa_storm9_sort)
	press_med = press_epa_storm9_sort[(Npress_epa_storm9_sort/2)] 
	lower_half = press_epa_storm9_sort[0:(Npress_epa_storm9_sort/2)-1]
	upper_half = press_epa_storm9_sort[(Npress_epa_storm9_sort/2):(Npress_epa_storm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = press_epa_storm9_sort[0.05*Npress_epa_storm9_sort]
quartile_95 = press_epa_storm9_sort[0.95*Npress_epa_storm9_sort]

press_epa_storm9_ptile = [quartile_05, quartile_25, press_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: press
press_epa_nstorm9 = press_nstorm_sept[iepa_sept_ns]-MEAN(press_nstorm_sept[nepa_sept_ns],/NAN)
press_epa_nstorm9_sort  = press_epa_nstorm9[SORT(press_epa_nstorm9)]
IF (N_ELEMENTS(press_epa_nstorm9_sort) MOD 2 EQ 0) THEN BEGIN
	Npress_epa_nstorm9_sort 	= N_ELEMENTS(press_epa_nstorm9_sort)
	press_med = (press_epa_nstorm9_sort[(Npress_epa_nstorm9_sort/2)-1] + press_epa_nstorm9_sort[(Npress_epa_nstorm9_sort/2)]) / 2.0
	lower_half = press_epa_nstorm9_sort[0:(Npress_epa_nstorm9_sort/2)-1]
	upper_half = press_epa_nstorm9_sort[(Npress_epa_nstorm9_sort/2):(Npress_epa_nstorm9_sort-1)]
ENDIF ELSE BEGIN
	Npress_epa_nstorm9_sort 	= N_ELEMENTS(press_epa_nstorm9_sort)
	press_med = press_epa_nstorm9_sort[(Npress_epa_nstorm9_sort/2)] 
	lower_half = press_epa_nstorm9_sort[0:(Npress_epa_nstorm9_sort/2)-1]
	upper_half = press_epa_nstorm9_sort[(Npress_epa_nstorm9_sort/2):(Npress_epa_nstorm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = press_epa_nstorm9_sort[0.05*Npress_epa_nstorm9_sort]
quartile_95 = press_epa_nstorm9_sort[0.95*Npress_epa_nstorm9_sort]

press_epa_nstorm9_ptile = [quartile_05, quartile_25, press_med, quartile_75,quartile_95]


;;;pressn-EPA exceedances with storms: press
press_nepa_storm9 = press_storm_sept[nepa_sept_s]-MEAN(press_nstorm_sept[nepa_sept_ns],/NAN)
press_nepa_storm9_sort  = press_nepa_storm9[SORT(press_nepa_storm9)]
IF (N_ELEMENTS(press_nepa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
	Npress_nepa_storm9_sort 	= N_ELEMENTS(press_nepa_storm9_sort)
	press_med = (press_nepa_storm9_sort[(Npress_nepa_storm9_sort/2)-1] + press_nepa_storm9_sort[(Npress_nepa_storm9_sort/2)]) / 2.0
	lower_half = press_nepa_storm9_sort[0:(Npress_nepa_storm9_sort/2)-1]
	upper_half = press_nepa_storm9_sort[(Npress_nepa_storm9_sort/2):(Npress_nepa_storm9_sort-1)]
ENDIF ELSE BEGIN
	Npress_nepa_storm9_sort 	= N_ELEMENTS(press_nepa_storm9_sort)
	press_med = press_nepa_storm9_sort[(Npress_nepa_storm9_sort/2)] 
	lower_half = press_nepa_storm9_sort[0:(Npress_nepa_storm9_sort/2)-1]
	upper_half = press_nepa_storm9_sort[(Npress_nepa_storm9_sort/2):(Npress_nepa_storm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = press_nepa_storm9_sort[0.05*Npress_nepa_storm9_sort]
quartile_95 = press_nepa_storm9_sort[0.95*Npress_nepa_storm9_sort]

press_nepa_storm9_ptile = [quartile_05, quartile_25, press_med, quartile_75,quartile_95]


data = [[press_epa_storm9_ptile], [press_epa_nstorm9_ptile], [press_nepa_storm9_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','pressn-EPA w/ Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'Sept: Pressure', $
		XRANGE 		= [-10, 20], $
		YRANGE 		= [-1, 3], $
		XTITLE 		= 'Pressure (hPa)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: wind_spd
wind_spd_epa_storm9 = wind_spd_storm_sept[iepa_sept_s]-MEAN(wind_spd_nstorm_sept[nepa_sept_ns],/NAN)
wind_spd_epa_storm9_sort  = wind_spd_epa_storm9[SORT(wind_spd_epa_storm9)]
IF (N_ELEMENTS(wind_spd_epa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_spd_epa_storm9_sort 	= N_ELEMENTS(wind_spd_epa_storm9_sort)
	wind_spd_med = (wind_spd_epa_storm9_sort[(Nwind_spd_epa_storm9_sort/2)-1] + wind_spd_epa_storm9_sort[(Nwind_spd_epa_storm9_sort/2)]) / 2.0
	lower_half = wind_spd_epa_storm9_sort[0:(Nwind_spd_epa_storm9_sort/2)-1]
	upper_half = wind_spd_epa_storm9_sort[(Nwind_spd_epa_storm9_sort/2):(Nwind_spd_epa_storm9_sort-1)]
ENDIF ELSE BEGIN
	Nwind_spd_epa_storm9_sort 	= N_ELEMENTS(wind_spd_epa_storm9_sort)
	wind_spd_med = wind_spd_epa_storm9_sort[(Nwind_spd_epa_storm9_sort/2)] 
	lower_half = wind_spd_epa_storm9_sort[0:(Nwind_spd_epa_storm9_sort/2)-1]
	upper_half = wind_spd_epa_storm9_sort[(Nwind_spd_epa_storm9_sort/2):(Nwind_spd_epa_storm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_spd_epa_storm9_sort[0.05*Nwind_spd_epa_storm9_sort]
quartile_95 = wind_spd_epa_storm9_sort[0.95*Nwind_spd_epa_storm9_sort]

wind_spd_epa_storm9_ptile = [quartile_05, quartile_25, wind_spd_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: wind_spd
wind_spd_epa_nstorm9 = wind_spd_nstorm_sept[iepa_sept_ns]-MEAN(wind_spd_nstorm_sept[nepa_sept_ns],/NAN)
wind_spd_epa_nstorm9_sort  = wind_spd_epa_nstorm9[SORT(wind_spd_epa_nstorm9)]
IF (N_ELEMENTS(wind_spd_epa_nstorm9_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_spd_epa_nstorm9_sort 	= N_ELEMENTS(wind_spd_epa_nstorm9_sort)
	wind_spd_med = (wind_spd_epa_nstorm9_sort[(Nwind_spd_epa_nstorm9_sort/2)-1] + wind_spd_epa_nstorm9_sort[(Nwind_spd_epa_nstorm9_sort/2)]) / 2.0
	lower_half = wind_spd_epa_nstorm9_sort[0:(Nwind_spd_epa_nstorm9_sort/2)-1]
	upper_half = wind_spd_epa_nstorm9_sort[(Nwind_spd_epa_nstorm9_sort/2):(Nwind_spd_epa_nstorm9_sort-1)]
ENDIF ELSE BEGIN
	Nwind_spd_epa_nstorm9_sort 	= N_ELEMENTS(wind_spd_epa_nstorm9_sort)
	wind_spd_med = wind_spd_epa_nstorm9_sort[(Nwind_spd_epa_nstorm9_sort/2)] 
	lower_half = wind_spd_epa_nstorm9_sort[0:(Nwind_spd_epa_nstorm9_sort/2)-1]
	upper_half = wind_spd_epa_nstorm9_sort[(Nwind_spd_epa_nstorm9_sort/2):(Nwind_spd_epa_nstorm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_spd_epa_nstorm9_sort[0.05*Nwind_spd_epa_nstorm9_sort]
quartile_95 = wind_spd_epa_nstorm9_sort[0.95*Nwind_spd_epa_nstorm9_sort]

wind_spd_epa_nstorm9_ptile = [quartile_05, quartile_25, wind_spd_med, quartile_75,quartile_95]


;;;wind_spdn-EPA exceedances with storms: wind_spd
wind_spd_nepa_storm9 = wind_spd_storm_sept[nepa_sept_s]-MEAN(wind_spd_nstorm_sept[nepa_sept_ns],/NAN)
wind_spd_nepa_storm9_sort  = wind_spd_nepa_storm9[SORT(wind_spd_nepa_storm9)]
IF (N_ELEMENTS(wind_spd_nepa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_spd_nepa_storm9_sort 	= N_ELEMENTS(wind_spd_nepa_storm9_sort)
	wind_spd_med = (wind_spd_nepa_storm9_sort[(Nwind_spd_nepa_storm9_sort/2)-1] + wind_spd_nepa_storm9_sort[(Nwind_spd_nepa_storm9_sort/2)]) / 2.0
	lower_half = wind_spd_nepa_storm9_sort[0:(Nwind_spd_nepa_storm9_sort/2)-1]
	upper_half = wind_spd_nepa_storm9_sort[(Nwind_spd_nepa_storm9_sort/2):(Nwind_spd_nepa_storm9_sort-1)]
ENDIF ELSE BEGIN
	Nwind_spd_nepa_storm9_sort 	= N_ELEMENTS(wind_spd_nepa_storm9_sort)
	wind_spd_med = wind_spd_nepa_storm9_sort[(Nwind_spd_nepa_storm9_sort/2)] 
	lower_half = wind_spd_nepa_storm9_sort[0:(Nwind_spd_nepa_storm9_sort/2)-1]
	upper_half = wind_spd_nepa_storm9_sort[(Nwind_spd_nepa_storm9_sort/2):(Nwind_spd_nepa_storm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_spd_nepa_storm9_sort[0.05*Nwind_spd_nepa_storm9_sort]
quartile_95 = wind_spd_nepa_storm9_sort[0.95*Nwind_spd_nepa_storm9_sort]

wind_spd_nepa_storm9_ptile = [quartile_05, quartile_25, wind_spd_med, quartile_75,quartile_95]


data = [[wind_spd_epa_storm9_ptile], [wind_spd_epa_nstorm9_ptile], [wind_spd_nepa_storm9_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','non-EPA w/ Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'Sept: Wind Speed', $
		XRANGE 		= [-10, 10], $
		YRANGE 		= [-1, 3], $
		XTITLE 		= 'Wind Speed (m/s)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: wind_dir
wind_dir_epa_storm9 = wind_dir_storm_sept[iepa_sept_s]-MEAN(wind_dir_nstorm_sept[nepa_sept_ns],/NAN)
wind_dir_epa_storm9_sort  = wind_dir_epa_storm9[SORT(wind_dir_epa_storm9)]
IF (N_ELEMENTS(wind_dir_epa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_dir_epa_storm9_sort 	= N_ELEMENTS(wind_dir_epa_storm9_sort)
	wind_dir_med = (wind_dir_epa_storm9_sort[(Nwind_dir_epa_storm9_sort/2)-1] + wind_dir_epa_storm9_sort[(Nwind_dir_epa_storm9_sort/2)]) / 2.0
	lower_half = wind_dir_epa_storm9_sort[0:(Nwind_dir_epa_storm9_sort/2)-1]
	upper_half = wind_dir_epa_storm9_sort[(Nwind_dir_epa_storm9_sort/2):(Nwind_dir_epa_storm9_sort-1)]
ENDIF ELSE BEGIN
	Nwind_dir_epa_storm9_sort 	= N_ELEMENTS(wind_dir_epa_storm9_sort)
	wind_dir_med = wind_dir_epa_storm9_sort[(Nwind_dir_epa_storm9_sort/2)] 
	lower_half = wind_dir_epa_storm9_sort[0:(Nwind_dir_epa_storm9_sort/2)-1]
	upper_half = wind_dir_epa_storm9_sort[(Nwind_dir_epa_storm9_sort/2):(Nwind_dir_epa_storm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_dir_epa_storm9_sort[0.05*Nwind_dir_epa_storm9_sort]
quartile_95 = wind_dir_epa_storm9_sort[0.95*Nwind_dir_epa_storm9_sort]

wind_dir_epa_storm9_ptile = [quartile_05, quartile_25, wind_dir_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: wind_dir
wind_dir_epa_nstorm9 = wind_dir_nstorm_sept[iepa_sept_ns]-MEAN(wind_dir_nstorm_sept[nepa_sept_ns],/NAN)
wind_dir_epa_nstorm9_sort  = wind_dir_epa_nstorm9[SORT(wind_dir_epa_nstorm9)]
IF (N_ELEMENTS(wind_dir_epa_nstorm9_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_dir_epa_nstorm9_sort 	= N_ELEMENTS(wind_dir_epa_nstorm9_sort)
	wind_dir_med = (wind_dir_epa_nstorm9_sort[(Nwind_dir_epa_nstorm9_sort/2)-1] + wind_dir_epa_nstorm9_sort[(Nwind_dir_epa_nstorm9_sort/2)]) / 2.0
	lower_half = wind_dir_epa_nstorm9_sort[0:(Nwind_dir_epa_nstorm9_sort/2)-1]
	upper_half = wind_dir_epa_nstorm9_sort[(Nwind_dir_epa_nstorm9_sort/2):(Nwind_dir_epa_nstorm9_sort-1)]
ENDIF ELSE BEGIN
	Nwind_dir_epa_nstorm9_sort 	= N_ELEMENTS(wind_dir_epa_nstorm9_sort)
	wind_dir_med = wind_dir_epa_nstorm9_sort[(Nwind_dir_epa_nstorm9_sort/2)] 
	lower_half = wind_dir_epa_nstorm9_sort[0:(Nwind_dir_epa_nstorm9_sort/2)-1]
	upper_half = wind_dir_epa_nstorm9_sort[(Nwind_dir_epa_nstorm9_sort/2):(Nwind_dir_epa_nstorm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_dir_epa_nstorm9_sort[0.05*Nwind_dir_epa_nstorm9_sort]
quartile_95 = wind_dir_epa_nstorm9_sort[0.95*Nwind_dir_epa_nstorm9_sort]

wind_dir_epa_nstorm9_ptile = [quartile_05, quartile_25, wind_dir_med, quartile_75,quartile_95]


;;;wind_dirn-EPA exceedances with storms: wind_dir
wind_dir_nepa_storm9 = wind_dir_storm_sept[nepa_sept_s]-MEAN(wind_dir_nstorm_sept[nepa_sept_ns],/NAN)
wind_dir_nepa_storm9_sort  = wind_dir_nepa_storm9[SORT(wind_dir_nepa_storm9)]
IF (N_ELEMENTS(wind_dir_nepa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
	Nwind_dir_nepa_storm9_sort 	= N_ELEMENTS(wind_dir_nepa_storm9_sort)
	wind_dir_med = (wind_dir_nepa_storm9_sort[(Nwind_dir_nepa_storm9_sort/2)-1] + wind_dir_nepa_storm9_sort[(Nwind_dir_nepa_storm9_sort/2)]) / 2.0
	lower_half = wind_dir_nepa_storm9_sort[0:(Nwind_dir_nepa_storm9_sort/2)-1]
	upper_half = wind_dir_nepa_storm9_sort[(Nwind_dir_nepa_storm9_sort/2):(Nwind_dir_nepa_storm9_sort-1)]
ENDIF ELSE BEGIN
	Nwind_dir_nepa_storm9_sort 	= N_ELEMENTS(wind_dir_nepa_storm9_sort)
	wind_dir_med = wind_dir_nepa_storm9_sort[(Nwind_dir_nepa_storm9_sort/2)] 
	lower_half = wind_dir_nepa_storm9_sort[0:(Nwind_dir_nepa_storm9_sort/2)-1]
	upper_half = wind_dir_nepa_storm9_sort[(Nwind_dir_nepa_storm9_sort/2):(Nwind_dir_nepa_storm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = wind_dir_nepa_storm9_sort[0.05*Nwind_dir_nepa_storm9_sort]
quartile_95 = wind_dir_nepa_storm9_sort[0.95*Nwind_dir_nepa_storm9_sort]

wind_dir_nepa_storm9_ptile = [quartile_05, quartile_25, wind_dir_med, quartile_75,quartile_95]

data = [[wind_dir_epa_storm9_ptile], [wind_dir_epa_nstorm9_ptile], [wind_dir_nepa_storm9_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','non-EPA w/ Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'Sept: Wind Direction', $
		XRANGE 		= [-360, 360], $
		YRANGE 		= [-1, 3], $
		XTITLE 		= 'Wind Dir. (deg)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EPA exceedances with storms: rh
rh_epa_storm9 = rh_storm_sept[iepa_sept_s]-MEAN(rh_nstorm_sept[nepa_sept_ns],/NAN)
rh_epa_storm9_sort  = rh_epa_storm9[SORT(rh_epa_storm9)]
IF (N_ELEMENTS(rh_epa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
	Nrh_epa_storm9_sort 	= N_ELEMENTS(rh_epa_storm9_sort)
	rh_med = (rh_epa_storm9_sort[(Nrh_epa_storm9_sort/2)-1] + rh_epa_storm9_sort[(Nrh_epa_storm9_sort/2)]) / 2.0
	lower_half = rh_epa_storm9_sort[0:(Nrh_epa_storm9_sort/2)-1]
	upper_half = rh_epa_storm9_sort[(Nrh_epa_storm9_sort/2):(Nrh_epa_storm9_sort-1)]
ENDIF ELSE BEGIN
	Nrh_epa_storm9_sort 	= N_ELEMENTS(rh_epa_storm9_sort)
	rh_med = rh_epa_storm9_sort[(Nrh_epa_storm9_sort/2)] 
	lower_half = rh_epa_storm9_sort[0:(Nrh_epa_storm9_sort/2)-1]
	upper_half = rh_epa_storm9_sort[(Nrh_epa_storm9_sort/2):(Nrh_epa_storm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = rh_epa_storm9_sort[0.05*Nrh_epa_storm9_sort]
quartile_95 = rh_epa_storm9_sort[0.95*Nrh_epa_storm9_sort]

rh_epa_storm9_ptile = [quartile_05, quartile_25, rh_med, quartile_75,quartile_95]


;;;EPA exceedances without storms: rh
rh_epa_nstorm9 = rh_nstorm_sept[iepa_sept_ns]-MEAN(rh_nstorm_sept[nepa_sept_ns],/NAN)
rh_epa_nstorm9_sort  = rh_epa_nstorm9[SORT(rh_epa_nstorm9)]
IF (N_ELEMENTS(rh_epa_nstorm9_sort) MOD 2 EQ 0) THEN BEGIN
	Nrh_epa_nstorm9_sort 	= N_ELEMENTS(rh_epa_nstorm9_sort)
	rh_med = (rh_epa_nstorm9_sort[(Nrh_epa_nstorm9_sort/2)-1] + rh_epa_nstorm9_sort[(Nrh_epa_nstorm9_sort/2)]) / 2.0
	lower_half = rh_epa_nstorm9_sort[0:(Nrh_epa_nstorm9_sort/2)-1]
	upper_half = rh_epa_nstorm9_sort[(Nrh_epa_nstorm9_sort/2):(Nrh_epa_nstorm9_sort-1)]
ENDIF ELSE BEGIN
	Nrh_epa_nstorm9_sort 	= N_ELEMENTS(rh_epa_nstorm9_sort)
	rh_med = rh_epa_nstorm9_sort[(Nrh_epa_nstorm9_sort/2)] 
	lower_half = rh_epa_nstorm9_sort[0:(Nrh_epa_nstorm9_sort/2)-1]
	upper_half = rh_epa_nstorm9_sort[(Nrh_epa_nstorm9_sort/2):(Nrh_epa_nstorm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = rh_epa_nstorm9_sort[0.05*Nrh_epa_nstorm9_sort]
quartile_95 = rh_epa_nstorm9_sort[0.95*Nrh_epa_nstorm9_sort]

rh_epa_nstorm9_ptile = [quartile_05, quartile_25, rh_med, quartile_75,quartile_95]


;;;rhn-EPA exceedances with storms: rh
rh_nepa_storm9 = rh_storm_sept[nepa_sept_s]-MEAN(rh_nstorm_sept[nepa_sept_ns],/NAN)
rh_nepa_storm9_sort  = rh_nepa_storm9[SORT(rh_nepa_storm9)]
IF (N_ELEMENTS(rh_nepa_storm9_sort) MOD 2 EQ 0) THEN BEGIN
	Nrh_nepa_storm9_sort 	= N_ELEMENTS(rh_nepa_storm9_sort)
	rh_med = (rh_nepa_storm9_sort[(Nrh_nepa_storm9_sort/2)-1] + rh_nepa_storm9_sort[(Nrh_nepa_storm9_sort/2)]) / 2.0
	lower_half = rh_nepa_storm9_sort[0:(Nrh_nepa_storm9_sort/2)-1]
	upper_half = rh_nepa_storm9_sort[(Nrh_nepa_storm9_sort/2):(Nrh_nepa_storm9_sort-1)]
ENDIF ELSE BEGIN
	Nrh_nepa_storm9_sort 	= N_ELEMENTS(rh_nepa_storm9_sort)
	rh_med = rh_nepa_storm9_sort[(Nrh_nepa_storm9_sort/2)] 
	lower_half = rh_nepa_storm9_sort[0:(Nrh_nepa_storm9_sort/2)-1]
	upper_half = rh_nepa_storm9_sort[(Nrh_nepa_storm9_sort/2):(Nrh_nepa_storm9_sort-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = rh_nepa_storm9_sort[0.05*Nrh_nepa_storm9_sort]
quartile_95 = rh_nepa_storm9_sort[0.95*Nrh_nepa_storm9_sort]

rh_nepa_storm9_ptile = [quartile_05, quartile_25, rh_med, quartile_75,quartile_95]

data = [[rh_epa_storm9_ptile], [rh_epa_nstorm9_ptile], [rh_nepa_storm9_ptile]]
ytitle = ['EPA w/ Storms', 'EPA w/o Storms','non-EPA w/ Storms']
boxes = BOXPLOT(data, $
		TITLE		= 'Sept: RH', $
		XRANGE 		= [-20, 45], $
		YRANGE 		= [-1, 3], $
		XTITLE 		= 'RH (%)', $
		YTICKNAME 	= ytitle, $
		YTICKVALUES = INDGEN(N_ELEMENTS(ytitle)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

STOP


END
