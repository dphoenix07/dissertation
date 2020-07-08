PRO REMAKE_WRF_NC, run, state, $
	TRACER  = tracer, $
	DBP_TR  = dbp_tr, $
	DOMAIN  = domain, $
	VERBOSE = verbose

;+
; Name:
;		REMAKE_WRF_NC
; Purpose:
;		This is a procedure for remaking WRF output files by eliminating unused
;		variables and converting perturbation and base state variables into
;		full output variables. Staggered variables are also transferred to the
;		regular common output grid. 
; Calling sequence:
;		REMAKE_WRF_NC, run, state
; Input:
;		run   : Model simulation name. (e.g., 'APR04')
;		state : Model initial state. Typically 'era' or 'gfs'.
; Output:
;		A restructured netCDF file.
; Keywords:
;		TRACER  : If set, read/write passive tracers.
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Cameron R. Homeyer  2012-11-23.
;								  2015-02-15. Updated to make tracers optional
;-

COMPILE_OPT IDL2																										;Set compile options
	
IF (N_ELEMENTS(domain) EQ 0) THEN domain = 'd01'															;Set default domain
	
indir  = !WRF_DIRECTORY + run + '/' + state + '/'															;Set input directory
infile = FILE_SEARCH(indir + 'wrfout_' + domain + '*.nc', COUNT = nfile)							;Set input filepath
outdir = indir + 'reduced/'																						;Set output filepath
	
FILE_MKDIR, outdir																									;Create output directory, if neccessary
	
FOR i = 0, nfile -1 DO BEGIN	
	outfile = STRMID(infile[i], 16, /REVERSE_OFFSET)														;Set output file name
		
	iid = NCDF_OPEN(infile[i])																						;Open input file for reading
	IF (i EQ 0) THEN BEGIN
		NCDF_VARGET, iid, 'T', values																				;Read single variable for output file definition
		NCDF_ATTGET, iid, 'DX', dx, /GLOBAL																		;Read grid resolution
		NCDF_ATTGET, iid, 'DT', dt, /GLOBAL																		;Read grid resolution

		dim = SIZE(values, /DIMENSIONS)																			;Get grid dimension sizes
	ENDIF
	
	CATCH, error_status																								;Catch any errors with netcdf control or file creation

	IF (error_status NE 0) THEN BEGIN
		NCDF_CLOSE, oid																								;Close previous failed file
		oid = NCDF_CREATE(outdir + domain + '_' + outfile, CLOBBER = 1, /NETCDF3_64BIT)								;Create output file for writing
	ENDIF ELSE $
		oid = NCDF_CREATE(outdir + domain + '_' + outfile, CLOBBER = 1, /NETCDF3_64BIT)								;Create output file for writing
	
	xid = NCDF_DIMDEF(oid, 'x', dim[0])																			;Define output file dimensions
	yid = NCDF_DIMDEF(oid, 'y', dim[1])	
	zid = NCDF_DIMDEF(oid, 'z', dim[2])	
	tid = NCDF_DIMDEF(oid, 't', 14    )	
	
	vid = NCDF_VARDEF(oid, 'Time', [tid], /CHAR)																;Define the time variable
	NCDF_ATTPUT, oid, 'Time', 'long_name', 'ISO Date String'												;Name attribute
	NCDF_ATTPUT, oid, 'Time', 'units',     'YYYYMMDD_HHMM_'												;Units attribute
	
	vid = NCDF_VARDEF(oid, 'Longitude', [xid, yid], /FLOAT)												;Define the longitude variable
	NCDF_ATTPUT, oid, 'Longitude', 'long_name', 'Grid Point Longitude'								;Name attribute
	NCDF_ATTPUT, oid, 'Longitude', 'units',     'degrees E'												;Units attribute
	
	vid = NCDF_VARDEF(oid, 'Latitude', [xid, yid], /FLOAT)												;Define the latitude variable
	NCDF_ATTPUT, oid, 'Latitude', 'long_name', 'Grid Point Latitude'									;Name attribute
	NCDF_ATTPUT, oid, 'Latitude', 'units',     'degrees N'												;Units attribute
		
	vid = NCDF_VARDEF(oid, 'P', [xid, yid, zid], /FLOAT)													;Define the pressure variable
	NCDF_ATTPUT, oid, 'P', 'long_name', 'Air Pressure'														;Name attribute
	NCDF_ATTPUT, oid, 'P', 'units',     'hPa'																	;Units attribute

	vid = NCDF_VARDEF(oid, 'P_pert', [xid, yid, zid], /FLOAT)													;Define the pressure variable
	NCDF_ATTPUT, oid, 'P_pert', 'long_name', 'Air Pressure Perturbation'										;Name attribute
	NCDF_ATTPUT, oid, 'P_pert', 'units',     'hPa'																;Units attribute
	
	vid = NCDF_VARDEF(oid, 'T', [xid, yid, zid], /FLOAT)													;Define the temperature variable
	NCDF_ATTPUT, oid, 'T', 'long_name', 'Air Temperature'													;Name attribute
	NCDF_ATTPUT, oid, 'T', 'units',     'K'																	;Units attribute

	vid = NCDF_VARDEF(oid, 'Td', [xid, yid, zid], /FLOAT)													;Define the temperature variable
	NCDF_ATTPUT, oid, 'Td', 'long_name', 'Dew Point Temperature'										;Name attribute
	NCDF_ATTPUT, oid, 'Td', 'units',     'C'																	;Units attribute

	vid = NCDF_VARDEF(oid, 'Z', [xid, yid, zid], /FLOAT)													;Define the geopotential height variable
	NCDF_ATTPUT, oid, 'Z', 'long_name', 'Geopotential Height'											;Name attribute
	NCDF_ATTPUT, oid, 'Z', 'units',     'm'																	;Units attribute
	
	vid = NCDF_VARDEF(oid, 'u', [xid, yid, zid], /FLOAT)													;Define the zonal wind variable
	NCDF_ATTPUT, oid, 'u', 'long_name', 'Zonal Wind'														;Name attribute
	NCDF_ATTPUT, oid, 'u', 'units',     'm s^-1'																;Units attribute
	
	vid = NCDF_VARDEF(oid, 'v', [xid, yid, zid], /FLOAT)													;Define the meridional wind variable
	NCDF_ATTPUT, oid, 'v', 'long_name', 'Meridional Wind'													;Name attribute
	NCDF_ATTPUT, oid, 'v', 'units',     'm s^-1'																;Units attribute
	
	vid = NCDF_VARDEF(oid, 'w', [xid, yid, zid], /FLOAT)													;Define the vertical wind variable
	NCDF_ATTPUT, oid, 'w', 'long_name', 'Vertical Wind'													;Name attribute
	NCDF_ATTPUT, oid, 'w', 'units',     'm s^-1'																;Units attribute
	
	vid = NCDF_VARDEF(oid, 'PV', [xid, yid, zid], /FLOAT)													;Define the radar reflectivity variable
	NCDF_ATTPUT, oid, 'PV', 'long_name', 'Potential Vorticity'											;Name attribute
	NCDF_ATTPUT, oid, 'PV', 'units',     'pvu'																;Units attribute

	vid = NCDF_VARDEF(oid, 'absvor', [xid, yid, zid], /FLOAT)											;Define the radar reflectivity variable
	NCDF_ATTPUT, oid, 'absvor', 'long_name', 'Absolute Vorticity'										;Name attribute
	NCDF_ATTPUT, oid, 'absvor', 'units',     's^-1'															;Units attribute

	vid = NCDF_VARDEF(oid, 'REFL', [xid, yid, zid], /FLOAT)												;Define the radar reflectivity variable
	NCDF_ATTPUT, oid, 'REFL', 'long_name', '10 cm Radar Reflectivity'									;Name attribute
	NCDF_ATTPUT, oid, 'REFL', 'units',     'dBZ'																;Units attribute

   	vid = NCDF_VARDEF(oid, 'CLOUD_NUM_TOTAL', [xid,yid,zid], /FLOAT)										;Define the cloud fraction variable
	NCDF_ATTPUT, oid, 'CLOUD_NUM_TOTAL', 'long_name', 'Total Cloud Particle # Concentration'				;Name attribute
	NCDF_ATTPUT, oid, 'CLOUD_NUM_TOTAL', 'units',     'L^-1'												;Units attribute

    vid = NCDF_VARDEF(oid, 'CLOUD_NUM', [xid,yid,zid], /FLOAT)                                      ;Define the cloud fraction variable
    NCDF_ATTPUT, oid, 'CLOUD_NUM', 'long_name', 'Cloud Particle # Concentration'                       ;Name attribute
    NCDF_ATTPUT, oid, 'CLOUD_NUM', 'units',     'kg kg-1'                                           ;Units attribute

    vid = NCDF_VARDEF(oid, 'RAIN_NUM', [xid,yid,zid], /FLOAT)                                      ;Define the rain fraction variable
    NCDF_ATTPUT, oid, 'RAIN_NUM', 'long_name', 'Rain # Concentration'                     			   ;Name attribute
    NCDF_ATTPUT, oid, 'RAIN_NUM', 'units',     'kg kg-1'                                           ;Units attribute

    vid = NCDF_VARDEF(oid, 'ICE_NUM', [xid,yid,zid], /FLOAT)                                       ;Define the ice fraction variable
    NCDF_ATTPUT, oid, 'ICE_NUM', 'long_name', 'Ice # Concentration'                     			   ;Name attribute
    NCDF_ATTPUT, oid, 'ICE_NUM', 'units',     'kg kg-1'                                            ;Units attribute

    vid = NCDF_VARDEF(oid, 'GRAUPEL_NUM', [xid,yid,zid], /FLOAT)                                   ;Define the graupel fraction variable
    NCDF_ATTPUT, oid, 'GRAUPEL_NUM', 'long_name', 'Graupel # Concentration'                     	   ;Name attribute
    NCDF_ATTPUT, oid, 'GRAUPEL_NUM', 'units',     'kg kg-1'                                        ;Units attribute

    vid = NCDF_VARDEF(oid, 'HAIL_NUM', [xid,yid,zid], /FLOAT)                                      ;Define the hail fraction variable
    NCDF_ATTPUT, oid, 'HAIL_NUM', 'long_name', 'Hail # Concentration'                     			   ;Name attribute
    NCDF_ATTPUT, oid, 'HAIL_NUM', 'units',     'kg kg-1'                                           ;Units attribute

    vid = NCDF_VARDEF(oid, 'SNOW_NUM', [xid,yid,zid], /FLOAT)                                      ;Define the snow fraction variable
    NCDF_ATTPUT, oid, 'SNOW_NUM', 'long_name', 'Snow # Concentration'                     			   ;Name attribute
    NCDF_ATTPUT, oid, 'SNOW_NUM', 'units',     'kg kg-1' 
 
  	vid = NCDF_VARDEF(oid, 'CLOUD_MIX_TOTAL', [xid,yid,zid], /FLOAT)                                      ;Define the cloud mixing ratio variable
    NCDF_ATTPUT, oid, 'CLOUD_MIX_TOTAL', 'long_name', 'Total Cloud Particle Mixing Ratio'                 ;Name attribute
    NCDF_ATTPUT, oid, 'CLOUD_MIX_TOTAL', 'units',     'kg kg-1'                                           ;Units attribute

    vid = NCDF_VARDEF(oid, 'CLOUD_MIX', [xid,yid,zid], /FLOAT)                                      ;Define the cloud mixing ratio variable
    NCDF_ATTPUT, oid, 'CLOUD_MIX', 'long_name', 'Cloud Particle Mixing Ratio'                       ;Name attribute
    NCDF_ATTPUT, oid, 'CLOUD_MIX', 'units',     'kg kg-1'                                           ;Units attribute

    vid = NCDF_VARDEF(oid, 'RAIN_MIX', [xid,yid,zid], /FLOAT)                                      ;Define the rain mixing ratio variable
    NCDF_ATTPUT, oid, 'RAIN_MIX', 'long_name', 'Rain Mixing Ratio'                     			   ;Name attribute
    NCDF_ATTPUT, oid, 'RAIN_MIX', 'units',     'kg kg-1'                                           ;Units attribute

    vid = NCDF_VARDEF(oid, 'ICE_MIX', [xid,yid,zid], /FLOAT)                                       ;Define the ice mixing ratio variable
    NCDF_ATTPUT, oid, 'ICE_MIX', 'long_name', 'Ice Mixing Ratio'                     			   ;Name attribute
    NCDF_ATTPUT, oid, 'ICE_MIX', 'units',     'kg kg-1'                                            ;Units attribute

    vid = NCDF_VARDEF(oid, 'GRAUPEL_MIX', [xid,yid,zid], /FLOAT)                                   ;Define the graupel mixing ratio variable
    NCDF_ATTPUT, oid, 'GRAUPEL_MIX', 'long_name', 'Graupel Mixing Ratio'                     	   ;Name attribute
    NCDF_ATTPUT, oid, 'GRAUPEL_MIX', 'units',     'kg kg-1'                                        ;Units attribute

    vid = NCDF_VARDEF(oid, 'HAIL_MIX', [xid,yid,zid], /FLOAT)                                      ;Define the hail mixing ratio variable
    NCDF_ATTPUT, oid, 'HAIL_MIX', 'long_name', 'Hail Mixing Ratio'                     			   ;Name attribute
    NCDF_ATTPUT, oid, 'HAIL_MIX', 'units',     'kg kg-1'                                           ;Units attribute

    vid = NCDF_VARDEF(oid, 'SNOW_MIX', [xid,yid,zid], /FLOAT)                                      ;Define the snow mixing ratio variable
    NCDF_ATTPUT, oid, 'SNOW_MIX', 'long_name', 'Snow Mixing Ratio'                     			   ;Name attribute
    NCDF_ATTPUT, oid, 'SNOW_MIX', 'units',     'kg kg-1'   

	vid = NCDF_VARDEF(oid, 'H2O', [xid,yid,zid], /FLOAT)													;Define the H2O variable
	NCDF_ATTPUT, oid, 'H2O', 'long_name', 'Water Vapor Mixing Ratio'									;Name attribute
	NCDF_ATTPUT, oid, 'H2O', 'units',     'kg kg^-1'														;Units attribute

	IF KEYWORD_SET(dbp_tr) THEN BEGIN
		vid = NCDF_VARDEF(oid, 'TR_tracer', [xid, yid, zid], /FLOAT)							   		;Define the tropospheric tracer variable
		NCDF_ATTPUT, oid, 'TR_tracer', 'long_name', 'Tropospheric Tracer'						   	 	;Name attribute
		NCDF_ATTPUT, oid, 'TR_tracer', 'units',     'Troposphere = o3 conc., 0 otherwise'				;Units attribute

		vid = NCDF_VARDEF(oid, 'BL_tracer', [xid, yid, zid], /FLOAT)									;Define the tropospheric boundary layer tracer variable
		NCDF_ATTPUT, oid, 'BL_tracer', 'long_name', 'Tropospheric Boundary Layer Tracer'				;Name attribute
		NCDF_ATTPUT, oid, 'BL_tracer', 'units',     'Boundary Layer = o3 conc., 0 otherwise'			;Units attribute

		vid = NCDF_VARDEF(oid, 'UTLS_tracer', [xid, yid, zid], /FLOAT)									;Define the middle troposphere tracer variable
		NCDF_ATTPUT, oid, 'UTLS_tracer', 'long_name', 'UTLS tracer'										;Name attribute
		NCDF_ATTPUT, oid, 'UTLS_tracer', 'units',     'UTLS = o3 conc., 0 otherwise'					;Units attribute

		vid = NCDF_VARDEF(oid, 'ST_tracer', [xid, yid, zid], /FLOAT)									;Define the upper troposphere tracer variable
		NCDF_ATTPUT, oid, 'ST_tracer', 'long_name', 'Stratospheric Tracer'								;Name attribute
		NCDF_ATTPUT, oid, 'ST_tracer', 'units',     'Stratosphere = o3 conc., 0 otherwise'				;Units attribute

		vid = NCDF_VARDEF(oid, 'Updraft_tracer', [xid, yid, zid], /FLOAT)								;Define the upper troposphere tracer variable
		NCDF_ATTPUT, oid, 'Updraft_tracer', 'long_name', 'Updraft Tracer'								;Name attribute
		NCDF_ATTPUT, oid, 'Updraft_tracer', 'units',     'Updraft > 2m/s = 1, 0 otherwise'				;Units attribute

		vid = NCDF_VARDEF(oid, 'Cloud_tracer', [xid, yid, zid], /FLOAT)									;Define the upper troposphere tracer variable
		NCDF_ATTPUT, oid, 'Cloud_tracer', 'long_name', 'Cloud Tracer'									;Name attribute
		NCDF_ATTPUT, oid, 'Cloud_tracer', 'units',     'Cloud at tropopause = 1, 0 otherwise'			;Units attribute

		vid = NCDF_VARDEF(oid, 'O3_tracer', [xid, yid, zid], /FLOAT)									;Define the upper troposphere tracer variable
		NCDF_ATTPUT, oid, 'O3_tracer', 'long_name', 'Ozone Tracer'										;Name attribute
		NCDF_ATTPUT, oid, 'O3_tracer', 'units',     'ppbv'												;Units attribute

		vid = NCDF_VARDEF(oid, 'CO_tracer', [xid, yid, zid], /FLOAT)									;Define the upper troposphere tracer variable
		NCDF_ATTPUT, oid, 'CO_tracer', 'long_name', 'CO Tracer'											;Name attribute
		NCDF_ATTPUT, oid, 'CO_tracer', 'units',     'ppbv'												;Units attribute
	ENDIF

PRINT, 'working 0...'

	IF KEYWORD_SET(tracer) THEN BEGIN
		vid = NCDF_VARDEF(oid, 'TR_tracer', [xid, yid, zid], /FLOAT)									;Define the tropospheric tracer variable
		NCDF_ATTPUT, oid, 'TR_tracer', 'long_name', 'Tropospheric Tracer'								;Name attribute
		NCDF_ATTPUT, oid, 'TR_tracer', 'units',     'Troposphere = 1, 0 otherwise'					;Units attribute

		vid = NCDF_VARDEF(oid, 'BL_tracer', [xid, yid, zid], /FLOAT)									;Define the tropospheric boundary layer tracer variable
		NCDF_ATTPUT, oid, 'BL_tracer', 'long_name', 'Tropospheric Boundary Layer Tracer'			;Name attribute
		NCDF_ATTPUT, oid, 'BL_tracer', 'units',     'Boundary Layer = 1, 0 otherwise'				;Units attribute

		vid = NCDF_VARDEF(oid, 'MT_tracer', [xid, yid, zid], /FLOAT)									;Define the middle troposphere tracer variable
		NCDF_ATTPUT, oid, 'MT_tracer', 'long_name', 'Middle Troposphere Tracer'						;Name attribute
		NCDF_ATTPUT, oid, 'MT_tracer', 'units',     'Troposphere = 1, 0 otherwise'					;Units attribute

		vid = NCDF_VARDEF(oid, 'UT_tracer', [xid, yid, zid], /FLOAT)									;Define the upper troposphere tracer variable
		NCDF_ATTPUT, oid, 'UT_tracer', 'long_name', 'Upper Troposphere Tracer'						;Name attribute
		NCDF_ATTPUT, oid, 'UT_tracer', 'units',     'Troposphere = 1, 0 otherwise'					;Units attribute

		vid = NCDF_VARDEF(oid, 'ST_tracer', [xid, yid, zid], /FLOAT)									;Define the Stratospheric tracer variable
		NCDF_ATTPUT, oid, 'ST_tracer', 'long_name', 'Stratospheric Tracer'							;Name attribute
		NCDF_ATTPUT, oid, 'ST_tracer', 'units',     'Stratosphere = 1, 0 otherwise'				;Units attribute

		vid = NCDF_VARDEF(oid, 'LS_tracer', [xid, yid, zid], /FLOAT)									;Define the lower stratosphere tracer variable
		NCDF_ATTPUT, oid, 'LS_tracer', 'long_name', 'Lower Stratosphere Tracer'						;Name attribute
		NCDF_ATTPUT, oid, 'LS_tracer', 'units',     'Stratosphere = 1, 0 otherwise'				;Units attribute

		vid = NCDF_VARDEF(oid, 'US_tracer', [xid, yid, zid], /FLOAT)									;Define the overworld stratosphere tracer variable
		NCDF_ATTPUT, oid, 'US_tracer', 'long_name', 'Upper Stratosphere (Overworld) Tracer'		;Name attribute
		NCDF_ATTPUT, oid, 'US_tracer', 'units',     'Stratosphere = 1, 0 otherwise'				;Units attribute
	ENDIF
		
	vid = NCDF_VARDEF(oid, 'PBL', [xid, yid], /FLOAT)														;Define the PBL height variable
	NCDF_ATTPUT, oid, 'PBL', 'long_name', 'PBL Height'														;Name attribute
	NCDF_ATTPUT, oid, 'PBL', 'units',     'm'																	;Units attribute
	
	vid = NCDF_VARDEF(oid, 'Z0', [xid, yid], /FLOAT)														;Define the terrain height variable
	NCDF_ATTPUT, oid, 'Z0', 'long_name', 'Terrain Height'													;Name attribute
	NCDF_ATTPUT, oid, 'Z0', 'units',     'm'																	;Units attribute
	
	vid = NCDF_VARDEF(oid, 'T_sfc', [xid, yid], /FLOAT)													;Define the surface temperature variable
	NCDF_ATTPUT, oid, 'T_sfc', 'long_name', 'Air Temperature at 2 M'									;Name attribute
	NCDF_ATTPUT, oid, 'T_sfc', 'units',     'K'																;Units attribute
	
	vid = NCDF_VARDEF(oid, 'p_sfc', [xid, yid], /FLOAT)													;Define the surface pressure variable
	NCDF_ATTPUT, oid, 'p_sfc', 'long_name', 'Surface Pressure'											;Name attribute
	NCDF_ATTPUT, oid, 'p_sfc', 'units',     'hPa'															;Units attribute
	
	vid = NCDF_VARDEF(oid, 'u_sfc', [xid, yid], /FLOAT)													;Define the surface zonal wind variable
	NCDF_ATTPUT, oid, 'u_sfc', 'long_name', 'Zonal Wind at 10 M'										;Name attribute
	NCDF_ATTPUT, oid, 'u_sfc', 'units',     'm s^-1'														;Units attribute
	
	vid = NCDF_VARDEF(oid, 'v_sfc', [xid, yid], /FLOAT)													;Define the surface meridional wind variable
	NCDF_ATTPUT, oid, 'v_sfc', 'long_name', 'Meridional Wind at 10 M'									;Name attribute
	NCDF_ATTPUT, oid, 'v_sfc', 'units',     'm s^-1'														;Units attribute

	vid = NCDF_VARDEF(oid, 'Z_trop', [xid, yid], /FLOAT)													;Define the tropopause height variable
	NCDF_ATTPUT, oid, 'Z_trop', 'long_name', 'WMO Primary Tropopause Geopotential Height'		;Name attribute
	NCDF_ATTPUT, oid, 'Z_trop', 'units',     'm'																;Units attribute

	vid = NCDF_VARDEF(oid, 'Z_trop2', [xid, yid], /FLOAT)													;Define the tropopause height variable
	NCDF_ATTPUT, oid, 'Z_trop2', 'long_name', 'WMO Secondary Tropopause Geopotential Height'	;Name attribute
	NCDF_ATTPUT, oid, 'Z_trop2', 'units',     'm'															;Units attribute

	vid = NCDF_VARDEF(oid, 'PRECIP', [xid, yid], /FLOAT)													;Define the precip variable
	NCDF_ATTPUT, oid, 'PRECIP', 'long_name', 'Accumulated Total Grid-scale Precipitation'		;Name attribute
	NCDF_ATTPUT, oid, 'PRECIP', 'units',     'mm'															;Units attribute

PRINT, 'working 1...'
	
	NCDF_ATTPUT, oid, 'DX', dx, /GLOBAL
	NCDF_ATTPUT, oid, 'DT', dt, /GLOBAL

PRINT, 'working 1.5...'

	NCDF_CONTROL, oid, /ENDEF

PRINT, 'working 1.75...'

	NCDF_VARPUT, oid, 'Time', STRMID(outfile, 0, 14)														;Write date string to file
	
	NCDF_VARGET, iid, 'XLONG',     values																		;Read longitude values
	NCDF_VARPUT, oid, 'Longitude', values																		;Write longitude

	NCDF_VARGET, iid, 'XLAT',     y																				;Read latitude values
	NCDF_VARPUT, oid, 'Latitude', y																				;Write latitude
	
	NCDF_VARGET, iid, 'PB', p																						;Read pressure variables
	NCDF_VARGET, iid, 'P',  values	
	p = 0.01*(p + values)																							;Convert pressure to hPa
	NCDF_VARPUT, oid, 'P', p	

	NCDF_VARGET, iid, 'P', values	
	values = 0.01*values																					;Read pressure variables
	NCDF_VARPUT, oid, 'P_pert', values	
	
	NCDF_VARGET, iid, 'QVAPOR', values																			;Read the water vapor mixing ratio
	NCDF_VARPUT, oid, 'H2O', values																				;Write the water vapor mixing ratio
	
	evap = 1000.0*values*P/(1000.0*values+0.622)																;Compute the vapor pressure (in hPa)
	NCDF_VARPUT, oid, 'Td',	5417.0/(19.83 - ALOG(evap/6.11)) + !KtoC									;Compute dew point temperature via C-C equation and write to file

	NCDF_VARGET, iid, 'PHB', z																						;Read geopotential variables
	NCDF_VARGET, iid, 'PH',  values	
	z = (z + values)/!g																								;Convert geopotential to height (km)
	z = (0.5*(SHIFT(z, 0, 0, -1) + z))[*,*,0:(dim[2]-1)]													;Keep geopotential height at half-mass levels
	NCDF_VARPUT, oid, 'Z', z	
	
	NCDF_VARGET, iid, 'U', values																					;Read zonal wind
	u = (0.5*(SHIFT(values, -1, 0, 0) + values))[0:(dim[0]-1),*,*]										;Keep zonal wind at half-x levels
	NCDF_VARPUT, oid, 'u', u																						;Write zonal wind
		
	NCDF_VARGET, iid, 'V', values																					;Read meridional wind
	v = (0.5*(SHIFT(values, 0, -1, 0) + values))[*,0:(dim[1]-1),*]										;Keep meridional wind at half-y levels
	NCDF_VARPUT, oid, 'v', v																						;Write meridional wind
		
	NCDF_VARGET, iid, 'T', T																						;Read temperature array

PRINT, 'working 2...'

	absvor = WRF_VORTICITY(u, v, y, dx, /ABSOLUTE)															;Compute absolute vorticity
	values = -!g*(absvor*WRF_STABILITY(T, p) - (SHIFT(v,  0,  0, -1) - SHIFT(v, 0, 0, 1))/$	;Compute full potential vorticity (PV)
				(SHIFT(p,  0,  0, -1) - SHIFT(p, 0, 0, 1))*(SHIFT(T, -1,  0,  0) - $
				SHIFT(T, 1, 0, 0))/(2.0*dx) + (SHIFT(u,  0,  0, -1) - SHIFT(u, 0, 0, 1))/$
				(SHIFT(p,  0,  0, -1) - SHIFT(p, 0, 0, 1))*(SHIFT(T,  0, -1,  0) - $
				SHIFT(T, 0, 1, 0))/(2.0*dx))

	NCDF_VARPUT, oid, 'PV', 1.0E4*values																		;Write PV to file
	NCDF_VARPUT, oid, 'absvor', absvor																			;Write absolute vorticity to file

	T = (T + 300.0)*((p/1000.0)^(!Rair/!Cp))																	;Convert potential temp to T (K)
	NCDF_VARPUT, oid, 'T', T																						;Write temperature array

	NCDF_VARGET, iid, 'W', values																					;Read vertical wind
	values = (0.5*(SHIFT(values, 0, 0, -1) + values))[*,*,0:(dim[2]-1)]								;Keep vertical wind at half-mass levels
	NCDF_VARPUT, oid, 'w', values																					;Write vertical wind
	
	NCDF_VARGET, iid, 'REFL_10CM', values																		;Read reflectivity
	NCDF_VARPUT, oid, 'REFL',      values																		;Write reflectivity

	NCDF_VARGET, iid, 'QNCLOUD',   cloud														;Read cloud concentration;
	NCDF_VARPUT, oid, 'CLOUD_NUM', cloud

	NCDF_VARGET, iid, 'QNRAIN',    qnrain														;Read rain conc.
	NCDF_VARPUT, oid, 'RAIN_NUM',  qnrain																			

	NCDF_VARGET, iid, 'QNICE',     qnice														;Read ice conc.
	NCDF_VARPUT, oid, 'ICE_NUM',   qnice
																				
	NCDF_VARGET, iid, 'QNSNOW',    qnsnow														;Read snow conc.
	NCDF_VARPUT, oid, 'SNOW_NUM',  qnsnow
																				
	NCDF_VARGET, iid, 'QNGRAUPEL',   qngraupel														;Read graupel conc.
	NCDF_VARPUT, oid, 'GRAUPEL_NUM', qngraupel
	 	 	 																	
	NCDF_VARGET, iid, 'QNHAIL',    qnhail														;Read hail conc.
	NCDF_VARPUT, oid, 'HAIL_NUM',  qnhail
	
	cloud = cloud + qnrain + qnice + qnsnow + qngraupel + qnhail								;Add conc. to cloud variable
	NCDF_VARPUT, oid, 'CLOUD_NUM_TOTAL',  cloud													;Write cloud conc.
                                                   
 	NCDF_VARGET, iid, 'QCLOUD',    cloud_mix                                                     ;Read cloud mixing ratio
    NCDF_VARPUT, oid, 'CLOUD_MIX', cloud_mix
    
    NCDF_VARGET, iid, 'QRAIN',    qrain
    NCDF_VARPUT, oid, 'RAIN_MIX', qrain                                                         ;Read rain mixing ratio
    
    NCDF_VARGET, iid, 'QICE',     qice
     NCDF_VARPUT, oid, 'ICE_MIX',  qice                                                         	;Read ice mixing ratio
    
    NCDF_VARGET, iid, 'QSNOW',    qsnow
    NCDF_VARPUT, oid, 'SNOW_MIX', qsnow
                                                                      							;Read snow mixing ratio
    NCDF_VARGET, iid, 'QGRAUP',      qgraup
    NCDF_VARPUT, oid, 'GRAUPEL_MIX', qgraup                                                     ;Read graupel mixing ratio

    NCDF_VARGET, iid, 'QHAIL',      qhail
    NCDF_VARPUT, oid, 'HAIL_MIX',   qhail                                                    ;Read graupel mixing ratio

	cloud_mix = cloud_mix + qrain + qice + qsnow + qgraup + qhail
	NCDF_VARPUT, oid, 'CLOUD_MIX_TOTAL',  cloud_mix  

	IF KEYWORD_SET(dbp_tr) THEN BEGIN
		NCDF_VARGET, iid, 'tr17_1',         values																	;Read tracer
		NCDF_VARPUT, oid, 'TR_tracer',      values																	;Write tracer
	
		NCDF_VARGET, iid, 'tr17_2',         values																	;Read tracer
		NCDF_VARPUT, oid, 'BL_tracer',      values																	;Write tracer
	
		NCDF_VARGET, iid, 'tr17_3',         values																	;Read tracer
		NCDF_VARPUT, oid, 'UTLS_tracer',    values																	;Write tracer
	
		NCDF_VARGET, iid, 'tr17_4',         values																	;Read tracer
		NCDF_VARPUT, oid, 'ST_tracer',      values																	;Write tracer

		NCDF_VARGET, iid, 'tr17_5',		    values
		NCDF_VARPUT, oid, 'Cloud_tracer',   values

		NCDF_VARGET, iid, 'tr17_6',		    values
		NCDF_VARPUT, oid, 'Updraft_tracer', values

		NCDF_VARGET, iid, 'tr17_7',		    values
		NCDF_VARPUT, oid, 'O3_tracer',      values

		NCDF_VARGET, iid, 'tr17_8',		    values
		NCDF_VARPUT, oid, 'CO_tracer',      values
	ENDIF

	IF KEYWORD_SET(tracer) THEN BEGIN
		NCDF_VARGET, iid, 'tr17_1',    values																	;Read tracer
		NCDF_VARPUT, oid, 'TR_tracer', values																	;Write tracer
	
		NCDF_VARGET, iid, 'tr17_2',    values																	;Read tracer
		NCDF_VARPUT, oid, 'BL_tracer', values																	;Write tracer
	
		NCDF_VARGET, iid, 'tr17_3',    values																	;Read tracer
		NCDF_VARPUT, oid, 'MT_tracer', values																	;Write tracer
	
		NCDF_VARGET, iid, 'tr17_4',    values																	;Read tracer
		NCDF_VARPUT, oid, 'UT_tracer', values																	;Write tracer
	
		NCDF_VARGET, iid, 'tr17_5',    values																	;Read tracer
		NCDF_VARPUT, oid, 'ST_tracer', values																	;Write tracer
	
		NCDF_VARGET, iid, 'tr17_6',    values																	;Read tracer
		NCDF_VARPUT, oid, 'LS_tracer', values																	;Write tracer
	
		NCDF_VARGET, iid, 'tr17_7',    values																	;Read tracer
		NCDF_VARPUT, oid, 'US_tracer', values																	;Write tracer
	ENDIF
	
	NCDF_VARGET, iid, 'PBLH', values																				;Read PBL height
	NCDF_VARPUT, oid, 'PBL',  values																				;Write PBL height to output file
	
	NCDF_VARGET, iid, 'HGT', values																				;Read terrain height
	NCDF_VARPUT, oid, 'Z0',  values																				;Write terrain height to output file
	
	NCDF_VARGET, iid, 'T2',    values																			;Read surface temperature
	NCDF_VARPUT, oid, 'T_sfc', values																			;Write surface temperature
	
	NCDF_VARGET, iid, 'PSFC',  values																			;Read surface pressure
	NCDF_VARPUT, oid, 'p_sfc', 0.01*values																		;Write surface pressure (in hPa)
		
	NCDF_VARGET, iid, 'U10',   values																			;Read surface zonal wind
	NCDF_VARPUT, oid, 'u_sfc', values																			;Write surface zonal wind
		
	NCDF_VARGET, iid, 'V10',   values																			;Read surface meridional wind
	NCDF_VARPUT, oid, 'v_sfc', values																			;Write surface meridional wind

	trop = MAKE_ARRAY([dim[0],dim[1],2], VALUE = !Values.F_NaN)											;Create array to store tropopause altitudes
	FOR ii = 0, dim[0] -1 DO $
		FOR jj = 0, dim[1] -1 DO $
			trop[ii,jj,*] = TROPOPAUSE(T[ii,jj,*], 0.001*z[ii,jj,*], p[ii,jj,*], NTROPS = 2)		;Compute tropopause altitudes

	NCDF_VARPUT, oid, 'Z_trop',  REFORM(1000.0*trop[*,*,0], dim[0], dim[1])							;Output tropopause altitudes
	NCDF_VARPUT, oid, 'Z_trop2', REFORM(1000.0*trop[*,*,1], dim[0], dim[1])
		

	NCDF_VARGET, iid, 'RAINNC', values																			;Read total accumulated precip
	NCDF_VARPUT, oid, 'PRECIP', values																			;Write precip
		
	NCDF_CLOSE, oid																									;Close output file
	NCDF_CLOSE, iid																									;Close input file
	
	PRINT, 'File ' + infile[i] + ' processed.' 
	
	IF KEYWORD_SET(verbose) THEN PRINT, 'File ' + infile[i] + ' processed.'							;Print verbose message
ENDFOR

END
