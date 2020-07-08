PRO MAKE_AP_FILES, run, scheme, $
	TRACER  = tracer, $
	DOMAIN  = domain, $
	VERBOSE = verbose

;+
; Name:
;		MAKE_AP_FILES
; Purpose:
;		This is a procedure for remaking WRF output files by eliminating unused
;		variables and converting perturbation and base state variables into
;		full output variables. Staggered variables are also transferred to the
;		regular common output grid. 
; Calling sequence:
;		MAKE_AP_FILES, run, scheme
; Input:
;		run   : Model simulation name. (e.g., '20120519')
;		scheme : Model initial state. Typically 'morrison' or 'nssl'.
; Output:
;		A restructured netCDF file.
; Keywords:
;		TRACER  : If set, read/write passive tracers.
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel Phoenix	    2016-04-10.
;-

COMPILE_OPT IDL2																										;Set compile options
	
IF (N_ELEMENTS(domain )  EQ 0) THEN domain = 'd02'															;Set default domain
IF (N_ELEMENTS(verbose)  EQ 0) THEN verbose = 1
	
indir  = !WRF_DIRECTORY + run + '/' + scheme + '/'															;Set input directory
infile = FILE_SEARCH(indir + 'wrfout_' + domain + '*.nc', COUNT = nfile)							;Set input filepath
outdir = indir + 'files_for_eliz/'																			;Set output filepath

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
		oid = NCDF_CREATE(outdir + domain + '_' + outfile, CLOBBER = 1)								;Create output file for writing
	ENDIF ELSE $
		oid = NCDF_CREATE(outdir + domain + '_' + outfile, CLOBBER = 1)								;Create output file for writing
	
	xid = NCDF_DIMDEF(oid, 'x', 11)																			;Define output file dimensions
	yid = NCDF_DIMDEF(oid, 'y', 11)	
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
	
	vid = NCDF_VARDEF(oid, 'T', [xid, yid, zid], /FLOAT)													;Define the temperature variable
	NCDF_ATTPUT, oid, 'T', 'long_name', 'Air Temperature'													;Name attribute
	NCDF_ATTPUT, oid, 'T', 'units',     'K'																	;Units attribute

	vid = NCDF_VARDEF(oid, 'Td', [xid, yid, zid], /FLOAT)													;Define the temperature variable
	NCDF_ATTPUT, oid, 'Td', 'long_name', 'Dew Point Temperature'										;Name attribute
	NCDF_ATTPUT, oid, 'Td', 'units',     'C'																	;Units attribute

	vid = NCDF_VARDEF(oid, 'PH', [xid, yid, zid], /FLOAT)													;Define the ph variable
	NCDF_ATTPUT, oid, 'PH', 'long_name', 'Perturbation Geopotential'										;Name attribute
	NCDF_ATTPUT, oid, 'PH', 'units',     'm2 s-2'																	;Units attribute

	vid = NCDF_VARDEF(oid, 'PHB', [xid, yid, zid], /FLOAT)													;Define the phb variable
	NCDF_ATTPUT, oid, 'PHB', 'long_name', 'Base-state Geopotential'										;Name attribute
	NCDF_ATTPUT, oid, 'PHB', 'units',     'm2 s-2'																	;Units attribute

;	vid = NCDF_VARDEF(oid, 'Z', [xid, yid, zid], /FLOAT)													;Define the geopotential height variable
;	NCDF_ATTPUT, oid, 'Z', 'long_name', 'Geopotential Height'											;Name attribute
;	NCDF_ATTPUT, oid, 'Z', 'units',     'm'																	;Units attribute
	
	vid = NCDF_VARDEF(oid, 'u', [xid, yid, zid], /FLOAT)													;Define the zonal wind variable
	NCDF_ATTPUT, oid, 'u', 'long_name', 'Zonal Wind'														;Name attribute
	NCDF_ATTPUT, oid, 'u', 'units',     'm s^-1'																;Units attribute
	
	vid = NCDF_VARDEF(oid, 'v', [xid, yid, zid], /FLOAT)													;Define the meridional wind variable
	NCDF_ATTPUT, oid, 'v', 'long_name', 'Meridional Wind'													;Name attribute
	NCDF_ATTPUT, oid, 'v', 'units',     'm s^-1'																;Units attribute
	
	vid = NCDF_VARDEF(oid, 'w', [xid, yid, zid], /FLOAT)													;Define the vertical wind variable
	NCDF_ATTPUT, oid, 'w', 'long_name', 'Vertical Wind'													;Name attribute
	NCDF_ATTPUT, oid, 'w', 'units',     'm s^-1'																;Units attribute
	
	vid = NCDF_VARDEF(oid, 'REFL', [xid, yid, zid], /FLOAT)												;Define the radar reflectivity variable
	NCDF_ATTPUT, oid, 'REFL', 'long_name', '10 cm Radar Reflectivity'									;Name attribute
	NCDF_ATTPUT, oid, 'REFL', 'units',     'dBZ'																;Units attribute

	vid = NCDF_VARDEF(oid, 'H2O', [xid,yid,zid], /FLOAT)											    ;Define the H2O variable
	NCDF_ATTPUT, oid, 'H2O', 'long_name', 'Water Vapor Mixing Ratio'									;Name attribute
	NCDF_ATTPUT, oid, 'H2O', 'units',     'kg kg^-1'													;Units attribute

    vid = NCDF_VARDEF(oid, 'NO2', [xid, yid, zid], /FLOAT)                                         ;Define the NO2 attribute
    NCDF_ATTPUT, oid, 'NO2', 'long_name', 'NO2 mixing ratio'                                       ;Name attribute
    NCDF_ATTPUT, oid, 'NO2', 'units', 'ppmv'                                                       ;Units attribute

    vid = NCDF_VARDEF(oid, 'NO', [xid, yid, zid], /FLOAT)                                          ;Define the NO attribute
    NCDF_ATTPUT, oid, 'NO', 'long_name', 'NO mixing ratio'                                         ;Name attribute
    NCDF_ATTPUT, oid, 'NO', 'units', 'ppmv'                                                        ;Units attribute

    vid = NCDF_VARDEF(oid, 'O3', [xid, yid, zid], /FLOAT)                                          ;Define the O3 attribute
    NCDF_ATTPUT, oid, 'O3', 'long_name', 'O3 mixing ratio'                                         ;Name attribute
    NCDF_ATTPUT, oid, 'O3', 'units', 'ppmv'                                                        ;Units attribute

	vid = NCDF_VARDEF(oid, 'PBL', [xid, yid], /FLOAT)														;Define the PBL height variable
	NCDF_ATTPUT, oid, 'PBL', 'long_name', 'PBL Height'														;Name attribute
	NCDF_ATTPUT, oid, 'PBL', 'units',     'm'																	;Units attribute
	
	vid = NCDF_VARDEF(oid, 'HGT', [xid, yid], /FLOAT)														;Define the terrain height variable
	NCDF_ATTPUT, oid, 'HGT', 'long_name', 'Terrain Height'													;Name attribute
	NCDF_ATTPUT, oid, 'HGT', 'units',     'm'																	;Units attribute
	
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

	NCDF_ATTPUT, oid, 'DX', dx, /GLOBAL
	NCDF_ATTPUT, oid, 'DT', dt, /GLOBAL

	NCDF_CONTROL, oid, /ENDEF

	NCDF_VARPUT, oid, 'Time', STRMID(outfile, 0, 14)														;Write date string to file
	
	NCDF_VARGET, iid, 'XLONG',     values														;Read longitude values
	values = values[74:84,129:139,*]	
	NCDF_VARPUT, oid, 'Longitude', values															;Write longitude
PRINT, 'working...'
	NCDF_VARGET, iid, 'XLAT',     y																;Read latitude values
	y = y[74:84,129:139,*]
	NCDF_VARPUT, oid, 'Latitude', y																				;Write latitude

	NCDF_VARGET, iid, 'PB', p																						;Read pressure variables
	NCDF_VARGET, iid, 'P',  values	
	p = 0.01*(p + values)																							;Convert pressure to hPa
	p = p[74:84,129:139,*]
	NCDF_VARPUT, oid, 'P', p

	NCDF_VARGET, iid, 'QVAPOR', values																			;Read the water vapor mixing ratio
	values = values[74:84,129:139,*]	
	NCDF_VARPUT, oid, 'H2O', values																				;Write the water vapor mixing ratio
	
	evap = 1000.0*values*P/(1000.0*values+0.622)																;Compute the vapor pressure (in hPa)
	NCDF_VARPUT, oid, 'Td',	5417.0/(19.83 - ALOG(evap/6.11)) + !KtoC									;Compute dew point temperature via C-C equation and write to file

	NCDF_VARGET, iid, 'PH', values																			
	ph = (0.5*(SHIFT(values, 0, 0, -1) + values))[*,*,0:(dim[2]-1)]
	ph = ph[74:84,129:139,*]	
	NCDF_VARPUT, oid, 'PH', ph																			

	NCDF_VARGET, iid, 'PHB', values																			
	phb = (0.5*(SHIFT(values, 0, 0, -1) + values))[*,*,0:(dim[2]-1)]
	phb = phb[74:84,129:139,*]	
	NCDF_VARPUT, oid, 'PHB', phb																			

;	NCDF_VARGET, iid, 'PHB', z																						;Read geopotential variables
;	NCDF_VARGET, iid, 'PH',  values	
;	z = (z + values)/!g																								;Convert geopotential to height (km)
;	z = (0.5*(SHIFT(z, 0, 0, -1) + z))[*,*,0:(dim[2]-1)]													;Keep geopotential height at half-mass levels
;	z = z[74:84,129:139,*]	
;	NCDF_VARPUT, oid, 'Z', z	
	
	NCDF_VARGET, iid, 'U', values																					;Read zonal wind
	u = (0.5*(SHIFT(values, -1, 0, 0) + values))[0:(dim[0]-1),*,*]										;Keep zonal wind at half-x levels
	u = u[74:84,129:139,*]	
	NCDF_VARPUT, oid, 'u', u																						;Write zonal wind
		
	NCDF_VARGET, iid, 'V', values																					;Read meridional wind
	v = (0.5*(SHIFT(values, 0, -1, 0) + values))[*,0:(dim[1]-1),*]										;Keep meridional wind at half-y levels
	v = v[74:84,129:139,*]	
	NCDF_VARPUT, oid, 'v', v																		;Write meridional wind
		
	NCDF_VARGET, iid, 'T', T																		;Read temperature array
	T = T[74:84,129:139,*]	
	T = (T + 300.0)*((p/1000.0)^(!Rair/!Cp))																	;Convert potential temp to T (K)
	NCDF_VARPUT, oid, 'T', T																						;Write temperature array

	NCDF_VARGET, iid, 'W', values																					;Read vertical wind
	values = (0.5*(SHIFT(values, 0, 0, -1) + values))[*,*,0:(dim[2]-1)]								;Keep vertical wind at half-mass levels
	values = values[74:84,129:139,*]	
	NCDF_VARPUT, oid, 'w', values																					;Write vertical wind
	
	NCDF_VARGET, iid, 'REFL_10CM', values																			;Read reflectivity
	values = values[74:84,129:139,*]
	NCDF_VARPUT, oid, 'REFL',      values																	;Write reflectivity

	NCDF_VARGET, iid, 'no2', values																					;Read NO2
	values = values[74:84,129:139,*]
	NCDF_VARPUT, oid, 'NO2', values																				;Write NO2

	NCDF_VARGET, iid, 'no', values																				;Read NO
	values = values[74:84,129:139,*]
	NCDF_VARPUT, oid, 'NO', values																				;Write NO

	NCDF_VARGET, iid, 'o3', values																				;Read O3
	values = values[74:84,129:139,*]
	NCDF_VARPUT, oid, 'O3', values																				;Write O3

	NCDF_VARGET, iid, 'PBLH', values																				;Read PBL height
	values = values[74:84,129:139,*]	
	NCDF_VARPUT, oid, 'PBL',  values																				;Write PBL height to output file
	
	NCDF_VARGET, iid, 'HGT', values																					;Read terrain height
	values = values[74:84,129:139,*]
	NCDF_VARPUT, oid, 'HGT',  values																				;Write terrain height to output file
	
	NCDF_VARGET, iid, 'T2',    values																				;Read surface temperature
	values = values[74:84,129:139,*]
	NCDF_VARPUT, oid, 'T_sfc', values																			;Write surface temperature
	
	NCDF_VARGET, iid, 'PSFC',  values																				;Read surface pressure
	values = values[74:84,129:139,*]
	NCDF_VARPUT, oid, 'p_sfc', 0.01*values																		;Write surface pressure (in hPa)
		
	NCDF_VARGET, iid, 'U10',   values																				;Read surface zonal wind
	values = values[74:84,129:139,*]
	NCDF_VARPUT, oid, 'u_sfc', values																			;Write surface zonal wind
		
	NCDF_VARGET, iid, 'V10',   values																			;Read surface meridional wind
	values = values[74:84,129:139,*]
	NCDF_VARPUT, oid, 'v_sfc', values																			;Write surface meridional wind

	NCDF_CLOSE, oid																									;Close output file
	NCDF_CLOSE, iid																									;Close input file
	
	IF KEYWORD_SET(verbose) THEN PRINT, 'File ' + infile[i] + ' processed.'							;Print verbose message
ENDFOR

END