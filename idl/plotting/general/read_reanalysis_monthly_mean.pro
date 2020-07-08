FUNCTION READ_REANALYSIS_MONTHLY_MEAN, year, month, reanalysis, REDUCED = reduced

;+
; Name:
;		READ_REANALYSIS_TROP_BREAK_ZONAL_MEAN
; Purpose:
;		This is a function to read Eulerian monthly mean files
;		for a chosen reanalysis system. 
; Calling sequence:
;		data = READ_REANALYSIS_TROP_BREAK_ZONAL_MEAN(date, reanalysis)
; Inputs:
;		year       : e.g., 2001, 2002, etc
;		month      : e.g., 1, 2, 3, etc
;		reanalysis : Name of reanalysis system. e.g., 'ERA_INTERIM', 'JRA55', etc
; Output:
;		data : Structure of monthly time means. 
; Keywords:
;		REDUCED    : If set, read reduced resolution based means.
; Author and history:
;		Cameron R. Homeyer  2014-09-09.
;-

COMPILE_OPT IDL2																									;Set Compile Options

IF KEYWORD_SET(reduced) THEN $
	subdir = 'monthly_mean_reduced/' + STRING(year,FORMAT="(I4.4)") + '/' ELSE $				;Set subdirectory of monthly mean file
	subdir = 'monthly_mean/' + STRING(year,FORMAT="(I4.4)") + '/'									;Set subdirectory of monthly mean file

filename = STRING(year,FORMAT="(I4.4)") + STRING(month,FORMAT="(I2.2)") + '.nc'				;Set input file name

;void = EXECUTE('infile = !' + reanalysis + '_DIRECTORY + subdir + filename')					;Set input file path

infile = '/data1/ERA-Interim/' + subdir + filename
PRINT, infile
IF FILE_TEST(infile) THEN BEGIN
	id = NCDF_OPEN(infile)																						;Open file for reading

	NCDF_VARGET, id, 'Longitude',              values													;Read longitude dimension
	NCDF_ATTGET, id, 'Longitude', 'long_name', name
	NCDF_ATTGET, id, 'Longitude', 'units',     units
	
	x = {values : values, $
		  name   : STRING(name), $
		  units  : STRING(units), $
		  n      : N_ELEMENTS(values)}

	NCDF_VARGET, id, 'Latitude',              values													;Read latitude dimension
	NCDF_ATTGET, id, 'Latitude', 'long_name', name
	NCDF_ATTGET, id, 'Latitude', 'units',     units
	
	y = {values : values, $
		  name   : STRING(name), $
		  units  : STRING(units), $
		  n      : N_ELEMENTS(values)}

	NCDF_VARGET, id, 'Altitude',              values													;Read pressure (altitude) dimension								
	NCDF_ATTGET, id, 'Altitude', 'long_name', name
	NCDF_ATTGET, id, 'Altitude', 'units',     units
	
	z = {values : values, $
		  name   : STRING(name), $
		  units  : STRING(units), $
		  n      : N_ELEMENTS(values)}

	NCDF_VARGET, id, 'u',              values																;Read mean zonal wind
	NCDF_ATTGET, id, 'u', 'long_name', name
	NCDF_ATTGET, id, 'u', 'units',     units
	
	u =  {values : values, $
			name   : STRING(name), $
			units  : STRING(units)}

	NCDF_VARGET, id, 'v',              values																;Read mean meridional wind
	NCDF_ATTGET, id, 'v', 'long_name', name
	NCDF_ATTGET, id, 'v', 'units',     units
	
	v =  {values : values, $
			name   : STRING(name), $
			units  : STRING(units)}

	NCDF_VARGET, id, 'w',              values																;Read mean vertical motion
	NCDF_ATTGET, id, 'w', 'long_name', name
	NCDF_ATTGET, id, 'w', 'units',     units
	
	w =  {values : values, $
			name   : STRING(name), $
			units  : STRING(units)}

	NCDF_VARGET, id, 'T',              values																;Read mean temperature
	NCDF_ATTGET, id, 'T', 'long_name', name
	NCDF_ATTGET, id, 'T', 'units',     units
	
	T =  {values : values, $
			name   : STRING(name), $
			units  : STRING(units)}

	NCDF_VARGET, id, 'Z',              values																;Read geopotential height
	NCDF_ATTGET, id, 'Z', 'long_name', name
	NCDF_ATTGET, id, 'Z', 'units',     units
	
	GPH = {values : values, $
			 name   : STRING(name), $
			 units  : STRING(units)}

	NCDF_VARGET, id, 'PV',              values															;Read mean potential vorticity
	NCDF_ATTGET, id, 'PV', 'long_name', name
	NCDF_ATTGET, id, 'PV', 'units',     units
	
	PV = {values : values, $
			name   : STRING(name), $
			units  : STRING(units)}

	NCDF_VARGET, id, 'SH',              values															;Read mean specific humidity
	NCDF_ATTGET, id, 'SH', 'long_name', name
	NCDF_ATTGET, id, 'SH', 'units',     units
	
	SH = {values : values, $
			name   : STRING(name), $
			units  : STRING(units)}

	NCDF_VARGET, id, 'ptrop',              values														;Read mean tropopause pressure
	NCDF_ATTGET, id, 'ptrop', 'long_name', name
	NCDF_ATTGET, id, 'ptrop', 'units',     units
	
	ptrop = {values : values, $
				name   : STRING(name), $
				units  : STRING(units)}

	NCDF_CLOSE, id																									;Close input file
	
	RETURN, {reanal   : reanalysis, $																		;Return data structure
				x        : x,          $
				y        : y,          $
				z        : z,          $
				u        : u,          $
				v        : v,          $
				w        : w,          $
				T        : T,          $
				GPH      : GPH,        $
				PV       : PV,         $
				SH       : SH,         $
				ptrop    : ptrop       }
ENDIF ELSE $
	RETURN, -1																										;Return missing flag if file doesn't exist

END
