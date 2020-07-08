FUNCTION ERA_INTERIM_FILEPATH, date, type, DIRECTORY = directory

;+
; Name:
;		ERA_INTERIM_FILEPATH
; Purpose:
;		This is a function to return ERA-Interim file paths. 
; Calling sequence:
;		value = ERA_INTERIM_FILEPATH(date)
; Inputs:
;		date : File analysis date. {CDATE}
;		type : Grib file type. If given, grib filepath is returned. 
;				 Else, netCDF filepath is returned.
; Output:
;		A string of the complete filepath.
; Keywords:
;		DIRECTORY : Set to a named variable to return file directory.
; Author and history:
;		Cameron R. Homeyer  2012-03-12.
;-

COMPILE_OPT IDL2																									;Set Compile Options

yyyy        = MAKE_ISO_DATE_STRING(date, PREC='year',  /COMPACT, /UTC)							;Create date strings
yyyymm      = MAKE_ISO_DATE_STRING(date, PREC='month', /COMPACT, /UTC)
yyyymmdd    = MAKE_ISO_DATE_STRING(date, PREC='day',   /COMPACT, /UTC)
yyyymmddhh  = MAKE_ISO_DATE_STRING(date, PREC='hour',  /COMPACT, /UTC)
date_string = yyyymmdd + STRING(date.hour, FORMAT="(I2.2)")

era_interim_ncd = '/data1/ERA-Interim/netcdf/' 
IF (N_ELEMENTS(type) GT 0) THEN BEGIN
	directory = !ERA_INTERIM_GRB + yyyy + '/' + yyyymm + '/'											;Set directory
	
	RETURN, directory + date_string + type + '.grb'														;Return grib filepath
ENDIF ELSE BEGIN
	directory = era_interim_ncd + yyyy + '/' + yyyymm + '/'											;Set directory
	
	RETURN, directory + yyyymmddhh + '.nc'																	;Return netCDF filepath
ENDELSE

END
