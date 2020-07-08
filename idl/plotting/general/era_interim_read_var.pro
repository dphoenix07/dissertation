FUNCTION ERA_INTERIM_READ_VAR, name, date, $
	P0        = p0,        $
	VERBOSE   = verbose, $
	HELP      = help

;+
; Name:
;		ERA_INTERIM_READ_VAR
; Purpose:
;		This program reads one variable from one ERA-Interim netCDF file.
; Category:
;		ERA-Interim data utility.
; Calling sequence:
;		data = ERA_INTERIM_READ_VAR(name, date)
; Inputs:
;		name      : name of variable to read (Z, T, u, v, w, or relvor)
;		date      : date structure containing the analysis time.  This is used to 
;						select the input file.
; Keywords:
;		P0        : Scalar floating point value.  If set, return data only for closests pressure level to p0.
;		VERBOSE   : If set, print verbose informational messages.
; Output:
;		Structure containing the requested variable, or -1 if file does not exist, or
;		-2 if variable does not exist in file.
; Procedure:
;		This programs opens one ERA-Interim data file and returns the value(s) of a
;		single variable.
; Author:
;		Cameron R. Homeyer  2012-03-13.
;								  2017-05-24. Updated to remove netCDF-3 compression handling.
;-

COMPILE_OPT IDL2																									;Set compile options

IF KEYWORD_SET(help) THEN BEGIN
	PRINT, 'Calling sequence : '
	PRINT, '   data = ERA_INTERIM_READ_VAR(name, date[, fhour], $'
	PRINT, '             DIRECTORY = directory, $'
	PRINT, '             VERBOSE   = verbose, $'
	PRINT, '             HELP      = help)'
	RETURN, -1
ENDIF

IF (N_ELEMENTS(name     ) EQ 0) THEN name      = 'T'													;Default variable name
IF (N_ELEMENTS(date     ) EQ 0) THEN date      = MAKE_DATE(2007,12,1)							;Default analysis time

ERA_INTERIM_VAR_NAME, name, ncdf_name, ndims																;Look up names

infile  = ERA_INTERIM_FILEPATH(date)																		;Get filepath

x = ERA_INTERIM_READ_COORD('Longitude', infile)															;Read longitude coordinates
y = ERA_INTERIM_READ_COORD('Latitude',  infile)															;Read latitude coordinates
IF (ndims EQ 3) THEN $
	z = ERA_INTERIM_READ_COORD('Altitude', infile)														;Read altitude coordinates

id = NCDF_OPEN(infile)																							;Open input file

vid = NCDF_VARID(id, ncdf_name)																				;Get variable ID
IF (vid EQ -1) THEN BEGIN
	NCDF_CLOSE, id																									;Close input file
	RETURN, -2																										;Variable does not exist
ENDIF

NCDF_VARGET, id, 'Date', time																					;Get the time
t = READ_ISO_DATE_STRING(STRING(time))																		;Convert to date structure

NCDF_ATTGET, id, ncdf_name, 'long_name', long_name														;Get long name attribute
NCDF_ATTGET, id, ncdf_name, 'units',     units															;Get units attribute

CASE ndims OF
	2 : BEGIN
		NCDF_VARGET, id, ncdf_name, values
		NCDF_CLOSE, id																								;Close input file

		RETURN, {name       : name,              $														;Return 2-D variable
					values     : values,            $
					ndims      : ndims,             $
					long_name  : STRING(long_name), $
					units      : STRING(units),     $
					x          : x,                 $
					y          : y,                 $
					t          : t                  }
	END
	
	3 : BEGIN

		IF (N_ELEMENTS(p0) EQ 1) THEN BEGIN
			k = INDEX_OF_NEAREST(p0, z.values)																;Find index of requested pressure level
			NCDF_VARGET, id, ncdf_name, values, $
				OFFSET = [0,0,k], COUNT = [x.n,y.n,1]
			NCDF_CLOSE, id																							;Close input file
			
			values = REFORM(values, x.n, y.n)
			RETURN, {name       : name,              $													;Return one level for 3-D variable
						values     : values,            $
						ndims      : ndims,             $
						long_name  : STRING(long_name), $
						units      : STRING(units),     $
						x          : x,                 $
						y          : y,                 $
						z          : z,                 $
						k          : k,                 $
						t          : t                  }
		ENDIF ELSE BEGIN
			NCDF_VARGET, id, ncdf_name, values
			NCDF_CLOSE, id																							;Close input file
			
			RETURN, {name       : name,              $													;Return complete 3-D variable
						values     : values,            $
						ndims      : ndims,             $
						long_name  : STRING(long_name), $
						units      : STRING(units),     $
						x          : x,                 $
						y          : y,                 $
						z          : z,                 $
						t          : t                  }
		ENDELSE
		
	END
	
ENDCASE

END

