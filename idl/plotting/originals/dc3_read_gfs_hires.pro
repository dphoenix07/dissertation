FUNCTION DC3_READ_GFS_HIRES, flight_name, date

;+
; Name:
;		DC3_READ_GFS_HIRES
; Purpose:
;		This is a template for creating IDL function files. 
; Calling sequence:
;		value = DC3_READ_GFS_HIRES(flight_name, date)
; Inputs:
;		flight_name : DC3 GV flight name (e.g., 'rf02')
;		date        : Desire date {CDATE}
; Output:
;		A data structure containing WMO tropopause and grid information.
; Keywords:
;		None. 
; Author and history:
;		Cameron R. Homeyer  2012-08-27.
;-

COMPILE_OPT IDL2																								;Set Compile Options

indir    = !DC3_DATA + 'gfs_hires/' + flight_name + '/'											;Set output directory
date_str = MAKE_ISO_DATE_STRING(date, PREC = 'MINUTE', /COMPACT, /UTC)						;Store date string for file reading
infile   = indir + date_str + '.nc'

IF FILE_TEST(infile) THEN BEGIN
	id = NCDF_OPEN(infile)																					;Open file for reading
	
	NCDF_VARGET, id, 'Longitude', values																;Read the longitude variable
	NCDF_ATTGET, id, 'Longitude', 'long_name', name
	NCDF_ATTGET, id, 'Longitude', 'units',     units
	
	x = {values    : values,        $
		  long_name : STRING(name ), $
		  units     : STRING(units)  }
	
	NCDF_VARGET, id, 'Latitude', values																	;Read the latitude variable
	NCDF_ATTGET, id, 'Latitude', 'long_name', name
	NCDF_ATTGET, id, 'Latitude', 'units',     units
	
	y = {values    : values,        $
		  long_name : STRING(name ), $
		  units     : STRING(units)  }
	
	NCDF_VARGET, id, 'Z_trop1', values																	;Read the primary tropopause variable
	NCDF_ATTGET, id, 'Z_trop1', 'long_name', name
	NCDF_ATTGET, id, 'Z_trop1', 'units',     units
	
	z1 = {values    : values,        $
			long_name : STRING(name ), $
			units     : STRING(units)  }

	NCDF_VARGET, id, 'Z_trop2', values																	;Read the secondary tropopause variable
	NCDF_ATTGET, id, 'Z_trop2', 'long_name', name
	NCDF_ATTGET, id, 'Z_trop2', 'units',     units
	
	z2 = {values    : values,        $
			long_name : STRING(name ), $
			units     : STRING(units)  }

	NCDF_VARGET, id, 'Z_trop3', values																	;Read the tertiary tropopause variable
	NCDF_ATTGET, id, 'Z_trop3', 'long_name', name
	NCDF_ATTGET, id, 'Z_trop3', 'units',     units
	
	z3 = {values    : values,        $
			long_name : STRING(name ), $
			units     : STRING(units)  }

	NCDF_CLOSE, id																								;Close input file

	RETURN, {z1 : z1, $																						;Return data structure
				z2 : z2, $
				z3 : z3, $
				x  : x,  $
				y  : y   }
ENDIF ELSE $
	RETURN, -1																									;Return missing flag

END
