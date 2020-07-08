FUNCTION NEXRAD_READ_LEVEL2_2, infile

;+
; Name:
;		NEXRAD_READ_LEVEL2_2
; Purpose:
;		This is a function for reading NEXRAD level 2 composites created using 
;		BIN_NEXRAD_LEVEL2_REGIONAL. 
; Calling sequence:
;		value = NEXRAD_READ_LEVEL2_2(infile)
; Inputs:
;		infile : Input file path.
; Output:
;		A structure containing NEXRAD composite data. 
; Keywords:
;		None.
; Author and history:
;		Cameron R. Homeyer  2013-09-27.
;-

COMPILE_OPT IDL2																									;Set Compile Options

IF FILE_TEST(infile) THEN BEGIN
	id = NCDF_OPEN(infile)
	
	NCDF_VARGET, id, 'stations_contributing', statid													;Read contributing station IDs
	NCDF_VARGET, id, 'zmin_contributing', z0_cont														;Read minimum altitude of contributing radar beams
	NCDF_VARGET, id, 'zmax_contributing', z1_cont														;Read maximum altitude of contributing radar beams
	NCDF_VARGET, id, 'nbeams_contributing', nbeams														;Read number of contributing radar beams

	NCDF_VARGET, id, 'Longitude', values																	;Read longitude variable
	NCDF_ATTGET, id, 'Longitude', 'long_name', name
	NCDF_ATTGET, id, 'Longitude', 'units',     units

	x = {values    : values,           $																	;Longitude structure
		  long_name : STRING(name),     $
		  units     : STRING(units),    $
		  n         : N_ELEMENTS(values)}

	NCDF_VARGET, id, 'Latitude', values																		;Read latitude variable
	NCDF_ATTGET, id, 'Latitude', 'long_name', name
	NCDF_ATTGET, id, 'Latitude', 'units',     units
	
	y = {values    : values,           $																	;Latitude structure
		  long_name : STRING(name),     $
		  units     : STRING(units),    $
		  n         : N_ELEMENTS(values)}

	NCDF_VARGET, id, 'Altitude', values																		;Read altitude variable
	NCDF_ATTGET, id, 'Altitude', 'long_name', name
	NCDF_ATTGET, id, 'Altitude', 'units',     units

	z = {values    : values,           $																	;Altitude structure
		  long_name : STRING(name),     $
		  units     : STRING(units),    $
		  n         : N_ELEMENTS(values)}

	NCDF_VARGET, id, 'Reflectivity', values																;Read reflectivity variable
	NCDF_ATTGET, id, 'Reflectivity', 'long_name', name
	NCDF_ATTGET, id, 'Reflectivity', 'units',     units
	NCDF_ATTGET, id, 'Reflectivity', 'scale_factor',  factor											;Read scaling and offset values
	NCDF_ATTGET, id, 'Reflectivity', 'add_offset',    offset
	NCDF_ATTGET, id, 'Reflectivity', 'missing_value', missing

	imiss  = WHERE((values EQ missing), nmiss)															;Search for missing values
	values = offset + factor*FLOAT(values)																	;Convert to float
	IF (nmiss GT 0) THEN values[imiss] = !Values.F_NaN													;Replace missing values with NaNs

	NCDF_VARGET, id, 'wReflectivity', wvalues																;Read cumulative reflectivity weights
	NCDF_ATTGET, id, 'wReflectivity', 'scale_factor', factor
	
	dBZ = {values    : values,           $																	;Reflectivity structure
			 wvalues   : factor*wvalues,   $
			 long_name : STRING(name),     $
			 units     : STRING(units),    $
			 n         : N_ELEMENTS(values)}

	IF ((NCDF_INQUIRE(id)).nvars GT 10) THEN BEGIN
		NCDF_VARGET, id, 'nbeams_contributing_dp', nbeams2												;Read number of contributing dual-pol beams
		
		NCDF_VARGET, id, 'DifferentialReflectivity', values											;Read differential reflectivity variable
		NCDF_ATTGET, id, 'DifferentialReflectivity', 'long_name', name
		NCDF_ATTGET, id, 'DifferentialReflectivity', 'units',     units
		NCDF_ATTGET, id, 'DifferentialReflectivity', 'scale_factor',  factor						;Read scaling and offset values
		NCDF_ATTGET, id, 'DifferentialReflectivity', 'add_offset',    offset
		NCDF_ATTGET, id, 'DifferentialReflectivity', 'missing_value', missing

		imiss  = WHERE((values EQ missing), nmiss)														;Search for missing values
		values = offset + factor*FLOAT(values)																;Convert to float
		IF (nmiss GT 0) THEN values[imiss] = !Values.F_NaN												;Replace missing values with NaNs

		NCDF_VARGET, id, 'wDifferentialReflectivity', wvalues											;Read cumulative differential reflectivity weights
		NCDF_ATTGET, id, 'wDifferentialReflectivity', 'scale_factor',  factor					;Read scaling and offset values

		Zdr = {values    : values,           $																;Differential reflectivity structure
				 wvalues   : factor*wvalues,   $
				 long_name : STRING(name),     $
				 units     : STRING(units),    $
				 n         : N_ELEMENTS(values)}

		NCDF_VARGET, id, 'DifferentialPhase', values														;Read specific differential phase variable
		NCDF_ATTGET, id, 'DifferentialPhase', 'long_name', name
		NCDF_ATTGET, id, 'DifferentialPhase', 'units',     units
		NCDF_ATTGET, id, 'DifferentialPhase', 'scale_factor',  factor								;Read scaling and offset values
		NCDF_ATTGET, id, 'DifferentialPhase', 'add_offset',    offset
		NCDF_ATTGET, id, 'DifferentialPhase', 'missing_value', missing
	
		imiss  = WHERE((values EQ missing), nmiss)														;Search for missing values
		values = offset + factor*FLOAT(values)																;Convert to float
		IF (nmiss GT 0) THEN values[imiss] = !Values.F_NaN												;Replace missing values with NaNs

		NCDF_VARGET, id, 'wDifferentialPhase', wvalues													;Read cumulative differential phase weights
		NCDF_ATTGET, id, 'wDifferentialPhase', 'scale_factor',  factor								;Read scaling and offset values

		KDP = {values    : values,           $																;Differential phase structure
				 wvalues   : factor*wvalues,   $
				 long_name : STRING(name),     $
				 units     : STRING(units),    $
				 n         : N_ELEMENTS(values)}
	ENDIF ELSE BEGIN
		Zdr     = -1
		KDP     = -1
		rhoHV   = -1
		nbeams2 = -1
	ENDELSE
	
	NCDF_CLOSE, id																									;Close input file

	date = READ_ISO_DATE_STRING(STRMID(infile,16,14,/REVERSE_OFFSET))								;Extract date from filename
	
	RETURN, {dBZ     : dBZ,            $																	;Return data structure
				Zdr     : Zdr,            $
				KDP     : KDP,            $
				x       : x,              $
				y       : y,              $
				z       : z,              $
				statid  : STRING(statid), $
				z0_cont : z0_cont,        $
				z1_cont : z1_cont,        $
				nbeams  : nbeams,         $
				nbeams2 : nbeams2,        $
				date    : date            }

ENDIF ELSE RETURN, -1

END
