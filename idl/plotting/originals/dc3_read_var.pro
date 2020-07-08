FUNCTION DC3_READ_VAR, var, flight_name, $
	FALCON   = falcon, $
	DC8      = dc8, $
	TEST     = test, $
	RESEARCH = research, $
	ALL      = all

;+
; Name:
;		DC3_READ_VAR
; Purpose:
;		This is a function to read a desired variable from a DC3 netCDF
;		file.   
; Calling sequence:
;		value = DC3_READ_VAR(var, flight_name)
; Inputs:
;		var         : Desired variable to read.
;		flight_name : Name(s) of desired flight(s). (e.g. - 'rf01', ['rf01', 'rf02'] or '20120519')
; Output:
;		A structure containing data and attributes. 
; Keywords:
;		FALCON      : If set, read Falcon data. Default is to read GV data.
;		DC8         : If set, read DC8 data. Default is to read GV data.
;		TEST        : If set, pool all test flights into data structure.
;		RESEARCH    : If set, pool all research flights into data structure.
;		ALL         : If set, pool all test and research flights into data structure.
; Author and history:
;		Cameron R. Homeyer  2012-08-07.
;-

COMPILE_OPT IDL2

IF ((N_ELEMENTS(flight_name) EQ 0) AND ~KEYWORD_SET(research)) THEN $
	research = 1																								;Default to read all research files, if no flight names

IF KEYWORD_SET(falcon) THEN BEGIN
	indir = !DC3_DATA + 'merged/falcon/*'
	
	IF KEYWORD_SET(all) OR KEYWORD_SET(research) THEN BEGIN
		flight_name = REPLICATE('rf', 13)																;Create array of flight names
		flight_name = flight_name + STRING(INDGEN(13)+1, FORMAT="(I2.2)")
	ENDIF
ENDIF ELSE IF KEYWORD_SET(dc8) THEN BEGIN
	indir = !DC3_DATA + 'merged/dc8/*'
	
	IF KEYWORD_SET(all) OR KEYWORD_SET(research) THEN BEGIN
		flight_name = REPLICATE('rf', 18)																	;Create array of flight names
		flight_name = flight_name + STRING(INDGEN(18)+1, FORMAT="(I2.2)")
	ENDIF
ENDIF ELSE BEGIN
	indir = !DC3_DATA + 'merged/gv/*'
	
	IF KEYWORD_SET(all) THEN BEGIN
		flight_name = [REPLICATE('tf', 4), REPLICATE('rf', 22)]										;Create array of flight names
		flight_name = flight_name + [STRING(INDGEN( 4)+1, FORMAT="(I2.2)"), $
											  STRING(INDGEN(22)+1, FORMAT="(I2.2)")]
	ENDIF ELSE IF KEYWORD_SET(research) THEN BEGIN
		flight_name = REPLICATE('rf', 22)																	;Create array of flight names
		flight_name = flight_name + STRING(INDGEN(22)+1, FORMAT="(I2.2)")
	ENDIF ELSE IF KEYWORD_SET(test) THEN BEGIN
		flight_name = REPLICATE('tf',  4)																	;Create array of flight names
		flight_name = flight_name + STRING(INDGEN( 4)+1, FORMAT="(I2.2)")
	ENDIF
ENDELSE

iflt     = 0
nflights = N_ELEMENTS(flight_name)

IF (KEYWORD_SET(dc8) AND (var EQ 'cloud')) THEN $
	indir = !DC3_DATA + 'merged/dc8/cloud_indicator/*'

IF (nflights EQ 1) THEN BEGIN
	CATCH, Error_Status																							;Find errors	
	IF (Error_Status NE 0) THEN skip_flight = 1 $														;If input error, then set flag to skip NCAR ozone
								  ELSE skip_flight = 0

	IF (var EQ 'cloud') THEN BEGIN
		IF ~skip_flight THEN BEGIN
			list     = FILE_SEARCH(indir + flight_name + '*', COUNT = nfiles)						;Get file list
			raf_file = list[nfiles -1]																			;Set input file
			data0    = READ_ASCII(raf_file, DATA_START = 49, DELIMITER = ',', COUNT = nval)	;Read iCART file
			
			data_val = REPLACE_MISSING_NAN(REFORM(data0.field1[1,*]), -9999)						;Replace missing values with NaNs and store data array
			range    = [0,3]
			data     = CREATE_STRUCT('name',      'cloud indicator', $								;Create structure
											 'units',     '0:none, 1:thin or ' + $
											 			     'not sure, 2:yes, 3:heavy', $
											 'long_name', 'SPEC particle imager & in-flight video' + $
											 				  ' based cloud flags')
			flight   = REPLICATE(LONG(STRMID(flight_name,2)), nval)
		ENDIF ELSE RETURN, -1
	ENDIF ELSE BEGIN
		IF ~skip_flight THEN BEGIN
			list     = FILE_SEARCH(indir + flight_name + '*', COUNT = nfiles)						;Get file list
			raf_file = list[nfiles -1]																			;Set input file
			id       = NCDF_OPEN(raf_file)
			data     = NCDF_READ_VAR(id, var)																;Read variable data	
	
			NCDF_CLOSE, id
	
			IF (var EQ 'Time') THEN BEGIN
				data_val   = TIME_INC(READ_ISO_DATE_STRING(STRMID(data.units, 14, 19)), data.values)	;Store data
				data.units = 'CDATE'
				range      = -1
			ENDIF ELSE BEGIN
				data_val = REPLACE_MISSING_NAN(data.values, data._FillValue)						;Store data
				range    = data.actual_range		
			ENDELSE

			flight   = REPLICATE(LONG(STRMID(flight_name,2)), data.n)
		ENDIF ELSE RETURN, -1
	ENDELSE
	
	CATCH, /CANCEL
ENDIF ELSE BEGIN
	FOR i = 0, nflights -1 DO BEGIN
		CATCH, Error_Status																						;Find errors	
		IF (Error_Status NE 0) THEN skip_flight = 1 $													;If input error, then set flag to skip NCAR ozone
									  ELSE skip_flight = 0

		IF (var EQ 'cloud') THEN BEGIN
			IF ~skip_flight THEN BEGIN
				list     = FILE_SEARCH(indir + flight_name[i] + '*', COUNT = nfiles)				;Get file list
				raf_file = list[nfiles -1]																		;Set input file
				data0    = READ_ASCII(raf_file, DATA_START = 49, DELIMITER = ',', COUNT = nval);Read iCART file
			
				IF (N_ELEMENTS(data_val) EQ 0) THEN BEGIN
					data_val = REPLACE_MISSING_NAN(REFORM(data0.field1[1,*]), -9999)				;Replace missing values with NaNs and store data array
					range    = [0,3]
					data     = CREATE_STRUCT('name',      'cloud indicator', $						;Create structure
													 'units',     '0:none, 1:thin or ' + $
																	  'not sure, 2:yes, 3:heavy', $
													 'long_name', 'SPEC particle imager & in-flight video' + $
																	  ' based cloud flags')
					flight   = REPLICATE(i+1, nval)
				ENDIF ELSE BEGIN
					data_val = [data_val, REPLACE_MISSING_NAN(REFORM(data0.field1[1,*]), -9999)]	;Concatenate with previous data
					flight   = [flight, REPLICATE(i+1, nval)]
				ENDELSE
			ENDIF
		ENDIF ELSE BEGIN		
			IF ~skip_flight THEN BEGIN
				list     = FILE_SEARCH(indir + flight_name[i] + '*', COUNT = nfiles)				;Get file list
				raf_file = list[nfiles -1]																		;Set input file
				id       = NCDF_OPEN(raf_file)
				data     = NCDF_READ_VAR(id, var)															;Read variable data	
		
				NCDF_CLOSE, id

				CATCH, /CANCEL
				IF (N_ELEMENTS(data_val) EQ 0) THEN BEGIN
					IF (var EQ 'Time') THEN BEGIN
						data_val   = TIME_INC(READ_ISO_DATE_STRING(STRMID(data.units, 14, 19)), data.values)	;Store data
						data.units = 'CDATE'
						range      = -1
					ENDIF ELSE BEGIN
						data_val = REPLACE_MISSING_NAN(data.values, data._FillValue)				;Store data
						range    = data.actual_range		
					ENDELSE
	
					flight   = REPLICATE(i+1, data.n)
				ENDIF ELSE BEGIN
					IF (var EQ 'Time') THEN BEGIN
						data_val   = [data_val, TIME_INC(READ_ISO_DATE_STRING(STRMID(data.units, 14, 19)), data.values)]	;Store data
						data.units = 'CDATE'
						range      = -1
					ENDIF ELSE BEGIN
						data_val = [data_val, REPLACE_MISSING_NAN(data.values, data._FillValue)];Concatentate with previous data
						range    = -1
					ENDELSE
				
					flight   = [flight, REPLICATE(i+1, data.n)]
				ENDELSE
			ENDIF
		ENDELSE
	ENDFOR
	
	IF (N_ELEMENTS(data_val) EQ 0) THEN RETURN, -1

	CATCH, /CANCEL																								;Disable error handler
ENDELSE

RETURN, {name      : data.name,            $															;Return data structure
			values    : data_val,             $
			flight    : flight,               $
			range     : range,                $
			n         : N_ELEMENTS(data_val), $
			units     : data.units,           $
			long_name : data.long_name,       $
			file      : raf_file}

END
