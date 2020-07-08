PRO EXTRACT_NOAA_FSL_SONDES, VERBOSE = verbose

;+
; Name:
;		EXTRACT_NOAA_FSL_SONDES
; Purpose:
;		This is a procedure to extract individual sounding profiles from
;		FSL ASCII formatted files downloaded via the NOAA ESRL raobs archive
;		(http://www.esrl.noaa.gov/raobs/). 
; Calling sequence:
;		EXTRACT_NOAA_FSL_SONDES, infile
; Input:
;		infile : File path to download FSL list.
; Output:
;		Individual sounding profiles written to '~/data/noaa_sondes/'
; Keywords:
;		OUTDIR  : Optional keyword to specify root output directory. Default is '~/data/noaa_sondes/'
;		VERBOSE : If set, message extraction process. 
; Author and history:
;		Cameron R. Homeyer  2012-08-23.
;-

COMPILE_OPT IDL2																									;Set compile options

infile = '/data3/dphoenix/wrf/20110518/radiosondes/ny_radiosondes_may2011.txt'
outdir = '/data3/dphoenix/wrf/20110518/radiosondes/'									

nlines = FILE_LINES(infile)																					;Count number of lines in input file
data   = STRARR(nlines)																							;Create array to store file lines

OPENR,    iunit, infile, /GET_LUN																			;Open input file for reading
READF,    iunit, data																							;Store file lines
FREE_LUN, iunit																									;Close input file

i0 = WHERE((STRMID(data, 4, 3) EQ '254'), nsondes)														;Find indices of individual sondes
i1 = i0 + 4																											;Get data start index for sondes
i2 = (SHIFT(i0, -1) -1) MOD nlines																			;Get data end index for sondes

FOR i = 0, nsondes - 1 DO BEGIN
	year   = LONG(STRMID(data[i0[i]],34,4))																;Extract time information from data strings
	day    = LONG(STRMID(data[i0[i]],14,7))
	hour   = LONG(STRMID(data[i0[i]], 7,7))
	month  = STRMID(data[i0[i]],27,3)
	lon    = FLOAT(STRMID(data[i0[i]+1],29,6))															;Extract station information from data strings
	lat    = FLOAT(STRMID(data[i0[i]+1],21,7))
	statid = 'K' + STRMID(data[i0[i]+3],18,3)

	CASE month OF 
		'JAN' : month = 1																							;Convert month string to numerical value
		'FEB' : month = 2
		'MAR' : month = 3
		'APR' : month = 4
		'MAY' : month = 5
		'JUN' : month = 6
		'JUL' : month = 7
		'AUG' : month = 8
		'SEP' : month = 9
		'OCT' : month = 10
		'NOV' : month = 11
		'DEC' : month = 12
		ELSE  : MESSAGE, 'Month ' + month + ' is not a valid month.'								;If bad month string, message error
	ENDCASE

	IF (STRMID(data[i0[i]+1],35,1) EQ 'W') THEN lon = -lon											;If west, then update longitude
	IF (STRMID(data[i0[i]+1],28,1) EQ 'S') THEN lat = -lat											;If south, then update latitude

	prs = LONG(STRMID(data[i1[i]:i2[i]], 7, 7))															;Extract measurements from data strings
	alt = LONG(STRMID(data[i1[i]:i2[i]],14, 7))
	tmp = LONG(STRMID(data[i1[i]:i2[i]],21, 7))
	dpt = LONG(STRMID(data[i1[i]:i2[i]],28, 7))
	dir = LONG(STRMID(data[i1[i]:i2[i]],35, 7))
	spd = LONG(STRMID(data[i1[i]:i2[i]],42, 7))

	ifin = WHERE((tmp NE 99999), fcount)																	;Look for valid temperature measurements
	IF (fcount GT 0) THEN BEGIN 
		ifin1 = WHERE((spd NE 99999), fcount1)
		ifin2 = WHERE(((tmp NE 99999) OR (spd NE 99999)), fcount2)
	
		dpt   = REPLACE_MISSING_NAN(0.1*FLOAT(dpt),9999.9)												;Convert dewpoint and replace missing numbers with NaNs
		
		tmp = INTERPOL(0.1*FLOAT(tmp[ifin ]), FLOAT(alt[ifin ]), FLOAT(alt[ifin2]))
		dpt = INTERPOL(          dpt[ifin ] , FLOAT(alt[ifin ]), FLOAT(alt[ifin2]))
		dir = INTERPOL(1.0*FLOAT(dir[ifin1]), FLOAT(alt[ifin1]), FLOAT(alt[ifin2])) 
		spd = INTERPOL(0.1*FLOAT(spd[ifin1]), FLOAT(alt[ifin1]), FLOAT(alt[ifin2]))
		prs =          0.1*FLOAT(prs[ifin2])																;Keep only valid temperature measurements
		alt =              FLOAT(alt[ifin2])

		FILE_MKDIR, outdir + statid + '/'																	;Create output directory, if necessary
	
		year  = STRING(year,  FORMAT = "(I4.4)")															;Convert time information to strings for output
		month = STRING(month, FORMAT = "(I2.2)")
		day   = STRING(day,   FORMAT = "(I2.2)")
		hour  = STRING(hour,  FORMAT = "(I2.2)")
	
		outfile = outdir + statid + '/' + year + month + day + 'T' + hour + 'Z.txt'			;Set output file name
		OPENW, ounit, outfile, /GET_LUN																		;Open output file for writing
	
		PRINTF, ounit, ' Longitude  Latitude'																;Write coordinate headers to file
		PRINTF, ounit, '     (deg)     (deg)'
		PRINTF, ounit, lon, lat, FORMAT = "(2F10.2)"														;Write sounding coordinate to file
		PRINTF, ounit
		PRINTF, ounit, '     Press       Alt      Temp      Dewpt  Wind Dir  Wind Spd'		;Write data headers to file
		PRINTF, ounit, '      (mb)       (m)       (C)        (C)     (deg)     (m/s)'

		FOR j = 0, fcount2 -1 DO $
			PRINTF, ounit, prs[j], alt[j], tmp[j], dpt[j], dir[j], spd[j], $						;Write sounding data to file
					FORMAT = "(6F10.1)"
		
		FREE_LUN, ounit																							;Close output file
	ENDIF
	
	IF KEYWORD_SET(verbose) THEN $
		PRINT, STRING(i+1, FORMAT="(I5)") + ' out of ' + STRTRIM(nsondes,2) + ' extracted'	;Verbose message
ENDFOR

END
