FUNCTION READ_NOAA_SONDE, statid, date, $
	INDIR  = indir, $
	INFILE = infile, $
	INTERP = interp

;+
; Name:
;		READ_NOAA_SONDE
; Purpose:
;		This is a function to read a NOAA radiosonde file. 
; Calling sequence:
;		value = READ_NOAA_SONDE(statid, date)
; Inputs:
;		statid : ICAO station identifier (e.g., 'KABR')
;		date   : CDATE structure of desired date.
; Output:
;		value  : A structure containing P, Z, T, Td, wind direction & speed.
; Keywords:
;		INDIR  : Optional keyword to set root input directory.
;		INFILE : Optional keyword to set file path.
;		INTERP : If set, interpolate profile to 100 m regular grid.
; Author and history:
;		Cameron R. Homeyer  2012-08-23.
;-

COMPILE_OPT IDL2																								;Set Compile Options

IF (N_ELEMENTS(indir) EQ 0) THEN indir = '/Users/chomeyer/data/radiosondes/'				;Root directory ;'/Users/chomeyer/data/dc3/radiosondes_hires/'

wc = 10																											;Set input file column width
IF (N_ELEMENTS(infile) EQ 0) THEN BEGIN
	date_str = MAKE_ISO_DATE_STRING(date, PREC='hour', /COMPACT, /UTC)						;Get date string
	infile   = indir + statid + '/' + date_str + '.txt'											;Piece together file name
ENDIF ELSE BEGIN
	statid = -1
	date   = -1
ENDELSE

IF FILE_TEST(infile) THEN BEGIN
	nlines = FILE_LINES(infile)																			;Count number of lines in file
	data   = STRARR(nlines)																					;Create array to store file lines
	
	OPENR,    iunit, infile, /GET_LUN																	;Open file for reading
	READF,    iunit, data																					;Read entire file
	FREE_LUN, iunit																							;Close input file

	lon = FLOAT(STRMID(data[2],   0, wc))																;Convert coordinates to floats
	lat = FLOAT(STRMID(data[2],  wc, wc))
	
	prs = FLOAT(STRMID(data[6:*],    0, wc))															;Convert data to floats
	alt = FLOAT(STRMID(data[6:*],   wc, wc))
	T   = FLOAT(STRMID(data[6:*], 2*wc, wc))
	Td  = FLOAT(STRMID(data[6:*], 3*wc, wc))
	dir = FLOAT(STRMID(data[6:*], 4*wc, wc))
	spd = FLOAT(STRMID(data[6:*], 5*wc, wc))

	ival = WHERE((alt NE 99999.0), nval)
	IF (nval GT 0) THEN BEGIN
		alt = alt[ival]
		prs = prs[ival]
		T   =   T[ival]
		Td  =  Td[ival]
		dir = dir[ival]
		spd = spd[ival]
	ENDIF ELSE RETURN, -1

	IF KEYWORD_SET(interp) THEN BEGIN
		alt2 = alt[0] + 10.0*FINDGEN(LONG((alt[-1] - alt[0])/10.0) + 1)
		iz   = INTERPOL(FINDGEN(N_ELEMENTS(alt)), alt, alt2)
		
		prs  = INTERPOLATE(prs, iz)
		T    = INTERPOLATE(T,   iz)
		Td   = INTERPOLATE(Td,  iz)
		dir  = INTERPOLATE(dir, iz)
		spd  = INTERPOLATE(spd, iz)
		alt  = alt2
	ENDIF
ENDIF ELSE RETURN, -1

RETURN, {id   : statid,       $																			;Return data structure
			x    : lon,          $
		   y    : lat,          $
			P    : prs,          $
			Z    : alt,          $
			T    : T,            $
			Td   : Td,           $
			wdir : dir,          $
			wspd : spd,          $
			date : date,         $
			n    : N_ELEMENTS(T) }

END
