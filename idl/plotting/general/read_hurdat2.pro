FUNCTION READ_HURDAT2

;+
; Name:
;		READ_HURDAT2
; Purpose:
;		This is a function to read tropical cylcone track information for Atlantic storms. 
; Calling sequence:
;		tracks = READ_HURDAT2()
; Inputs:
;		None.
; Output:
;		A structure of tropical cyclone track information for the Atlantic.
; Keywords:
;		None.
; Author and history:
;		Cameron R. Homeyer  2013-03-19.
;-

COMPILE_OPT IDL2																									;Set Compile Options

indir = !WRF_DIRECTORY + 'general/'
infile = indir + 'hurdat2-1851-2017-050118.txt'
nlines = FILE_LINES(infile)
data   = STRARR(nlines)

OPENR, iunit, infile, /GET_LUN
READF, iunit, data
FREE_LUN, iunit

istorm = WHERE((STRMID(data,0,2) EQ 'AL'), nstorm)
name   = STRTRIM(STRMID(data[istorm],18,10),2)
year   = LONG(STRMID(data[istorm],4,4))
num    = LONG(STRMID(data[istorm],2,2))
lines  = LONG(STRMID(data[istorm],33,3))
istart = istorm             - LINDGEN(nstorm)
iend   = istorm + (lines-1) - LINDGEN(nstorm)

FOR i = 0, nstorm -1 DO BEGIN
	date_str = STRMID(data[(istorm[i]+1):(istorm[i]+lines[i])],  0, 8) + 'T' + $
				  STRMID(data[(istorm[i]+1):(istorm[i]+lines[i])], 10, 4) + 'Z'
	sn       = 1 + (-2)*(STRMID(data[(istorm[i]+1):(istorm[i]+lines[i])],27,1) EQ 'S')
	we       = 1 + (-2)*(STRMID(data[(istorm[i]+1):(istorm[i]+lines[i])],35,1) EQ 'W')

	IF (i EQ 0) THEN BEGIN
		date  = READ_ISO_DATE_STRING(date_str)
		id    =       STRMID(data[(istorm[i]+1):(istorm[i]+lines[i])], 16, 1)
		class =       STRMID(data[(istorm[i]+1):(istorm[i]+lines[i])], 19, 2)
		x     = FLOAT(STRMID(data[(istorm[i]+1):(istorm[i]+lines[i])], 30, 5))*we
		y     = FLOAT(STRMID(data[(istorm[i]+1):(istorm[i]+lines[i])], 23, 4))*sn
		P     =  LONG(STRMID(data[(istorm[i]+1):(istorm[i]+lines[i])], 43, 4))
		wspd  =  LONG(STRMID(data[(istorm[i]+1):(istorm[i]+lines[i])], 38, 3))
	ENDIF ELSE BEGIN	
		date  = [date,                                     READ_ISO_DATE_STRING(date_str)]
		id    = [id,          STRMID(data[(istorm[i]+1):(istorm[i]+lines[i])], 16, 1)    ]
		class = [class,       STRMID(data[(istorm[i]+1):(istorm[i]+lines[i])], 19, 2)    ]
		x     = [x,     FLOAT(STRMID(data[(istorm[i]+1):(istorm[i]+lines[i])], 30, 5))*we]
		y     = [y,     FLOAT(STRMID(data[(istorm[i]+1):(istorm[i]+lines[i])], 23, 4))*sn]
		P     = [P,      LONG(STRMID(data[(istorm[i]+1):(istorm[i]+lines[i])], 43, 4))   ]
		wspd  = [wspd,   LONG(STRMID(data[(istorm[i]+1):(istorm[i]+lines[i])], 38, 3))   ]
	ENDELSE
ENDFOR

RETURN, {name   : name,   $
			year   : year,   $
			number : num,    $
			n      : nstorm, $
			istart : istart, $
			iend   : iend,   $
			date   : date,   $
			id     : id,     $
			class  : class,  $
			x      : x,      $
			y      : y,      $
			P      : P,      $
			wspd   : wspd    }

END
