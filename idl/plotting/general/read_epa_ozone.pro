FUNCTION READ_EPA_OZONE, year

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

indir = !WRF_DIRECTORY + 'general/o3_data/'
infile = indir + 'dallas_' + year + '.csv'

;nlines = FILE_LINES(infile)
;data   = STRARR(nlines)
;OPENR, iunit, infile, /GET_LUN
;READF, iunit, data
;FREE_LUN, iunit

data = READ_CSV(infile)

FOR i = 0, N_ELEMENTS(data.field01) -1 DO BEGIN
	date_str = STRMID(data.field12,0,4)+STRMID(data.field12,5,2)+STRMID(data.field12,8,2) + 'T' + $
				  STRMID(data.field13,0,2)+STRMID(data.field13,3,2) + 'Z'

	IF (i EQ 0) THEN BEGIN
	     date     = READ_ISO_DATE_STRING(date_str)
	     site     = data.field03
	     x        = data.field07
	     y        = data.field06
	     o3       = data.field14
	ENDIF ELSE BEGIN
		 date     = [date,  READ_ISO_DATE_STRING(date_str)]
	     site     = [site,  data.field03]
	     x        = [x,     data.field07]
	     y        = [y,     data.field06]
	     o3       = [o3,    data.field14]
	ENDELSE
PRINT, i
ENDFOR

RETURN, {date   : date,   $
			site   : site,     $
			x      : x,      $
			y      : y,      $
			o3     : o3}

END
