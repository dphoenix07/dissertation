FUNCTION READ_ISO_DATE_STRING, date_string, NO_LEAP = no_leap

; NAME:
;		READ_ISO_DATE_STRING
; PURPOSE:
;		This function reads an ISO 8601 formatted date string and converts 
;		it to a a CDATE structure.
; CATEGORY:
;		Date and time calculations.
; CALLING SEQUENCE:
;		date = READ_ISO_DATE_STRING(date_string)
; INPUT:
;		String containing the date as yyyy-mm-dd hh:mm:ss or other ISO variants.
;		Examples: 	2001-01-01 12:33:17
;						2001-01-01 12Z			(PRECISION = 'hour', /UTC)
;						20010101T123317Z		(/COMPACT, /UTC)
; OUTPUT:
;		date     : a cdate or cdate_noleap structure containing year, month, day, hour, minute, and second
; KEYWORDS:
;		no_leap  : if set, create a CDATE_NOLEAP structure (e.g., for CCM3 calendars).
; COMMON BLOCKS:
;		None.
; RESTRICTIONS:
;		This function assumes that the ISO date string always has the most significant part of the date.
;		It could have, for example, year only; year and month; year, month, and day; etc.  It cannot
;		have month only (without year) etc.
;		If the time has a UTC indicator (Z suffix), it is ignored.
; MODIFICATION HISTORY:
;		K. Bowman, 2002-02-21.
;		Cameron Homeyer, 2011-12. Vectorized.
;-

COMPILE_OPT IDL2																								;Set compile options

nt       = N_ELEMENTS(date_string)																		;Set number of times
year		= MAKE_ARRAY(nt, VALUE = 0)																	;Define variable type
month		= MAKE_ARRAY(nt, VALUE = 1)																	;Define variable type and set default value
day		= MAKE_ARRAY(nt, VALUE = 1)																	;Define variable type and set default value
hour		= MAKE_ARRAY(nt, VALUE = 0)																	;Define variable type and set default value
minute	= MAKE_ARRAY(nt, VALUE = 0)																	;Define variable type and set default value
second	= MAKE_ARRAY(nt, VALUE = 0)																	;Define variable type and set default value

len      = STRLEN(date_string)																			;Get string lengths
zpos     = STRPOS(date_string, 'Z')																		;Check for Z character
izpos    = WHERE((zpos NE -1), nzpos)
IF (nzpos GT 0) THEN len[izpos] = len[izpos] - 1													;Don't include Z character in length

seppos  = STRPOS(date_string, '-')
iseppos = WHERE((seppos EQ -1), nseppos, COMPLEMENT = icomp, NCOMPLEMENT = ncomp)		;Compact format

IF (nseppos GT 0) THEN BEGIN																				;Read compact format
	ilen = WHERE((len[iseppos] EQ 4), nlen)															;Search for strings of certain length
	IF (nlen GT 0) THEN BEGIN
		year[iseppos[ilen]] = LONG(STRMID(date_string[iseppos[ilen]], 0, 4))					;Store date information
	ENDIF
	
	ilen = WHERE((len[iseppos] EQ 6), nlen)															;Search for strings of certain length
	IF (nlen GT 0) THEN BEGIN
		year[iseppos[ilen]]  = LONG(STRMID(date_string[iseppos[ilen]], 0, 4))				;Store date information
		month[iseppos[ilen]] = LONG(STRMID(date_string[iseppos[ilen]], 4, 2))
	ENDIF

	ilen = WHERE((len[iseppos] EQ 8), nlen)															;Search for strings of certain length
	IF (nlen GT 0) THEN BEGIN
		year[iseppos[ilen]]  = LONG(STRMID(date_string[iseppos[ilen]], 0, 4))				;Store date information
		month[iseppos[ilen]] = LONG(STRMID(date_string[iseppos[ilen]], 4, 2))
		day[iseppos[ilen]]   = LONG(STRMID(date_string[iseppos[ilen]], 6, 2))
	ENDIF

	ilen = WHERE((len[iseppos] EQ 11), nlen)															;Search for strings of certain length
	IF (nlen GT 0) THEN BEGIN
		year[iseppos[ilen]]  = LONG(STRMID(date_string[iseppos[ilen]], 0, 4))				;Store date information
		month[iseppos[ilen]] = LONG(STRMID(date_string[iseppos[ilen]], 4, 2))
		day[iseppos[ilen]]   = LONG(STRMID(date_string[iseppos[ilen]], 6, 2))
		hour[iseppos[ilen]]  = LONG(STRMID(date_string[iseppos[ilen]], 9, 2))
	ENDIF

	ilen = WHERE((len[iseppos] EQ 13), nlen)															;Search for strings of certain length
	IF (nlen GT 0) THEN BEGIN
		year[iseppos[ilen]]   = LONG(STRMID(date_string[iseppos[ilen]], 0, 4))				;Store date information
		month[iseppos[ilen]]  = LONG(STRMID(date_string[iseppos[ilen]], 4, 2))
		day[iseppos[ilen]]    = LONG(STRMID(date_string[iseppos[ilen]], 6, 2))
		hour[iseppos[ilen]]   = LONG(STRMID(date_string[iseppos[ilen]], 9, 2))
		minute[iseppos[ilen]] = LONG(STRMID(date_string[iseppos[ilen]],11, 2))
	ENDIF
	
	ilen = WHERE((len[iseppos] GT 13), nlen)															;Search for strings of certain length
	IF (nlen GT 0) THEN BEGIN
		year[iseppos[ilen]]   = LONG(STRMID(date_string[iseppos[ilen]], 0, 4))				;Store date information
		month[iseppos[ilen]]  = LONG(STRMID(date_string[iseppos[ilen]], 4, 2))
		day[iseppos[ilen]]    = LONG(STRMID(date_string[iseppos[ilen]], 6, 2))
		hour[iseppos[ilen]]   = LONG(STRMID(date_string[iseppos[ilen]], 9, 2))
		minute[iseppos[ilen]] = LONG(STRMID(date_string[iseppos[ilen]],11, 2))
		second[iseppos[ilen]] = LONG(STRMID(date_string[iseppos[ilen]],13, 2))
	ENDIF
ENDIF

IF (ncomp GT 0) THEN BEGIN																					;Read standard format
	ilen = WHERE((len[icomp] EQ 4), nlen)																;Search for strings of certain length
	IF (nlen GT 0) THEN BEGIN
		year[icomp[ilen]] = LONG(STRMID(date_string[icomp[ilen]], 0, 4))						;Store date information
	ENDIF
	
	ilen = WHERE((len[icomp] EQ 7), nlen)																;Search for strings of certain length
	IF (nlen GT 0) THEN BEGIN
		year[icomp[ilen]]  = LONG(STRMID(date_string[icomp[ilen]], 0, 4))						;Store date information
		month[icomp[ilen]] = LONG(STRMID(date_string[icomp[ilen]], 5, 2))
	ENDIF

	ilen = WHERE((len[icomp] EQ 10), nlen)																;Search for strings of certain length
	IF (nlen GT 0) THEN BEGIN
		year[icomp[ilen]]  = LONG(STRMID(date_string[icomp[ilen]], 0, 4))						;Store date information
		month[icomp[ilen]] = LONG(STRMID(date_string[icomp[ilen]], 5, 2))
		day[icomp[ilen]]   = LONG(STRMID(date_string[icomp[ilen]], 8, 2))
	ENDIF

	ilen = WHERE((len[icomp] EQ 13), nlen)																;Search for strings of certain length
	IF (nlen GT 0) THEN BEGIN
		year[icomp[ilen]]  = LONG(STRMID(date_string[icomp[ilen]], 0, 4))						;Store date information
		month[icomp[ilen]] = LONG(STRMID(date_string[icomp[ilen]], 5, 2))
		day[icomp[ilen]]   = LONG(STRMID(date_string[icomp[ilen]], 8, 2))
		hour[icomp[ilen]]  = LONG(STRMID(date_string[icomp[ilen]],11, 2))
	ENDIF

	ilen = WHERE((len[icomp] EQ 16), nlen)																;Search for strings of certain length
	IF (nlen GT 0) THEN BEGIN
		year[icomp[ilen]]   = LONG(STRMID(date_string[icomp[ilen]], 0, 4))					;Store date information
		month[icomp[ilen]]  = LONG(STRMID(date_string[icomp[ilen]], 5, 2))
		day[icomp[ilen]]    = LONG(STRMID(date_string[icomp[ilen]], 8, 2))
		hour[icomp[ilen]]   = LONG(STRMID(date_string[icomp[ilen]],11, 2))
		minute[icomp[ilen]] = LONG(STRMID(date_string[icomp[ilen]],14, 2))
	ENDIF
	
	ilen = WHERE((len[icomp] GT 16), nlen)																;Search for strings of certain length
	IF (nlen GT 0) THEN BEGIN
		year[icomp[ilen]]   = LONG(STRMID(date_string[icomp[ilen]], 0, 4))					;Store date information
		month[icomp[ilen]]  = LONG(STRMID(date_string[icomp[ilen]], 5, 2))
		day[icomp[ilen]]    = LONG(STRMID(date_string[icomp[ilen]], 8, 2))
		hour[icomp[ilen]]   = LONG(STRMID(date_string[icomp[ilen]],11, 2))
		minute[icomp[ilen]] = LONG(STRMID(date_string[icomp[ilen]],14, 2))
		second[icomp[ilen]] = LONG(STRMID(date_string[icomp[ilen]],17, 2))
	ENDIF
ENDIF

RETURN, MAKE_DATE(year, month, day, hour, minute, second, NO_LEAP = no_leap)				;Make date structure and return

END
