FUNCTION MK_DATE_ARR, event, scheme, start_date, end_date, $
	DOMAIN   = domain, $
	DATE     = date, $
	TIMES    = time



;+
; Name:
;		MK_DATE_ARR
; Purpose:
;		This is a function to create an array of CDATEs based on a user defined
;		start and end date.
; Calling sequence:
;		value = MK_DATE_ARR(run, scheme, start_date, end_date)
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		scheme 	   : Model initial state. Typically 'morrison' or 'nssl'.
;		start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;		end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
; Output:
;		String array of CDATEs.
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel B. Phoenix	    2016-02-19. 
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 'd01'										;Set default domain
	
indir  = !WRF_DIRECTORY + event + '/' + scheme + '/reduced/'									;Set input directory

CD, indir 

infile = FILE_SEARCH(domain + '*.nc', COUNT = nfile)												;Get files

For i = 0, nfile - 1 DO infile[i] = STRMID(infile[i], 4, 14)									;Remove 'd02' and '.nc' from infile array

file_s = WHERE(start_date EQ infile, count)														;Find index of start time
file_e = WHERE(end_date EQ infile, count)														;Find index of last time

first_file = infile[file_s]																		;Confirm start and end date for user
last_file  = infile[file_e]

file_s = file_s[0]																				;Remove unwanted dimensions (convert array to long)
file_e = file_e[0]
ntimes = file_e - file_s

IF KEYWORD_SET(date) THEN BEGIN
	PRINT, 'Confirming start date file: ' + STRING(first_file)
	PRINT, 'Confirming end date file: ' + STRING(last_file)
	date_arr = [ ]
	FOR i = file_s, file_e DO BEGIN
		date 	 = READ_ISO_DATE_STRING(infile[i])
		date_arr = [date_arr, date]
	ENDFOR
	RETURN, date_arr
ENDIF

IF KEYWORD_SET(time) THEN BEGIN
	time_arr = [ ]
	FOR i = file_s, file_e DO BEGIN
		time     = STRMID(infile[i], 9, 4)
	  	time_arr = [time_arr, time]
	ENDFOR
	RETURN, time_arr
ENDIF


END