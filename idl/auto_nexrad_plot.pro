PRO AUTO_NEXRAD_PLOT, event, start_date, end_date, $
	PNG 	= png, $
	ANIMATE = animate


;+
; Name:
;               AUTO_NEXRAD_PLOT
; Purpose:
;               This is a procedure to produce a desired number of NEXRAD plots. Best
;				used for creating plots to combined in a GIF loop.
; Calling sequence:
;               AUTO_NEXRAD_PLOT, event, variable, date, end_hour
; Inputs:
;               event      : String variable of run name. (e.g., '20120519')
;				start_date : CDATE
;				end_date   : CDATE
; Output:
;               A set of plots and an animated GIF of those plots.
; Keywords:
;				ANIMATE	    : If set, calls WRITE_ANIMATED_GIF to create animation.
;				PNG			: Set if output to PNG desired.
; Author and history:
;               Daniel B. Phoenix	2016-05-11.
;-

COMPILE_OPT IDL2																				;Set compile options

IF (N_ELEMENTS(event	 ) EQ 0) THEN event 	 = '20120519'
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = MAKE_DATE(2012,5,19,22,30)
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = MAKE_DATE(2012,5,20,01,00)

key = 'nexrad'	
dt = TIME_DIFF(end_date,start_date)
nt = 6

nexfile = !NEXRAD_DIRECTORY + event + '/'

CD, nexfile
infile = FILE_SEARCH('*.nc', COUNT = nfile)														;Get files
PRINT, infile
For i = 0, nfile - 1 DO infile[i] = STRMID(infile[i], 0, 14)									;Remove 'd02' and '.nc' from infile array

start_date = MAKE_ISO_DATE_STRING(start_date, PREC='MINUTE', /COMPACT, /UTC)
end_date   = MAKE_ISO_DATE_STRING(end_date  , PREC='MINUTE', /COMPACT, /UTC)

file_s = WHERE(start_date EQ infile, count)														;Find index of start time
file_e = WHERE(end_date EQ infile, count)														;Find index of last time

first_file = infile[file_s]																		;Confirm start and end date for user
last_file  = infile[file_e]

file_s = file_s[0]																				;Remove unwanted dimensions (convert array to long)
file_e = file_e[0]
ntimes = file_e - file_s

date_arr = [ ]
FOR i = file_s, file_e, nt DO BEGIN
	date_string = READ_ISO_DATE_STRING(infile[i])
	date_arr = [date_arr, date_string]
ENDFOR

FOREACH d, date_arr DO MAP_NEXRAD_REFLECTIVITY, STRING(event), d, PNG = png

IF KEYWORD_SET(animate) THEN WRITE_ANIMATED_GIF, STRING(event), 'nexrad', 'nexrad_refl'

END
