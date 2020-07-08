PRO PRINT_TMATRIX_MV_CMD, date, $
	NLEV = nlev 


;+
; Name:
;		PRINT_TMATRIX_MV_CMD
; Purpose:
;		This is a procedure to print a command line 'mv' command list to change wrfout file
;		names for archiving. 
; Calling sequence:
;		PRINT_TMATRIX_MV_CMD, MAKE_DATE(2014,5,1,12), MAKE_DATE(2014,5,3,0)
; Input:
;		date1 : Initial date. {CDATE}
;		date2 : Final date. {CDATE}
; Output:
;		A list of UNIX command line commands.
; Keywords:
;		NLEV : number of vertical levels
; Author and history:
;		Cameron R. Homeyer  2012-10-05.
;		Daniel B. Phoenix	2016-06-06. Edited original for output from the PRS.
;-

COMPILE_OPT IDL2

IF (N_ELEMENTS(nlev) EQ 0) THEN nlev = 100

dom   = 'd02'
yyyy  = STRING(date.year,   FORMAT="(I4.4)")															;Make date strings
mm    = STRING(date.month,  FORMAT="(I2.2)")
dd_hh = STRING(date.day,    FORMAT="(I2.2)") + '_' + STRING(date.hour, FORMAT="(I2.2)")
min   = STRING(date.minute, FORMAT="(I2.2)")
dates = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)

FOR i = 0, nlev-1 DO BEGIN
	PRINT, 'mv wrfout_' + dom + '_' + yyyy + '-' + mm + '-' + dd_hh + $						;Print mv command
			 '\:' + min + '\:00m0k' + STRTRIM(i,1) + '_MF1t000000_dBZ.nc wrfout_' + $
			 	dom + '_' + dates + '.ncm0k' + STRTRIM(i,1) + '_MF1t000000_dBZ.nc' 
																					
ENDFOR

END