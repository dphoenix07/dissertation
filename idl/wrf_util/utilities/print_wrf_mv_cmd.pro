PRO PRINT_WRF_MV_CMD, date1, date2, $
	DT     = dt, $
	DOM    = dom, $
	OUTDOM = dom2

;+
; Name:
;		PRINT_WRF_MV_CMD
; Purpose:
;		This is a procedure to print a command line 'mv' command list to change wrfout file
;		names for archiving. 
; Calling sequence:
;		PRINT_WRF_MV_CMD, MAKE_DATE(2014,5,1,12), MAKE_DATE(2014,5,3,0)
; Input:
;		date1 : Initial date. {CDATE}
;		date2 : Final date. {CDATE}
; Output:
;		A list of UNIX command line commands.
; Keywords:
;		DT     : Frequency of wrfout files (in seconds). Default is 1800 (30 minutes). 
;		DOM    : wrfout domain name. (e.g., 'd01')
;		OUTDOM : Desired wrfout domain name. Default is same as DOM.
; Author and history:
;		Cameron R. Homeyer  2012-10-05.
;-

COMPILE_OPT IDL2

IF (N_ELEMENTS(dt )  EQ 0) THEN dt   = 1800																;Set defaults
IF (N_ELEMENTS(dom)  EQ 0) THEN dom  = 'd01'
IF (N_ELEMENTS(dom2) EQ 0) THEN dom2 = dom

date = date1
WHILE (TIME_DIFF(date, date2) LE 0) DO BEGIN
	yyyy  = STRING(date.year,   FORMAT="(I4.4)")															;Make date strings
	mm    = STRING(date.month,  FORMAT="(I2.2)")
	dd_hh = STRING(date.day,    FORMAT="(I2.2)") + '_' + STRING(date.hour, FORMAT="(I2.2)")
	min   = STRING(date.minute, FORMAT="(I2.2)")
	dates = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)
	
	PRINT, 'mv wrfout_' + dom + '_' + yyyy + '-' + mm + '-' + dd_hh + $						;Print mv command
			 '\:' + min + '\:00 wrfout_' + dom2 + '_' + dates + '.nc'

	date = TIME_INC(date, dt)																					;Increment date
ENDWHILE

END
