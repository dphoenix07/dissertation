PRO PRINT_WRF_TROP, event, date

;+
; Name:
;               PRINT_WRF_TROP
; Purpose:
;               This is a procedure to print a table of the mean and most frequent
;				tropopause value for three Microphysics schemes.
; Calling sequence:
; 				PRINT_WRF_TROP, event, date              
; Inputs:
;               event : String variable of the event (e.g., '20120519')
;				date  : Date desired (e.g., MAKE_DATE(2012,5,19,22))
; Output:
;               Table of most frequent tropopause mode value and mean tropopause for
;				each MP scheme.
; Keywords:
;               None. 
; Author and history:
;               Daniel B. Phoenix	2016-01-19.
;
;-

COMPILE_OPT IDL2

IF (N_ELEMENTS(event) EQ 0) THEN event = '20120519'
IF (N_ELEMENTS(date ) EQ 0) THEN date  = MAKE_DATE(2012,5,19,22,00)

scheme = ['morrison', 'milbyau', 'nssl']													;Define MP schemes
trop_mean = FINDGEN(3)	

FOR i = 0, N_ELEMENTS(scheme) - 1 DO BEGIN													
	indir = !WRF_DIRECTORY + event + '/' + STRING(scheme[i]) + '/reduced/' 
	z_trop = (WRF_READ_VAR('Z_trop', date, event, STRING(scheme[i]))).values 						;Read in Z_trop for each scheme

	z_trop	     = CALC_TROP_MODE(z_trop, scheme, i = i)									;Call CALC_TROP_MODE for each scheme
	trop_mean[i] = MEAN (z_trop)															;Take mean of tropopause values

ENDFOR


PRINT, FORMAT = '("MP Scheme", 5X, "Tropopause Mean (m)")'
PRINT, '================================='

FOR i = 0, N_ELEMENTS(scheme) - 1 DO BEGIN 
	PRINT, FORMAT = '(A8, 7X, F10.2)', STRING(scheme[i]), STRING(trop_mean[i])							
ENDFOR

END


