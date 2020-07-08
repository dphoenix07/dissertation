FUNCTION CALC_TROP_MODE_OBS, z_trop, threshold, $
		 TABLE = table, $
		 i     = i


;+
; Name:
;               CALC_TROP_MODE_OBS
; Purpose:
;               This is a function to calculate the most frequent altitude of the 
;				tropopause (m) and filter out values larger than 1 km above or below
;				the mode.
; Calling sequence:
;               value = CALC_TROP_MODE_OBS(z_trop, threshold)
; Inputs:
;               z_trop     : Array of tropopause altitudes (m)
;				threshold  : Distance above tropopause mode to filter out (e.g., 1000.0)
; Output:
;               value  	   : Array of filtered tropopause heights (m). 
; Keywords:
;               table 	   : If table set, will save and return mean and mode.
; Author and history:
;               Daniel B. Phoenix	2016-01-19.
;
;-

COMPILE_OPT IDL2																			;Set compile options

IF (N_ELEMENTS (threshold) EQ 0) THEN threshold = 1000.0
IF (N_ELEMENTS (i		 ) EQ 0) THEN i 		= 0

dbl_trop = 0
answer = ''


dim = SIZE(z_trop, /DIMENSIONS)																;Get matrix dimensions

hist    = HISTOGRAM(z_trop, MIN = MIN(z_trop))						 						;Calculate density
bigfreq = MAX(hist)																			;Find most largest density value

mode      = WHERE(hist EQ bigfreq) + MIN(z_trop)											;Find tropopause mode
PRINT, 'Tropopause mode is: ' + STRING(mode) $												;Check for two modes
		+ !NEW_LINE 
		

modes = SIZE(mode, /DIMENSIONS)																;Check for multiple modes
IF modes GT 1 THEN BEGIN																	;Ask user if averaging modes is desired
	PRINT, 'There are more than one mode.'
	PRINT, 'The modes are (in meters): ' 
	PRINT, mode
	READ, answer, PROMPT = 'Would you like to average the modes? Enter "y" or "n": '
		IF answer EQ 'y' THEN mode = MEAN(mode)
		IF answer EQ 'n' THEN BEGIN															;If averaging not desired, user enters 
			READ, answer, PROMPT = 'Enter mode to use: '									;tropopause mode (e.g., 16705.0)
			mode = FLOAT(answer)
		ENDIF
ENDIF

IF mode GT 13000.0 THEN BEGIN																;If tropopause mode is greater than 13 km
	PRINT, 'The most frequent tropopause value is: ' + STRING(mode) $					
		  + ' m.'
;	READ, answer, $																			;Prompts user if they want to filter data
;	PROMPT = 'It appears there is a secondary tropopause, would you like to filter ' $
;		  + 'it out? Enter "y" or "n": ' 
;	IF answer EQ 'y' THEN BEGIN 
;		dbl_trop = 1
;		READ, upper_val, $
;		PROMPT = 'Enter filtering threshold (in m): '										;Enter tropopause height above which to set to
;	ENDIF ELSE IF answer EQ 'n' THEN dbl_trop = 0											;primary tropopause mode (e.g., 15000.0 m)
ENDIF

IF KEYWORD_SET (dbl_trop) THEN BEGIN
	prim_trop = WHERE(z_trop LT upper_val, trop_count, COMPLEMENT = sec_trop, $				;Find values below 'upper_trop'
					  NCOMPLEMENT = sec_count)	
	IF (trop_count GT 0 AND sec_count GT 0) THEN BEGIN
		lower_trop   = z_trop[prim_trop]													;Store values below 'upper_trop'

		hist 	 	 = HISTOGRAM(lower_trop, MIN = MIN(lower_trop))						 	;Calculate density of values
		bigfreq  	 = MAX(hist)															;Find largest density value

		mode 	 	 = WHERE(hist EQ bigfreq) + MIN(lower_trop)								;Find primary tropopause mode
		mode_arr 	= (FLTARR(dim[0]) + 1.0) * mode											;Create array of primary tropopause mode values
		
		z_trop[prim_trop] = z_trop  [prim_trop]												;Filter z_trop: values below 14 km saved
		z_trop[sec_trop ] = mode_arr[sec_trop ]												;Values above 14 km set to primary tropopause mode		
		
		hist    = HISTOGRAM(z_trop, MIN = MIN(z_trop))						 				;Calculate density
		bigfreq = MAX(hist)																	;Find most largest density value

		mode      = WHERE(hist EQ bigfreq) + MIN(z_trop)									;Find tropopause mode		
		trop_diff = z_trop - mode

		a = WHERE(ABS(trop_diff) LT threshold, a_count, COMPLEMENT = b, $					;Find where absolute value of difference 
				  NCOMPLEMENT = b_count)													;btwn tropopause and mode are > 2km
	
		IF (a_count  GT 0) THEN z_trop[a] = z_trop[a]										;If difference is < 2km, keep original value
		IF (b_count  GT 0) THEN z_trop[b] = mode  [b]										;If difference is > 2km, assign mode value
	ENDIF
ENDIF
																						
trop_diff = z_trop - mode																	;Find difference between tropopause and mode

a = WHERE(ABS(trop_diff) LT threshold, a_count, COMPLEMENT = b, $							;Find where absolute value of difference
				  NCOMPLEMENT = b_count)													;btwn tropopause and mode are > 2km
	
IF (a_count  GT 0) THEN z_trop[a] = z_trop[a]												;If difference is < 2km, keep original value
IF (b_count  GT 0) THEN z_trop[b] = mode  [b]												;If difference is > 2km, assign mode value

	 
RETURN, z_trop

END