PRO PBLH, event, date

;+
; Name:
;		PBL_VERT_PROFILE
; Purpose:
;		This is a procedure to plot wind and ozone profiles in the lowest 2 km.
; Calling sequence:
;		PBL_VERT_PROFILE
; Input:
;		event    	: string of case date (e.g., '20120519')
;		start_date  : start time to plot
;		dt			: time increment
; Output:
;		plots the vertical profiles of user selected variables at three times
; Keywords:
;		REGION	  : Region to average wind and ozone over.
;		EPS       : If set, output to PostScript.
;		PDF       : If set, output to PDF.
;		PNG       : If set, write PNG image.
; Author and history:
;		Daniel B. Phoenix	2016-04-08.
;							2016-08-03.	Added keyword option to use code written for
;										air pollution project (O3, WSP, NOx profiles for
;										six different times) and added section to compare
;										PBL schemes at three different times.
;-

COMPILE_OPT IDL2																			;Set compile options

ERASE

schemes = ['nssl_ysu','nssl_qnse','nssl_acm2']
region  = [50, 50, 250, 190]															;Subset region 

pos1 = [0.100, 0.400, 0.700]
pos2 = [0.220, 0.220, 0.220]
pos3 = [0.350, 0.650, 0.950]
pos4 = [0.920, 0.920, 0.920]

FOR s=0,2 DO BEGIN
	var  = (WRF_READ_VAR('PBL', date, event, schemes[s], DOMAIN = domain, INDICES = region)).values
	hist 	 	 = HISTOGRAM(var, MIN = MIN(var))						 				;Calculate density of values
	bigfreq  	 = MAX(hist)															;Find largest density value
	mode 	 	 = WHERE(hist EQ bigfreq) + MIN(var)									;Find primary tropopause mode

	PRINT, STRING(schemes[s]) + " mode: " + STRING(mode)
	PRINT, STRING(schemes[s]) + " mean: " + STRING(MEAN(var))

	PLOT, hist, $
		TITLE  	 = schemes[s], $
		XRANGE   = [0, 3000], $
		YRANGE   = [0, 1000], $
		NOERASE  = 1, $
		POSITION = [pos1[s], pos2[s], pos3[s], pos4[s]]
	
ENDFOR

END
