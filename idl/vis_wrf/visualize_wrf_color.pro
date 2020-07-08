FUNCTION VISUALIZE_88D_COLOR, table

;+
; Name:
;		VISUALIZE_88D_COLOR
; Purpose:
;		This is a function to return color tables for VISUALIZE_88D plots. 
; Calling sequence:
;		value = VISUALIZE_88D_COLOR()
; Inputs:
;		table: Optional index of color table. Default is 0.
; Output:
;		15-element array of 24-bit color values.
; Keywords:
;		None.
; Author and history:
;		Cameron R. Homeyer  2015-01-22
;-

COMPILE_OPT IDL2

IF (N_ELEMENTS(table) EQ 0) THEN table = 0																;Set default color table

CASE (table) OF 
	0 : BEGIN
		r = [049,030,015,150,078,015,255,217,255,198,255,109,255,255,255]							;RGB color values
		g = [239,141,056,220,186,097,222,164,107,059,000,000,000,171,255]
		b = [237,192,151,150,025,003,000,000,000,000,000,000,255,255,255]
		RETURN, COLOR_24(r,g,b)																					;Return 24-bit color table
		END
	1 : BEGIN
		r = [000,064,131,000,000,000,005,005,105,255,255,255,255,255,255]							;RGB color values
		g = [000,064,131,024,132,255,192,125,192,255,147,036,000,171,255]
		b = [000,064,131,255,255,255,127,000,000,000,008,015,255,255,255]
		RETURN, COLOR_24(r,g,b)																					;Return 24-bit color table
		END
	2 : BEGIN
		r = [058,098,093,022,032,146,225,249,248,217,177,143,212,235,194]							;RGB color values
		g = [000,064,159,124,153,200,225,213,179,148,117,092,070,045,016]
		b = [140,220,195,014,024,144,225,016,024,042,057,070,101,090,050]
		RETURN, COLOR_24(r,g,b)																					;Return 24-bit color table
		END
	3 : RETURN, GRAYSCALE_24(15)
	4 : RETURN, BLUE_RED_24(30, 0.05, 0.8)
ENDCASE

END
