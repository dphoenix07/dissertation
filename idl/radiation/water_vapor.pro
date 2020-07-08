PRO WATER_VAPOR

;+
; Name:
;               WATER_VAPOR
; Purpose:
;               This is a procedure to calculate transmissivity and the weighting
;				function. For exam #8
; Calling sequence:
;               WATER_VAPOR
; Inputs:
;            
;			
; Output:
;               
;
; Author and history:
;               Daniel B. Phoenix	2017-05-03.
;
;-

COMPILE_OPT IDL2	

x=FINDGEN(40)*1.0E3   

rohv = 0.804 	
k 	 = rohv*A
H 	 = 8000
qa   = 392E-6

FOR i=0,2 DO BEGIN

	op_depth = k[i]*roh0*H*qa*EXP(-x/H)
	trans    = EXP(-op_depth/0.5)			;0.5 = cos(60) 

	beta_a = k[i]*qa*roh0*EXP(-x/H)
	
	W = (beta_a/0.5)*trans
	
	PLOT, W, x
	
	imax = WHERE(MAX(W) EQ W, count)
	
	PRINT, 'k = ', k[i]
	PRINT, W[imax]
	PRINT, x[imax]

ENDFOR

STOP
END