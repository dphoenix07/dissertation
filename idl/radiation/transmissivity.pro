PRO TRANSMISSIVITY

;+
; Name:
;               TRANSMISSIVITY
; Purpose:
;               This is a procedure to calculate transmissivity and the weighting
;				function. For exam #7
; Calling sequence:
;               TRANSMISSIVITY
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


;qa   = 392E-6


COMPILE_OPT IDL2	

x=FINDGEN(40)*1.0E3   

k 	 = [0.05, 0.1, 0.15]
roh0 = 4E-3
H 	 = 8000


FOR i=0,2 DO BEGIN

	op_depth = k[i]*roh0*H*EXP(-x/H)
	trans    = EXP(-op_depth/0.5)			;0.5 = cos(60) 

	beta_a = k[i]*roh0*EXP(-x/H)
	
	W = (beta_a/0.5)*trans
	
	PLOT, W, x
	
	imax = WHERE(MAX(W) EQ W, count)
	
	PRINT, 'k = ', k[i]
	PRINT, W[imax]
	PRINT, x[imax]

ENDFOR

STOP
END