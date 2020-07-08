FUNCTION SY, t, temps, dim, dt

;+
; Name:
;               SY
; Purpose:
;
; Calling sequence:
;               value = SY(x,a,b,c,d)
; Inputs:
;               il     : 
; Output:
;               value  :  
; Keywords:
;				None.
; Author and history:
;               Daniel B. Phoenix	2017-10-28.
;
;-

COMPILE_OPT IDL2																			;Set compile options

;Define constants
pi 	    = 3.141
delta_x = 0.015
sig     = 3.0E-6
width   = 0.30
t_final = 30*60

const = (sig * dt)/(2*(delta_x^2))
D = FLTARR(dim[0])
B = FLTARR(dim[0])
A = - const
C = - const

FOR n = 1, dim[0]-2 DO BEGIN
	B[n] = 1 + ((sig * dt)/(delta_x^2))
				
	IF (n EQ 1) THEN BEGIN
		D[n] = ((const * (temps[n+1,t]-(2*(temps[n,t]))+temps[n-1,t]))+temps[n,t]) - (A*temps[n-1,t+1])
	ENDIF 
	
	IF (n GT 1) THEN BEGIN
		IF (n EQ dim[0]-2) THEN BEGIN
			D[n] = ((const*(temps[n+1,t]-(2*(temps[n,t]))+temps[n-1,t]))+temps[n,t])-(C*temps[n+1,t+1])
		ENDIF ELSE BEGIN
			D[n] = (const*(temps[n+1,t]-(2*(temps[n,t]))+temps[n-1,t]))+temps[n,t]			
		ENDELSE

		PRINT, 'initial D'
		PRINT, D		
		R     = A/B[n-1]
		B[n] = B[n] - (R*C)
		D[n] = D[n] - (R*D[n-1])
	ENDIF

ENDFOR

PRINT, 'Before Sub'
PRINT, D
; Back substitution

D[1] = D[1]/B[1]

FOR i = 2, dim[0]-2 DO BEGIN
	J = dim[0]-i
	PRINT, J
	D[J] = (D[J] - C * D[J+1]) / B[J]
ENDFOR

PRINT, 'After Sub'
PRINT, D

RETURN, D

END 

