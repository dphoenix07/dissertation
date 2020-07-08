PRO INSOLATION, wave0, wave1, temp

;+
; Name:
;               INSOLATION
; Purpose:
;               This is a procedure to plot daily insolation
; Calling sequence:
;               INSOLATION, 
; Inputs:
;               wave		: array of wavelengths in micrometers
;				temp		: temperature in kelvin
; Output:
;               value  	    : plot of planck curve 
;
; Author and history:
;               Daniel B. Phoenix	2017-03-31.
;
;-

COMPILE_OPT IDL2																			

;Constants

h  = 6.63e-34
c  = 3.00e8
k  = 1.38e-23
pi = 3.14

a     = 1.49597887E8
sc    = 1370

;PART 1
lat   = 0.0

;PART 2
;lat = 66.5 * (!PI/180)

a_arr = [1.000110,  0.034221,  0.000719]
b_arr = [0.000000,  0.001280,  0.000077]
c_arr = [0.006918, -0.399912, -0.006758, -0.002697]
d_arr = [0.000000,  0.070257,  0.000907,  0.000148]

sol_day  = JULDAY(6,20,2017)-JULDAY(1,1,2017)
equi_day = JULDAY(3,20,2017)-JULDAY(1,1,2017)

t_sol     = (2*pi*sol_day)/365
t_equi 	  = (2*pi*equi_day)/365

denom = 0
FOR n=0,2 DO BEGIN
	denom = denom + (a_arr[n]*COS(n*t_equi) + b_arr[n]*SIN(n*t_equi))
ENDFOR

r_day = a / (SQRT(denom))

;;;;
t    = (2*!PI*equi_day)/365

denom = 0
n=0
FOR n=0,2 DO BEGIN
	denom = denom + (a_arr[n]*COS(n*t) + b_arr[n]*SIN(n*t))
ENDFOR

r_day = a / (SQRT(denom))

decl_t = 0
n = 0

FOR n=0,3 DO BEGIN
	decl_t = decl_t + (c_arr[n]*COS(n*t) + d_arr[n]*SIN(n*t))
ENDFOR

omega = 2*!PI
H 	  = ACOS(-TAN(lat)*TAN(decl_t))

Q = (sc/!PI) * denom * (COS(decl_t)*COS(lat)*SIN(H)+H*SIN(decl_t)*SIN(lat))
PRINT, Q
;;;;

STOP

cos_theta=0
omega = 2*pi
H = ACOS(-TAN(lat)*TAN(decl))
H_arr = MAKEN(H,-H,24)

cos_arr = [ ]
FOR h = 0,23 DO BEGIN
	cos_theta = cos_theta + ((SIN(lat)*SIN(decl) + COS(lat)*COS(decl)*COS(H_arr[h]))/omega)
	cos_arr = [cos_arr, ((SIN(lat)*SIN(decl) + COS(lat)*COS(decl)*COS(H_arr[h]))/omega)]
ENDFOR

q = sc * (a/r_day)^2 * cos_theta


q_alt = (sc/pi) * denom * (COS(decl)*COS(lat)*SIN(H)+H*SIN(decl)*SIN(lat)) *15.0
;PRINT, q_alt

STOP




END 