PRO SUN_IR, wave0, wave1, temp

;+
; Name:
;               SUN_IR
; Purpose:
;               This is a function to plot the irradiance. For exam #6
; Calling sequence:
;               SUN_IR, 0.1, 4, 5800
;				SUN_IR, 4, 7, 5800  
;				SUN_IR, 7, 2000, 5800
; Inputs:
;               wave		: array of wavelengths in micrometers
;				temp		: temperature in kelvin
; Output:
;               value  	    : plot of planck curve 
;
; Author and history:
;               Daniel B. Phoenix	2017-03-30.
;
;-

COMPILE_OPT IDL2																			

;Constants

h  = 6.63e-34
c  = 3.00e8
k  = 1.38e-23
pi = 3.14

c1 = 1.191042E8
c2 = 1.4387752E4

wave = MAKEN(wave0, wave1, 4*(wave1-wave0))
lambda = wave * 1e-6

numer = (2*h*(c^2)) / (lambda^5)   
denom = exp((h*c)/(k*temp*lambda))-1

irrad = pi * ((numer/denom)*1e-6)
radi  = ((numer/denom)*1e-6)

;plot = PLOT(wave, irrad)

PLOT,  wave, irrad
OPLOT, wave, radi

E_tot = 6.41645E7

wave_i = SIZE(wave,/DIMENSIONS)
wave_i = wave_i[0]

irrad_tot = 0
FOR i = 0, wave_i -1 DO BEGIN
	irrad_tot = irrad_tot + irrad[i]
ENDFOR

PRINT, 'Total irradiance'
PRINT, E_tot
PRINT, 'Irradiance in band'
PRINT, irrad_tot/(wave1-wave0)
STOP

END 