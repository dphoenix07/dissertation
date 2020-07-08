PRO PLANCK, wave0, wave1, temp

;+
; Name:
;               PLANCK
; Purpose:
;               This is a function to plot the irradiance. For exam #5
; Calling sequence:
;               PLANCK, 1, 50, 303
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
color 	   = [COLOR_24_DP('RED'), COLOR_24_DP('BLUE')]

wave = MAKEN(wave0, wave1, 4*(wave1-wave0))
lambda = wave * 1e-6

numer = (2*h*(c^2)) / (lambda^5)   
denom = exp((h*c)/(k*temp*lambda))-1

irrad = pi * ((numer/denom)*1e-6)
radi  = ((numer/denom)*1e-6)

rem_sens = pi* (c1/(wave^5 * EXP(c2/(wave*temp))-1))


;p=PLOT(wave, irrad, XTITLE = 'Wavelength (micron)', YTITLE = 'Radiance/Irradiance (W/m2/um)', COLOR=color[0], NAME='Irradiance')
;p1=PLOT(wave, radi, /OVERPLOT, COLOR=color[1], NAME='Radiance')


;leg = LEGEND(TARGET=[p], /DATA, /AUTO_TEXT_COLOR)


i_4  = WHERE(ABS(wave-4 ) LT 0.1) 
i_10 = WHERE(ABS(wave-10) LT 0.1) 
i_30 = WHERE(ABS(wave-30) LT 0.1) 
label = ['4','10','30']

PRINT, FORMAT = '("Wavelength (micron)", 5X, "Irradiance (W/m2/um/sr)", 5X, "Radiance (W/m2/um/sr)")'
PRINT, '==========================================================================='

PRINT, FORMAT = '(F10.2, 18X, F10.2, 15X, F10.2, 15X, F10.2)', '4  microns', irrad[i_4 ], radi[i_4 ], rem_sens[i_4 ]
PRINT, FORMAT = '(F10.2, 18X, F10.2, 15X, F10.2, 15X, F10.2)', '10 microns', irrad[i_10], radi[i_10], rem_sens[i_10]
PRINT, FORMAT = '(F10.2, 18X, F10.2, 15X, F10.2, 15X, F10.2)', '30 microns', irrad[i_30], radi[i_30], rem_sens[i_30]




;;Do exam #4 here

T = FINDGEN(7)+308.0
lambda = 2897/T

;p = PLOT(lambda,T, YRANGE =[308,314], XTITLE = 'Wavelength (microns)', YTITLE = 'Temperature (K)')
STOP

END 