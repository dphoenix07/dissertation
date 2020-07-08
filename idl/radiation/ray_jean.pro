PRO RAY_JEAN, wave0, wave1, temp, factor

;+
; Name:
;               RAY_JEAN
; Purpose:
;               This is a function to plot the irradiance. For exam #9
; Calling sequence:
;               RAY_JEAN, 1, 4, 303, 1E-3
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
color = [COLOR_24_DP('RED'), COLOR_24_DP('BLUE')]


lambda = wave * factor
numer = (2*h*(c^2)) / (lambda^5)   
denom = exp((h*c)/(k*temp*lambda))-1

irrad = ((numer/denom))

rj_appr = (2*c*k*temp)/(lambda^4)
;plot = PLOT(wave, irrad)

p =PLOT(wave, irrad, XLOG=1, YTITLE = 'Irradiance (W/m2/um/sr)', XTITLE='Wavelength x 10^-4 (m)', $
			COLOR = color[0], NAME='Planck')
p1=PLOT(wave, rj_appr,/OVERPLOT, COLOR = color[1], NAME='Approx.')

leg = LEGEND(TARGET=[p], /DATA, /AUTO_TEXT_COLOR)


;i_4  = WHERE(ABS(wave-4 ) LT 0.1) 
;i_10 = WHERE(ABS(wave-10) LT 0.1) 
;i_30 = WHERE(ABS(wave-30) LT 0.1) 
;label = ['4','10','30']

PRINT, FORMAT = '("Wavelength (user-defined)", 5X, "Planck (W/m2/um/sr)", 5X, "Rayleigh-Jean (W/m2/um/sr)", 5X, "Difference")'
PRINT, '==================================================================================================='

wave_i = SIZE(wave,/DIMENSIONS)-1
wave_i = wave_i[0]

FOR i = 0, wave_i DO BEGIN
    PRINT, FORMAT = '(E10.4, 24X, E10.4, 16X, E10.4, 15X, E10.2)', wave[i], irrad[i], rj_appr[i], (irrad[i]- rj_appr[i])
ENDFOR


STOP

END 