PRO OPTICAL_DEPTH

;+
; Name:
;               OPTICAL_DEPTH
; Purpose:
;               This is a procedure to plot optical depth. For exam #10
; Calling sequence:
;               OPTICAL_DEPTH
; Inputs:
;               wave		: array of wavelengths in micrometers
;				temp		: temperature in kelvin
; Output:
;               value  	    : plot of planck curve 
;
; Author and history:
;               Daniel B. Phoenix	2017-04-03.
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
color = [COLOR_24_DP('RED'), COLOR_24_DP('BLUE'), COLOR_24_DP('YELLOW'), COLOR_24_DP('gray50')]

sigma = 1.0E-36

z = [0, 2, 4, 6, 8, 10, 12, 14, 16]
N = [25.5, 20.9, 17.0, 13.7, 10.9, 8.60, 6.49, 4.74, 3.46]
N_fact = N*10^18

;PLOT, z, N

x = FINDGEN(17)
yfit = 0.053*x^2 - 2.1926*x + 25.5 

xfit = 0.017667*x^2 - 1.0963*x^2 + 25.5*x
tau = yfit*x
tau_alt = (xfit*x*sigma)

p  = PLOT( REVERSE(tau_alt)*(1/COS(30)),x, COLOR = color[0], NAME = '30 degrees', $
		YTITLE = 'Altitude (km)', XTITLE = 'tau', TITLE = 'Optical Depth')
p1 = PLOT(REVERSE(tau_alt),x, COLOR = color[1], NAME = 'Tau', /OVERPLOT)
p2 = PLOT(REVERSE(tau_alt)*(1/COS(0)),x, COLOR = color[2], NAME = '0 degrees', /OVERPLOT)
p3 = PLOT(REVERSE(tau_alt)*(1/COS(70)),x, COLOR = color[3], NAME = '70 degrees', /OVERPLOT)


leg = LEGEND(TARGET=[p], /DATA, /AUTO_TEXT_COLOR)

PRINT, x
PRINT, REVERSE(tau_alt)*(1/COS(30))
PRINT, x
PRINT, REVERSE(tau_alt)*(1/COS(70))
PRINT, x
PRINT, REVERSE(tau_alt)*(1/COS(0))







 
STOP
PLOT, tau, x					;part a
;OPLOT, tau/COS(30), x
;OPLOT, tau/COS(70), x
OPLOT, tau*.7, x
OPLOT, tau*.3, x

STOP
PRINT, FORMAT = '("Wavelength (user-defined)", 5X, "Planck (W/m2/um/sr)", 5X, "Rayleigh-Jean (W/m2/um/sr)", 5X, "Difference")'
PRINT, '==================================================================================================='

wave_i = SIZE(wave,/DIMENSIONS)-1
wave_i = wave_i[0]

FOR i = 0, wave_i DO BEGIN
    PRINT, FORMAT = '(E10.4, 24X, E10.4, 16X, E10.4, 15X, E10.2)', wave[i], irrad[i], rj_appr[i], (irrad[i]- rj_appr[i])
ENDFOR


STOP

END 