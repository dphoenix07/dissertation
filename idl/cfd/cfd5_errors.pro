PRO CFD5_errors, c   


;+
; Name:
;		CFD5_errors
; Purpose:
;		
; Calling sequence:
;		CFD5_errors
; Example: CFD5_errors
; Input:
;		c = courant number
; Output:
;		
; Keywords:
;		None.
; Author and history:
;		Daniel B. Phoenix	    2017-11-13. 
;-

COMPILE_OPT IDL2	


!P.MULTI = [0,2,1]

;Constants
pi = 3.141

c = [0.125,0.25,0.5,0.75]

a= 0
colors = [COLOR_24('black'), COLOR_24('red'), COLOR_24('blue'), COLOR_24('green'), COLOR_24('gray50'), $
			COLOR_24('orange'), COLOR_24('purple')]
FOREACH u, c DO BEGIN

    delta_x = 1.0
    
    wavelengths = [4:1:-0.1]
    k  = (2*pi)/(wavelengths*delta_x)
    
    lambda_imag = u*SIN(k*delta_x)
    lambda_real = 1-2*(u^2)*(SIN((k*delta_x)/2))^2
    
    theta_d = ATAN(lambda_imag/lambda_real)
    
    theta_a = -u*k*delta_x
    phase_err = theta_d/theta_a
    
    amplitude = (1 - 4*u^2*(SIN((k*delta_x)/2)) + 4*u^4*(SIN((k*delta_x)/2))^4 + u^2*(SIN(k*delta_x))^2)
    

  	p1 = PLOT( phase_err, OVERPLOT = 1, COLOR = colors[a], TITLE = 'Phase Error', $
  				XTITLE = 'Wavelength', YTITLE = 'Error') 
;    p2 = PLOT( amplitude, OVERPLOT = 1, COLOR = colors[a], TITLE = 'Amplitude Error', $
;    			XTITLE = 'Wavelength', YTITLE = 'Error')
    
    a = a+1
ENDFOREACH

STOP
END