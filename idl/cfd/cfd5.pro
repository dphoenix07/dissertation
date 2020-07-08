PRO CFD5, courant


;+
; Name:
;		CFD5
; Purpose:
;		
; Calling sequence:
;		CFD5
; Example: CFD5
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

IF (N_ELEMENTS(courant) EQ 0) THEN courant = 1.0

;Constants
pi = 3.141

c = 1.0
delta_x = 1.0

total_time = 500.0

L = 50
nx = L+3
x = [-1:nx]
colors = [COLOR_24('black'), COLOR_24('red'), COLOR_24('blue'), COLOR_24('green')]

!P.MULTI = [0,2,2]
a = 0
FOREACH cour, courant DO BEGIN
	delta_t = (cour*delta_x)/c
	n_times = (total_time/delta_t) + 1
	times = [0:total_time:delta_t]
	
	u = FLTARR(nx, n_times)

	FOR i = 0, nx-1 DO BEGIN
		IF (x[i] LT 9 ) OR  (x[i] GT 40) THEN u_0 = 0.0
		IF (x[i] GE 9 ) AND (x[i] LE 29) THEN u_0 = -1.0
		IF (x[i] GT 29) AND (x[i] LE 40) THEN u_0 = 1.0

		;Set initial condition
		u[i,0] = 2 + u_0*(1+0.3*SIN((2*pi*x[i])/(9*delta_x)))*(1+0.4*SIN((2*pi*x[i])/(10*delta_x)))
	ENDFOR
	
	pred = u

	FOR t = 0, N_ELEMENTS(times)-2 DO BEGIN
		;Calculate predictor
		FOR i = 1, nx-2 DO BEGIN
			pred[i,t+1] = u[i,t]-c*delta_t*(u[i+1,t]-u[i,t])/delta_x
		ENDFOR
		
		;Calculate corrector 
		FOR i = 1, nx-2 DO BEGIN
			u[i,t+1]=0.5*(u[i,t]+pred[i,t+1]-c*delta_t*(pred[i,t+1]-pred[i-1,t+1])/delta_x)
		ENDFOR
		
		;Set periodic boundary conditions
		u[0,t+1]    = u[L,t+1]
		u[nx-1,t+1] = u[2,t]
		u[1,t+1]    = u[L+1,t+1]
		
		time = times[t+1]			

	plt_times = [50.0,100.0,200.0,400.0]
	FOREACH plt_t, plt_times DO BEGIN
		IF ((t * delta_t) EQ plt_t) THEN BEGIN
			PRINT, 'Working on ', STRING(t*delta_t), ' delta_t = ', delta_t

			PLOT, u[*,t], 	 $   
		    			TITLE    = 'Time =' + STRING(plt_t), $
		    			YRANGE   = [0, 4], $
		    			XTITLE   = 'X - Distance (m)', $
		    			YTITLE   = 'U (m/s)'
			
			var_analyt = SQRT(VARIANCE(u[*,0]))
		    var_discrete = SQRT(VARIANCE(u[*,t]))
		    covariance = CORRELATE(u[*,0],u[*,t],/COVARIANCE)

			correl = CORRELATE(u[*,0],u[*,t])
			dissipation = ((var_analyt - var_discrete)^2 + ((MEAN(u[*,0]) - MEAN(u[*,t]))^2))
			dispersion = 2*(1-correl)*var_analyt*var_discrete
			
			PRINT, 'The Dissipation error: ', dissipation
			PRINT, 'The Dispersion error: ', dispersion
			PRINT, ' '

		ENDIF
	ENDFOREACH
	
	ENDFOR

ENDFOREACH

END