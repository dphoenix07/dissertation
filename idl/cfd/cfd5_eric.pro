PRO CFD5_ERIC


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

;Constants
pi = 3.141

c = 1.0
delta_x = 1.0

courant = [0.10, 0.25, 0.50, 1.0]
total_time = 400.0

L = 50
total_length = L+3
x = [-1:total_length]
curr_solution = FLTARR(total_length)

soln_mtx = FLTARR(total_length,5,N_ELEMENTS(courant))

!P.MULTI = [0,2,2]

FOREACH cour, courant DO BEGIN
	delta_t = (cour*delta_x)/c
	n_times = (total_time/delta_t) + 1
	times = [0:total_time:delta_t]
	
	u = FLTARR(total_length, n_times)

	FOR i = 0, total_length-1 DO BEGIN
		IF (x[i] LT 9 ) OR  (x[i] GT 40) THEN u_0 = 0.0
		IF (x[i] GE 9 ) AND (x[i] LE 29) THEN u_0 = -1.0
		IF (x[i] GT 29) AND (x[i] LE 40) THEN u_0 = 1.0

		;Set initial condition
		term1 = 1+0.3*SIN((2*pi*x[i])/(9*delta_x))
		term2 = 1+0.4*SIN((2*pi*x[i])/(10*delta_x))
		u[i,0] = 2 + u_0*term1*term2
		curr_solution[i] = u[i,0]
		soln_mtx[i,0,cour] = curr_solution[i]
	ENDFOR
	
	predictor = u

	FOR t = 0, N_ELEMENTS(times)-2 DO BEGIN
		FOR i = 1, total_length-2 DO BEGIN
			predictor[i,t+1] = u[i,t]-c*delta_t*(u[i+1,t]-u[i,t])/delta_x
		ENDFOR
		
		FOR i = 1, total_length-2 DO BEGIN
			u[i,t+1]=0.5*(u[i,t]+predictor[i,t+1]-c*delta_t*(predictor[i,t+1]-predictor[i-1,t+1])/delta_x)
		ENDFOR
		
		u[0,t+1] = u[L,t+1]
		u[total_length-1,t+1] = u[2,t]
		u[1,t+1] = u[L+1,t+1]
		
		curr_solution = u[*,t+1]
		time = times[t+1]			

			
	plt_times = [0.0,50.0,100.0,200.0,400.0]
	FOREACH plt_t, plt_times DO BEGIN
		IF ((t * delta_t) EQ plt_t) THEN BEGIN
			PRINT, 'Working on ', STRING(t*delta_t), ' delta_t = ', delta_t
		    time = STRING(t*delta_t)
		    plt = PLOT(u[*,t], $
		    	TITLE  = 'Time= ' + STRING(plt_t) + ' Timestep= ' + STRING(delta_t))
		
		    var_analyt = SQRT(VARIANCE(u[*,0]))
		    var_discrete = SQRT(VARIANCE(u[*,t]))
		    covariance = CORRELATE(u[*,0],u[*,t],/COVARIANCE)
;		correl = CORRELATE(u[*,0],u[*,jj])
;		dissipation = ((var_analyt - var_discrete)^2 + ((MEAN(u[*,0]) - MEAN(u[*,jj]))^2))
;		dispersion = 2*(1-correl[0,1])*var_analyt*var_discrete
		
;		PRINT, 'The Dissipation error: ', dissipation
;		PRINT, 'The Dispersion error: ', dispersion
;		PRINT, ' '
		ENDIF
	ENDFOREACH 

	ENDFOR

ENDFOREACH

STOP
END