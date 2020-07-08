PRO CFD6_ALT2, $
	LEAPFROG2 = leapfrog2, $
	ASSELIN   = asselin, $
	LEAPFROG4 = leapfrog4, $
	PLT_TIMES = plt_times
	

;+
; Name:
;		CFD6
; Purpose:
;		
; Calling sequence:
;		CFD6
; Example: CFD6
; Input:
;		c = courant number
; Output:
;		
; Keywords:
;		None.
; Author and history:
;		Daniel B. Phoenix	    2017-11-19. 
;-

COMPILE_OPT IDL2	

IF (N_ELEMENTS(courant) EQ 0) THEN courant = 0.1

courant = [0.1, 0.5, 0.7]

;Constants
pi = 3.141

c = 1.0
delta_x = 1.0/32.0
ep = 0.1

total_time = 3.0

L  = 1.0
x  = [(-2*delta_x):(L+2*delta_x):delta_x]
nx = N_ELEMENTS(x) 
colors = [COLOR_24('black'), COLOR_24('red'), COLOR_24('blue'), COLOR_24('green')]

a = 0
FOREACH cour, courant DO BEGIN
	delta_t = (cour*delta_x)/c
	n_times = (total_time/delta_t) + 1
	times = [0:total_time:delta_t]
	
	u = FLTARR(nx, n_times)
	u0 = FLTARR(nx)
		
	FOR i = 0, nx-1 DO BEGIN
		IF (x[i] LT (3.0/8.0)) THEN u_0 = 0.0
		IF (x[i] GT (5.0/8.0)) THEN u_0 = 0.0
		IF (x[i] GE (3.0/8.0)) AND (x[i] LE (5.0/8.0)) THEN u0[i] = [64.0*(((x[i])-0.5)^2 - (1.0/64.0))]^2
	ENDFOR
	
	u[*,0] = u0
	u_star = u
	FOR t = 1, N_ELEMENTS(times)-2 DO BEGIN
		IF (KEYWORD_SET(leapfrog2)) THEN BEGIN
			FOR i = 0, nx-2 DO BEGIN
				u[i,t+1] = u[i,t-1]-((c*delta_t)/(delta_x))*(u[i+1,t]-u[i-1,t])
			ENDFOR
		ENDIF

		IF (KEYWORD_SET(asselin)) THEN BEGIN
			FOR i = 0, nx-2 DO BEGIN
				u_star[i,t+1] = u[i,t-1] - c*delta_t*(u[i+1,t]-u[i-1,t])/delta_x
			ENDFOR
			
			FOR i = 0, nx-2 DO BEGIN
				u[i,t] = u_star[i,t] + ep*(u_star[i,t+1]-2.0*u_star[i,t]+u[i,t-1])
			ENDFOR
		ENDIF

		IF (KEYWORD_SET(leapfrog4)) THEN BEGIN
			FOR i = 0, nx-3 DO BEGIN
				u[i,t+1] = u[i,t-1] - c*(2.0*delta_t)*[(4.0/3.0)*((u[i+1,t]-u[i-1,t])/(2.0*delta_x)) - (1.0/3.0)*((u[i+2,t]-u[i-2,t])/(4.0*delta_x))]
			ENDFOR
		ENDIF	
				
		;Set periodic boundary conditions
		u[0,t+1]    = u[L,t+1]
;		u[nx-1,t+1] = u[2,t]
		u[1,t+1]    = u[L+1,t+1]

		u[nx-3,t+1] = u[2,t]
		u[nx-2,t+1] = u[3,t+1]
		u[nx-1,t+1] = u[4,t+1]
		
		time = times[t+1]			

		PRINT, 'Working on ', STRING(t), ' delta_t = ', delta_t

	IF KEYWORD_SET(plt_times) THEN BEGIN
		IF (t*delta_t EQ 2.9) THEN BEGIN 
			t1 = 1.0/delta_t
			t2 = 2.0/delta_t
			t3 = 2.9/delta_t
			PLOT, u[2:35,0],  $   
		  			TITLE    = 'Time =' + STRING(t), $;STRING(plt_t), $
		  			YRANGE   = [-0.4, 1.4], $
		  			XTITLE   = 'X - Distance (m)', $
		  			YTITLE   = 'U (m/s)', $
		  			COLOR    = colors[0]
		  	OPLOT, u[2:35,t1], COLOR=colors[1]
		  	OPLOT, u[2:35,t2], COLOR = colors[2]
		  	OPLOT, u[2:35,t3], COLOR = colors[3]
		  ENDIF
	ENDIF
				
		var_analyt = SQRT(VARIANCE(u[*,0]))
		var_discrete = SQRT(VARIANCE(u[*,t]))
		covariance = CORRELATE(u[*,0],u[*,t],/COVARIANCE)

		correl = CORRELATE(u[*,0],u[*,t])
		dissipation = ((var_analyt - var_discrete)^2 + ((MEAN(u[*,0]) - MEAN(u[*,t]))^2))
		dispersion  = 2*(1-correl)*var_analyt*var_discrete
		diffusion   = dissipation + dispersion
		
		PRINT, 'The Dissipation error: ', dissipation
		PRINT, 'The Dispersion error: ', dispersion
		PRINT, 'The Diffusion error: ', diffusion
		PRINT, ' '
	
	ENDFOR

	
	IF (cour EQ 0.1) THEN u1 = u[2:L+3,t-5]
	IF (cour EQ 0.5) THEN u2 = u[2:L+3,t-1]
	IF (cour EQ 0.7) THEN u3 = u[2:L+3,t]	

ENDFOREACH
	
	PLOT, u[2:L+3,0],  $   
			TITLE    = 'Time =' + STRING(t), $;STRING(plt_t), $
			YRANGE   = [-0.4, 1.4], $
			XTITLE   = 'X - Distance (m)', $
			YTITLE   = 'U (m/s)', $
		  	COLOR    = colors[0]
	
	OPLOT, u1, COLOR = colors[1]
	OPLOT, u2, COLOR = colors[2]
	OPLOT, u3, COLOR = colors[3] 


stop
END