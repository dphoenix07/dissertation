PRO CFD5_ALT, t_final


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
;courant = (c*delta_t)/delta_x
; c = 1.0
; delta_x = 1.0
delta_t = [1.0];, 0.25, 0.50, 1.0]
t_final = 10.0 

x  = 50
nx = x+3
x0 = 1
!P.MULTI = [0,2,2]

FOREACH dt, delta_t DO BEGIN
	num_times = t_final/dt
	u  = FLTARR(x+3, num_times)
	predictor = FLTARR(x+3, num_times)
	dim = SIZE(u, /DIMENSIONS)

	FOR jj = 0, num_times-2 DO BEGIN
		IF (jj EQ 0) THEN BEGIN
			FOR ii = 1, 51 DO BEGIN
				IF (ii LT 9 ) OR  (ii GT 40) THEN u_0 = 0.0
				IF (ii GE 9 ) AND (ii LE 29) THEN u_0 = -1.0
				IF (ii GT 29) AND (ii LE 40) THEN u_0 = 1.0

				;Set initial condition
				u[ii,jj ] = 2 + u_0*(1+0.3*sin((2*pi*ii)/(9*delta_x)))*(1+(0.4*sin((2*pi*ii)/(10*delta_x))))

				;Set periodic boundary condition
;				u[0,*] = u[50,0]
;				u[1,*] = u[51,0]
;				u[2,*] = u[52,0]
;;				u[0 ,*] = u[0 ,0]
;;				u[52,*] = u[52,0]
;;				u[1 ,*] = u[51,0] 
			ENDFOR
		ENDIF 
		IF (jj GE 1) THEN BEGIN
;				u[0,jj-1] = u[50,jj-1]
;				u[51,jj-1] = u[1,jj-1]
			FOR ii = 1, 51 DO BEGIN				
;				predict_plus = u[ii,jj-1] - (c*dt*(u[ii+1,jj-1]-u[ii,jj-1]))
;				predict_min  = u[ii-1,jj-1] - (dt*(u[ii,jj-1]-u[ii-1,jj-1]))
;				u[ii,jj] = 0.5*u[ii,jj-1] + predict_plus - (dt*(predict_plus - predict_min))

				predictor[ii,jj+1] = u[ii,jj]-c*dt*(u[ii+1,jj]-u[ii,jj])/delta_x
			ENDFOR
			
			FOR ii = 1, 51 DO BEGIN
					u[ii,jj+1] = 0.5*(u[ii,jj]+predictor[ii,jj+1]-c*dt*(predictor[ii,jj+1]-predictor[ii-1,jj+1])/delta_x)
			ENDFOR
						
			PRINT, u[0,jj]
			PRINT, u[51,jj]


			;Reset periodic boundary condition
			u[0,jj+1] = u[50,jj+1]
			u[1,jj+1] = u[51,jj+1]
			u[52,jj+1] = u[2,jj]
		
		ENDIF
				
			
	plt_times = [0.0,2.0];50.0,100.0,200.0,400.0]
	FOREACH plt_t, plt_times DO BEGIN
		IF ((jj * dt) EQ plt_t) THEN BEGIN
			PRINT, 'Working on ', STRING(jj*dt), ' delta_t = ', dt
		    time = STRING(jj*dt)
		    plt = PLOT(u[*,jj], $
		    	TITLE  = 'Time= ' + STRING(plt_t) + ' Timestep= ' + STRING(dt))
		
		    var_analyt = SQRT(VARIANCE(u[*,0]))
		    var_discrete = SQRT(VARIANCE(u[*,jj]))
		    covariance = CORRELATE(u[*,0],u[*,jj],/COVARIANCE)
;		correl = CORRELATE(u[*,0],u[*,jj])
;		dissipation = ((var_analyt - var_discrete)^2 + ((MEAN(u[*,0]) - MEAN(u[*,jj]))^2))
;		dispersion = 2*(1-correl[0,1])*var_analyt*var_discrete
		
;		PRINT, 'The Dissipation error: ', dissipation
;		PRINT, 'The Dispersion error: ', dispersion
;		PRINT, ' '
		ENDIF
	ENDFOREACH 

	ENDFOR
; map_bar_title = 'U (m/s)'														
; map_bar_min   = 0.0																	
; map_bar_max   = 5.0																	
; map_bar_ticks = 4																		
; map_table     = [VISUALIZE_88D_COLOR(3)]												
; map_levels    = [0.5*FINDGEN(N_ELEMENTS(map_table))]							
; 
; map_position  = [0.07, 0.20, 0.47, 0.95]
; bar_pos	   = [0.25, 0.07, 0.75, 0.10]

;x_arr = FLTARR(nx)
;time_arr = FLTARR(ny)
;CONTOUR, u, REBIN(x_arr,dim[0],dim[1],/SAMPLE), $
;			REBIN(REFORM(time_arr,1,dim[1]),[dim[0],dim[1]],/SAMPLE), $				
;    FILL      = 1, $
;    XRANGE    = [0, 50.0], $
;    YRANGE    = [0, 50.0], $
;    XTITLE    = 'X (m)', $
;    YTITLE    = 'Time (s)', $
;    LEVELS    = map_levels, $
;    C_COLOR   = map_table, $
;    POSITION  = map_position
;
;COLOR_BAR_24_KPB, map_table[1:*], OVER = map_table[-1], UNDER = map_table[0], $						
;	TICKS = map_bar_ticks, $
;	RANGE = [map_bar_min, map_bar_max], $
;	TITLE = map_bar_title, $
;	POSIT = bar_pos

ENDFOREACH

STOP
END