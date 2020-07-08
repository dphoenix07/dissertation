PRO CFD4a


;+
; Name:
;		CFD4a
; Purpose:
;		This is a procedure to solve the 1-D heat transfer equation using the FTCS
;		method.
; Calling sequence:
;		CFD4a
; Input:
;		None.      
; Output:
;		Plots of the 1-D heat transfer equation at two timesteps, solved using the 
;		FTCS method.
; Keywords:
;		None.
; Author and history:
;		Daniel Phoenix  2017-10-25.
;-

COMPILE_OPT IDL2	


;Define constants
pi 	    = 3.141
delta_x = 0.015
delta_t = [20, 60]
sig     = 3.0E-6
width   = 0.30
t_final = 30*60

!P.MULTI = [0,2,1]

FOREACH dt, delta_t DO BEGIN
	;Define arrays and set IC/BC
	time_arr  	= [0:t_final:dt]
	x_arr	  	= [0:width:delta_x]
	temps   	= FLTARR(N_ELEMENTS(x_arr), N_ELEMENTS(time_arr))
	dim	 	    = SIZE(temps, /DIMENSIONS)
	temps[*,0 ] = 100.0
	temps[0,* ] = 300.0
	temps[20,*] = 300.0

	;Compute temperature using FTCS method
	FOR t = 0, N_ELEMENTS(time_arr)-2 DO BEGIN
		FOR n = 1, N_ELEMENTS(x_arr)-2 DO BEGIN
			temps[n,t+1] = sig*(dt/(delta_x^2))*(temps[n-1,t] - 2*temps[n,t] + temps[n+1,t]) + temps[n,t]
		ENDFOR
	ENDFOR

	;Set Color Bar
	 map_bar_title = 'Temperature (K)'														;Set color bar title
	 map_bar_min   = 100.0																						;Set echo top minimum
	 map_bar_max   = 300.0																		;Set echo top maximum
	 map_bar_ticks = 4																			;Set number of color bar ticks
	 map_table     = [VISUALIZE_88D_COLOR(3)]										;Set color table
	 map_levels    = [100.0 + 15.0*FINDGEN(N_ELEMENTS(map_table))]							;Set contour levels
	 
	 ;Set map positions
	 IF (dt EQ 20) THEN map_position = [0.07, 0.20, 0.47, 0.95]
     IF (dt EQ 60) THEN map_position = [0.55, 0.20, 0.95, 0.95]
     bar_pos	   = [0.25, 0.07, 0.75, 0.10]

	;Produce contour plot
	CONTOUR, temps, REBIN(x_arr,dim[0],dim[1],/SAMPLE), $
				REBIN(REFORM(time_arr,1,dim[1]),[dim[0],dim[1]],/SAMPLE), $				
        FILL      = 1, $
        XRANGE    = [0, 0.3], $
        YRANGE    = [0, 1800], $
        XTITLE    = 'X (m)', $
        YTITLE    = 'Time (s)', $
        LEVELS    = map_levels, $
        C_COLOR   = map_table, $
        TITLE     = "Timestep =" + STRING(dt), $
        POSITION  = map_position

	;Produce color bar
	COLOR_BAR_24_KPB, map_table[1:*], OVER = map_table[-1], UNDER = map_table[0],  $						;Draw map color bar
		TICKS = map_bar_ticks, $
		RANGE = [map_bar_min, map_bar_max], $
		TITLE = map_bar_title, $
		POSIT = bar_pos
	
ENDFOREACH

!P.MULTI = 0

END