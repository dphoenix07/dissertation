PRO CFD4B_ALT, t1, t2


;+
; Name:
;		CFD4b
; Purpose:
;		This is a procedure to solve the 1-D heat transfer equation using the 
;		Crank-Nicholson method.
; Calling sequence:
;		CFD4b, t1, t2
; Input:
;		t1 : first timestep to evaluate at
;		t2 : second timestep to evaluate at     
; Output:
;		Contour plot of solution to heat transfer equation.
; Keywords:
;		None.
; Author and history:
;		Daniel Phoenix  2017-10-26.
;-

COMPILE_OPT IDL2	


;Define constants
pi 	    = 3.141
delta_x = 0.015
delta_t = [t1, t2]
sig     = 3.0E-6
width   = 0.30
t_final = 30*60

!P.MULTI = [0,2,1]

FOREACH dt, delta_t DO BEGIN
	;Set up arrays and set IC/BC
    time_arr  	= [0:t_final:dt]
    x_arr	  	= [0:width:delta_x]
    temps   	= FLTARR(N_ELEMENTS(x_arr), N_ELEMENTS(time_arr))
    dim	 	    = SIZE(temps, /DIMENSIONS)
    
    temps[* ,0] = 100.0
    temps[0 ,*] = 300.0
    temps[-1,*] = 300.0

	;Call function to compute D and solve using the tridiagonal solver    
    FOR t = 0, N_ELEMENTS(time_arr)-2 DO BEGIN
    	D = SY_ALT(t,temps,dim,dt)
    	temps[*,t+1] = D	
    	temps[0 ,* ] = 300.0
    	temps[-1,* ] = 300.0
    ENDFOR
    
    ;Set up color bar
     map_bar_title = 'Temperature (K)'														
     map_bar_min   = 100.0																	
     map_bar_max   = 300.0																	
     map_bar_ticks = 4																		
     map_table     = [VISUALIZE_88D_COLOR(3)]												
     map_levels    = [100.0 + 15.0*FINDGEN(N_ELEMENTS(map_table))]							
     
     ;Set map position
     IF (dt EQ t1) THEN map_position = [0.07, 0.20, 0.47, 0.95]
     IF (dt EQ t2) THEN map_position = [0.55, 0.20, 0.95, 0.95]
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
ENDFOREACH

;Produce color bar
COLOR_BAR_24_KPB, map_table[1:*], OVER = map_table[-1], UNDER = map_table[0], $						
	TICKS = map_bar_ticks, $
	RANGE = [map_bar_min, map_bar_max], $
	TITLE = map_bar_title, $
	POSIT = bar_pos


!P.MULTI = 0

END