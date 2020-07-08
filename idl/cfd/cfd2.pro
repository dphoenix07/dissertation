PRO CFD2


;+
; Name:
;		CFD2
; Purpose:
;		This is a procedure to plot u and v at all grid points in the 101x101 domain
; Calling sequence:
;		CFD2
; Input:
;		None.      
; Output:
;		Contour plots of u and v fields
; Keywords:
;		None.
; Author and history:
;		Daniel Phoenix  2017-10-01.
;-

COMPILE_OPT IDL2	


;Define constants
pi = 3.141
ep = 1
L  = 10		;100*delta_x = 100*0.1

u_arr = [ ]
v_arr = [ ]

FOR j=0.0,10.0,0.1 DO BEGIN
	FOR i=0.0,10.0,0.1 DO BEGIN
		IF (j GT i) THEN BEGIN
			v = 0.0
			u = 0.0
		ENDIF ELSE BEGIN
			v =  ((2*pi*ep)/(L^2))*COS((2*pi*((i-j)))/L)
;			v =  ((2*pi*ep)/(L^2))*COS((2*pi*((x-y)/10))/L)
			u = - v
		ENDELSE
		u_arr = [u_arr, u]
		v_arr = [v_arr, v]
	ENDFOR
ENDFOR

;;Method 2
;FOR j=0,100 DO BEGIN
;	FOR i=0,100 DO BEGIN
;		x = ((i-1.0)/(100.0))*10.0
;		y = ((j-1.0)/(100.0))*10.0
;		IF (y GT x) THEN BEGIN
;			v = 0.0
;			u = 0.0
;		ENDIF ELSE BEGIN
;			v =  ((2*pi*ep)/(L^2))*COS((2*pi*((x-y)))/L)
;;			v =  ((2*pi*ep)/(L^2))*COS((2*pi*((x-y)/10))/L)
;			u = - v
;		ENDELSE
;		u_arr = [u_arr, u]
;		v_arr = [v_arr, v]
;	ENDFOR
;ENDFOR

u_arr = REFORM(u_arr,100,100)
v_arr = REFORM(v_arr,100,100)

x = FINDGEN(100)
y = FINDGEN(100)

;;Set up contour plot
!P.MULTI = [0,2,1]

map_bar_min   = MIN(u_arr)																	;Set echo top minimum
map_bar_max   = MAX(u_arr)																	;Set echo top maximum
map_bar_ticks = 7																			;Set number of color bar ticks
map_table     = [VISUALIZE_88D_COLOR(3)]													;Set color table
map_levels    = [MAKEN(MIN(u_arr),MAX(u_arr),15)]											;Set contour levels

map_pos1 = [0.05, 0.20, 0.48, 0.95]																			;Set map position
bar_pos1 = [0.10, 0.07, 0.43, 0.10]																			;Set color bar position

map_pos2 = [0.55, 0.20, 0.98, 0.95]																			;Set map position
bar_pos2 = [0.60, 0.07, 0.93, 0.10]																			;Set color bar position


CONTOUR, u_arr, x, y, $
	TITLE     = 'U Perturbation Velocities', $
	XTITLE    = 'X', $
	YTITLE    = 'Y', $
	FILL      = 1, $
	LEVELS    = map_levels, $
	C_COLOR   = map_table, $
	POSITION  = map_pos1

COLOR_BAR_24_KPB, map_table[1:*], OVER = map_table[-1], UNDER = map_table[0], $						;Draw map color bar
	TICKS = map_bar_ticks, $
	RANGE = [map_bar_min, map_bar_max], $
	TITLE = map_bar_title, $
	POSIT = bar_pos1

CONTOUR, v_arr, x, y, $
	TITLE     = 'V Perturbation Velocities', $
	XTITLE    = 'X', $
	YTITLE    = 'Y', $
	FILL      = 1, $
	LEVELS    = map_levels, $
	C_COLOR   = map_table, $
	POSITION  = map_pos2

COLOR_BAR_24_KPB, map_table[1:*], OVER = map_table[-1], UNDER = map_table[0], $						;Draw map color bar
	TICKS = map_bar_ticks, $
	RANGE = [map_bar_min, map_bar_max], $
	TITLE = map_bar_title, $
	POSIT = bar_pos2


END
