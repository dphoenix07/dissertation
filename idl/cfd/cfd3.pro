PRO CFD3, delta_x


;+
; Name:
;		CFD3
; Purpose:
;		This is a procedure to compare analytical values of df/dx at a point with those 
;		in different numerical approximations.
; Calling sequence:
;		CFD3
; Input:
;		None.      
; Output:
;		Table comparing values of df/dx.
; Keywords:
;		None.
; Author and history:
;		Daniel Phoenix  2017-10-07.
;-

COMPILE_OPT IDL2	

IF (N_ELEMENTS(delta_x) EQ 0) THEN delta_x = 0.1

;Define constants
pi = 3.141
x_range = FINDGEN(10)*delta_x
i = 5 ;x = 0.5 at index 5

;analytic function
f_a = sin((pi*x_range)/2)

;numerical approximations
f_cd = (f_a[i+1] - f_a[i-1])/(2*delta_x)
f_fd = (f_a[i+1] - f_a[i])/(delta_x)
f_bd = (f_a[i] - f_a[i-1])/(delta_x)
f_3p = (-1.5*f_a[i] + 2*f_a[i+1] - 0.5*f_a[i+2])/(delta_x)
f_4p = (f_a[i-2] - 8*f_a[i-1] + 8*f_a[i+1] - f_a[i+2])/(12*delta_x)

;Solve analytic function
df_dx_a = (pi/2)*cos((pi*x_range[i])/2)

;PRINT, df_dx_a
;PRINT, f_cd
;PRINT, f_fd
;PRINT, f_bd
;PRINT, f_3p
;PRINT, f_4p

method = ['Analytical', 'Centered Difference', 'Forward Difference', 'Backward Difference', $
			'3-point Asymmetric', '4-point Symmetric']

solution = [df_dx_a, f_cd, f_fd, f_bd, f_3p, f_4p]


;Compute relative errors
PRINT, FORMAT = '("Method", 20X, "Result", 7X, "Relative Difference")'								;Print table header information
PRINT, '============================================================='

FOR j = 0, N_ELEMENTS(method)-1 DO $
	PRINT, FORMAT = '(A20, 2X, F10.4, 10X, F10.7)', method[j], solution[j], $
			(solution[j]-df_dx_a)/(df_dx_a)

PRINT, ' ' 
PRINT, ' '

;;Numerical Error w/ leading term
t_cd = -(((delta_x^2)/6)*(pi/2)^3*cos((pi*x_range[i])/2))		
t_fd = -((delta_x/2)*(pi/2)^2*sin((pi*x_range[i])/2))
t_bd = -((-delta_x/2)*(pi/2)^2*sin((pi*x_range[i])/2))
t_3p = -((-(delta_x^2)/3)*(pi/2)^3*cos((pi*x_range[i])/2))
t_4p = -((-(delta_x^4)/30)*(pi/2)^5*cos((pi*x_range[i])/2))

error = [t_cd, t_fd, t_bd, t_3p, t_4p]

;;Relative Error w/ leading term
tr_cd = -(((delta_x^2)/6)*(pi/2)^3*cos((pi*x_range[i])/2))/df_dx_a		
tr_fd = -((delta_x/2)*(pi/2)^2*sin((pi*x_range[i])/2))/df_dx_a
tr_bd = -((-delta_x/2)*(pi/2)^2*sin((pi*x_range[i])/2))/df_dx_a
tr_3p = -((-(delta_x^2)/3)*(pi/2)^3*cos((pi*x_range[i])/2))/df_dx_a
tr_4p = -((-(delta_x^4)/30)*(pi/2)^5*cos((pi*x_range[i])/2))/df_dx_a

rel_error = [tr_cd, tr_fd, tr_bd, tr_3p, tr_4p]

PRINT, FORMAT = '("Method", 20X, "Error", 10X, "Relative Error")'								;Print table header information
PRINT, '============================================================='

FOR j = 0, N_ELEMENTS(method)-2 DO $
	PRINT, FORMAT = '(A20, 3X, F10.7, 8X, F10.7)', method[j+1], error[j], $
			(rel_error[j])

PRINT, ' '
PRINT, ' '

delta_xs = [0.5, 0.25, 0.125, 0.1, 0.05, 0.025, 0.0125]

cd_arr =[ ]
fd_arr =[ ]
bd_arr =[ ]
tp_arr =[ ]
fp_arr =[ ]

j=0
FOREACH delta, delta_xs DO BEGIN
	x_range = [0.0:2.0:delta]
	i		= INDEX_OF_NEAREST_CRH(x_range, 0.5)
	df_dx_a = (pi/2)*cos((pi*x_range[i])/2)
	f_a 	= sin((pi*x_range)/2)

	f_cd = (f_a[i+1] - f_a[i-1])/(2*delta)
	f_cd = (f_cd - df_dx_a)/df_dx_a
	
	f_fd = (f_a[i+1] - f_a[i])/(delta)
	f_fd = (f_fd - df_dx_a)/df_dx_a

	f_bd = (f_a[i] - f_a[i-1])/(delta)
	f_bd = (f_bd - df_dx_a)/df_dx_a

	f_3p = (-1.5*f_a[i] + 2*f_a[i+1] - 0.5*f_a[i+2])/(delta)
	f_3p = (f_3p - df_dx_a)/df_dx_a

	f_4p = (f_a[i-2] - 8*f_a[i-1] + 8*f_a[i+1] - f_a[i+2])/(12*delta)
	f_4p = (f_4p - df_dx_a)/df_dx_a
	
	cd_arr = ABS([cd_arr, f_cd])
	fd_arr = ABS([fd_arr, f_fd])
	bd_arr = ABS([bd_arr, f_bd])
	tp_arr = ABS([tp_arr, f_3p])
	fp_arr = ABS([fp_arr, f_4p])

ENDFOREACH


figure = PLOT(-ALOG10(delta_xs), -ALOG10(cd_arr), $
		XTITLE   = '-LOG10(Delta x)', $
		YTITLE   = '-LOG10 |Error|', $
		NAME     = 'Centered Diff.', $
		COLOR 	 = COLOR_24('red'))

figure1 = PLOT(-ALOG10(delta_xs), -ALOG10(fd_arr), $
		COLOR 	 = COLOR_24('blue'), $
		NAME     = 'Forward Diff.', $
		OVERPLOT = 1)

figure2 = PLOT(-ALOG10(delta_xs), -ALOG10(bd_arr), $
		COLOR 	 = COLOR_24('green'), $
		NAME     = 'Backward Diff.', $
		OVERPLOT = 1)

figure3 = PLOT(-ALOG10(delta_xs), -ALOG10(tp_arr), $
		COLOR 	 = COLOR_24('yellow'), $
		NAME     = '3-point Asym.', $
		OVERPLOT = 1)


figure4 = PLOT(-ALOG10(delta_xs), -ALOG10(fp_arr), $
		COLOR 	 = COLOR_24('black'), $
		NAME     = '4-point Sym.', $
		OVERPLOT = 1)

leg = LEGEND(TARGET=[figure,figure1,figure2,figure3,figure4], POSITION = [0.7,5.65], /DATA, /AUTO_TEXT_COLOR)


END