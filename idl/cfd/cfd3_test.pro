PRO CFD3_test, delta_x


;+
; Name:
;		CFD3_test
; Purpose:
;		This is a procedure to compare analytical values of df/dx at a point with those 
;		in different numerical approximations.
; Calling sequence:
;		CFD3_test
; Input:
;		None.      
; Output:
;		Table comparing values of df/dx.
; Keywords:
;		None.
; Author and history:
;		Daniel Phoenix  2017-10-18.
;-

COMPILE_OPT IDL2	

IF (N_ELEMENTS(delta_x) EQ 0) THEN delta_x = 0.01

;Define constants
pi = 3.141
x_range = [0:2.0:0.01]
i = 5 ;x = 0.5 at index 5
k_arr = [1.0, 5.0, 20.0]
color = [COLOR_24('red'), COLOR_24('blue'), COLOR_24('green')]

;analytic function
FOR i = 0, 2 DO BEGIN 
	f_a = tanh(k_arr[i]*(x_range-1))	
	p1  = PLOT(f_a, COLOR = color[i], /OVERPLOT)
ENDFOR

;numerical approximations
delta_xs = [0.320, 0.160, 0.080, 0.040, 0.020, 0.010]
xx = 1.05

FOR k=1,2 DO BEGIN
    cd_arr =[ ]
    fd_arr =[ ]
    bd_arr =[ ]
    tp_arr =[ ]
    fp_arr =[ ]

    FOREACH delta, delta_xs DO BEGIN
		df_dx_a = k_arr[k]/(COSH(k_arr[k]*(xx-1))^2)
		df_dx_a = k_arr[k]*(1-((tanh(k*(xx-1)))^2))
		
		f_ab2 = tanh(k_arr[k]*((xx-2*delta)-1))
		f_ab1 = tanh(k_arr[k]*((xx-delta)-1))

		f_a = tanh(k_arr[k]*(xx-1))	
		
		f_af1 = tanh(k_arr[k]*((xx+delta)-1))
		f_af2 = tanh(k_arr[k]*((xx+2*delta)-1))
		
;    	f_cd = (f_a[i+1] - f_a[i-1])/(2*delta)
 		f_cd = (f_af1 - f_ab1)/(2*delta)
    	f_cd = (f_cd - df_dx_a)/df_dx_a
    	
 ;   	f_fd = (f_a[i+1] - f_a[i])/(delta)
 		f_fd = (f_af1 - f_a)/(delta)
    	f_fd = (f_fd - df_dx_a)/df_dx_a
    
;    	f_bd = (f_a[i] - f_a[i-1])/(delta)
		f_bd = (f_a - f_ab1)/(delta)
    	f_bd = (f_bd - df_dx_a)/df_dx_a
    
;    	f_3p = (-1.5*f_a[i] + 2*f_a[i+1] - 0.5*f_a[i+2])/(delta)
 		f_3p = (-1.5*f_a + 2*f_af1 - 0.5*f_af2)/(delta)
    	f_3p = (f_3p - df_dx_a)/df_dx_a
    
;    	f_4p = (f_a[i-2] - 8*f_a[i-1] + 8*f_a[i+1] - f_a[i+2])/(12*delta)
 		f_4p = (f_ab2 - 8*f_ab1 + 8*f_af1 - f_af2)/(12*delta)
    	f_4p = (f_4p - df_dx_a)/df_dx_a

		PRINT, 'delta x = ', delta
		PRINT,  (f_ab2 - 8*f_ab1 + 8*f_af1 - f_af2)/(12*delta)
		PRINT, df_dx_a
		PRINT, ABS(f_4p)
		
		PRINT, ' '   
    	cd_arr = ABS([cd_arr, f_cd])
    	fd_arr = ABS([fd_arr, f_fd])
    	bd_arr = ABS([bd_arr, f_bd])
    	tp_arr = ABS([tp_arr, f_3p])
    	fp_arr = ABS([fp_arr, f_4p])
    ENDFOREACH
    


    figure = PLOT(-ALOG(delta_xs), -ALOG(cd_arr), $
    		TITLE    = k_arr[k], $
 ;   		XRANGE   = [0.5, 2], $
  ;  		YRANGE   = [0, 6], $
    		XTITLE   = '-LOG10(Delta x)', $
    		YTITLE   = '-LOG10 |Error|', $
    		COLOR 	 = COLOR_24('red'))
    
    figure1 = PLOT(-ALOG(delta_xs), -ALOG(fd_arr), $
 ;   		XRANGE   = [0.5, 2], $
  ;  		YRANGE   = [0, 6], $
    		COLOR 	 = COLOR_24('blue'), $
    		OVERPLOT = 1)
    
    figure2 = PLOT(-ALOG(delta_xs), -ALOG(bd_arr), $
  ;  		XRANGE   = [0.5, 2], $
  ;  		YRANGE   = [0, 6], $
    		COLOR 	 = COLOR_24('green'), $
    		OVERPLOT = 1)
    
    figure3 = PLOT(-ALOG(delta_xs), -ALOG(tp_arr), $
  ;  		XRANGE   = [0.5, 2], $
  ;  		YRANGE   = [0, 6], $
    		COLOR 	 = COLOR_24('yellow'), $
    		OVERPLOT = 1)
    
    figure4 = PLOT(-ALOG(delta_xs), -ALOG(fp_arr), $
  ;  		XRANGE   = [0.5, 2], $
  ;  		YRANGE   = [0, 6], $
    		COLOR 	 = COLOR_24('black'), $
    		OVERPLOT = 1)


ENDFOR
STOP
END