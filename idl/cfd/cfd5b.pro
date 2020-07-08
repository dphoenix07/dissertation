PRO CFD5B

;+
; Name:
;		CFD5B
; Purpose:
;		
; Calling sequence:
;		CFD5B
; Input:
;		None.
; Output:
;		Plots of the function for different courant numbers and with different FDEs.
;		The diffusion, dispersion, and dissipation errors are outputted.
; Keywords:
;		None.
; Author and history:
;		Daniel B. Phoenix 	2017-11-17.
;-

COMPILE_OPT IDL2	


delta_x = 1.0/32.0
ep = 0.1
courant = [0.1, 0.5, 0.7]
nx = 32

!P.MULTI = [0,2,2]

FOREACH cour, courant DO BEGIN
   ny = (3.0/(cour*delta_x))+3.0
   u_2lf     = FLTARR(nx+3,ny)
   u_asselin = FLTARR(nx+3,ny)
   u_4lf     = FLTARR(nx+5,ny)
   
   FOR jj = 0, ny-1 DO BEGIN
      ; Initial Conditions
      IF ((jj EQ 0) OR (jj EQ 1)) THEN BEGIN
         ; Two Boundary Edges
         FOR ii = 0, nx+2 DO BEGIN
            xx = (ii - 1)/32.0
            IF ((ii GT 13) AND (ii LT 21)) THEN BEGIN
               u_2lf[ii,jj] = [64.0*(((xx)-0.5)^2 - (1.0/64.0))]^2
            ENDIF ELSE BEGIN
               u_2lf[ii,jj] = 0
            ENDELSE
        ENDFOR

         u_asselin[*,jj] = u_2lf[*,jj]

         ; Four Boundary Edges
         FOR ii = 0, nx+4 DO BEGIN
            xx = (ii - 2)/32.0
            IF ((ii GT 14) AND (ii LT 22)) THEN BEGIN
               u_4lf[ii,jj] = [64.0*(((xx)-0.5)^2 - (1.0/64.0))]^2
            ENDIF ELSE BEGIN
               u_4lf[ii,jj] = 0
            ENDELSE
        ENDFOR


      ; Run the LeapFrog Scheme
      ENDIF ELSE BEGIN 
         FOR ii = 1, nx+1 DO BEGIN
            ; Second Order Leap Frog
            u_2lf[ii,jj]  = u_2lf[ii,jj-2] -  (cour *(u_2lf[ii+1,jj-1]-u_2lf[ii-1,jj-1]))
            u_asselin[ii,jj] = u_asselin[ii,jj-2] -  (cour *(u_asselin[ii+1,jj-1]-u_asselin[ii-1,jj-1]))
            ; Time Filter 
            u_asselin[ii,jj-1] = u_asselin[ii,jj-1] + (ep*(u_asselin[ii,jj-2]-(2*u_asselin[ii,jj-1])+u_asselin[ii,jj]))
		  ENDFOR

         ; Periodic BC's
         u_2lf[0,jj] = u_2lf[nx,jj]
         u_2lf[nx+2,jj] = u_2lf[2,jj]

         u_asselin[0,jj] = u_asselin[nx,jj]
         u_asselin[nx+2,jj] = u_asselin[2,jj]


         FOR ii = 2, nx+2 DO BEGIN
            ; Fourth Order Leap Frog
            u_4lf[ii,jj]=((1.0/6.0)*cour*(u_4lf[ii+2,jj-1]-u_4lf[ii-2,jj-1])) - ((4.0/3.0)*cour*(u_4lf[ii+1,jj-1]-u_4lf[ii-1,jj-1])) + u_4lf[ii,jj-2]
         ENDFOR

         ; Periodic BC's        
         u_4lf[1,jj] = u_4lf[nx+1,jj]
         u_4lf[0,jj] = u_4lf[nx,jj]
         u_4lf[nx+3,jj] = u_4lf[3,jj]
         u_4lf[nx+4,jj] = u_4lf[4,jj]
      ENDELSE
    ENDFOR

    IF (cour EQ 0.1) THEN BEGIN
    	u1 = u_2lf[*,jj-1]
    	f1 = u_4lf[*,jj-1] 
    	t1 = u_asselin[*,jj-1] 
    ENDIF
    	 
    IF (cour EQ 0.5) THEN BEGIN
    	u2 = u_2lf[*,jj-1]  
    	f2 = u_4lf[*,jj-1] 
    	t2 = u_asselin[*,jj-1] 
    ENDIF
    	
    IF (cour EQ 0.7) THEN BEGIN
    	u3 = u_2lf[*,jj-1] 
		f3 = u_4lf[*,jj-1] 
    	t3 = u_asselin[*,jj-1] 
	ENDIF
	
	;Compute errors
	
	PRINT, 'Courant number = ' + STRING(cour)
	
	var_analyt = SQRT(VARIANCE(u_2lf[*,0]))
    var_discrete = SQRT(VARIANCE(u_2lf[*,jj-1]))
    covariance = CORRELATE(u_2lf[*,0],u_2lf[*,jj-1],/COVARIANCE)
    
    correl = CORRELATE(u_2lf[*,0],u_2lf[*,jj-1])
    dissipation = ((var_analyt - var_discrete)^2 + ((MEAN(u_2lf[*,0]) - MEAN(u_2lf[*,jj-1]))^2))
    dispersion  = 2*(1-correl)*var_analyt*var_discrete
    diffusion   = dissipation + dispersion
    
    PRINT, '2nd order leapfrog scheme'
    PRINT, 'The Dissipation error: ', dissipation
    PRINT, 'The Dispersion error: ', dispersion
    PRINT, 'The Diffusion error: ', diffusion
    PRINT, ' '
    
    
    var_analyt = SQRT(VARIANCE(u_asselin[*,0]))
    var_discrete = SQRT(VARIANCE(u_asselin[*,jj-1]))
    covariance = CORRELATE(u_asselin[*,0],u_asselin[*,jj-1],/COVARIANCE)
    
    correl = CORRELATE(u_asselin[*,0],u_asselin[*,jj-1])
    dissipation = ((var_analyt - var_discrete)^2 + ((MEAN(u_asselin[*,0]) - MEAN(u_asselin[*,jj-1]))^2))
    dispersion  = 2*(1-correl)*var_analyt*var_discrete
    diffusion   = dissipation + dispersion
    
    PRINT, 'Asselin time filter'
    PRINT, 'The Dissipation error: ', dissipation
    PRINT, 'The Dispersion error: ', dispersion
    PRINT, 'The Diffusion error: ', diffusion
    PRINT, ' '
    
    
    var_analyt = SQRT(VARIANCE(u_4lf[*,0]))
    var_discrete = SQRT(VARIANCE(u_4lf[*,jj-1]))
    covariance = CORRELATE(u_4lf[*,0],u_4lf[*,jj-1],/COVARIANCE)
    
    correl = CORRELATE(u_4lf[*,0],u_4lf[*,jj-1])
    dissipation = ((var_analyt - var_discrete)^2 + ((MEAN(u_4lf[*,0]) - MEAN(u_4lf[*,jj-1]))^2))
    dispersion  = 2*(1-correl)*var_analyt*var_discrete
    diffusion   = dissipation + dispersion
    
    PRINT, '4th order leapfrog scheme'
    PRINT, 'The Dissipation error: ', dissipation
    PRINT, 'The Dispersion error: ', dispersion
    PRINT, 'The Diffusion error: ', diffusion
    PRINT, ' '

	
ENDFOREACH
 
	PLOT, u_2lf[*,0], $
        TITLE  = '2nd order Leapfrog, Time = 3s', $
        YRANGE = [-0.4, 1.4], $
        XTITLE   = 'X - Distance (m)', $
		YTITLE   = 'U (m/s)'
 
          
    OPLOT, u1,  COLOR = COLOR_24('red')
    OPLOT, u2,  COLOR = COLOR_24('green')
    OPLOT, u3,  COLOR = COLOR_24('blue')


   PLOT, u_4lf[*,0], $
        TITLE = '4th order Leapfrog Time = 3s', $
        YRANGE = [-0.4, 1.4], $ 
        XTITLE   = 'X - Distance (m)', $
		YTITLE   = 'U (m/s)'
 
    OPLOT, f1,  COLOR = COLOR_24('red')
    OPLOT, f2,  COLOR = COLOR_24('green')
    OPLOT, f3,  COLOR = COLOR_24('blue')

   PLOT, u_asselin[*,0], $
        TITLE = 'Asselin Time Filter Time = 3s', $
        YRANGE = [-0.4, 1.4], $ 
        XTITLE   = 'X - Distance (m)', $
		YTITLE   = 'U (m/s)'
 
    OPLOT, t1,  COLOR = COLOR_24('red')
    OPLOT, t2,  COLOR = COLOR_24('green')
    OPLOT, t3,  COLOR = COLOR_24('blue')

    XYOUTS, 650.0, 200.0, 'Courant = 0.1  [---]', COLOR = COLOR_24('red'  ), /DEVICE, CHARSIZE = 1.5
    XYOUTS, 650.0, 170.0, 'Courant = 0.5  [---]', COLOR = COLOR_24('green'), /DEVICE, CHARSIZE = 1.5
    XYOUTS, 650.0, 140.0, 'Courant = 0.7  [---]', COLOR = COLOR_24('blue' ), /DEVICE, CHARSIZE = 1.5
    XYOUTS, 650.0, 110.0, 'Analytical Soln  [---]', COLOR = COLOR_24('black'), /DEVICE, CHARSIZE = 1.5    

END
