PRO H_hour, THE

;+
; Name:
;               INSOLATION
; Purpose:
;               This is a procedure to plot H for the year (exam #8 plot)
; Calling sequence:
;               H_hour
; Inputs:
;               wave		: array of wavelengths in micrometers
;				temp		: temperature in kelvin
; Output:
;               value  	    : plot of planck curve 
;
; Author and history:
;               Daniel B. Phoenix	2017-03-31.
;
;-

COMPILE_OPT IDL2																			

;Constants

h  = 6.63e-34
c  = 3.00e8
k  = 1.38e-23

a     = 1.49597887E8
sc    = 1370

lat_n = FINDGEN(66) + 1.0
;lat_s = -FINDGEN(66)

;lat  = [lat_s, lat_n]
;lat  = lat[1:131]
;slat = SORT(lat)
;lat  = lat[slat]

latitude = [-66, -45, -23.5, -10, 0, 10, 23.5, 45, 66]
lat_name = ['-66 S', '-45 S', '-23.5 S', '-10 S','EQ','10 N','23.5 N','45 N','66 N', '89 N', '-89 S']
lat_arr = [ ]

color = [COLOR_24_DP('RED'), COLOR_24_DP('BLUE'), COLOR_24_DP('YELLOW'), COLOR_24_DP('gray50'), $
			COLOR_24_DP('GREEN'), COLOR_24_DP('MAGENTA'), COLOR_24_DP('ORANGE'), COLOR_24_DP('BLACK'), $
				COLOR_24_DP('PURPLE'), COLOR_24_DP('DARKRED'), COLOR_24_DP('BROWN') ]

no_data = FLTARR(365)

p = PLOT( no_data, YRANGE=[0,24], XTITLE = 'Day of Year', YTITLE = 'Hours of Daylight (H*2)', /NODATA)

i=0
FOREACH lat, latitude DO BEGIN 
    lat = lat * (!PI/180)
    
    a_arr = [1.000110,  0.034221,  0.000719]
    b_arr = [0.000000,  0.001280,  0.000077]
    c_arr = [0.006918, -0.399912, -0.006758, -0.002697]
    d_arr = [0.000000,  0.070257,  0.000907,  0.000148]
    
    day = FINDGEN(365) + 1.0
    
    H_arr = [ ]
    decl_arr = [ ]
    
    FOR d=1,365 DO BEGIN
        t    = (2*!PI*d)/365
        
        denom = 0
        n=0
        FOR n=0,2 DO BEGIN
        	denom = denom + (a_arr[n]*COS(n*t) + b_arr[n]*SIN(n*t))
        ENDFOR
        
        r_day = a / (SQRT(denom))
        
        decl_t = 0
        n = 0
        
        FOR n=0,3 DO BEGIN
        	decl_t = decl_t + (c_arr[n]*COS(n*t) + d_arr[n]*SIN(n*t))
        ENDFOR
        
     	decl_arr = [decl_arr, decl_t]
    
        omega = 2*!PI
        H 	  = ACOS(-TAN(lat)*TAN(decl_t))
        H_arr = [H_arr, H]
        
    ENDFOR
    
	H_arr = H_arr * 3.82 * 2.0

;	lat_arr = [[lat_arr], [H_arr]]

	p1 = PLOT(H_arr, COLOR=color[i], NAME = lat_name[i], /OVERPLOT)

	i = i + 1
ENDFOREACH


;;; NH pole

lat = 89
    lat = lat * (!PI/180)
    
    a_arr = [1.000110,  0.034221,  0.000719]
    b_arr = [0.000000,  0.001280,  0.000077]
    c_arr = [0.006918, -0.399912, -0.006758, -0.002697]
    d_arr = [0.000000,  0.070257,  0.000907,  0.000148]
    
    day = FINDGEN(365) + 1.0
    
    H_arr = [ ]
    decl_arr = [ ]
    
    FOR d=1,365 DO BEGIN
        t    = (2*!PI*d)/365
        
        denom = 0
        n=0
        FOR n=0,2 DO BEGIN
        	denom = denom + (a_arr[n]*COS(n*t) + b_arr[n]*SIN(n*t))
        ENDFOR
        
        r_day = a / (SQRT(denom))
        
        decl_t = 0
        n = 0
        
        FOR n=0,3 DO BEGIN
        	decl_t = decl_t + (c_arr[n]*COS(n*t) + d_arr[n]*SIN(n*t))
        ENDFOR
        
     	decl_arr = [decl_arr, decl_t]
    
        omega = 2*!PI
        H = ACOS(-TAN(lat)*TAN(decl_t))
    
		IF (FINITE(H,/NAN)) THEN BEGIN
			IF (d LT 80) THEN H = 0.0
			IF (d GT 80 AND d LT 265) THEN H = 24.0 / (3.82*2.0)
			IF (d GT 265) THEN H = 0.0
		ENDIF
		
        H_arr = [H_arr, H]

        
    ENDFOR
    
	H_arr = H_arr * 3.82 * 2.0

	p2 = PLOT(H_arr, COLOR=color[9], NAME = lat_name[9], /OVERPLOT)



;;;; SH

lat = -89
    lat = lat * (!PI/180)
    
    a_arr = [1.000110,  0.034221,  0.000719]
    b_arr = [0.000000,  0.001280,  0.000077]
    c_arr = [0.006918, -0.399912, -0.006758, -0.002697]
    d_arr = [0.000000,  0.070257,  0.000907,  0.000148]
    
    day = FINDGEN(365) + 1.0
    
    H_arr = [ ]
    decl_arr = [ ]
    
    FOR d=1,365 DO BEGIN
        t    = (2*!PI*d)/365
        
        denom = 0
        n=0
        FOR n=0,2 DO BEGIN
        	denom = denom + (a_arr[n]*COS(n*t) + b_arr[n]*SIN(n*t))
        ENDFOR
        
        r_day = a / (SQRT(denom))
        
        decl_t = 0
        n = 0
        
        FOR n=0,3 DO BEGIN
        	decl_t = decl_t + (c_arr[n]*COS(n*t) + d_arr[n]*SIN(n*t))
        ENDFOR
        
     	decl_arr = [decl_arr, decl_t]
    
        omega = 2*!PI
        H = ACOS(-TAN(lat)*TAN(decl_t))
    
		IF (FINITE(H,/NAN)) THEN BEGIN
			IF (d LT 80) THEN H = 24.0 / (3.82*2.0)
			IF (d GT 80 AND d LT 265) THEN H = 0.0
			IF (d GT 265) THEN H = 24.0 / (3.82*2.0)
		ENDIF

        H_arr = [H_arr, H]

        
    ENDFOR
    
	H_arr = H_arr * 3.82 * 2.0

	p3 = PLOT(H_arr, COLOR=color[10], NAME = lat_name[10], /OVERPLOT)

leg = LEGEND(TARGET=[p], /DATA, /AUTO_TEXT_COLOR)



END 