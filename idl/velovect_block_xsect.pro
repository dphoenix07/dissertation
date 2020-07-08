;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Beginning of block of code to plot storm-relative horizontal wind as vectors
;These vectors are plotted over color-filled ozone contours
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

u = (WRF_READ_VAR('u', date, experiment, state, DOMAIN = domain)).values						
v = (WRF_READ_VAR('v', date, experiment, state, DOMAIN = domain)).values	
w = (WRF_READ_VAR('w', date, experiment, state, DOMAIN = domain)).values	

u_sect = INTERPOLATE(u, iisect, jjsect, kksect, MISSING = !Values.F_NaN)		 
v_sect = INTERPOLATE(v, iisect, jjsect, kksect, MISSING = !Values.F_NaN)		 
w_sect = INTERPOLATE(w, iisect, jjsect, kksect, MISSING = !Values.F_NaN)		 

;sample every 100 points along cross section path and 5 points vertically
x_freq = 100
z_freq = 5

dim = SIZE(u_sect,/DIMENSIONS)
angle_sect = ATAN((x1-x0)/(y1-y0))*!RADEG + 180.0
angle_wind = FLTARR(dim[0],dim[1])
angle_new  = FLTARR(dim[0],dim[1])
delta_angle = angle_sect - 90.0

FOR i = 0, dim[0]-1 DO BEGIN
	FOR j = 0, dim[1]-1 DO BEGIN
    	IF ((u_sect[i,j] GT 0.0) AND (v_sect[i,j] GT 0.0)) THEN angle_wind[i,j] = ATAN(u_sect[i,j]/v_sect[i,j])*!RADEG
        IF ((u_sect[i,j] LT 0.0) AND (v_sect[i,j] GT 0.0)) THEN angle_wind[i,j] = ATAN(u_sect[i,j]/v_sect[i,j])*!RADEG + 360.0
        IF ((u_sect[i,j] LT 0.0) AND (v_sect[i,j] LT 0.0)) THEN angle_wind[i,j] = ATAN(u_sect[i,j]/v_sect[i,j])*!RADEG + 180.0
        IF ((u_sect[i,j] GT 0.0) AND (v_sect[i,j] LT 0.0)) THEN angle_wind[i,j] = ATAN(u_sect[i,j]/v_sect[i,j])*!RADEG + 180.0

		angle_new[i,j] = angle_wind[i,j] - angle_sect
		IF (angle_new[i,j] LT 0.0) THEN angle_new[i,j] = 360.0 + angle_new[i,j]
	ENDFOR
ENDFOR

spd = SQRT(u_sect^2 + v_sect^2 + w_sect^2)
section_wind = spd * SIN(angle_new*!DDTOR) - storm_motion

h_sample = FLTARR(dim[0],dim[1])*!Values.F_NaN
h_sample[0:dim[0]-1:x_freq, 0:dim[1]-1:z_freq]= section_wind [0:dim[0]-1:x_freq, 0:dim[1]-1:z_freq]

w_sample = FLTARR(dim[0],dim[1])*!Values.F_NaN
w_sample[0:dim[0]-1:x_freq, 0:dim[1]-1:z_freq]= w_sect[0:dim[0]-1:x_freq, 0:dim[1]-1:z_freq]

IF (section_type EQ 2) THEN VELOVECT, h_sample, w_sample, FINDGEN(nsect), 0.001*zsect, /OVERPLOT, LENGTH= 50, THICK=3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End of block of code to plot storm-relative horizontal wind as vectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





