PRO INSOLATION2, day, lat

;+
; Name:
;               INSOLATION2
; Purpose:
;               This is a procedure to plot daily insolation
; Calling sequence:
;               INSOLATION2, 
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
pi = 3.14

a     = 1.49597887E8
sc    = 1367
p     = 1010.3
t     = 0.85

;decl  = 23.5	
;lat   = 0.0

sol_day  = JULDAY(6,20,2017)-JULDAY(1,1,2017)
equi_day = JULDAY(3,20,2017)-JULDAY(1,1,2017)

;day_of_year = day - JULDAY(1,1,2017)
day_of_year = day
hours 	= [7,8,9,10,11,12,13,14,15,16,17]
hangle	= (12.0-hours)*15.0*pi/180.0  

declangle = 23.45*sin((2.0*pi*(284.0+day_of_year))/365.0)*(pi/180.0)

cosz	  = SIN((lat*pi)/180.0)*sin(declangle)+cos((lat*pi)/180.0)*cos(declangle)*cos(hangle)

m=p/(101.3*cosz); %optical airmass

Sb=cosz*sc*(t^m)			;  Sb is the beam radiation on a horizontal surface
Sd=0.3*(1.0-t^m)*sc*cosz	;  Sd is the diffuse radiation on a horizontal surface
St=Sb+Sd					;  St is the total radiation


insol = sc*cosz

PRINT, insol
         
STOP
END 