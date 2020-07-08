PRO ARC_AZIMUTH_KPB_DBP, x0, y0, x1, y1, arc_dist, az, $
	RADIANS = radians
            
;+
; NAME:
;		ARC_AZIMUTH_KPB
;
; PURPOSE:
;		Compute the arc distance and azimuth between pairs of points on the sphere
;		given the longitudes and latitudes of the points.  By default coordinates are
;		assumed to be in degrees.
;
;		This program is based on the IDL library function MAP_2POINTS, which is
;		copyright by ITTVIS.
;
; CATEGORY:
;		Mapping, geography.
;
; CALLING SEQUENCE:
;		Result = ARC_AZIMUTH_KPB(x0, y0, x1, y1)
;
; INPUT:
;		x0       : longitudes of the initial points
;		y0       : latitudes of the initial points
;		x1       : longitudes of the final points
;		y1       : latitudes of the final points
;
; KEYWORD PARAMETERS:
;		RADIANS  : If set, inputs and outputs are in radians, otherwise degrees.
;
; OUTPUT:
;		arc_dist : great circle arc distances between pairs of points
;		az       : azimuth angles between pairs of points.
;
;		Arc distances are angles between 0 to 180 degrees (or 0 to pi if 
;		the RADIANS keyword is set).  Azimuth is measured in degrees or 
;		radians east of north.
;
; EXAMPLES:
;		Distance and azimuth from Boulder to London.
;
;		IDL> ARC_AZIMUTH_KPB, -105.19, 40.02, -0.07, 51.30, arc_dist, az
;		IDL> PRINT, arc_dist, az
;           67.8543
;           40.6678
;
; MODIFICATION HISTORY:
;		Kenneth P. Bowman, April 2010.  Heavily modified from MAP_2POINTS to vectorize calculations
;			of arc distance and azimuth between pairs of points.  Other capabilities of MAP_2POINTS
;			are not included in this function.
;			To view with proper formatting, set the tab size to 3.
;-

COMPILE_OPT IDL2																					;Set compile options

IF KEYWORD_SET(radians) THEN s = 1.0 ELSE s = !DTOR									;Use correct units for angles

n = MAX([N_ELEMENTS(x0), N_ELEMENTS(y0), N_ELEMENTS(x1), N_ELEMENTS(y1)])		;Size of largest input array

cosy0 = COS(s*y0)																					;Cosines of initial latitudes
siny0 = SIN(s*y0)																					;Sines of initial latitudes
cosy1 = COS(s*y1)																					;Cosines of final latitudes
siny1 = SIN(s*y1)																					;Sines of final latitudes

cosdx = COS(s*(x1 - x0))																		;Cosines of longitude difference
sindx = SIN(s*(x1 - x0))																		;Sines of longitude difference

cosc  = -1.0 > siny0*siny1 + cosy0*cosy1*cosdx < 1.0									;Cosines of angle between points
sinc  = SQRT(1.0 - cosc^2)																		;Sines of angle between points

cosaz = FLTARR(n)																					;Create azimuth cosine array
sinaz = FLTARR(n)																					;Create azimuth sine array

i = WHERE(ABS(sinc) LT 1.0E-7, icount, COMPLEMENT = j, NCOMPLEMENT = jcount)	;Find small angles
IF (icount GT 0) THEN BEGIN
	cosaz[i] = 1.0																					;Set cosine to 1.0
	sinaz[i] = 0.0																					;Set sine to 0.0
ENDIF

IF (jcount GT 0) THEN BEGIN
	cosaz[j] = (cosy0[j]*siny1[j] - siny0[j]*cosy1[j]*cosdx[j])/sinc[j]			;Compute cosines of azimuths
	sinaz[j] = sindx[j]*cosy1[j]/sinc[j]													;Compute sines of azimuths
ENDIF

arc_dist = (1.0/s) * ACOS(cosc)																;Compute arc distance
az       = (1.0/s) * ATAN(sinaz, cosaz)													;Compute azimuth

END
