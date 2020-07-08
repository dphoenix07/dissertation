PRO MAP_WRF_GRID, x0, y0, nx, ny, delta, TRUE_LATS = true_lats, OVERPLOT = overplot

;+
; Name:
;		MAP_WRF_GRID
; Purpose:
;		This is a procedure to draw a WRF Lambert projected grid. 
; Calling sequence:
;		MAP_WRF_GRID, x0, y0, nx, ny, delta
; Input:
;		x0    : Center Longitude
;		y0    : Center Latitude
;		nx    : Number of points in x-direction
;		ny    : Number of points in y-direction
;		delta : Grid resolution (in km).
; Output:
;		A map projection of a WRF grid.
; Keywords:
;		OVERPLOT : Optional keyword to specify a 2-element array of parent
;					  grid offests (in km)
; Author and history:
;		Cameron R. Homeyer  2012-09-12.
;-

COMPILE_OPT IDL2																									;Set compile options

IF (N_ELEMENTS(true_lats) EQ 0) THEN true_lats = [10.0, 60.0]

IF KEYWORD_SET(overplot) THEN BEGIN
	x0y0 = !WRF_GRID[0:1]
	x1y1 = !WRF_GRID[2:3]
	az0  = ABS(!WRF_GRID[4] + 90.0)
	
	dagr = (5480.0)*!DDTOR*(MAP_2POINTS(x0, y0, x0y0[0], x0y0[1]))[0]
	dx   = ROUND(dagr*COS(az0*!DDTOR))
	dy   = ROUND(dagr*SIN(az0*!DDTOR))

	xyn0 = CONVERT_COORD(x0y0[0], x0y0[1], /DATA, /TO_NORMAL)
	xyn1 = CONVERT_COORD(x1y1[0], x1y1[1], /DATA, /TO_NORMAL)
	dxn  = xyn1[0] - xyn0[0]
	dyn  = xyn1[1] - xyn0[1]
	
	x0   = xyn0[0] + dxn*(overplot[ 0])/(2*dx)
	x1   = x0      + dxn*(nx-1)*delta/(2*dx)
	y0   = xyn0[1] + dyn*overplot[ 1]/(2*dy)
	y1   = y0      + dyn*(ny-1)*delta/(2*dy)
	
	IF (!WRF_GRID[5] EQ 1) THEN BEGIN
		PLOTS, [xyn0[0], xyn1[0], xyn1[0], xyn0[0], xyn0[0]], $
				 [xyn0[1], xyn0[1], xyn1[1], xyn1[1], xyn0[1]], THICK = 2, /NORMAL
		XYOUTS, xyn0[0] + 0.025*dxn, xyn1[1] - 0.05*dyn, 'D01', CHARSIZE = 2.0, /NORMAL
	ENDIF
	
	PLOTS, [x0,x1,x1,x0,x0], [y0,y0,y1,y1,y0], THICK = 2, /NORMAL
	XYOUTS, x0 + 0.025*dxn, y1 - 0.05*dyn, 'D' + $
		STRING(!WRF_GRID[5] + 1, FORMAT="(I2.2)"), CHARSIZE = 2.0, /NORMAL
	
	!WRF_GRID[5] += 1
ENDIF ELSE BEGIN
	dx  = 0.5*delta*(nx-1)																						;Compute grid lengths
	dy  = 0.5*delta*(ny-1)
	dxy = SQRT(dx^2 + dy^2)
	daz = dxy/(5480.0)																							;Compute arc distance (in degrees)

	az1 = -ASIN(dy/dxy)/!DDTOR	-  90.0																		;Compute direction azimuth
	az2 = -ASIN(dx/dxy)/!DDTOR
	az3 =  ASIN(dx/dxy)/!DDTOR
	az4 =  ASIN(dy/dxy)/!DDTOR +  90.0
	
	x1y1 = REVERSE(LL_ARC_DISTANCE([x0,y0], daz, az1, /DEGREES))									;Initialize corner coordinates
	x2y2 = REVERSE(LL_ARC_DISTANCE([x0,y0], daz, az2, /DEGREES))
	x3y3 = REVERSE(LL_ARC_DISTANCE([x0,y0], daz, az3, /DEGREES))
	x4y4 = REVERSE(LL_ARC_DISTANCE([x0,y0], daz, az4, /DEGREES))
	
	DEFSYSV, '!WRF_GRID', [REVERSE(x1y1),REVERSE(x3y3),az1,1]										;Store initial grid point as system variable
	
	factor = 400.0/nx + 400.0/ny
	
	IF (nx LT 500) THEN xsize = LONG(factor*nx) $
						ELSE xsize = nx

	IF (ny LT 500) THEN ysize = LONG(factor*ny) $
						ELSE ysize = ny
						
;	WINDOW, 0, XSIZE = xsize, YSIZE = ysize
	MAP_SET, y0, x0, 0, /CONIC, STANDARD_PARALLELS = true_lats, LIMIT = [x1y1,x2y2,x3y3,x4y4], $
		/CONT, /USA, /GRID, LONDEL = 10.0, LATDEL = 5.0, /ISO, LABEL = 1
ENDELSE
	

END
