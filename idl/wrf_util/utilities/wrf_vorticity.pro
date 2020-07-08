FUNCTION WRF_VORTICITY, u, v, y, dx, ABSOLUTE = absolute

;+
;NAME:
;     WRF_VORTICITY
;PURPOSE:
;     This computes the vorticity from the WRF wind data.
;CATEGORY:
;     Data handling utility.
;CALLING SEQUENCE:
;     zeta = WRF_VORTICITY(u, v)
;INPUT:
;		dx   : Grid resolution (in meters)
;OUTPUT:
;     zeta : vertical component of the vorticity
;KEYWORDS:
;		DOMAIN   : Optional keyword to specify WRF domain. Default is 2.
;     ABSOLUTE : If set, compute the absolute vorticity (zeta + f).
;MODIFICATION HISTORY:
;     Cameron R. Homeyer 2012-11-28.
;-

COMPILE_OPT IDL2

zeta = u	 																											;Create the zeta variable

dim = SIZE(u, /DIMENSIONS)																						;Get grid dimensions
nx  = dim[0]
ny  = dim[1]
nz  = dim[2]

zeta = (SHIFT(v, -1,  0, 0) - SHIFT(v, 1, 0, 0) - $													;Compute relative vorticity
		  SHIFT(u,  0, -1, 0) + SHIFT(u, 0, 1, 0))/(2.0*dx) 

zeta[   0,   *,*] = !Values.F_NaN																			;Set boundaries to NaNs
zeta[nx-1,   *,*] = !Values.F_NaN
zeta[   *,   0,*] = !Values.F_NaN
zeta[   *,ny-1,*] = !Values.F_NaN

IF KEYWORD_SET(absolute) THEN $
   zeta = zeta + 2.0*!OMEGA*SIN(!DTOR*REBIN(y,nx,ny,nz,/SAMPLE))									;Add f to zeta

RETURN, zeta

END
