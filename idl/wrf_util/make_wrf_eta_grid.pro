PRO MAKE_WRF_ETA_GRID, dz, sonde, PTOP = ptop, DZSFC = dzsfc

;+
; Name:
;		MAKE_WRF_ETA_GRID
; Purpose:
;		This is a procedure to create a WRF eta-grid based on radiosonde data and a
;		specified resolution within and above the boundary layer (BL). 
; Calling sequence:
;		MAKE_WRF_ETA_GRID, dz, sonde
; Input:
;		dz    : Above-BL target resolution (in km).
;		sonde : Data structure for radiosonde.
; Output:
;		This routine prints options that can be copied and pasted to a WRF namelist file.
; Keywords:
;		PTOP  : Desired model top (in hPa).
;		DZSFC : Desired vertical resolution near the surface (for BL scaling; in km). 
; Author and history:
;		Cameron R. Homeyer  2013-07-13.
;-

COMPILE_OPT IDL2																									;Set compile options

IF (N_ELEMENTS(dz   ) EQ 0) THEN dz    = 0.5																;Set procedure defaults
IF (N_ELEMENTS(sonde) EQ 0) THEN sonde = READ_NOAA_SONDE('KOUN', MAKE_DATE(2012,4,2,12))
IF (N_ELEMENTS(ptop ) EQ 0) THEN ptop  = 30.0
IF (N_ELEMENTS(dzsfc) EQ 0) THEN dzsfc = 0.15

r33  = SQRT((-25.0)/ALOG(dzsfc/dz))																			;Compute Gaussian scaling factor for BL

ztop = 0.001*INTERPOL(REVERSE(sonde.z), REVERSE(sonde.p), ptop)									;Get altitude of model top pressure from sonde
nz   = LONG((ztop-5.0)/dz) + 1																				;Compute number of levels between top of BL (5 km) and model top at specified resolution

z   = ztop - dz*FINDGEN(nz)																					;Create altitude array for troposphere
p   = INTERPOL(sonde.p, sonde.z, 1000.0*z[-1])															;Interpolate pressure from sonde to ~5 km altitude
WHILE (p LT 1000) DO BEGIN																						;Loop from 5 km down to surface
	dz2 = dz*EXP(-((5.0-z[-1])^2)/(r33^2))																	;Compute resolution for level in BL
	
	z   = [z, z[-1] - dz2]																						;Compute next lowest altitude based on scaled resolution
	p   = INTERPOL(sonde.p, sonde.z, 1000.0*z[-1])														;Interpolate pressure to altitude from sonde
ENDWHILE

p   = INTERPOL(sonde.p, sonde.z, 1000.0*z)																;Interpolate sonde pressures to computed model profile					
eta = (p-ptop)/(1000.0-ptop)																					;Compute eta levels

z  = REVERSE(z)																									;Restructure altitudes for plotting
dz = SHIFT(z, -1) - z
z2 = z + 0.5*dz

WINDOW, XSIZE = 600, YSIZE = 850
!P.CHARSIZE = 2.0
PLOT, 1000.0*dz[0:(n_elements(z) -2)], z2[0:(n_elements(z) -2)], $								;Plot profile
	PSYM = 4, XRANGE = [0.0, 1.5*MAX(1000.0*dz[0:(n_elements(z) -2)])], $
	/XSTYLE, XTITLE = 'Vertical Resolution (m)', $
	YTITLE = 'Altitude (km)', YRANGE = [0, 30.0], /YSTYLE

PRINT, 'e_vert          = ' + STRTRIM(N_ELEMENTS(eta),2) + ','										;Print namelist parameters for WRF
PRINT, 'eta_levels      = ', STRING((REVERSE(eta) < 1.0), FORMAT="(F6.4,',')")
PRINT, 'p_top_requested = ' + STRTRIM(LONG(ptop*100.0),2) + ','

END
