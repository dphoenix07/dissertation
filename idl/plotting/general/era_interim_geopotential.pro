FUNCTION ERA_INTERIM_GEOPOTENTIAL, T, SH, p, ps, Z0

;+
; Name:
;		ERA_INTERIM_GEOPOTENTIAL
; Purpose:
;		This is a function to compute geopotential height for ERA-Interim model-level output. 
; Calling sequence:
;		value = ERA_INTERIM_GEOPOTENTIAL(T, SH, p)
; Inputs:
;		T  : Air Temperature (in K)
;		SH : Specific Humidity (in kg kg^-1)
;		p  : Pressure (in hPa)
;		ps : Surface Pressure (in hPa)
;		Z0 : Surface Geopotential height (in m)
; Output:
;		value : Geopotential Height (in m)
; Keywords:
;		None.
; Author and history:
;		Cameron R. Homeyer  2014-05-08.
;-

COMPILE_OPT IDL2																									;Set Compile Options

eps       = 0.621970585																							;Ratio of the molecular weights of water and dry air
T_virtual = T*(1 + (SH/(1-SH))*(1-eps)/eps)																;Compute virtual temperature
dz        = (!Rair*0.5*(T_virtual + SHIFT(T_virtual,0,0,1))/!g)*$									;Use hydrostatic equation to compute layer thickness
					(ALOG(SHIFT(p,0,0,1)) - ALOG(p))

dz[*,*,0] = (!Rair*T_virtual[*,*,0]/!g)*(ALOG(ps) - ALOG(p[*,*,0])) + Z0
;dz[*,*,0] = Z0

RETURN, TOTAL(dz,3,/CUMULATIVE)																				;Return geopotential height

END
