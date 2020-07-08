FUNCTION WRF_REFLECTIVITY, vapor, rain, snow, graupel, T, P, $
	ILIQSKIN = iliqskin, $
	IN0S     = in0s, $
	IN0G     = in0g, $
	IN0R     = in0r

;+
; Name:
;		WRF_REFLECTIVITY
; Purpose:
;		This is a function to compute the equivalent radar reflectivity factor
;		from WRF simulations of the mixing ratios of rain, snow, and graupel. 
;		This routine mimics the code in RIP4.
; Calling sequence:
;		value = WRF_REFLECTIVITY(vapor, rain, snow, graupel, T, P)
; Inputs:
;		vapor   : Water vapor mixing ratio
;		rain    : Rain mixing ratio.
;		snow    : Snow mixing ratio.
;		graupel : graupel mixing ratio.
;		T       : Temperature.
;		P       : Pressure.
; Output:
;		value : Equivalent radar reflectivity factor for WRF variables.
; Keywords:
;		ILIQSKIN : If set, frozen particles have liquid skin.
;		IN0S     : If set, N_0s as in Thompson et al.
;		IN0G     : If set, N_0g as in Thompson et al.
;		IN0R     : If set, N_0r as in Thompson et al.
; Author and history:
;		Cameron R. Homeyer  2013-04-26.
;-

COMPILE_OPT IDL2																									;Set Compile Options

rn0_r = 8.e6    																									;Constant intercepts (m^-4)
rn0_s = 2.e7
rn0_g = 4.e6

r1          = 1.E-15																								;Constants used to calculate variable intercepts
ron         = 8.E6
ron2        = 1.E10
son         = 2.E7
gon         = 5.E7
ron_min     = 8.E6
ron_qr0     = 0.00010
ron_delqr0  = 0.25*ron_qr0
ron_const1r = (ron2-ron_min)*0.5
ron_const2r = (ron2+ron_min)*0.5

gamma_seven = 720.																								;Gamma function of 7
rho_r       = 1000.0																								;Density of water (kg m^-3)
rho_s       = 100.0																								;Density of snow (kg m^-3)
rho_g       = 400.0																								;Density of graupel (kg m^-3)
alpha       = 0.224
factor_r    = gamma_seven*(1.E18)*((1.0/(!Pi*rho_r))^1.75)
factor_s    = gamma_seven*(1.E18)*((1.0/(!Pi*rho_s))^1.75)*((rho_s/rho_r)^2)*alpha
factor_g    = gamma_seven*(1.E18)*((1.0/(!Pi*rho_g))^1.75)*((rho_g/rho_r)^2)*alpha

dim = SIZE(vapor, /DIMENSIONS)

rhoair=P*100./(!Rair*T*(0.622+vapor)/(0.622*(1.+vapor)))												;air density

;Adjust factor for brightband, where snow or graupel particle
;scatters like liquid water (alpha=1.0) because it is assumed to
;have a liquid skin.

factorb_s=factor_s
factorb_g=factor_g
IF KEYWORD_SET(iliqskin) THEN BEGIN
	ihigh = WHERE((T GT !CtoK), nhigh)
	IF (nhigh GT 0) THEN BEGIN
		factorb_s = REBIN(factor_s, dim)
		factorb_g = REBIN(factor_g, dim)
		
		factorb_s[ihigh]=factor_s/alpha
		factorb_g[ihigh]=factor_g/alpha
	ENDIF
ENDIF

;Calculate variable intercept parameters if wanted

IF KEYWORD_SET(in0s) THEN BEGIN																				;N_0s as in Thompson et al.
	temp_c = ((-0.001) < (T + !KtoC))
	sonv   = (2.0E8 < 2.0E6*EXP(-0.12*temp_c))
ENDIF ELSE $
	sonv = rn0_s

IF KEYWORD_SET(in0g) THEN BEGIN																				;N_0g as in Thompson et al.
	gonv     = gon
	igraupel = WHERE((graupel GT r1), ngraupel)
	IF (ngraupel GT 0) THEN BEGIN
		gonv = REBIN(gonv, dim)

		gonv[igraupel] = 2.38*((!Pi*rho_g/(rhoair*graupel[igraupel]))^0.92)

		gonv = (1.e4 > (gonv < gon))
	ENDIF
ENDIF ELSE $
	gonv = rn0_g

IF KEYWORD_SET(in0r) THEN BEGIN																				;N_0r as in Thompson et al.
	ronv  = ron2
	irain = WHERE((rain GT r1), nrain)
	IF (nrain GT 0) THEN BEGIN
		ronv = REBIN(ronv, dim)
		
		ronv[irain] = ron_const1r*TANH((ron_qr0-rain[irain])/ron_delqr0) + ron_const2r
	ENDIF
ENDIF ELSE $
	ronv = rn0_r

;Total equivalent reflectivity factor (z_e, in mm^6 m^-3) is
;the sum of z_e for each hydrometeor species:

z_e =   factor_r*( (rhoair*rain   )^1.75)/(ronv^0.75) $
		+ factorb_s*((rhoair*snow   )^1.75)/(sonv^0.75) $
		+ factorb_g*((rhoair*graupel)^1.75)/(gonv^0.75)

z_e = (z_e > 0.01)																								;Adjust small values of Z_e so that dBZ is no lower than -20

dbz = 10.0*ALOG10(z_e)																							;Convert to dBZ

RETURN, dbz

END
