FUNCTION GOFF_GRATCH_VAPOR_PRESSURE, T, ICE = ice

;+
; Name:
;		GOFF_GRATCH_VAPOR_PRESSURE
; Purpose:
;		This is a function to calculate saturation vapor pressure. 
; Calling sequence:
;		value = FUNCTION_TEMPLATE(arg1)
; Inputs:
;		T : Air temperature (in K). 
; Output:
;		es : Saturation vapor pressure (in hPa)
; Keywords:
;		ICE : If set, saturation vapor pressure over ice is returned. Default is 
;				saturation vapor pressure over liquid.
; Author and history:
;		Cameron R. Homeyer  2014-04-24.
;-

COMPILE_OPT IDL2																									;Set Compile Options

IF KEYWORD_SET(ice) THEN $
	es = 10.0^((-9.09718)*(273.16/T - 1.0) - $													;Compute saturation vapor pressure at temperatures below freezing
				 3.56654*ALOG10(273.16/T) + $
				 0.876793*(1.0-T/273.16) + $
				 ALOG10(6.1071)) $
ELSE $
	es = 10.0^((-7.90298)*(373.16/T - 1.0) + $													;Compute saturation vapor pressure at temperatures above freezing
				  5.02808*ALOG10(373.16/T) - $
				  (1.3816E-7)*(10.0^(11.344*(1.0 - T/373.16)) - 1.0) + $
				  (8.1328E-3)*(10.0^(-3.49149*(373.16/T - 1.0)) - 1.0) + $
				  ALOG10(1013.246))

RETURN, es

END
