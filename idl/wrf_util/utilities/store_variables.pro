FUNCTION RESHAPE_TROP, z, date, event, scheme, $
	DOMAIN		= domain, $
	BINNED      = binned, $
	FILTERING   = filtering, $
	THRESHOLD   = threshold, $
	IN_CLOUD    = in_cloud, $
	PDF			= pdf, $
	EPS         = eps, $
	PNG    		= png


;+
; Name:
;               RESHAPE_TROP
; Purpose:
;               This is a function to read tropopause values, call the function to 
;				calculate and filter values and then returns a 3 dimensional matrix
;				of filtered tropopause values.
; Calling sequence:
;               STORE_VARIABLES(date, event, scheme)
; Inputs:
;               event      : String variable of run name. (e.g., '20120519')
;				scheme     : String variable of microphysics scheme (e.g., 'morrison')
; Output:
;               A three-dimensional matrix of filtered tropopause values.
; Keywords:
;		DOMAIN    : Simulation domain number. Default is 2. 
;		BINNED    : Set to bin gas concentrations.
;		FILTERING : Set to filter out values where REFL > 30 dBZ.
;		THRESHOLD : Distance above tropopause mode to reset to mode (e.g., 2000.0 m)
;		IN_CLOUD  : Set to plot values where cloud is simulated (> 1 L-1)
;		EPS       : If set, output to PostScript.
;		PDF       : If set, output to PDF.
;		PNG	      : Set if output to PNG desired.
; Author and history:
;               Daniel B. Phoenix	2016-02-16.
;-

COMPILE_OPT IDL2 
						
dim = SIZE(z.values, /DIMENSIONS)																;Get geopotential height dimensions
														
z_trop = (WRF_READ_VAR('Z_trop', date, event, scheme, DOMAIN = domain)).values					;Read tropopause height data (x,y)
z_trop = CALC_TROP_MODE(z_trop, scheme, threshold) 												;Filter tropopause values

xyz_trop = REBIN(z_trop, dim[0], dim[1], dim[2], /SAMPLE)										;Rebin tropopause height to dimensions of geopotential height data

RETURN, xyz_trop

END
