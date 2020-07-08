PRO PLOT_OZONE_THETA_PROFILE, event, scheme, start_date, end_date, $
	DOMAIN   = domain, $
	REGION   = region, $
	HIST	 = hist, $
	PNG	     = png, $
	EPS   	 = eps


;+
; Name:
;		PLOT_OZONE_THETA_PROFILE
; Purpose:
;		Plot theta vs ozone profile to find the theta value of the ozone transition 
;		between the troposphere and stratosphere for a mid-latitude air mass.
; Calling sequence:
;		PLOT_OZONE_THETA_PROFILE, run, scheme, start_date, end_date
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		scheme 	   : Model initial state. Typically 'morrison' or 'nssl'.
;		start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;		end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
; Output:
;		Plot of theta vs ozone profile
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel B. Phoenix	    2018-01-10. 
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain


outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/plots/ozone_theta_profiles/'
epsfile = outdir + scheme + '_' + end_date  + '.eps'						;EPS filename
pdffile = outdir + scheme + '_' + end_date  + '.pdf'						;PDF filename
pngfile = outdir + scheme + '_' + end_date  + '.png'						;PNG filename

FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(event, scheme, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(event, scheme, start_date, end_date, /DATE)	


FOREACH date, date_arr DO BEGIN
	PRINT, date
	
    o3      = (WRF_READ_VAR('O3'             , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3					
    y       = (WRF_READ_VAR('Latitude'       , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    x       = (WRF_READ_VAR('Longitude'      , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    z       = (WRF_READ_VAR('Z'		         , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    ztrop   = (WRF_READ_VAR('Z_trop'	     , date, event, scheme, DOMAIN=domain, INDICES=region)).values	
	theta   = (WRF_READ_VAR('T'			     , date, event, scheme, DOMAIN=domain, INDICES=region)).values					
	theta   = ((1000.0/(WRF_READ_VAR('P', date, event, scheme, DOMAIN = domain, INDICES=region)).values)^(!Rair/!Cp))*(theta)

    dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
    ztrop    = CALC_TROP_MODE(ztrop, scheme, threshold) 												;Filter tropopause values
    xyz_trop = REBIN(ztrop, dim[0], dim[1], dim[2], /SAMPLE)
    yy       = REBIN(y,     dim[0], dim[1], dim[2], /SAMPLE)
    xx       = REBIN(x,     dim[0], dim[1], dim[2], /SAMPLE)
    filt     = WHERE(xyz_trop EQ 999999 , filt_count, COMPLEMENT = good, NCOMPLEMENT = good_count)
    
    xyz_trop[good] = xyz_trop[good]
    xyz_trop[filt] = !Values.F_NaN
 
	PLOT, o3, theta, XRANGE = [0, 300], YRANGE = [279, 360], XTITLE = 'Ozone (ppb)', YTITLE = 'Theta (K)'

ENDFOREACH	

END