PRO LMD_PROFILE, run, experiment, start_date, end_date, $
	DOMAIN   = domain, $
	REGION   = region, $
	WRAPPING = wrapping, $
	PNG	     = png, $
	EPS   	 = eps


;+
; Name:
;		LMD_PROFILE
; Purpose:
;		Calculates LMD using divergence and ice mass approximations
; Calling sequence:
;		LMD_PROFILE, run, scheme, start_date, end_date
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		scheme 	   : Model initial state. Typically 'morrison' or 'nssl'.
;		start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;		end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
; Output:
;		Profile of LMD using mass divergence and ice mass approximations
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel B. Phoenix	    2019-01-14. 
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain

outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/plots/lmd/'
FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(run, experiment, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(run, experiment, start_date, end_date, /DATE)	


z = (WRF_READ_VAR('Z', date_arr[0], run, experiment, DOMAIN = domain, INDICES = region)).values
dim = SIZE(z,/DIMENSIONS)

w_array 		= [ ]
w_tot_arr 	= [ ]
date_index = 0
index1 = FLTARR(dim[0],dim[1])*!Values.F_NaN

FOREACH date, date_arr DO BEGIN
	IF make_iso_date_string(date_arr,/COMPACT) EQ '20120530T230000' THEN region = [250,300,1000,784]
	IF make_iso_date_string(date_arr,/COMPACT) EQ '20120531T000000' THEN region = [325,275,1200,784]
	IF make_iso_date_string(date_arr,/COMPACT) EQ '20120531T010000' THEN region = [325,175,1300,784]
	IF make_iso_date_string(date_arr,/COMPACT) EQ '20120531T020000' THEN region = [350,075,1350,784]
	IF make_iso_date_string(date_arr,/COMPACT) EQ '20120531T030000' THEN region = [400,050,1400,784] 
    x     = WRF_READ_VAR('Longitude'       , date, run, experiment, DOMAIN = domain, INDICES = region)		;Read variables
    y     = WRF_READ_VAR('Latitude'        , date, run, experiment, DOMAIN = domain, INDICES = region)
    z     = (WRF_READ_VAR('Z' 	           , date, run, experiment, DOMAIN = domain, INDICES = region)).values
    u     = (WRF_READ_VAR('u' 	           , date, run, experiment, DOMAIN = domain, INDICES = region)).values
    v     = (WRF_READ_VAR('v' 	           , date, run, experiment, DOMAIN = domain, INDICES = region)).values
 	w     = (WRF_READ_VAR('w'              , date, run, experiment, DOMAIN = domain, INDICES = region)).values
	qice  = (WRF_READ_VAR('ICE_MIX'        , date, run, experiment, DOMAIN = domain, INDICES = region)).values
	refl  = (WRF_READ_VAR('REFL'     	   , date, run, experiment, DOMAIN = domain, INDICES = region)).values
;  	cloud = (WRF_READ_VAR('CLOUD_MIX_TOTAL', date, run, experiment, DOMAIN = domain, INDICES = region)).values
;	h2o   = (WRF_READ_VAR('H2O'			   , date, run, experiment, DOMAIN = domain, INDICES = region)).values *1.0E6
;    updrt = (WRF_READ_VAR('Updraft_tracer' , date, run, experiment, DOMAIN = domain, INDICES = region)).values
;    press = (WRF_READ_VAR('P'	           , date, run, experiment, DOMAIN = domain, INDICES = region)).values							
    ztrop = (WRF_READ_VAR('Z_trop'         , date, run, experiment, DOMAIN = domain, INDICES = region)).values
;    temp  = (WRF_READ_VAR('T'	           , date, run, experiment, DOMAIN = domain, INDICES = region)).values						
;    theta =  WRF_READ_VAR('T'	           , date, run, experiment, DOMAIN = domain, INDICES = region)								;Read temperature variable from WRF output
;    theta.values = ((1000.0/(WRF_READ_VAR('P', date, run, experiment, $										;Compute potential temperature
;    		 						DOMAIN = domain, INDICES = region)).values)^(!Rair/!Cp))*(theta.values)
	dim = SIZE(z,/DIMENSIONS)
    ztrop    = MEDIAN(ztrop, 30)    
	trop_3d  = REBIN (ztrop, dim[0], dim[1], dim[2], /SAMPLE)
	min_trop = MIN(ztrop,/NAN)
    xyz_trop = FLTARR(dim[0],dim[1],dim[2]) + min_trop

	dx = 500.0
	roh_0 = 1.22
	H = 7000.0
	roh = roh_0*EXP(z/H)
	iwc = (8.0E-3)*(refl+6.7)^0.61
	
	drohw_dz   = FLTARR(dim[0],dim[1],dim[2])
	dw_dz 	   = FLTARR(dim[0],dim[1],dim[2])
	divergence = FLTARR(dim[0],dim[1],dim[2])
	FOR ii = 0, dim[0]-1 DO BEGIN
		FOR jj = 0, dim[1]-1 DO BEGIN
			FOR kk = 0, dim[2]-2 DO BEGIN
				drohw_dz  [ii,jj,kk] = (roh[ii,jj,kk+1]*w[ii,jj,kk+1])-(roh[ii,jj,kk]*w[ii,jj,kk])/(z[ii,jj,kk+1]-z[ii,jj,kk])
				dw_dz	  [ii,jj,kk] = (w[ii,jj,kk+1]-w[ii,jj,kk]) / (z[ii,jj,kk+1]-z[ii,jj,kk])
				divergence[ii,jj,kk] = ((u[ii,jj,kk+1]-u[ii,jj,kk]) / (z[ii,jj,kk+1]-z[ii,jj,kk])) + ((v[ii,jj,kk+1]-v[ii,jj,kk]) / (z[ii,jj,kk+1]-z[ii,jj,kk]))
			ENDFOR
		ENDFOR
	ENDFOR

	PRINT, 'done calculating drohw_dz'
	
	mean_w		 = FLTARR(dim[2])
	mass_detrain = FLTARR(dim[2])
	mean_refl 	 = FLTARR(dim[2])
	mean_iwc 	 = FLTARR(dim[2])
	mean_z 	     = FLTARR(dim[2])
	mean_dwdz 	 = FLTARR(dim[2])
	mean_div 	 = FLTARR(dim[2])

	FOR kk = 0, dim[2]-2 DO BEGIN
		mean_w	     [kk] = MEAN(w[*,*,kk],/NAN)
		mass_detrain [kk] = TOTAL(drohw_dz[*,*,kk],/NAN)*dx^2
		mean_refl    [kk] = MEAN(refl[*,*,kk],/NAN)
    	mean_iwc	 [kk] = MEAN(iwc[*,*,kk],/NAN)
    	mean_z		 [kk] = MEAN(z[*,*,kk],/NAN)
    	mean_dwdz	 [kk] = MEAN(dw_dz[*,*,kk],/NAN)
    	mean_div	 [kk] = mean(divergence[*,*,kk],/NAN)
    ENDFOR
    
    
STOP
    date_index = date_index + 1
ENDFOREACH

mass_detrain[-1]=!Values.F_NaN
PLOT, mass_detrain, mean_z*1.0E-3, $
	YRANGE   = [0,15.0], $
	YTITLE   = 'Z (km)', $
	XRANGE   = [-2.5E11, 2.5E11], $
	XTITLE   = 'Total Mass Divergence (kg/m/s)', $
	COLOR    = COLOR_24('black'), $
	TITLE    = 'Level of Max. Detrainment', $
	CHARSIZE = 2
OPLOT, [-3.0E11, 3.0E11], [MEAN(ztrop)*1.0E-3,MEAN(ztrop)*1.0E-3], COLOR = COLOR_24('black'),THICK=2

mean_w[-1] = !Values.F_NaN
PLOT, mean_w*1.0E2, mean_z*1.0E-3, $
	YRANGE   = [0,15.0], $
	YTITLE   = 'Z (km)', $
	XRANGE   = [-2.0, 8.0], $
	XTITLE   = 'Mean Vertical Velocity (cm/s)', $
	COLOR    = COLOR_24('black'), $
	TITLE    = 'Mean Vertical Velocity', $
	CHARSIZE = 2
OPLOT, [-2.0, 8.0], [MEAN(ztrop)*1.0E-3,MEAN(ztrop)*1.0E-3], COLOR = COLOR_24('black'),THICK=2

mean_iwc[-1] = !Values.F_NaN
PLOT, mean_iwc*1.0E3, mean_z*1.0E-3, $
	YRANGE   = [0,15.0], $
	YTITLE   = 'Z (km)', $
	XRANGE   = [25.0, 31.0], $
	XTITLE   = 'Mean IWC (x10^3 g/m)', $
	COLOR    = COLOR_24('black'), $
	TITLE    = 'Mean IWC', $
	CHARSIZE = 2
OPLOT, [24.0,32.0], [MEAN(ztrop)*1.0E-3,MEAN(ztrop)*1.0E-3], COLOR = COLOR_24('black'),THICK=2

mean_div[-1] = !Values.F_NaN
PLOT, mean_div, mean_z*1.0E-3, $
	YRANGE   = [0,15.0], $
	YTITLE   = 'Z (km)', $
	XRANGE   = [-0.01, 0.01], $
	XTITLE   = 'Mean Divergence', $
	COLOR    = COLOR_24('black'), $
	TITLE    = 'Mean Divergence', $
	CHARSIZE = 2
OPLOT, [-0.01, 0.01], [MEAN(ztrop)*1.0E-3,MEAN(ztrop)*1.0E-3], COLOR = COLOR_24('black'),THICK=2

mean_dwdz[-1] = !Values.F_NaN
PLOT, mean_dwdz, mean_z*1.0E-3, $
	YRANGE   = [0,15.0], $
	YTITLE   = 'Z (km)', $
	XRANGE   = [-1.0E-5, 1.0E-5], $
	XTITLE   = 'Mean dw/dz', $
	COLOR    = COLOR_24('black'), $
	TITLE    = 'Mean dw/dz', $
	CHARSIZE = 2
OPLOT, [-1.0E-5, 1.0E-5], [MEAN(ztrop)*1.0E-3,MEAN(ztrop)*1.0E-3], COLOR = COLOR_24('black'),THICK=2

STOP
END

