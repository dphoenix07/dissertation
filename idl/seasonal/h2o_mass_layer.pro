PRO H2O_MASS_LAYER, run, experiment, start_date, end_date, $
	DOMAIN   = domain, $
	REGION   = region, $
	WRAPPING = wrapping, $
	PNG	     = png, $
	EPS   	 = eps


;+
; Name:
;		H2O_MASS_LAYER
; Purpose:
;		Calculates mass (kg) of H2O in some layer
; Calling sequence:
;		H2O_MASS_LAYER, run, scheme, start_date, end_date
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		scheme 	   : Model initial state. Typically 'morrison' or 'nssl'.
;		start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;		end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
; Output:
;		Timeseries of dH2O/dt, H2O flux along boundaries, and net change in H2O
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel B. Phoenix	    2018-11-22. 
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain

outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/plots/h2o_mass_layer/'
FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(run, experiment, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(run, experiment, start_date, end_date, /DATE)	


z = (WRF_READ_VAR('Z', date_arr[0], run, experiment, DOMAIN = domain, INDICES = region)).values
dim = SIZE(z,/DIMENSIONS)

time_flux   	= [ ]
time_layer_flux = [ ]
h2o_array 		= [ ]
h2o_tot_arr 	= [ ]
date_index = 0
index1 = FLTARR(dim[0],dim[1])*!Values.F_NaN

FOREACH date, date_arr DO BEGIN
    x     = WRF_READ_VAR('Longitude'       , date, run, experiment, DOMAIN = domain, INDICES = region)		;Read variables
    y     = WRF_READ_VAR('Latitude'        , date, run, experiment, DOMAIN = domain, INDICES = region)
    z     = (WRF_READ_VAR('Z' 	           , date, run, experiment, DOMAIN = domain, INDICES = region)).values
    u     = (WRF_READ_VAR('u' 	           , date, run, experiment, DOMAIN = domain, INDICES = region)).values
    v     = (WRF_READ_VAR('v' 	           , date, run, experiment, DOMAIN = domain, INDICES = region)).values
 	w     = (WRF_READ_VAR('w'              , date, run, experiment, DOMAIN = domain, INDICES = region)).values
  	cloud = (WRF_READ_VAR('CLOUD_MIX_TOTAL', date, run, experiment, DOMAIN = domain, INDICES = region)).values
	h2o   = (WRF_READ_VAR('H2O'			   , date, run, experiment, DOMAIN = domain, INDICES = region)).values *1.0E6
    updrt = (WRF_READ_VAR('Updraft_tracer' , date, run, experiment, DOMAIN = domain, INDICES = region)).values
    press = (WRF_READ_VAR('P'	           , date, run, experiment, DOMAIN = domain, INDICES = region)).values							
    ztrop = (WRF_READ_VAR('Z_trop'         , date, run, experiment, DOMAIN = domain, INDICES = region)).values
    temp  = (WRF_READ_VAR('T'	           , date, run, experiment, DOMAIN = domain, INDICES = region)).values						
    theta =  WRF_READ_VAR('T'	           , date, run, experiment, DOMAIN = domain, INDICES = region)								;Read temperature variable from WRF output
    theta.values = ((1000.0/(WRF_READ_VAR('P', date, run, experiment, $										;Compute potential temperature
    		 						DOMAIN = domain, INDICES = region)).values)^(!Rair/!Cp))*(theta.values)

    ztrop    = MEDIAN(ztrop, 30)    
	trop_3d  = REBIN (ztrop, dim[0], dim[1], dim[2], /SAMPLE)
	min_trop = MIN(ztrop,/NAN)
    xyz_trop = FLTARR(dim[0],dim[1],dim[2]) + min_trop

	molec_weight = 18.0
    r_star = 8.314

	flux_u   = FLTARR(dim[2])
	flux_v   = FLTARR(dim[2])
	flux_tot = FLTARR(dim[2])

 	;Estimate Flux of h2o in and out of domain
	FOR k = 0, dim[2]-1 DO BEGIN
		flux_u  [k] = (MEAN((u[5,*,k]*h2o[5,*,k] - u[dim[0]-5,*,k]*h2o[dim[0]-5,*,k]),/NAN)) / ((dim[0] - 11) * 500.0)
		flux_v  [k] = (MEAN((v[*,5,k]*h2o[*,5,k] - v[*,dim[1]-5,k]*h2o[*,dim[1]-5,k]),/NAN)) / ((dim[1] - 11) * 500.0)
		flux_tot[k] = flux_u[k] + flux_v[k]
	ENDFOR

	time_flux = [[[time_flux]], [[flux_tot]]]
	
	convect = WHERE(updrt GT 0.25, count, COMPLEMENT = nconv)
	IF (KEYWORD_SET(WRAPPING)) THEN h2o [convect] = !Values.F_NaN
    h2o [convect] = !Values.F_NaN
    overshoot  = 0.001*((MAX((cloud GE 1.0E-5)*z, DIM = 3, /NAN)) - trop_3d)						;Set map variable to cloud top altitude

	h2o_ppm_2d = FLTARR(dim[0],dim[1]) * !Values.F_NaN
	
	;!!!!! COMMENTED OUT FOR ALL VERTICAL LEVELS !!!!!!!
	;Find min vertical level at tropopause for initial time only
	IF (date_index EQ 0) THEN BEGIN
		PRINT, 'Find minimum level of tropopause'
		FOR ii = 0, dim[0]-1 DO BEGIN
			FOR jj = 0, dim[1]-1 DO BEGIN
				index1[ii,jj] = MIN(VALUE_LOCATE(REFORM(z[ii,jj,*],dim[2],1,1),REFORM(xyz_trop[ii,jj,*],dim[2],1,1)))
			ENDFOR
		ENDFOR
		PRINT, 'Done finding minimum level of tropopause' 
	ENDIF

	;Calculate Mean/Total h2o concentration in layer
	index0 = MIN(index1,/NAN)
	index = index0+5
;	index0 = 0
;	index = dim[2]-1		
	; !!!!! END COMMENT !!!!!

	flux_layer = MEAN(flux_tot[index0:index],/NAN)
	time_layer_flux = [time_layer_flux, flux_layer]
	PRINT, 'layer flux = ', flux_layer
	h2o_ppm_2d = h2o[5:dim[0]-5,5:dim[1]-5,index0:index]


	IF (KEYWORD_SET(WRAPPING)) THEN BEGIN
	    PRINT, 'wrapping keyword set'
	    box = 30
	    anvil = SMOOTH(FLOAT((cloud GT 1.0E-5)), [box,box,1])
	    igood = WHERE((anvil[*,*,index0:index] GT 0.1) AND (anvil[*,*,index0:index] LT 0.25), count) 
   	    inan  = WHERE(FINITE(h2o[*,*,index0:index],/NAN),nan_count, COMPLEMENT = ivalue, NCOMPLEMENT = value_count)
    
 		IF (count GT 0) THEN h2o_wrap = (TOTAL(h2o_ppm_2d[igood],/NAN))/value_count 
 		PRINT, value_count
  	ENDIF ELSE BEGIN
   	    PRINT, 'wrapping keyword not set'
   	    inan  = WHERE(FINITE(h2o[5:dim[0]-5,5:dim[1]-5,index0:index],/NAN),nan_count, COMPLEMENT = ivalue, NCOMPLEMENT = value_count)
  		h2o_wrap = (TOTAL(h2o_ppm_2d,/NAN))/value_count
  		h2o_total = TOTAL(h2o_ppm_2d,/NAN) 
	ENDELSE  	
  	
    PRINT, date
    PRINT, 'Layer Limits: ', MEAN(z[*,*,index0],/NAN), MEAN(z[*,*,index],/NAN)
    h2o_array = [h2o_array, h2o_wrap]
    h2o_tot_arr = [h2o_tot_arr, h2o_total]
    PRINT, h2o_wrap
    
    date_index = date_index + 1
ENDFOREACH

dh2o_dt = FLTARR(N_ELEMENTS(date_arr))
FOR tt = 0, N_ELEMENTS(date_arr) -3 DO BEGIN
	dh2o_dt[tt] = (h2o_array[tt+1] - h2o_array[tt]) / 300.0
	PRINT, dh2o_dt[tt]
ENDFOR

PLOT, SHIFT(dh2o_dt,1), $
	YRANGE   = [-2.0e-3,7e-3], $
	YTITLE   = 'h2o (ppm/s)', $
	COLOR    = COLOR_24('black'), $
	TITLE    = run, $
	CHARSIZE = 2, $
	/NODATA
	
OPLOT, SHIFT(dh2o_dt,1), COLOR = COLOR_24('red')
OPLOT, time_layer_flux , COLOR = COLOR_24('blue')
OPLOT, [0,100], [0,0]  , COLOR = COLOR_24('black')

net = SHIFT(dh2o_dt,1) - time_layer_flux
OPLOT, net, COLOR = COLOR_24('green')

XYOUTS, 20, 4.0E-3, 'dh2o/dt' , COLOR = COLOR_24('red'  ), CHARSIZE=2, /DATA
XYOUTS, 20, 3.0E-3, 'h2o flux', COLOR = COLOR_24('blue' ), CHARSIZE=2, /DATA
XYOUTS, 20, 2.0E-3, 'Net'     , COLOR = COLOR_24('green'), CHARSIZE=2, /DATA

STOP
END

