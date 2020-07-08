PRO H2O_MASS_LAYER_COPY, run, experiment, start_date, end_date, $
	DOMAIN   = domain, $
	REGION   = region, $
	WRAPPING = wrapping, $
	MID_LAT  = mid_lat, $
	TROPICS  = tropics, $
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

	count = 0
 	IF (KEYWORD_SET(tropics)) THEN igood = WHERE(trop_3d GT 15000.0, count, COMPLEMENT = ibad)
 	IF (KEYWORD_SET(mid_lat)) THEN igood = WHERE(trop_3d LT 15000.0, count, COMPLEMENT = ibad)

	IF (count GT 0) THEN BEGIN
        z           [ibad] = !Values.F_NaN
        trop_3d     [ibad] = !Values.F_NaN
        cloud		[ibad] = !Values.F_NaN
        h2o         [ibad] = !Values.F_NaN
        theta.values[ibad] = !Values.F_NaN
        updrt   	[ibad] = !Values.F_NaN
        temp		[ibad] = !Values.F_NaN
        u			[ibad] = !Values.F_NaN
        v			[ibad] = !Values.F_NaN
        press		[ibad] = !Values.F_NaN
    ENDIF
	
	min_trop = MIN(trop_3d,/NAN)
    xyz_trop = FLTARR(dim[0],dim[1],dim[2]) + min_trop
    IF (count GT 0) THEN xyz_trop [ibad] = !Values.F_NaN
    
	molec_weight = 18.0
    r_star = 8.314
	
	;convect = WHERE(updrt GT 0.25, count, COMPLEMENT = nconv)
	;IF (KEYWORD_SET(WRAPPING)) THEN h2o [convect] = !Values.F_NaN
    ;h2o [convect] = !Values.F_NaN
    overshoot  = 0.001*((MAX((cloud GE 1.0E-5)*z, DIM = 3, /NAN)) - trop_3d)						;Set map variable to cloud top altitude
    
    ;Convert o3 from ppm to kg
    h2o_ugm3  = (h2o * press*1.0E2 * molec_weight) / (r_star * temp)
    h2o_ug    = h2o_ugm3*3000.0*3000.0*250.0
    h2o_kg    = h2o_ug*1.0E-9
	h2o_kg_2d = FLTARR(dim[0],dim[1]) * !Values.F_NaN
	h2o_ppm_2d = FLTARR(dim[0],dim[1]) * !Values.F_NaN

	flux_u   = FLTARR(dim[2])
	flux_v   = FLTARR(dim[2])
	flux_tot = FLTARR(dim[2])

 	;Estimate Flux of h2o in and out of domain
	FOR k = 0, dim[2]-1 DO BEGIN
		flux_u  [k] = (MEAN((u[5,*,k]*h2o_kg[5,*,k] - u[dim[0]-5,*,k]*h2o_kg[dim[0]-5,*,k]),/NAN)) / ((dim[0] - 11) * 3000.0)
		flux_v  [k] = (MEAN((v[*,5,k]*h2o_kg[*,5,k] - v[*,dim[1]-5,k]*h2o_kg[*,dim[1]-5,k]),/NAN)) / ((dim[1] - 11) * 3000.0)
		flux_tot[k] = flux_u[k] + flux_v[k]
	ENDFOR

	time_flux = [[[time_flux]], [[flux_tot]]]

	;!!!!! COMMENTED OUT FOR ALL VERTICAL LEVELS !!!!!!!
	;Find min vertical level at tropopause for initial time only
	IF (date_index EQ 0) THEN BEGIN
		PRINT, 'Find P95 level of tropopause'
		FOR ii = 0, dim[0]-1 DO BEGIN
			FOR jj = 0, dim[1]-1 DO BEGIN
				index1[ii,jj] = MAX(VALUE_LOCATE(REFORM(z[ii,jj,*],dim[2],1,1),REFORM(trop_3d[ii,jj,*],dim[2],1,1)))
			ENDFOR
		ENDFOR
		PRINT, 'Done finding P95 level of tropopause' 
	ENDIF

	index_nan = WHERE(index1 LE 0.0,nan_count)  
	index1[index_nan] = !Values.F_NaN
	;Calculate Mean/Total h2o concentration in layer
	index0 = index1[PERCENTILE(index1,95,/NOSORT,/NAN)]
	index = index0+5
;	index0 = 0
;	index = dim[2]-1		
	; !!!!! END COMMENT !!!!!
	
	flux_layer = MEAN(flux_tot[index0:index],/NAN)
	PRINT, index0, index, flux_tot[index0:index]
	time_layer_flux = [time_layer_flux, flux_layer]
	PRINT, 'layer flux = ', flux_layer
	h2o_kg_2d = h2o_kg[5:dim[0]-5,5:dim[1]-5,index0:index]

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
   	    inan  = WHERE(FINITE(h2o_kg[5:dim[0]-5,5:dim[1]-5,index0:index],/NAN),nan_count, COMPLEMENT = ivalue, NCOMPLEMENT = value_count)
  		h2o_wrap = (TOTAL(h2o_kg_2d,/NAN))/value_count
  		h2o_total = TOTAL(h2o_kg_2d,/NAN) 
	ENDELSE  	
  	
    PRINT, date
    PRINT, 'Layer Limits: ', MEAN(z[*,*,index0],/NAN), MEAN(z[*,*,index],/NAN)
    h2o_array   = [h2o_array, h2o_wrap]
    h2o_tot_arr = [h2o_tot_arr, h2o_total]
    PRINT, h2o_wrap
    
    date_index = date_index + 1
ENDFOREACH

dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string

outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/plots/h2o_timeseries/'

epsfile = outdir + dom_string + '_' + date_string + '_kg.eps'											;EPS filename
pdffile = outdir + dom_string + '_' + date_string + '_kg.pdf'											;PDF filename
pngfile = outdir + dom_string + '_' + date_string + '_kg.png'											;PNG filename

FILE_MKDIR, outdir																								;Create output directory, if necessary
wfactor = 400.0/(dim[0]) + 400.0/(dim[1])																	;Set map factor

IF KEYWORD_SET(z_buff) THEN BEGIN
	SET_PLOT, 'Z'																									;Output to Z buffer
	DEVICE, SET_PIXEL_DEPTH = 24, SET_RESOLUTION = [wfactor*(dim[0]), wfactor*(dim[1])], $	;Set device resolution and bit depth
		SET_CHARACTER_SIZE = [12, 20]
	!P.COLOR      = COLOR_24('black')																		;Foreground color
	!P.BACKGROUND = COLOR_24('white')																		;Background color
	!P.CHARSIZE   = 1.5																							;Set character size
	!P.FONT       = -1
ENDIF ELSE BEGIN
	IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN	
		PS_ON, FILENAME = epsfile, PAGE_SIZE = [16.0,8.0], MARGIN = 0.0, /INCHES;PAGE_SIZE = 0.001*dim*wfactor			;Switch to Postscript device
		DEVICE, /ENCAPSULATED
		!P.FONT     = 0																								;Hardware fonts
		!P.CHARSIZE = 0.75	
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS																							;Load basic color definitions
	ENDIF ELSE BEGIN
		SET_PLOT, 'X'
		WINDOW, XSIZE = wfactor*(dim[0]), YSIZE = wfactor*(dim[1])										;Open graphics window
		!P.COLOR      = COLOR_24('black')																		;Foreground color
		!P.BACKGROUND = COLOR_24('white')																		;Background color
		!P.CHARSIZE   = 2.0		
		!P.FONT       = -1																							;Use Hershey fonts
	ENDELSE
ENDELSE


;IF (event) EQ 'seasonal_final' THEN num_sec = 3600.0
num_sec = 3600.0
dh2o_dt = FLTARR(N_ELEMENTS(date_arr))
FOR tt = 0, N_ELEMENTS(date_arr) -3 DO BEGIN
	dh2o_dt[tt] = (h2o_array[tt+1] - h2o_array[tt]) / num_sec
	PRINT, dh2o_dt[tt]
ENDFOR

PRINT, MAX(dh2o_dt,/NAN)
PRINT, MIN(dh2o_dt,/NAN)

IF (run EQ '20110518') THEN BEGIN 
	PLOT, SHIFT(dh2o_dt,1), $
		TITLE    = run, $
		YRANGE   = [-6.0e-1,6.0e-1], $		
		YTITLE   = 'h2o (kg/s)', $
		COLOR    = COLOR_24('black'), $
		CHARSIZE = 2, $
		/NODATA

	AXIS, YAXIS = 0, $																								;Draw altitude axis
		SAVE     = 1, $
		YRANGE   = [-6.0e-1,6.0e-1], $
		YTITLE   = 'h2o (kg/s)', $
		COLOR    = COLOR_24('black'), $
		YTICKS   = 1, $
		YSTYLE   = 1, $
		CHARSIZE = 2
	
	OPLOT, SHIFT(dh2o_dt,1), COLOR = COLOR_24('red'),  THICK=3
	OPLOT, time_layer_flux , COLOR = COLOR_24('blue'), THICK=3
	OPLOT, [0,250], [0,0]  , COLOR = COLOR_24('black'),THICK=3

	net = SHIFT(dh2o_dt,1) - time_layer_flux
	OPLOT, net, COLOR = COLOR_24('darkgreen'), THICK=3

	AXIS, YAXIS = 1, $																								;Draw altitude axis
		SAVE     = 1, $
		YRANGE   = [8E3,25E3], $
		YTITLE   = 'h2o (kg)', $
		COLOR    = COLOR_24('black'), $
		YTICKS   = 1, $
		YSTYLE   = 1, $
		CHARSIZE = 2

	OPLOT, h2o_array, COLOR = COLOR_24('gray60'), THICK=3

	XYOUTS, 215, 23.5E3, 'dh2o/dt' , COLOR = COLOR_24('red'   	  ), CHARSIZE=2, /DATA
	XYOUTS, 215, 22.5E3, 'h2o flux', COLOR = COLOR_24('blue'  	  ), CHARSIZE=2, /DATA
	XYOUTS, 215, 21.5E3, 'Net'     , COLOR = COLOR_24('darkgreen' ), CHARSIZE=2, /DATA
	XYOUTS, 215, 20.5E3, 'h2o'     , COLOR = COLOR_24('gray60'	  ), CHARSIZE=2, /DATA
ENDIF

IF (run EQ '20130805') THEN BEGIN 
	PLOT, SHIFT(dh2o_dt,1), $
		TITLE    = run, $
		YRANGE   = [-4.0e-1,4.0e-1], $
		YTITLE   = 'h2o (kg/s)', $
		COLOR    = COLOR_24('black'), $
		CHARSIZE = 2, $
		/NODATA

	AXIS, YAXIS = 0, $																								;Draw altitude axis
		SAVE     = 1, $
		YRANGE   = [-4.0e-1,4.0e-1], $
		YTITLE   = 'h2o (kg/s)', $
		COLOR    = COLOR_24('black'), $
		YTICKS   = 1, $
		YSTYLE   = 1, $
		CHARSIZE = 2
	
	OPLOT, SHIFT(dh2o_dt,1), COLOR = COLOR_24('red'),  THICK=3
	OPLOT, time_layer_flux , COLOR = COLOR_24('blue'), THICK=3
	OPLOT, [0,250], [0,0]  , COLOR = COLOR_24('black'),THICK=3

	net = SHIFT(dh2o_dt,1) - time_layer_flux
	OPLOT, net, COLOR = COLOR_24('darkgreen'), THICK=3

	AXIS, YAXIS = 1, $																								;Draw altitude axis
		SAVE     = 1, $
		YRANGE   = [25E3,35E3], $
		YTITLE   = 'h2o (kg)', $
		COLOR    = COLOR_24('black'), $
		YTICKS   = 1, $
		YSTYLE   = 1, $
		CHARSIZE = 2

	OPLOT, h2o_array, COLOR = COLOR_24('gray60'), THICK=3

	XYOUTS, 215, 140, 'dh2o/dt' , COLOR = COLOR_24('red'   ), CHARSIZE=2, /DATA
	XYOUTS, 215, 133, 'h2o flux', COLOR = COLOR_24('blue'  ), CHARSIZE=2, /DATA
	XYOUTS, 215, 126, 'Net'     , COLOR = COLOR_24('green' ), CHARSIZE=2, /DATA
	XYOUTS, 215, 119, 'h2o'     , COLOR = COLOR_24('gray60'), CHARSIZE=2, /DATA
ENDIF

IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file

STOP
END

