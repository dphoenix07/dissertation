PRO WRITE_H2O_THETA_TIMESERIES_FILE, run, experiment, start_date, end_date, $
	DOMAIN        = domain, $
	REGION        = region, $
	CONVECTIVE 	  = convective, $
	NONCONVECTIVE = nonconvective, $
	TROPICS		  = tropics, $
	MIDLATS   	  = midlats, $
	PNG	     	  = png, $
	EPS   	 	  = eps


;+
; Name:
;		WRITE_H2O_THETA_TIMESERIES_FILE
; Purpose:
;		Calculates mass (kg) of O3 in some layer
; Calling sequence:
;		WRITE_H2O_THETA_TIMESERIES_FILE, run, scheme, start_date, end_date
;		WRITE_H2O_THETA_TIMESERIES_FILE, '20120530_ncar','d03_30km','20120530T2200Z','20120531T0500Z'
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		scheme 	   : Model initial state. Typically 'morrison' or 'nssl'.
;		start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;		end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
; Output:
;		Map of tropopause relative cloud tops with markers where cloud top > 1km above 
;		tropopause
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel B. Phoenix	    2018-10-22. 
;-

COMPILE_OPT IDL2																				;Set compile options

start_time = SYSTIME(/SECONDS)	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain

IF (experiment EQ 'd02_30km') THEN region = [120, 115, 524, 272]

IF (run EQ '20110518') THEN BEGIN
	midlats = 1
	PRINT, 'midlats = ', midlats
	lower_theta = 350.0
	upper_theta = 380.0
ENDIF

IF (run EQ '20130805') THEN BEGIN
	tropics = 1
	PRINT, 'tropics = ', tropics
	lower_theta = 370.0
	upper_theta = 400.0
ENDIF

outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/reduced/h2o_mass_layer_theta/'
FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(run, experiment, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(run, experiment, start_date, end_date, /DATE)	

z = (WRF_READ_VAR('Z', date_arr[0], run, experiment, DOMAIN = domain, INDICES = region)).values
dim = SIZE(z,/DIMENSIONS)

time_flux   	 	= [ ]
time_flux_tot   	= [ ]
time_layer_flux 	= [ ]
time_layer_flux_tot = [ ]
h2o_array 		= [ ]
h2o_tot_arr 		= [ ]

h2o_conv_arr     = [ ]
h2o_nconv_arr    = [ ]
h2o_conv_tarr    = [ ]
h2o_nconv_tarr   = [ ]

cloud_frac_tot   = [ ]
trop_arr		 = [ ]
trop_95          = [ ]
trop_5           = [ ]
trop_ave	     = [ ]
layer_top        = [ ]

value_count_arr  = [ ]

date_index = 0
index0  = FLTARR(dim[0],dim[1])*!Values.F_NaN

FOREACH date, date_arr DO BEGIN
	PRINT, date

    x     = WRF_READ_VAR('Longitude'       , date, run, experiment, DOMAIN = domain, INDICES = region)		;Read variables
    y     = WRF_READ_VAR('Latitude'        , date, run, experiment, DOMAIN = domain, INDICES = region)
    z     = (WRF_READ_VAR('Z' 	           , date, run, experiment, DOMAIN = domain, INDICES = region)).values
    u     = (WRF_READ_VAR('u' 	           , date, run, experiment, DOMAIN = domain, INDICES = region)).values
    v     = (WRF_READ_VAR('v' 	           , date, run, experiment, DOMAIN = domain, INDICES = region)).values
 	w     = (WRF_READ_VAR('w'              , date, run, experiment, DOMAIN = domain, INDICES = region)).values
  	cloud = (WRF_READ_VAR('CLOUD_MIX_TOTAL', date, run, experiment, DOMAIN = domain, INDICES = region)).values
	h2o   = (WRF_READ_VAR('H2O'			   , date, run, experiment, DOMAIN = domain, INDICES = region)).values 
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
	IF (KEYWORD_SET(tropics)) THEN igood = WHERE(trop_3d GE 14000.0, count, COMPLEMENT = ibad)
	IF (KEYWORD_SET(midlats)) THEN igood = WHERE(trop_3d LT 14000.0, count, COMPLEMENT = ibad)

	h2o_nan = h2o
	IF (count GT 0) THEN BEGIN
		h2o [ibad] = 0.0
		h2o_nan[ibad] = !Values.F_NaN
	ENDIF

	icloud = WHERE(cloud GE 1.0E-5, numcloud, COMPLEMENT=ncloud, NCOMPLEMENT=nnumcloud)
	IF (numcloud GT 0) THEN BEGIN
		h2o    [icloud] = 0.0
		h2o_nan[icloud] = !Values.F_NaN
	ENDIF
	
	molec_weight = 18.0
    r_star = 8.314
	
    overshoot  = 0.001*((MAX((cloud GE 1.0E-5)*z, DIM = 3, /NAN)) - trop_3d)						;Set map variable to cloud top altitude

	IF (experiment EQ 'd02_30km'	   ) THEN dx = 2500.0
	IF (experiment EQ 'd03_30km'	   ) THEN dx = 500.0
	IF (experiment EQ 'd03_30km_icloud') THEN dx = 500.0
	IF ((run EQ '20110518') OR (run EQ '20130805')) THEN dx = 3000.0
   
    ;Convert h2o from ppm to kg
    h2o_ugm3  = (h2o * press*1.0E2 * molec_weight) / (r_star * temp)
    h2o_ug    = h2o_ugm3*dx*dx*250.0
    h2o_kg    = h2o_ug*1.0E-9
	h2o_kg_2d = FLTARR(dim[0],dim[1]) * !Values.F_NaN
	h2o_kg_ave= FLTARR(dim[0],dim[1]) * !Values.F_NaN
	cloud_fraction = FLTARR(dim[0],dim[1]) *!Values.F_NaN
			
	flux_u_left_ave   = FLTARR(1,dim[1])
	flux_u_right_ave  = FLTARR(1,dim[1])
	flux_v_bottom_ave = FLTARR(dim[0],1)
	flux_v_top_ave	  = FLTARR(dim[0],1)

	flux_u_left_tot   = FLTARR(1,dim[1])
	flux_u_right_tot  = FLTARR(1,dim[1])
	flux_v_bottom_tot = FLTARR(dim[0],1)
	flux_v_top_tot	  = FLTARR(dim[0],1)
	
	value_count  = FLTARR(dim[0],dim[1])

	h2o_conv_2d = FLTARR(dim[0],dim[1])
	h2o_nconv_2d = FLTARR(dim[0],dim[1])

	;value_count_conv  = FLTARR(dim[0],dim[1])
	;value_count_nconv = FLTARR(dim[0],dim[1])
		

	FOR ii = 5, dim[0]-5 DO BEGIN
		FOR jj = 5, dim[1]-5 DO BEGIN
;			index0 = MEAN(VALUE_LOCATE(REFORM(z[ii,jj,*],dim[2],1,1),REFORM(trop_3d[ii,jj,*],dim[2],1,1)))
			index0 = MEAN(VALUE_LOCATE(REFORM(theta.values[ii,jj,*],dim[2],1,1),lower_theta))
			index1 = MEAN(VALUE_LOCATE(REFORM(theta.values[ii,jj,*],dim[2],1,1),upper_theta))

			IF (index1 EQ index0) THEN index1 = index0+4
			nz = index1 - index0
			
			cloudlocs = WHERE(cloud[ii,jj,index0:(index1)] GT 1.0E-5)
			cloud_fraction [ii,jj] = FLOAT(N_ELEMENTS(cloudlocs))/FLOAT((N_ELEMENTS(cloud[ii,jj,index0:(index1)])))
		
			;Estimate Flux of h2o in and out of domain
			;IF (ii EQ 	     5) THEN flux_u_left[*,jj,*]   = u[ii,jj,index0:(index1)]*h2o_kg[ii,jj,index0:(index1)]
			;IF (ii EQ dim[0]-5) THEN flux_u_right[*,jj,*]  = u[ii,jj,index0:(index1)]*h2o_kg[ii,jj,index0:(index1)]
			;IF (jj EQ 	     5) THEN flux_v_bottom[ii,*,*] = v[ii,jj,index0:(index1)]*h2o_kg[ii,jj,index0:(index1)]
			;IF (jj EQ dim[1]-5) THEN flux_v_top[ii,*,*]    = v[ii,jj,index0:(index1)]*h2o_kg[ii,jj,index0:(index1)]
			IF (ii EQ 	     5) THEN BEGIN
				nanl = WHERE((u[ii,jj,index0:(index1)]*h2o_kg[ii,jj,index0:(index1)]) NE 0.0, left_count0)
				flux_u_left_ave  [*,jj] = TOTAL(u[ii,jj,index0:(index1)]*h2o_kg[ii,jj,index0:(index1)],/NAN) / left_count0
				flux_u_left_tot  [*,jj] = TOTAL(u[ii,jj,index0:(index1)]*h2o_kg[ii,jj,index0:(index1)],/NAN) 
			ENDIF
			IF (ii EQ dim[0]-5) THEN BEGIN
				nanr = WHERE((u[ii,jj,index0:(index1)]*h2o_kg[ii,jj,index0:(index1)]) NE 0.0, right_count0)
				flux_u_right_ave [*,jj] = TOTAL(u[ii,jj,index0:(index1)]*h2o_kg[ii,jj,index0:(index1)],/NAN) / right_count0
				flux_u_right_tot [*,jj] = TOTAL(u[ii,jj,index0:(index1)]*h2o_kg[ii,jj,index0:(index1)],/NAN) 
			ENDIF
			IF (jj EQ 	     5) THEN BEGIN
				nanb = WHERE((v[ii,jj,index0:(index1)]*h2o_kg[ii,jj,index0:(index1)]) NE 0.0, bottom_count0)
				flux_v_bottom_ave[ii,*] = TOTAL(v[ii,jj,index0:(index1)]*h2o_kg[ii,jj,index0:(index1)],/NAN) / bottom_count0
				flux_v_bottom_tot[ii,*] = TOTAL(v[ii,jj,index0:(index1)]*h2o_kg[ii,jj,index0:(index1)],/NAN) 
			ENDIF
			IF (jj EQ dim[1]-5) THEN BEGIN
				nant = WHERE((v[ii,jj,index0:(index1)]*h2o_kg[ii,jj,index0:(index1)]) NE 0.0, top_count0)
				flux_v_top_ave   [ii,*] = TOTAL(v[ii,jj,index0:(index1)]*h2o_kg[ii,jj,index0:(index1)],/NAN) / top_count0
				flux_v_top_tot   [ii,*] = TOTAL(v[ii,jj,index0:(index1)]*h2o_kg[ii,jj,index0:(index1)],/NAN) 
			ENDIF

			h2o_kg_2d    [ii,jj] = TOTAL(h2o_kg[ii,jj,index0:(index1)])
			
			h2o_conv  = FLTARR(nz)
			h2o_nconv = FLTARR(nz)
	
			FOR kk = 0, nz-1 DO BEGIN
				IF (updrt[ii,jj,kk+index0] GE 0.25) THEN h2o_conv  [kk] = h2o_kg [ii,jj,kk+index0]
				IF (updrt[ii,jj,kk+index0] LT 0.25) THEN h2o_nconv [kk] = h2o_kg [ii,jj,kk+index0]
			ENDFOR
			
			h2o_conv_2d  [ii,jj] = TOTAL(h2o_conv ,/NAN)
			h2o_nconv_2d [ii,jj] = TOTAL(h2o_nconv,/NAN)
			
			inan     = WHERE(FINITE(h2o_nan      [ii,jj,index0:(index1)],/NAN),nan_count, COMPLEMENT = ivalue, NCOMPLEMENT = value_count1)
			;inan_c   = WHERE(FINITE(h2o_conv [ii,jj,index0[ii,jj]:(index0[ii,jj]+10)],/NAN),nan_count, COMPLEMENT = ivalue, NCOMPLEMENT = value_count_conv1)
			;inan_nc  = WHERE(FINITE(h2o_nconv[ii,jj,index0[ii,jj]:(index0[ii,jj]+10)],/NAN),nan_count, COMPLEMENT = ivalue, NCOMPLEMENT = value_count_nconv1)
			value_count [ii,jj] = value_count1			

			h2o_kg_ave [ii,jj] = h2o_kg_2d [ii,jj] / value_count1
		ENDFOR
	ENDFOR			
	
	nanl = WHERE(flux_u_left_ave   NE 0.0, left_count0)
	nanr = WHERE(flux_u_right_ave  NE 0.0, right_count0)
	nant = WHERE(flux_v_top_ave    NE 0.0, top_count0)
	nanb = WHERE(flux_v_bottom_ave NE 0.0, bottom_count0)
		
	flux_u_tot 	= (TOTAL((flux_u_left_tot - flux_u_right_tot),/NAN)) / ((dim[0] - 11) * dx)
	flux_v_tot 	= (TOTAL((flux_v_bottom_tot - flux_v_top_tot),/NAN)) / ((dim[1] - 11) * dx)

	flux_u_ave 	= (TOTAL((flux_u_left_ave - flux_u_right_ave),/NAN)) / ((dim[0] - 11) * dx)
	flux_v_ave 	= (TOTAL((flux_v_bottom_ave - flux_v_top_ave),/NAN)) / ((dim[1] - 11) * dx)

	flux_u 	= flux_u_ave / (left_count0 + right_count0)
	flux_v 	= flux_v_ave / (top_count0 + bottom_count0)
	
	flux_tot     = flux_u+flux_v
	flux_tot_tot = flux_u_tot+flux_v_tot
	
	time_flux   = [time_flux, flux_tot]
	time_flux_tot = [time_flux_tot, flux_tot_tot]
	  	
  	nan_ave = WHERE(FINITE(h2o_kg_ave,/NAN),nan_count, COMPLEMENT=ivalue, NCOMPLEMENT = value_count)
  	
	h2o_array   = [h2o_array, TOTAL(h2o_kg_ave,/NAN)/value_count]
	h2o_tot_arr = [h2o_tot_arr, TOTAL(h2o_kg_2d,/NAN)]
	
	;h2o_conv_arr   = [h2o_conv_arr  , h2o_ave_conv ]
	;h2o_nconv_arr  = [h2o_nconv_arr , h2o_ave_nconv]
	h2o_conv_tarr  = [h2o_conv_tarr , TOTAL(h2o_conv_2d ,/NAN)]
	h2o_nconv_tarr = [h2o_nconv_tarr, TOTAL(h2o_nconv_2d,/NAN)]
 
	cloud_frac_tot = [cloud_frac_tot, MEAN(cloud_fraction,/NAN)]
	
	value_count_arr = [[value_count_arr], [TOTAL(value_count,/NAN)]]
	date_index = date_index + 1

ENDFOREACH

dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string

outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/plots/h2o_timeseries_theta/'

epsfile = outdir + dom_string + '_' + date_string + '_final2.eps'											;EPS filename
pdffile = outdir + dom_string + '_' + date_string + '.pdf'											;PDF filename
pngfile = outdir + dom_string + '_' + date_string + '.png'											;PNG filename

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

dh2o_dt   = FLTARR(N_ELEMENTS(date_arr))
dh2oc_dt  = FLTARR(N_ELEMENTS(date_arr))
dh2onc_dt = FLTARR(N_ELEMENTS(date_arr))

IF (experiment EQ 'd02_30km'	   ) THEN dt = 3600.0
IF (experiment EQ 'd03_30km'	   ) THEN dt = 300.0
IF (experiment EQ 'd03_30km_icloud') THEN dt = 300.0
IF ((run EQ '20110518') OR (run EQ '20130805')) THEN dt = 3600.0

PRINT, 'dt= ', dt
FOR tt = 0, N_ELEMENTS(date_arr) -3 DO BEGIN
	dh2o_dt[tt]   = (h2o_array[tt+1]   - h2o_array[tt]  ) / dt
	;dh2oc_dt[tt]  = (h2o_conv_arr[tt+1]   - h2o_conv_arr[tt]  ) / dt
	;dh2onc_dt[tt] = (h2o_nconv_arr[tt+1] - h2o_nconv_arr[tt]) / dt
	
	;dh2o_dt[tt]   = (h2o_tot_arr[tt+1]   - h2o_tot_arr[tt]  ) / dt
	;dh2oc_dt[tt]  = (h2o_conv_tarr[tt+1]   - h2o_conv_tarr[tt]  ) / dt
	;dh2onc_dt[tt] = (h2o_nconv_tarr[tt+1] - h2o_nconv_tarr[tt]) / dt

	PRINT, dh2o_dt[tt]
ENDFOR

IF (run EQ '20110518') THEN BEGIN 
    PLOT, SHIFT(dh2o_dt,1), $
    	YRANGE 	 = [-8E-8,8E-8], $
    	YTITLE   = 'h2o (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	TITLE    = 'may h2o Mass (kg)', $
 		XTICKINTERVAL = 12, $
    	CHARSIZE = 2, $
    	/NODATA
    
    AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [-8E-8,8E-8], $
    	YTITLE   = 'h2o (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
  		XTICKINTERVAL = 12, $
	    CHARSIZE = 2
    	
   OPLOT, SHIFT(dh2o_dt,1), COLOR = COLOR_24('red'), THICK=2
   OPLOT, time_flux, COLOR = COLOR_24('blue'), THICK=3
   OPLOT, [0,250], [0,0], COLOR = COLOR_24('black'), THICK=2
    
    net = SHIFT(dh2o_dt,1) - time_flux
	h2o_vertflux = h2o_tot_arr+time_flux_tot*3600
	h2o_vertflux_conv =  h2o_conv_tarr +time_flux_tot*3600
	h2o_vertflux_nconv = h2o_nconv_tarr+time_flux_tot*3600
    OPLOT, net, COLOR = COLOR_24('darkgreen'), THICK=3
    
     AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [10.0, 30.0], $
    	YTITLE   = 'Cloud Fraction (%)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
  		XTICKINTERVAL = 12, $
	    CHARSIZE = 2
    	
    OPLOT, cloud_frac_tot*100.0, COLOR = COLOR_24('black'), THICK=4

    AXIS, YAXIS = 1, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE   = [5.0E1,1.5E4], $
    	YTITLE   = 'h2o (kg)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
 		XTICKINTERVAL = 12, $
    	CHARSIZE = 2
    
	;;OPLOT, h2o_conv_arr,  COLOR = COLOR_24('gray30'), THICK=3, LINESTYLE=2
	;;OPLOT, h2o_nconv_arr, COLOR = COLOR_24('gray70'), THICK=3, LINESTYLE=2	    
	
	OPLOT, h2o_vertflux_conv,  COLOR = COLOR_24('gray30'), THICK=3, LINESTYLE=2
	OPLOT, h2o_vertflux_nconv, COLOR = COLOR_24('gray70'), THICK=3, LINESTYLE=2	    
    
    ;;OPLOT, h2o_array, COLOR = COLOR_24('gray60'	 ), THICK=3

    AXIS, YAXIS = 1, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE   = [9.5E3,2.0E4], $
    	YTITLE   = 'h2o (kg)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
 		XTICKINTERVAL = 12, $
    	CHARSIZE = 2
    
	;OPLOT, h2o_conv_tarr,  COLOR = COLOR_24('gray30'), THICK=3, LINESTYLE=2
	;OPLOT, h2o_nconv_tarr, COLOR = COLOR_24('gray70'), THICK=3, LINESTYLE=2	    
    
    ;OPLOT, h2o_tot_arr, COLOR = COLOR_24('gray60'), THICK=3
    OPLOT, h2o_vertflux, COLOR = COLOR_24('gray60'), THICK=5

	; XYOUTS, 12, 2.95E4,'layer limits', COLOR = COLOR_24('red'	  ), CHARSIZE=2, /DATA
	; XYOUTS, 12, 2.90E4,'trop 95%'    , COLOR = COLOR_24('darkgreen'), CHARSIZE=2, /DATA
	; XYOUTS, 12, 2.85E4,'trop 5%'     , COLOR = COLOR_24('black'    ), CHARSIZE=2, /DATA
	; XYOUTS, 12, 2.80E4,'trop ave'    , COLOR = COLOR_24('blue'   ), CHARSIZE=2, /DATA
	; XYOUTS, 12, 2.75E4,'- Tot h2o'   , COLOR = COLOR_24('gray60'   ), CHARSIZE=2, /DATA
	; XYOUTS, 12, 2.70E4,'conv. fraction' , COLOR = COLOR_24('black'   ), CHARSIZE=2, /DATA

  XYOUTS, 12, 1.95E4, '- dh2o/dt' , COLOR = COLOR_24('red'	  ), CHARSIZE=2, /DATA
  XYOUTS, 12, 1.90E4, '- net' 	, COLOR = COLOR_24('darkgreen'), CHARSIZE=2, /DATA
  XYOUTS, 12, 1.85E4, '- h2o flux', COLOR = COLOR_24('blue'     ), CHARSIZE=2, /DATA
  XYOUTS, 12, 1.80E4, '- Tot h2o' , COLOR = COLOR_24('gray60'   ), CHARSIZE=2, /DATA
  XYOUTS, 12, 1.75E4, 'h2o-conv'  , COLOR = COLOR_24('gray30'   ), CHARSIZE=2, /DATA
  XYOUTS, 12, 1.70E4, 'h2o-nconv' , COLOR = COLOR_24('gray70'   ), CHARSIZE=2, /DATA
ENDIF

IF (run EQ '20130805') THEN BEGIN 
    PLOT, SHIFT(dh2o_dt,1), $
    	YRANGE 	 = [-8E-9,8E-9], $
    	YTITLE   = 'h2o (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	TITLE    = 'August h2o Mass (kg)', $
 		XTICKINTERVAL = 12, $
    	CHARSIZE = 2, $
    	/NODATA
    
    AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [-8E-9,8E-9], $
    	YTITLE   = 'h2o (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
  		XTICKINTERVAL = 12, $
	    CHARSIZE = 2
    	
   OPLOT, SHIFT(dh2o_dt,1), COLOR = COLOR_24('red'), THICK=2
   OPLOT, time_flux, COLOR = COLOR_24('blue'), THICK=3
   OPLOT, [0,250], [0,0], COLOR = COLOR_24('black'), THICK=2
    
    net = SHIFT(dh2o_dt,1) - time_flux
	h2o_vertflux = h2o_tot_arr+time_flux_tot*3600
	h2o_vertflux_conv =  h2o_conv_tarr +time_flux_tot*3600
	h2o_vertflux_nconv = h2o_nconv_tarr+time_flux_tot*3600
    OPLOT, net, COLOR = COLOR_24('darkgreen'), THICK=3
    
     AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [12.0, 20.0], $
    	YTITLE   = 'Cloud Fraction (%)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
  		XTICKINTERVAL = 12, $
	    CHARSIZE = 2
    	
    OPLOT, cloud_frac_tot*100.0, COLOR = COLOR_24('black'), THICK=4

    AXIS, YAXIS = 1, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE   = [2E2,8.5E3], $
    	YTITLE   = 'h2o (kg)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
 		XTICKINTERVAL = 12, $
    	CHARSIZE = 2
    
	;;OPLOT, h2o_conv_arr,  COLOR = COLOR_24('gray30'), THICK=3, LINESTYLE=2
	;;OPLOT, h2o_nconv_arr, COLOR = COLOR_24('gray70'), THICK=3, LINESTYLE=2	    
	
	OPLOT, h2o_vertflux_conv,  COLOR = COLOR_24('gray30'), THICK=3, LINESTYLE=2
	OPLOT, h2o_vertflux_nconv, COLOR = COLOR_24('gray70'), THICK=3, LINESTYLE=2	    
    
    ;;OPLOT, h2o_array, COLOR = COLOR_24('gray60'	 ), THICK=3

    AXIS, YAXIS = 1, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE   = [2E2,8.5E3], $
    	YTITLE   = 'h2o (kg)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
 		XTICKINTERVAL = 12, $
    	CHARSIZE = 2
    
	;OPLOT, h2o_conv_tarr,  COLOR = COLOR_24('gray30'), THICK=3, LINESTYLE=2
	;OPLOT, h2o_nconv_tarr, COLOR = COLOR_24('gray70'), THICK=3, LINESTYLE=2	    
    
    ;OPLOT, h2o_tot_arr, COLOR = COLOR_24('gray60'), THICK=3
    OPLOT, h2o_vertflux, COLOR = COLOR_24('gray60'), THICK=5

	; XYOUTS, 12, 2.95E4,'layer limits', COLOR = COLOR_24('red'	  ), CHARSIZE=2, /DATA
	; XYOUTS, 12, 2.90E4,'trop 95%'    , COLOR = COLOR_24('darkgreen'), CHARSIZE=2, /DATA
	; XYOUTS, 12, 2.85E4,'trop 5%'     , COLOR = COLOR_24('black'    ), CHARSIZE=2, /DATA
	; XYOUTS, 12, 2.80E4,'trop ave'    , COLOR = COLOR_24('blue'   ), CHARSIZE=2, /DATA
	; XYOUTS, 12, 2.75E4,'- Tot h2o'   , COLOR = COLOR_24('gray60'   ), CHARSIZE=2, /DATA
	; XYOUTS, 12, 2.70E4,'conv. fraction' , COLOR = COLOR_24('black'   ), CHARSIZE=2, /DATA

  XYOUTS, 192, 8.00E3, '- dh2o/dt' , COLOR = COLOR_24('red'	  ), CHARSIZE=2, /DATA
  XYOUTS, 192, 7.50E3, '- net' 	, COLOR = COLOR_24('darkgreen'), CHARSIZE=2, /DATA
  XYOUTS, 192, 7.00E3, '- h2o flux', COLOR = COLOR_24('blue'     ), CHARSIZE=2, /DATA
  XYOUTS, 192, 6.50E3, '- Tot h2o' , COLOR = COLOR_24('gray60'   ), CHARSIZE=2, /DATA
  XYOUTS, 192, 6.00E3, 'h2o-conv'  , COLOR = COLOR_24('gray30'   ), CHARSIZE=2, /DATA
  XYOUTS, 192, 5.50E3, 'h2o-nconv' , COLOR = COLOR_24('gray70'   ), CHARSIZE=2, /DATA
ENDIF

IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file


PRINT, 'time to run code: ' , SYSTIME(/SECONDS)-start_time


fname = outdir+run+'_h2o_theta.txt'
OPENW, lun, fname, /GET_LUN     

PRINTF, lun, FORMAT = '("  Date", 6X, "dh2o/dt", 6X, "Horiz. Flux", 6X, "Vert. Flux", 6X, "h2o-Conv.", 6X, "h2o-NConv.", 6X, "h2o-Total", 6X, "layer size")'								
PRINTF, lun, '======================================================================================================================='

FOR i = 0, N_ELEMENTS(date_arr)-1 DO BEGIN
	PRINTF, lun, FORMAT = '(I3, 4X, E12.4, 4X, E12.4, 4X, E12.4, 4X, E12.4, 4X, E12.4, 4X, E12.4, 4X, F12.2)', i,(dh2o_dt[i]),(time_flux[i]), net[i],h2o_vertflux_conv[i], h2o_vertflux_nconv[i],h2o_vertflux[i], value_count_arr[i]		
ENDFOR
FREE_LUN, lun


STOP
END

