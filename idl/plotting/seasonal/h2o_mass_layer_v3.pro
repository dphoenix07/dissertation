PRO H2O_MASS_LAYER_V3, run, experiment, start_date, end_date, $
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
;		H2O_MASS_LAYER_V3
; Purpose:
;		Calculates mass (kg) of O3 in some layer
; Calling sequence:
;		H2O_MASS_LAYER_V3, run, scheme, start_date, end_date
;		H2O_MASS_LAYER_V3, '20120530_ncar','d03_30km','20120530T2200Z','20120531T0500Z'
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
	tropopause = 12650.0
ENDIF

IF (run EQ '20130805') THEN BEGIN
	tropics = 1
	PRINT, 'tropics = ', tropics
	tropopause = 15500.0
ENDIF

outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/plots/h2o_mass_layer/'
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

date_index = 0
index1  = FLTARR(dim[0],dim[1])*!Values.F_NaN
index01 = FLTARR(dim[0],dim[1])*!Values.F_NaN

FOREACH date, date_arr DO BEGIN
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
	trop_filtered = trop_3d
	
	count = 0
	IF (KEYWORD_SET(tropics)) THEN igood = WHERE(trop_3d GE 15000.0, count, COMPLEMENT = ibad)
	IF (KEYWORD_SET(midlats)) THEN igood = WHERE(trop_3d LT 14000.0, count, COMPLEMENT = ibad)

	IF (count GT 0) THEN BEGIN
		h2o [ibad] = 0.0
		trop_filtered [ibad] = !Values.F_NaN
	ENDIF

	molec_weight = 18.0
    r_star = 8.314
	
    overshoot  = 0.001*((MAX((cloud GE 1.0E-5)*z, DIM = 3, /NAN)) - trop_3d)						;Set map variable to cloud top altitude

	IF (experiment EQ 'd02_30km'	   ) THEN dx = 2500.0
	IF (experiment EQ 'd03_30km'	   ) THEN dx = 500.0
	IF (experiment EQ 'd03_30km_icloud') THEN dx = 500.0
	IF ((run EQ '20110518') OR (run EQ '20130805')) THEN dx = 3000.0
    PRINT, 'dx= ', dx
   
    ;Convert h2o from ppm to kg
    h2o_ugm3  = (h2o * press*1.0E2 * molec_weight) / (r_star * temp)
    h2o_ug    = h2o_ugm3*dx*dx*250.0
    h2o_kg    = h2o_ug*1.0E-9
	h2o_kg_2d = FLTARR(dim[0],dim[1]) * !Values.F_NaN
	
	cloud_fraction = FLTARR(dim[0],dim[1]) *!Values.F_NaN
	;Find min vertical level at tropopause for initial time only
	;First filter out tropopauses either above/below 14 km
	
;	IF (date_index EQ 0) THEN BEGIN
		PRINT, 'calculating mean level of tropopause'
		FOR ii = 0, dim[0]-1 DO BEGIN
			FOR jj = 0, dim[1]-1 DO BEGIN
				index01[ii,jj] = MEAN(VALUE_LOCATE(REFORM(z[ii,jj,*],dim[2],1,1),REFORM(trop_filtered[ii,jj,*],dim[2],1,1)))
				index1 [ii,jj] = MEAN(VALUE_LOCATE(REFORM(z[ii,jj,*],dim[2],1,1),tropopause))
			ENDFOR
		ENDFOR
;	ENDIF
	
	index_nan = WHERE(index1 LT 0)
	index1[index_nan] = !Values.F_NaN

	index01_nan = WHERE(index01 LT 0)
	index01[index01_nan] = !Values.F_NaN
	index_sort = SORT(index01)
	index01_finite = WHERE(FINITE(index01[index_sort]))
	index02 = index01[index_sort[index01_finite]]
	
	;Calculate Mean/Total h2o concentration in layer
	tr_95  = index02[PERCENTILE(index02,95 ,/NAN)]
	tr_5   = index02[PERCENTILE(index02,5 ,/NAN)]
	tr_ave = (MEAN(index01,/NAN))
	index0 = FLOOR(MEAN(index1,/NAN))
	PRINT, 'index0=  ', index0
	PRINT, '95th % = ', tr_95
	PRINT, '5th % =  ', tr_5
	index = index0+10		
	
	cloudlocs = WHERE(cloud[*,*,index0:index] GT 1.0E-5)
	cloud_fraction = FLOAT(N_ELEMENTS(cloudlocs))/FLOAT((N_ELEMENTS(cloud[*,*,index0:index])))

	molec_weight = 18.0
    r_star = 8.314
	
    overshoot  = 0.001*((MAX((cloud GE 1.0E-5)*z, DIM = 3, /NAN)) - trop_3d)						;Set map variable to cloud top altitude

	IF (experiment EQ 'd02_30km'	   ) THEN dx = 2500.0
	IF (experiment EQ 'd03_30km'	   ) THEN dx = 500.0
	IF (experiment EQ 'd03_30km_icloud') THEN dx = 500.0
	IF ((run EQ '20110518') OR (run EQ '20130805')) THEN dx = 3000.0
    PRINT, 'dx= ', dx
   
    ;Convert co from ppm to kg
    h2o_ugm3  = (h2o * press*1.0E2 * molec_weight) / (r_star * temp)
    h2o_ug    = h2o_ugm3*dx*dx*250.0
    h2o_kg    = h2o_ug*1.0E-9
	h2o_kg_2d = FLTARR(dim[0],dim[1]) * !Values.F_NaN
	
	flux_u      = FLTARR(dim[2])
	flux_v      = FLTARR(dim[2])
	flux_tot    = FLTARR(dim[2])
	flux_u_tot	= FLTARR(dim[2])
	flux_v_tot	= FLTARR(dim[2])
	flux_tot_tot= FLTARR(dim[2])
	
	;!!!DBP 2/7/19 Estimate total co flux in and out---Make this change!!!!!
 	;Estimate Flux of carbonmo in and out of domain
	FOR k = 0, dim[2]-1 DO BEGIN
		flux_u 		[k] = (MEAN((u[5,*,k]*h2o_kg[5,*,k] - u[dim[0]-5,*,k]*h2o_kg[dim[0]-5,*,k]),/NAN)) / ((dim[0] - 11) * dx)
		flux_v 		[k] = (MEAN((v[*,5,k]*h2o_kg[*,5,k] - v[*,dim[1]-5,k]*h2o_kg[*,dim[1]-5,k]),/NAN)) / ((dim[1] - 11) * dx)
		flux_u_tot  [k] = (TOTAL((u[5,*,k]*h2o_kg[5,*,k] - u[dim[0]-5,*,k]*h2o_kg[dim[0]-5,*,k]),/NAN)) / ((dim[0] - 11) * dx)
		flux_v_tot  [k] = (TOTAL((v[*,5,k]*h2o_kg[*,5,k] - v[*,dim[1]-5,k]*h2o_kg[*,dim[1]-5,k]),/NAN)) / ((dim[1] - 11) * dx)
		flux_tot    [k] = flux_u[k] + flux_v[k]
		flux_tot_tot[k] = flux_u_tot[k] + flux_v_tot[k]
	ENDFOR

	time_flux = [[[time_flux]], [[flux_tot]]]
	time_flux_tot = [[[time_flux_tot]], [[flux_tot_tot]]]
	
	h2o_conv  = FLTARR(dim[0],dim[1],dim[2])
	h2o_conv  = FLTARR(dim[0],dim[1],dim[2])
	h2o_nconv = FLTARR(dim[0],dim[1],dim[2])
	h2o_nconv = FLTARR(dim[0],dim[1],dim[2])
	
	convect = WHERE(updrt GT 0.25, count, COMPLEMENT = nconv)
	h2o_conv [convect] = h2o_kg [convect]
	h2o_conv [nconv  ] = !Values.F_NaN
	h2o_nconv[nconv  ] = h2o_kg[nconv]
	h2o_nconv[convect] = !Values.F_NaN	

	flux_layer 	        = MEAN(flux_tot[index0:index],/NAN)
	flux_layer_tot      = TOTAL(flux_tot_tot[index0:index],/NAN) 
	time_layer_flux     = [time_layer_flux, flux_layer]
	time_layer_flux_tot = [time_layer_flux_tot, flux_layer_tot]
	PRINT, 'layer flux = ', flux_layer
	
	h2o_kg_2d = h2o_kg[5:dim[0]-5,5:dim[1]-5,index0:index]
	h2o_conv_2d  = h2o_conv [5:dim[0]-5,5:dim[1]-5,index0:index]
	h2o_nconv_2d = h2o_nconv[5:dim[0]-5,5:dim[1]-5,index0:index]

   	PRINT, 'wrapping keyword not set'
   	inan    = WHERE(FINITE(h2o      [5:dim[0]-5,5:dim[1]-5,index0:index],/NAN),nan_count, COMPLEMENT = ivalue, NCOMPLEMENT = value_count)
   	inan_c  = WHERE(FINITE(h2o_conv [5:dim[0]-5,5:dim[1]-5,index0:index],/NAN),nan_count, COMPLEMENT = ivalue, NCOMPLEMENT = value_count_conv)
   	inan_nc = WHERE(FINITE(h2o_nconv[5:dim[0]-5,5:dim[1]-5,index0:index],/NAN),nan_count, COMPLEMENT = ivalue, NCOMPLEMENT = value_count_nconv)

	PRINT,'total convective h2o mass= ', (TOTAL(h2o_conv_2d ,/NAN))
	PRINT,'convective count= ', value_count_conv

	PRINT,'total non-convective h2o mass= ', (TOTAL(h2o_nconv_2d ,/NAN))
	PRINT,'non-convective count dracula= ', value_count_nconv

  	h2o_wrap   = (TOTAL(h2o_kg_2d,/NAN))/value_count
  	h2o_total  =  TOTAL(h2o_kg_2d,/NAN) 
	
	h2o_ave_conv  = (TOTAL(h2o_conv_2d ,/NAN))/value_count_conv
	h2o_ave_nconv = (TOTAL(h2o_nconv_2d,/NAN))/value_count_nconv
	h2o_tot_conv  = (TOTAL(h2o_conv_2d ,/NAN))
	h2o_tot_nconv = (TOTAL(h2o_nconv_2d,/NAN))	

    PRINT, date
    PRINT, 'Layer Limits: ', MEAN(z[*,*,index0],/NAN), MEAN(z[*,*,index],/NAN)
  
    trop_95      = [trop_95  , MEAN(z[*,*,tr_95 ],/NAN)]
    trop_5       = [trop_5   , MEAN(z[*,*,tr_5  ],/NAN)]
    trop_ave	 = [trop_ave , MEAN(z[*,*,tr_ave],/NAN)] 
    layer_top    = [layer_top, MEAN(z[*,*,index ],/NAN)] 
    trop_arr     = [trop_arr , MEAN(z[*,*,index0],/NAN)]
    
    h2o_array   = [h2o_array, h2o_wrap]
    h2o_tot_arr = [h2o_tot_arr, h2o_total]
    
    h2o_conv_arr   = [h2o_conv_arr  , h2o_ave_conv ]
    h2o_nconv_arr  = [h2o_nconv_arr , h2o_ave_nconv]
    h2o_conv_tarr  = [h2o_conv_tarr , h2o_tot_conv ]
    h2o_nconv_tarr = [h2o_nconv_tarr, h2o_tot_nconv]
 
    cloud_frac_tot = [cloud_frac_tot, cloud_fraction]
   
    PRINT, 'Convective mass= ', h2o_tot_conv 
    PRINT, 'Non-convective mass= ', h2o_tot_nconv 
    
    PRINT, 'Difference from total= ', (h2o_total - (h2o_tot_conv+h2o_tot_nconv))
    date_index = date_index + 1

ENDFOREACH

dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string

outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/plots/h2o_timeseries/'

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
	dh2oc_dt[tt]  = (h2o_conv_arr[tt+1]   - h2o_conv_arr[tt]  ) / dt
	dh2onc_dt[tt] = (h2o_nconv_arr[tt+1] - h2o_nconv_arr[tt]) / dt
	;dh2o_dt[tt]   = (h2o_tot_arr[tt+1]   - h2o_tot_arr[tt]  ) / dt
	;dh2oc_dt[tt]  = (h2o_conv_tarr[tt+1]   - h2o_conv_tarr[tt]  ) / dt
	;dh2onc_dt[tt] = (h2o_nconv_tarr[tt+1] - h2o_nconv_tarr[tt]) / dt

	PRINT, dh2o_dt[tt]
ENDFOR

IF (run EQ '20120530_ncar') AND (experiment NE 'd02_30km') THEN BEGIN 
    PLOT, SHIFT(dh2o_dt,1), $
    	YRANGE = [-1E-4,1E-4], $
    	YTITLE   = 'h2o (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	TITLE    = 'h2o Mass (kg)', $
 		XTICKINTERVAL = 12, $
    	CHARSIZE = 2, $
    	/NODATA
    
    AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [-1E-4,1E-4], $
    	YTITLE   = 'h2o (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
  		XTICKINTERVAL = 12, $
	    CHARSIZE = 2
    	
    OPLOT, SHIFT(dh2o_dt,1), COLOR = COLOR_24('red'), THICK=3
    OPLOT, time_layer_flux, COLOR = COLOR_24('blue'), THICK=3
    OPLOT, [0,250], [0,0], COLOR = COLOR_24('black'), THICK=3
    
    net = SHIFT(dh2o_dt,1) - time_layer_flux
	h2o_vertflux = h2o_tot_arr+time_layer_flux_tot*3600
	h2o_vertflux_conv =  h2o_conv_arr +time_layer_flux*3600
	h2o_vertflux_nconv = h2o_nconv_arr+time_layer_flux*3600
    OPLOT, net, COLOR = COLOR_24('darkgreen'), THICK=3
    
    AXIS, YAXIS = 1, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE   = [2.0,7.0], $
    	YTITLE   = 'h2o (kg)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
 		XTICKINTERVAL = 12, $
    	CHARSIZE = 2
    
	OPLOT, h2o_conv_arr,  COLOR = COLOR_24('gray30'), THICK=3, LINESTYLE=2
	OPLOT, h2o_nconv_arr, COLOR = COLOR_24('gray70'), THICK=3, LINESTYLE=2	    
    ;OPLOT, h2o_array, COLOR = COLOR_24('gray60'	 ), THICK=3

    AXIS, YAXIS = 1, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE   = [4E6,8.5E7], $
    	YTITLE   = 'h2o (kg)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
 		XTICKINTERVAL = 12, $
    	CHARSIZE = 2
    
	;OPLOT, h2o_conv_tarr,  COLOR = COLOR_24('gray30'), THICK=3, LINESTYLE=2
	;OPLOT, h2o_nconv_tarr, COLOR = COLOR_24('gray70'), THICK=3, LINESTYLE=2	    
    OPLOT, h2o_tot_arr, COLOR = COLOR_24('gray60'), THICK=3


    XYOUTS, 85, 7.5E7, '- dh2o/dt' , COLOR = COLOR_24('red'	  ), CHARSIZE=2, /DATA
    XYOUTS, 85, 7.1E7, '- net' 	, COLOR = COLOR_24('darkgreen'), CHARSIZE=2, /DATA
    XYOUTS, 85, 6.7E7, '- h2o flux', COLOR = COLOR_24('blue'     ), CHARSIZE=2, /DATA
    XYOUTS, 85, 6.3E7, '- Tot h2o' , COLOR = COLOR_24('gray60'   ), CHARSIZE=2, /DATA
    XYOUTS, 85, 5.9E7, 'h2o-conv'  , COLOR = COLOR_24('gray30'   ), CHARSIZE=2, /DATA
    XYOUTS, 85, 5.5E7, 'h2o-nconv' , COLOR = COLOR_24('gray70'   ), CHARSIZE=2, /DATA
ENDIF

IF (experiment EQ 'd02_30km') THEN BEGIN 
    PLOT, SHIFT(dh2o_dt,1), $
;    	YRANGE = [-600,600], $
 		YRANGE = [-4E-3, 4E-3], $
    	YTITLE   = 'h2o (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	TITLE    = 'h2o Mass (kg)', $
    	CHARSIZE = 2, $
    	/NODATA
    
    AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
;    	YRANGE 	 = [-600,600], $
 		YRANGE = [-4E-3, 4E-3], $
    	YTITLE   = 'h2o (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 2
    	
    OPLOT, SHIFT(dh2o_dt,1), COLOR = COLOR_24('red'), THICK=3
    OPLOT, time_layer_flux, COLOR = COLOR_24('blue'), THICK=3
    OPLOT, [0,250], [0,0], COLOR = COLOR_24('black'), THICK=3
    
    net = SHIFT(dh2o_dt,1) - time_layer_flux
	h2o_vertflux = h2o_tot_arr+time_layer_flux_tot*3600
	h2o_vertflux_conv =  h2o_conv_tarr +time_layer_flux_tot*3600
	h2o_vertflux_nconv = h2o_nconv_tarr+time_layer_flux_tot*3600
    OPLOT, net, COLOR = COLOR_24('darkgreen'), THICK=3
    
    AXIS, YAXIS = 1, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE   = [6E6,8E7], $
 ; 		YRANGE   = [80, 130], $
    	YTITLE   = 'h2o (kg)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 2
    
	;OPLOT, h2o_conv_arr,  COLOR = COLOR_24('gray30'), THICK=3, LINESTYLE=2
	;OPLOT, h2o_nconv_arr, COLOR = COLOR_24('gray70'), THICK=3, LINESTYLE=2	    
    ;OPLOT, h2o_array,  COLOR = COLOR_24('gray60'), THICK=3

	OPLOT, h2o_vertflux_conv,  COLOR = COLOR_24('gray30'), THICK=3, LINESTYLE=2
	OPLOT, h2o_vertflux_nconv, COLOR = COLOR_24('gray70'), THICK=3, LINESTYLE=2	    
    OPLOT, h2o_vertflux, COLOR = COLOR_24('gray60'), THICK=3

    XYOUTS, 7.0, 7.5E7, '- dh2o/dt' , COLOR = COLOR_24('red'	  ), CHARSIZE=2, /DATA
    XYOUTS, 7.0, 7.1E7, '- net' 	, COLOR = COLOR_24('darkgreen'), CHARSIZE=2, /DATA
    XYOUTS, 7.0, 6.7E7, '- h2o flux', COLOR = COLOR_24('blue'     ), CHARSIZE=2, /DATA
    XYOUTS, 7.0, 6.3E7, '- Tot h2o' , COLOR = COLOR_24('gray60'   ), CHARSIZE=2, /DATA
    XYOUTS, 7.0, 5.9E7, 'h2o-conv'  , COLOR = COLOR_24('gray30'   ), CHARSIZE=2, /DATA
    XYOUTS, 7.0, 5.5E7, 'h2o-nconv' , COLOR = COLOR_24('gray70'   ), CHARSIZE=2, /DATA
ENDIF

IF (run EQ '20110518') THEN BEGIN 
    PLOT, SHIFT(dh2o_dt,1), $
    	YRANGE 	 = [-2E-7,2E-7], $
    	YTITLE   = 'h2o (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	TITLE    = 'may h2o Mass (kg)', $
 		XTICKINTERVAL = 12, $
    	CHARSIZE = 2, $
    	/NODATA
    
    AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [-2E-7,2E-7], $
    	YTITLE   = 'h2o (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
  		XTICKINTERVAL = 12, $
	    CHARSIZE = 2
    	
  	OPLOT, SHIFT(dh2o_dt,1), COLOR = COLOR_24('red'), THICK=3
	OPLOT, time_layer_flux, COLOR = COLOR_24('blue'), THICK=3
	OPLOT, [0,250], [0,0], COLOR = COLOR_24('black'), THICK=3
    
    net = SHIFT(dh2o_dt,1) - time_layer_flux
	h2o_vertflux = h2o_tot_arr+time_layer_flux_tot*3600
	h2o_vertflux_conv =  h2o_conv_arr +time_layer_flux*3600
	h2o_vertflux_nconv = h2o_nconv_arr+time_layer_flux*3600
    OPLOT, net, COLOR = COLOR_24('darkgreen'), THICK=3

  ;   AXIS, YAXIS = 0, $																								;Draw altitude axis
  ;  	SAVE     = 1, $
  ;  	YRANGE 	 = [9.0, 16.0], $
  ;  	YTITLE   = 'Tropopause', $
  ;  	COLOR    = COLOR_24('black'), $
  ;  	YTICKS   = 1, $
  ;  	YSTYLE   = 1, $
  ;		XTICKINTERVAL = 12, $
	;    CHARSIZE = 2
  ;  	
  ;  OPLOT, trop_arr *1.0E-3, COLOR = COLOR_24('red'  ), THICK=4
  ;  OPLOT, layer_top*1.0E-3, COLOR = COLOR_24('red'  ), THICK=4, LINESTYLE=2 
  ;  OPLOT, trop_95  *1.0E-3, COLOR = COLOR_24('darkgreen'), THICK=4, LINESTYLE=1	
  ;  OPLOT, trop_5   *1.0E-3, COLOR = COLOR_24('black'), THICK=4, LINESTYLE=2 
  ;  OPLOT, trop_ave *1.0E-3, COLOR = COLOR_24('blue'), THICK=4, LINESTYLE=3

    
     AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [0.0, 100.0], $
    	YTITLE   = 'Cloud Fraction (%)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
  		XTICKINTERVAL = 12, $
	    CHARSIZE = 2
    	
    OPLOT, cloud_frac_tot*100.0, COLOR = COLOR_24('black'), THICK=4

    AXIS, YAXIS = 1, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE   = [0.001,0.03], $
    	YTITLE   = 'h2o (kg)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
 		XTICKINTERVAL = 12, $
    	CHARSIZE = 2
    
	;;OPLOT, h2o_conv_arr,  COLOR = COLOR_24('gray30'), THICK=3, LINESTYLE=2
	;;OPLOT, h2o_nconv_arr, COLOR = COLOR_24('gray70'), THICK=3, LINESTYLE=2	    
	
	;OPLOT, h2o_vertflux_conv,  COLOR = COLOR_24('gray30'), THICK=3, LINESTYLE=2
	;OPLOT, h2o_vertflux_nconv, COLOR = COLOR_24('gray70'), THICK=3, LINESTYLE=2	    
    
    ;;OPLOT, h2o_array, COLOR = COLOR_24('gray60'	 ), THICK=3

    AXIS, YAXIS = 1, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE   = [1.5E4,3.0E4], $
    	YTITLE   = 'h2o (kg)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
 		XTICKINTERVAL = 12, $
    	CHARSIZE = 2
    
	;OPLOT, h2o_conv_tarr,  COLOR = COLOR_24('gray30'), THICK=3, LINESTYLE=2
	;OPLOT, h2o_nconv_tarr, COLOR = COLOR_24('gray70'), THICK=3, LINESTYLE=2	    
    
    ;OPLOT, h2o_tot_arr, COLOR = COLOR_24('gray60'), THICK=3
    OPLOT, h2o_vertflux, COLOR = COLOR_24('gray60'), THICK=3

	; XYOUTS, 12, 2.95E4,'layer limits', COLOR = COLOR_24('red'	  ), CHARSIZE=2, /DATA
	; XYOUTS, 12, 2.90E4,'trop 95%'    , COLOR = COLOR_24('darkgreen'), CHARSIZE=2, /DATA
	; XYOUTS, 12, 2.85E4,'trop 5%'     , COLOR = COLOR_24('black'    ), CHARSIZE=2, /DATA
	; XYOUTS, 12, 2.80E4,'trop ave'    , COLOR = COLOR_24('blue'   ), CHARSIZE=2, /DATA
	; XYOUTS, 12, 2.75E4,'- Tot h2o'   , COLOR = COLOR_24('gray60'   ), CHARSIZE=2, /DATA
	; XYOUTS, 12, 2.70E4,'conv. fraction' , COLOR = COLOR_24('black'   ), CHARSIZE=2, /DATA

    XYOUTS, 12, 2.95E4, '- dh2o/dt' , COLOR = COLOR_24('red'	  ), CHARSIZE=2, /DATA
    XYOUTS, 12, 2.90E4, '- net' 	, COLOR = COLOR_24('darkgreen'), CHARSIZE=2, /DATA
    XYOUTS, 12, 2.85E4, '- h2o flux', COLOR = COLOR_24('blue'     ), CHARSIZE=2, /DATA
    XYOUTS, 12, 2.80E4, '- Tot h2o' , COLOR = COLOR_24('gray60'   ), CHARSIZE=2, /DATA
    XYOUTS, 12, 2.75E4, 'h2o-conv'  , COLOR = COLOR_24('gray30'   ), CHARSIZE=2, /DATA
    XYOUTS, 12, 2.70E4, 'h2o-nconv' , COLOR = COLOR_24('gray70'   ), CHARSIZE=2, /DATA
ENDIF

IF (run EQ '20130805') THEN BEGIN 
    PLOT, SHIFT(dh2o_dt,1), $
    	YRANGE 	 = [-2E-8,2E-8], $
    	YTITLE   = 'h2o (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	TITLE    = 'august h2o Mass (kg)', $
 		XTICKINTERVAL = 12, $
    	CHARSIZE = 2, $
    	/NODATA
    
    AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [-2E-8,2E-8], $
    	YTITLE   = 'h2o (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
  		XTICKINTERVAL = 12, $
	    CHARSIZE = 2
    	
    ;OPLOT, SHIFT(dh2o_dt,1), COLOR = COLOR_24('red'), THICK=3
    ;OPLOT, time_layer_flux, COLOR = COLOR_24('blue'), THICK=3
    ;OPLOT, [0,250], [0,0], COLOR = COLOR_24('black'), THICK=3
    
    net = SHIFT(dh2o_dt,1) - time_layer_flux
	h2o_vertflux = h2o_tot_arr+time_layer_flux_tot*3600
	h2o_vertflux_conv =  h2o_conv_arr +time_layer_flux*3600
	h2o_vertflux_nconv = h2o_nconv_arr+time_layer_flux*3600
    ;OPLOT, net, COLOR = COLOR_24('darkgreen'), THICK=3
    
     AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [11.0, 19.0], $
    	YTITLE   = 'Tropopause', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
  		;XTICKINTERVAL = 12, $
	    CHARSIZE = 2
    	
    OPLOT, trop_arr *1.0E-3, COLOR = COLOR_24('red'  ), THICK=4
    OPLOT, layer_top*1.0E-3, COLOR = COLOR_24('red'  ), THICK=4, LINESTYLE=2 
    OPLOT, trop_95  *1.0E-3, COLOR = COLOR_24('darkgreen'), THICK=4, LINESTYLE=1	
    OPLOT, trop_5   *1.0E-3, COLOR = COLOR_24('black'), THICK=4, LINESTYLE=2 
    OPLOT, trop_ave *1.0E-3, COLOR = COLOR_24('blue'), THICK=4, LINESTYLE=3


     AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [0.0, 10.0], $
    	YTITLE   = 'Cloud Fraction (%)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
  		XTICKINTERVAL = 12, $
	    CHARSIZE = 2
    	
    OPLOT, cloud_frac_tot*100.0, COLOR = COLOR_24('black'), THICK=4

    AXIS, YAXIS = 1, $																								;Draw altitude axis
    	SAVE     = 1, $
;    	YRANGE   = [0.001,0.006], $
    	YRANGE   = [0.0006,0.0025], $
    	YTITLE   = 'h2o (kg)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
 		XTICKINTERVAL = 12, $
    	CHARSIZE = 2
    
	;;OPLOT, h2o_conv_arr,  COLOR = COLOR_24('gray30'), THICK=3, LINESTYLE=2
	;;OPLOT, h2o_nconv_arr, COLOR = COLOR_24('gray70'), THICK=3, LINESTYLE=2	    
	;OPLOT, h2o_vertflux_conv,  COLOR = COLOR_24('gray30'), THICK=3, LINESTYLE=2
	;OPLOT, h2o_vertflux_nconv, COLOR = COLOR_24('gray70'), THICK=3, LINESTYLE=2	    
    ;;OPLOT, h2o_array, COLOR = COLOR_24('gray60'	 ), THICK=3

    AXIS, YAXIS = 1, $																								;Draw altitude axis
    	SAVE     = 1, $
;    	YRANGE   = [2.0E4,3.0E4], $
    	YRANGE   = [7E3,1.0E4], $
    	YTITLE   = 'h2o (kg)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
 		XTICKINTERVAL = 12, $
    	CHARSIZE = 2
    
	;OPLOT, h2o_conv_tarr,  COLOR = COLOR_24('gray30'), THICK=3, LINESTYLE=2
	;OPLOT, h2o_nconv_tarr, COLOR = COLOR_24('gray70'), THICK=3, LINESTYLE=2	    

    ;OPLOT, h2o_tot_arr, COLOR = COLOR_24('gray60'), THICK=3
    OPLOT, h2o_vertflux, COLOR = COLOR_24('gray60'), THICK=3

    XYOUTS, 84 , 9.90E3, 'layer limits', COLOR = COLOR_24('red'	  ), CHARSIZE=2, /DATA
    XYOUTS, 84 , 9.80E3, 'trop 95%'    , COLOR = COLOR_24('darkgreen'), CHARSIZE=2, /DATA
    XYOUTS, 84 , 9.70E3, 'trop 5%'     , COLOR = COLOR_24('black'    ), CHARSIZE=2, /DATA
    XYOUTS, 120, 9.90E3, 'trop ave'    , COLOR = COLOR_24('blue'   ), CHARSIZE=2, /DATA
    XYOUTS, 120, 9.80E3, '- Tot h2o'   , COLOR = COLOR_24('gray60'   ), CHARSIZE=2, /DATA
    XYOUTS, 120, 9.70E3, 'conv. fraction' , COLOR = COLOR_24('black'   ), CHARSIZE=2, /DATA

    ;XYOUTS, 84 , 9.90E3, '- dh2o/dt' , COLOR = COLOR_24('red'	  ), CHARSIZE=2, /DATA
    ;XYOUTS, 84 , 9.80E3, '- net' 	, COLOR = COLOR_24('darkgreen'), CHARSIZE=2, /DATA
    ;XYOUTS, 84 , 9.70E3, '- h2o flux', COLOR = COLOR_24('blue'     ), CHARSIZE=2, /DATA
    ;XYOUTS, 120, 9.90E3, '- Tot h2o' , COLOR = COLOR_24('gray60'   ), CHARSIZE=2, /DATA
    ;XYOUTS, 120, 9.80E3, 'h2o-conv'  , COLOR = COLOR_24('gray30'   ), CHARSIZE=2, /DATA
    ;XYOUTS, 120, 9.70E3, 'h2o-nconv' , COLOR = COLOR_24('gray70'   ), CHARSIZE=2, /DATA
ENDIF

IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file

PRINT, 'time to run code: ' , SYSTIME(/SECONDS)-start_time

STOP
END

