PRO OZONE_MASS_LAYER_COPY, run, experiment, start_date, end_date, $
	DOMAIN        = domain, $
	REGION        = region, $
	WRAPPING      = wrapping, $
	CONVECTIVE 	  = convective, $
	NONCONVECTIVE = nonconvective, $
	PNG	     	  = png, $
	EPS   	 	  = eps


;+
; Name:
;		OZONE_MASS_LAYER
; Purpose:
;		Calculates mass (kg) of O3 in some layer
; Calling sequence:
;		OZONE_MASS_LAYER, run, scheme, start_date, end_date
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
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain

IF (experiment EQ 'd02_30km') THEN region = [120, 115, 524, 272]

outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/plots/ozone_mass_layer/'
FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(run, experiment, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(run, experiment, start_date, end_date, /DATE)	

z = (WRF_READ_VAR('Z', date_arr[0], run, experiment, DOMAIN = domain, INDICES = region)).values
dim = SIZE(z,/DIMENSIONS)

time_flux   	= [ ]
time_layer_flux = [ ]
ozone_array 	= [ ]
ozone_tot_arr 	= [ ]

o3_conv_arr     = [ ]
o3_nconv_arr    = [ ]
o3_conv_tarr    = [ ]
o3_nconv_tarr   = [ ]

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
	IF ((experiment EQ 'd03_30km') OR (experiment EQ 'd03_30km_icloud')) THEN $
		ozone = (WRF_READ_VAR('O3_tracer', date, run, experiment, DOMAIN = domain, INDICES = region)).values 
	IF ((experiment NE 'd03_30km') AND (experiment NE 'd03_30km_icloud')) THEN $
		ozone = (WRF_READ_VAR('O3', date, run, experiment, DOMAIN = domain, INDICES = region)).values 
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

	molec_weight = 48.0
    r_star = 8.314
	
    overshoot  = 0.001*((MAX((cloud GE 1.0E-5)*z, DIM = 3, /NAN)) - trop_3d)						;Set map variable to cloud top altitude

	IF (experiment EQ 'd02_30km'	   ) THEN dx = 2500.0
	IF (experiment EQ 'd03_30km'	   ) THEN dx = 500.0
	IF (experiment EQ 'd03_30km_icloud') THEN dx = 500.0
	IF ((run EQ '20110518') OR (run EQ '20130805')) THEN dx = 3000.0
    PRINT, 'dx= ', dx
   
    ;Convert o3 from ppm to kg
    ozone_ugm3  = (ozone * press*1.0E2 * molec_weight) / (r_star * temp)
    ozone_ug    = ozone_ugm3*dx*dx*250.0
    ozone_kg    = ozone_ug*1.0E-9
	ozone_kg_2d = FLTARR(dim[0],dim[1]) * !Values.F_NaN
	
	flux_u   = FLTARR(dim[2])
	flux_v   = FLTARR(dim[2])
	flux_tot = FLTARR(dim[2])

	;!!!DBP 2/7/19 Estimate total O3 flux in and out---Make this change!!!!!
 	;Estimate Flux of Ozone in and out of domain
	FOR k = 0, dim[2]-1 DO BEGIN
		flux_u  [k] = (MEAN((u[5,*,k]*ozone_kg[5,*,k] - u[dim[0]-5,*,k]*ozone_kg[dim[0]-5,*,k]),/NAN)) / ((dim[0] - 11) * dx)
		flux_v  [k] = (MEAN((v[*,5,k]*ozone_kg[*,5,k] - v[*,dim[1]-5,k]*ozone_kg[*,dim[1]-5,k]),/NAN)) / ((dim[1] - 11) * dx)
		;flux_u  [k] = (TOTAL((u[5,*,k]*ozone_kg[5,*,k] - u[dim[0]-5,*,k]*ozone_kg[dim[0]-5,*,k]),/NAN)) / ((dim[0] - 11) * dx)
		;flux_v  [k] = (TOTAL((v[*,5,k]*ozone_kg[*,5,k] - v[*,dim[1]-5,k]*ozone_kg[*,dim[1]-5,k]),/NAN)) / ((dim[1] - 11) * dx)
		flux_tot[k] = flux_u[k] + flux_v[k]
	ENDFOR

	time_flux = [[[time_flux]], [[flux_tot]]]

	;;+ If only convective or non-convective air is desired, use this block
	;IF KEYWORD_SET(CONVECTIVE) THEN BEGIN
	;	convect = WHERE(updrt GT 0.25, count, COMPLEMENT = nconv)
	;	IF (KEYWORD_SET(WRAPPING)) THEN ozone_kg [convect] = !Values.F_NaN
   	;	ozone_kg [convect] = ozone_kg [convect]
   	;	ozone_kg [nconv  ] = !Values.F_NaN
   	;ENDIF
	;
	;IF KEYWORD_SET(NONCONVECTIVE) THEN BEGIN
	;	convect = WHERE(updrt GT 0.25, count, COMPLEMENT = nconv)
	;	IF (KEYWORD_SET(WRAPPING)) THEN ozone_kg [convect] = !Values.F_NaN
   	;	ozone_kg [nconv  ] = ozone_kg [nconv]
   	;	ozone_kg [convect] = !Values.F_NaN
   	;ENDIF
	;;-end
	
	ozone_conv  = FLTARR(dim[0],dim[1],dim[2])
	ozone_conv  = FLTARR(dim[0],dim[1],dim[2])
	ozone_nconv = FLTARR(dim[0],dim[1],dim[2])
	ozone_nconv = FLTARR(dim[0],dim[1],dim[2])
	
	convect = WHERE(updrt GT 0.25, count, COMPLEMENT = nconv)
	ozone_conv [convect] = ozone_kg [convect]
	ozone_conv [nconv  ] = !Values.F_NaN
	ozone_nconv[nconv  ] = ozone_kg[nconv]
	ozone_nconv[convect] = !Values.F_NaN	

	;!!!!! COMMENTED OUT FOR FULL DOMAIN !!!!!!!
	;Find min vertical level at tropopause for initial time only
	IF (date_index EQ 0) THEN BEGIN
		PRINT, 'Find minimum level of tropopause'
		FOR ii = 0, dim[0]-1 DO BEGIN
			FOR jj = 0, dim[1]-1 DO BEGIN
				index1[ii,jj] = MEAN(VALUE_LOCATE(REFORM(z[ii,jj,*],dim[2],1,1),REFORM(trop_3d[ii,jj,*],dim[2],1,1)))
			ENDFOR
		ENDFOR
		PRINT, 'Done finding minimum level of tropopause' 
	ENDIF

	;Calculate Mean/Total Ozone concentration in layer
	index = FLOOR(MEAN(index1,/NAN))
	index0 = index-10
;	index0 = 0
;	index = dim[2]-1		
	; !!!!! END COMMENT !!!!!

	flux_layer = MEAN(flux_tot[index0:index],/NAN)
	time_layer_flux = [time_layer_flux, flux_layer]
	PRINT, 'layer flux = ', flux_layer
	
	ozone_kg_2d = ozone_kg[5:dim[0]-5,5:dim[1]-5,index0:index]
	o3_conv_2d  = ozone_conv [5:dim[0]-5,5:dim[1]-5,index0:index]
	o3_nconv_2d = ozone_nconv[5:dim[0]-5,5:dim[1]-5,index0:index]

	;'wrapping' keyword only looks at ozone near cloud
	IF (KEYWORD_SET(WRAPPING)) THEN BEGIN
	    PRINT, 'wrapping keyword set'
	    box = 100
	    anvil = SMOOTH(FLOAT((cloud GT 1.0E-5)), [box,box,1])
	    igood = WHERE((anvil[*,*,index0:index] GT 0.1) AND (anvil[*,*,index0:index] LT 0.25), count) 
   	    inan  = WHERE(FINITE(ozone[*,*,index0:index],/NAN),nan_count, COMPLEMENT = ivalue, NCOMPLEMENT = value_count)
    
 		IF (count GT 0) THEN ozone_wrap = (TOTAL(ozone_kg_2d[igood],/NAN))/value_count 
 		PRINT, value_count
  	ENDIF ELSE BEGIN
   	    PRINT, 'wrapping keyword not set'
   	    inan    = WHERE(FINITE(ozone	  [5:dim[0]-5,5:dim[1]-5,index0:index],/NAN),nan_count, COMPLEMENT = ivalue, NCOMPLEMENT = value_count)
   	    inan_c  = WHERE(FINITE(ozone_conv [5:dim[0]-5,5:dim[1]-5,index0:index],/NAN),nan_count, COMPLEMENT = ivalue, NCOMPLEMENT = value_count_conv)
   	    inan_nc = WHERE(FINITE(ozone_nconv[5:dim[0]-5,5:dim[1]-5,index0:index],/NAN),nan_count, COMPLEMENT = ivalue, NCOMPLEMENT = value_count_nconv)

		PRINT,'total convective ozone mass= ', (TOTAL(o3_conv_2d ,/NAN))
		PRINT,'convective count= ', value_count_conv

		PRINT,'total non-convective ozone mass= ', (TOTAL(o3_nconv_2d ,/NAN))
		PRINT,'non-convective count dracula= ', value_count_nconv

  		ozone_wrap   = (TOTAL(ozone_kg_2d,/NAN))/value_count
  		ozone_total  =  TOTAL(ozone_kg_2d,/NAN) 
	
		o3_ave_conv  = (TOTAL(o3_conv_2d ,/NAN))/value_count_conv
		o3_ave_nconv = (TOTAL(o3_nconv_2d,/NAN))/value_count_nconv
		o3_tot_conv  = (TOTAL(o3_conv_2d ,/NAN))
		o3_tot_nconv = (TOTAL(o3_nconv_2d,/NAN))	
	ENDELSE  	

    PRINT, date
    PRINT, 'Layer Limits: ', MEAN(z[*,*,index0],/NAN), MEAN(z[*,*,index],/NAN)
    ozone_array   = [ozone_array, ozone_wrap]
    ozone_tot_arr = [ozone_tot_arr, ozone_total]
    
    o3_conv_arr   = [o3_conv_arr  , o3_ave_conv ]
    o3_nconv_arr  = [o3_nconv_arr , o3_ave_nconv]
    o3_conv_tarr  = [o3_conv_tarr , o3_tot_conv ]
    o3_nconv_tarr = [o3_nconv_tarr, o3_tot_nconv]
    
    PRINT, 'Convective mass= ', o3_tot_conv 
    PRINT, 'Non-convective mass= ', o3_tot_nconv 
    
    PRINT, 'Difference from total= ', (ozone_total - (o3_tot_conv+o3_tot_nconv))
    date_index = date_index + 1
ENDFOREACH

dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string

outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/plots/o3_timeseries/'

epsfile = outdir + dom_string + '_' + date_string + '.eps'											;EPS filename
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

do3_dt   = FLTARR(N_ELEMENTS(date_arr))
do3c_dt  = FLTARR(N_ELEMENTS(date_arr))
do3nc_dt = FLTARR(N_ELEMENTS(date_arr))

IF (experiment EQ 'd02_30km'	   ) THEN dt = 3600.0
IF (experiment EQ 'd03_30km'	   ) THEN dt = 300.0
IF (experiment EQ 'd03_30km_icloud') THEN dt = 300.0
IF ((run EQ '20110518') OR (run EQ '20130805')) THEN dt = 3600.0

PRINT, 'dt= ', dt
FOR tt = 0, N_ELEMENTS(date_arr) -3 DO BEGIN
	do3_dt[tt]   = (ozone_array[tt+1]   - ozone_array[tt]  ) / dt
	do3c_dt[tt]  = (o3_conv_arr[tt+1]   - o3_conv_arr[tt]  ) / dt
	do3nc_dt[tt] = (o3_nconv_arr[tt+1] - o3_nconv_arr[tt]) / dt
	;do3_dt[tt]   = (ozone_tot_arr[tt+1]   - ozone_tot_arr[tt]  ) / dt
	;do3c_dt[tt]  = (o3_conv_tarr[tt+1]   - o3_conv_tarr[tt]  ) / dt
	;do3nc_dt[tt] = (o3_nconv_tarr[tt+1] - o3_nconv_tarr[tt]) / dt

	PRINT, do3_dt[tt]
ENDFOR

IF (run EQ '20120530_ncar') AND (experiment NE 'd02_30km') THEN BEGIN 
    PLOT, SHIFT(do3_dt,1), $
    	YRANGE = [-1E-4,1E-4], $
    	YTITLE   = 'Ozone (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	TITLE    = 'O3 Mass (kg)', $
    	CHARSIZE = 2, $
    	/NODATA
    
    AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [-1E-4,1E-4], $
    	YTITLE   = 'Ozone (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 2
    	
    OPLOT, SHIFT(do3_dt,1), COLOR = COLOR_24('red'), THICK=3
    OPLOT, time_layer_flux, COLOR = COLOR_24('blue'), THICK=3
    OPLOT, [0,250], [0,0], COLOR = COLOR_24('black'), THICK=3
    
    net = SHIFT(do3_dt,1) - time_layer_flux
    OPLOT, net, COLOR = COLOR_24('darkgreen'), THICK=3
    
    AXIS, YAXIS = 1, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE   = [0,10], $
    	YTITLE   = 'ozone (kg)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 2
    
	OPLOT, o3_conv_arr,  COLOR = COLOR_24('darkgreen'), THICK=3, LINESTYLE=2
	OPLOT, o3_nconv_arr, COLOR = COLOR_24('red'	     ), THICK=3, LINESTYLE=2	    
    OPLOT, ozone_array, COLOR = COLOR_24('gray60'	 ), THICK=3

    XYOUTS, 85, 9.25, '-- NConv' , COLOR = COLOR_24('red'      ), CHARSIZE=2, /DATA
    XYOUTS, 85, 8.75, '-- Conv'  , COLOR = COLOR_24('darkgreen'), CHARSIZE=2, /DATA
    XYOUTS, 85, 8.25, '- do3/dt' , COLOR = COLOR_24('red'	    ), CHARSIZE=2, /DATA
    XYOUTS, 85, 7.75, '- net' 	  , COLOR = COLOR_24('darkgreen'), CHARSIZE=2, /DATA
    XYOUTS, 85, 7.25, '- O3 flux', COLOR = COLOR_24('blue'     ), CHARSIZE=2, /DATA
    XYOUTS, 85, 6.75, '- Tot O3' , COLOR = COLOR_24('gray60'   ), CHARSIZE=2, /DATA
ENDIF

IF (experiment EQ 'd02_30km') THEN BEGIN 
    PLOT, SHIFT(do3_dt,1), $
    	YRANGE = [-4E-3,4E-3], $
    	YTITLE   = 'Ozone (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	TITLE    = 'O3 Mass (kg)', $
    	CHARSIZE = 2, $
    	/NODATA
    
    AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [-4E-3,4E-3], $
    	YTITLE   = 'Ozone (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 2
    	
    OPLOT, SHIFT(do3_dt,1), COLOR = COLOR_24('red'), THICK=3
    OPLOT, time_layer_flux, COLOR = COLOR_24('blue'), THICK=3
    OPLOT, [0,250], [0,0], COLOR = COLOR_24('black'), THICK=3
    
    net = SHIFT(do3_dt,1) - time_layer_flux
    OPLOT, net, COLOR = COLOR_24('darkgreen'), THICK=3
    
    AXIS, YAXIS = 1, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE   = [50,175], $
    	YTITLE   = 'ozone (kg)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 2
    
	OPLOT, o3_conv_arr,  COLOR = COLOR_24('darkgreen'), THICK=3, LINESTYLE=2
	OPLOT, o3_nconv_arr, COLOR = COLOR_24('red'	     ), THICK=3, LINESTYLE=2	    
    OPLOT, ozone_array, COLOR = COLOR_24('gray60'	 ), THICK=3

    XYOUTS, 6.75, 170.0, '- do3/dt' , COLOR = COLOR_24('red'	  ), CHARSIZE=2, /DATA
    XYOUTS, 6.75, 165.0, '- net' 	, COLOR = COLOR_24('darkgreen'), CHARSIZE=2, /DATA
    XYOUTS, 6.75, 160.0, '- O3 flux', COLOR = COLOR_24('blue'     ), CHARSIZE=2, /DATA
    XYOUTS, 6.75, 155.0, '- Tot O3' , COLOR = COLOR_24('gray60'   ), CHARSIZE=2, /DATA
    XYOUTS, 6.75, 150.0, 'o3-conv'  , COLOR = COLOR_24('gray30'   ), CHARSIZE=2, /DATA
    XYOUTS, 6.75, 145.0, 'o3-nconv' , COLOR = COLOR_24('gray70'   ), CHARSIZE=2, /DATA
ENDIF

IF (run EQ '20110518') THEN BEGIN 
    PLOT, SHIFT(do3_dt,1), $
    	YRANGE = [-6e-3,6e-3], $
    	YTITLE   = 'Ozone (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	TITLE    = run, $
    	CHARSIZE = 2, $
    	/NODATA
    
    AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [-6e-3,6e-3], $
    	YTITLE   = 'Ozone (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 2
    	
    OPLOT, SHIFT(do3_dt,1), COLOR = COLOR_24('red'), THICK=3
    OPLOT, time_layer_flux, COLOR = COLOR_24('blue'), THICK=3
    OPLOT, [0,250], [0,0], COLOR = COLOR_24('black'), THICK=3
    
    net = SHIFT(do3_dt,1) - time_layer_flux
    OPLOT, net, COLOR = COLOR_24('green'), THICK=3
    
    AXIS, YAXIS = 1, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE   = [0,360], $
    	YTITLE   = 'ozone (kg)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 2
    
    OPLOT, ozone_tot_arr, COLOR = COLOR_24('gray60'), THICK=3
    
    XYOUTS, 2, 140, 'dO3/dt' , COLOR = COLOR_24('red'   ), CHARSIZE=2, /DATA
    XYOUTS, 2, 133, 'O3 flux', COLOR = COLOR_24('blue'  ), CHARSIZE=2, /DATA
    XYOUTS, 2, 126, 'Net'    , COLOR = COLOR_24('green' ), CHARSIZE=2, /DATA
    XYOUTS, 2, 119, 'o3'     , COLOR = COLOR_24('gray60'), CHARSIZE=2, /DATA
ENDIF

IF (run EQ '20130805') THEN BEGIN 
    PLOT, SHIFT(do3_dt,1), $
    	YRANGE = [-6e-3,6e-3], $
    	YTITLE   = 'Ozone (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	TITLE    = run, $
    	CHARSIZE = 2, $
    	/NODATA
    
    AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [-6e-3,6e-3], $
    	YTITLE   = 'Ozone (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 2
    	
    OPLOT, SHIFT(do3_dt,1), COLOR = COLOR_24('red'), THICK=3
    OPLOT, time_layer_flux, COLOR = COLOR_24('blue'), THICK=3
    OPLOT, [0,250], [0,0], COLOR = COLOR_24('black'), THICK=3
    
    net = SHIFT(do3_dt,1) - time_layer_flux
    OPLOT, net, COLOR = COLOR_24('green'), THICK=3
    
    AXIS, YAXIS = 1, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE   = [0,360], $
    	YTITLE   = 'ozone (kg)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 2
    
    OPLOT, ozone_tot_arr, COLOR = COLOR_24('gray60'), THICK=3
    
    XYOUTS, 2, 140, 'dO3/dt' , COLOR = COLOR_24('red'   ), CHARSIZE=2, /DATA
    XYOUTS, 2, 133, 'O3 flux', COLOR = COLOR_24('blue'  ), CHARSIZE=2, /DATA
    XYOUTS, 2, 126, 'Net'    , COLOR = COLOR_24('green' ), CHARSIZE=2, /DATA
    XYOUTS, 2, 119, 'o3'     , COLOR = COLOR_24('gray60'), CHARSIZE=2, /DATA
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

