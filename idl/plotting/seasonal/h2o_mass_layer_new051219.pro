PRO H2O_MASS_LAYER_NEW051219, run, experiment, start_date, end_date, $
	DOMAIN        = domain, $
	REGION        = region, $
	WRAPPING      = wrapping, $
	CONVECTIVE 	  = convective, $
	NONCONVECTIVE = nonconvective, $
	PNG	          = png, $
	EPS   	      = eps


;+
; Name:
;		H2O_MASS_LAYER_NEW051219
; Purpose:
;		Calculates mass (kg) of O3 in some layer
; Calling sequence:
;		H2O_MASS_LAYER_NEW051219, run, scheme, start_date, end_date
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
ozone_array_conv   = []
ozone_array_nconv  = []
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
	ozone = (WRF_READ_VAR('H2O'			   , date, run, experiment, DOMAIN = domain, INDICES = region)).values 
    updrt = (WRF_READ_VAR('Updraft_tracer' , date, run, experiment, DOMAIN = domain, INDICES = region)).values
    press = (WRF_READ_VAR('P'	           , date, run, experiment, DOMAIN = domain, INDICES = region)).values							
    ztrop = (WRF_READ_VAR('Z_trop'         , date, run, experiment, DOMAIN = domain, INDICES = region)).values
    temp  = (WRF_READ_VAR('T'	           , date, run, experiment, DOMAIN = domain, INDICES = region)).values						
    theta =  WRF_READ_VAR('T'	           , date, run, experiment, DOMAIN = domain, INDICES = region)								;Read temperature variable from WRF output
    theta.values = ((1000.0/(WRF_READ_VAR('P', date, run, experiment, $										;Compute potential temperature
    		 						DOMAIN = domain, INDICES = region)).values)^(!Rair/!Cp))*(theta.values)

    ztrop    = MEDIAN(ztrop, 30)    
	trop_3d  = REBIN (ztrop, dim[0], dim[1], dim[2], /SAMPLE)
	;min_trop = MIN(ztrop,/NAN)
    ;xyz_trop = FLTARR(dim[0],dim[1],dim[2]) + min_trop

	molec_weight = 18.0
    r_star = 8.314
    overshoot  = 0.001*((MAX((cloud GE 1.0E-5)*z, DIM = 3, /NAN)) - trop_3d)						;Set map variable to cloud top altitude

	dx = 3000.0
	IF ((run EQ '20120530_ncar') AND (experiment EQ 'd03_30km')) THEN dx = 500.0
    ;Convert o3 from ppm to kg
    air_gm3     = ((press*1.0E2 * 28.97) / (r_star * temp))
    air_kg	    = ((press*1.0E2)*dx*dx)/9.81
	o3_ugm3     = (ozone * press*1.0E2 * molec_weight) / (r_star * temp)
	ozone_kg_2d = FLTARR(dim[0],dim[1]) * !Values.F_NaN
	;o3_perc_2d  = FLTARR(dim[0],dim[1]) * !Values.F_NaN
	;o3_perc_2d_copy  = FLTARR(dim[0],dim[1]) * !Values.F_NaN
	;o3_perc_2d_copy2 = FLTARR(dim[0],dim[1]) * !Values.F_NaN
	;o3_perc_2d_copy3 = FLTARR(dim[0],dim[1]) * !Values.F_NaN
	o3_perc_2d_copy4 = FLTARR(dim[0],dim[1]) * !Values.F_NaN
	o3_perc_2d_conv = FLTARR(dim[0],dim[1]) * !Values.F_NaN
	o3_perc_2d_nconv = FLTARR(dim[0],dim[1]) * !Values.F_NaN
	
	;Find index of tropopause at each x,y point, set layer top to be 5 grid points above
	flux_o3_left   = FLTARR(1,dim[1],6)
	flux_o3_right  = FLTARR(1,dim[1],6)
	flux_o3_bottom = FLTARR(dim[0],1,6)
	flux_o3_top	= FLTARR(dim[0],1,6)
	
	flux_air_left   = FLTARR(1,dim[1],6)
	flux_air_right  = FLTARR(1,dim[1],6)
	flux_air_bottom = FLTARR(dim[0],1,6)
	flux_air_top	= FLTARR(dim[0],1,6)

	;total_mass 	  = FLTARR(dim[0],dim[1])
	total_mass 		= FLTARR(dim[2])
	total_mass_copy = FLTARR(dim[2])
	o3_kg	   		= FLTARR(dim[2])
	o3_kg_conv		= FLTARR(dim[2])
	tot_mass_conv	= FLTARR(dim[2])
	o3_kg_nconv		= FLTARR(dim[2])
	tot_mass_nconv	= FLTARR(dim[2])
	
	FOR ii = 5, dim[0]-6 DO BEGIN
		FOR jj = 5, dim[1]-6 DO BEGIN
			PRINT, ii,jj
			index1= MAX(VALUE_LOCATE(REFORM(z[ii,jj,*],dim[2],1,1),REFORM(trop_3d[ii,jj,*],dim[2],1,1)))
			index2 = index1+5
			PRINT, z[ii,jj,index1], z[ii,jj,index2]
	    	FOR kk = index1, index2 DO BEGIN
	    	    o3_kg [kk]     = o3_ugm3[ii,jj,kk]*dx*dx*(z[ii,jj,kk+1]-z[ii,jj,kk])*1.0E-9 						;convert o3 in ug/m3 to kg using dz of layer 
				total_mass[kk] = air_gm3[ii,jj,kk]*dx*dx*(z[ii,jj,kk+1]-z[ii,jj,kk])*1.0E-3									;total mass of layer in kg
				
				IF (updrt[ii,jj,kk] GT 0.1) THEN BEGIN
					o3_kg_conv[kk] = o3_kg[kk]
					tot_mass_conv[kk] = total_mass[kk]
				ENDIF ELSE BEGIN
					o3_kg_nconv[kk] = o3_kg[kk]
					tot_mass_nconv[kk] = total_mass[kk]
				ENDELSE
			ENDFOR

			;o3_perc_2d [ii,jj] = ((TOTAL(o3_kg,/NAN))/(TOTAL(air_kg[ii,jj,index1:index2],/NAN)))
			;o3_perc_2d_copy[ii,jj] = ((TOTAL(o3_kg[index1:index2],/NAN))/(TOTAL(air_kg[ii,jj,index1:index2],/NAN)))
			;o3_perc_2d_copy2[ii,jj] = ((MEAN(o3_kg[index1:index2],/NAN))/(MEAN(air_kg[ii,jj,index1:index2],/NAN)))
			;o3_perc_2d_copy3[ii,jj] = TOTAL(o3_kg[index1:index2]/air_kg[ii,jj,index1:index2])
			o3_perc_2d_copy4[ii,jj] = MEAN(o3_kg[index1:index2]/total_mass[index1:index2])
			o3_perc_2d_conv [ii,jj] = MEAN(o3_kg_conv[index1:index2]/tot_mass_conv[index1:index2])
			o3_perc_2d_nconv[ii,jj] = MEAN(o3_kg_nconv[index1:index2]/tot_mass_nconv[index1:index2])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Some notes: 
;o3_perc_2d_copy[ii,jj] = ((TOTAL(o3_kg[index1:index2],/NAN))/(TOTAL(air_kg[ii,jj,index1:index2],/NAN)))
;does not equal
;o3_perc_2d_copy[ii,jj] = TOTAL(o3_kg[index1:index2]/air_kg[ii,jj,index1:index2])
;I think method 2 is more appropriate, since you want to look at the relative mass of o3
;in each grid box
;Probably want to use total_mass instead of air_kg --> not sure which calculation is better,
;but is concerning that they are off by 1 order of magnitude. Best to be consistent with 
;flux calculation which also uses total_mass
;Additionally, Mean =/= Total, so need to reconcile with flux calculation (5/2 uses Mean)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Do (O3 mass/Air mass)*wind for flux and then divide by dx--> (%kg/s)		
			;Calculate flux of o3 and flux of air
			;Then divide fluxes			
			;IF (ii EQ 5) THEN BEGIN
			;	flux_o3_left  [*,jj,*] = (u[5,jj,index1:index2]*(o3_kg	    [index1:index2]))
			;	flux_air_left [*,jj,*] = (u[5,jj,index1:index2]*(total_mass[index1:index2]))
			;ENDIF
			;IF (ii EQ dim[0]-6) THEN BEGIN
			;	flux_o3_right [*,jj,*] = (u[dim[0]-6,jj,index1:index2]*(o3_kg	  [index1:index2]))
			;	flux_air_right[*,jj,*] = (u[dim[0]-6,jj,index1:index2]*(total_mass[index1:index2]))
			;ENDIF
			;IF (jj EQ 5) THEN BEGIN
			;	flux_o3_bottom [ii,*,*] = (v[ii,5,index1:index2]*(o3_kg	    [index1:index2]))
			;	flux_air_bottom[ii,*,*] = (v[ii,5,index1:index2]*(total_mass[index1:index2]))
			;ENDIF
			;IF (jj EQ dim[1]-6) THEN BEGIN
			;	flux_o3_top [ii,*,*] = (v[ii,dim[1]-6,index1:index2]*(o3_kg     [index1:index2]))
			;	flux_air_top[ii,*,*] = (v[ii,dim[1]-6,index1:index2]*(total_mass[index1:index2]))
			;ENDIF
			IF (ii EQ 5) THEN BEGIN
				flux_o3_left  [*,jj,*] = MEAN(u[5,jj,index1:index2]*(o3_kg	    [index1:index2]))
				flux_air_left [*,jj,*] = MEAN((total_mass[index1:index2]))
			ENDIF
			IF (ii EQ dim[0]-6) THEN BEGIN
				flux_o3_right [*,jj,*] = MEAN(u[dim[0]-6,jj,index1:index2]*(o3_kg	  [index1:index2]))
				flux_air_right[*,jj,*] = MEAN((total_mass[index1:index2]))
			ENDIF
			IF (jj EQ 5) THEN BEGIN
				flux_o3_bottom [ii,*,*] = MEAN(v[ii,5,index1:index2]*(o3_kg	    [index1:index2]))
				flux_air_bottom[ii,*,*] = MEAN((total_mass[index1:index2]))
			ENDIF
			IF (jj EQ dim[1]-6) THEN BEGIN
				flux_o3_top [ii,*,*] = MEAN(v[ii,dim[1]-6,index1:index2]*(o3_kg     [index1:index2]))
				flux_air_top[ii,*,*] = MEAN((total_mass[index1:index2]))
			ENDIF
		ENDFOR
	ENDFOR

	flux_ew1 = MEAN(((flux_o3_left/flux_air_left) - (flux_o3_right/flux_air_right)), /NAN); / ((dim[0]-11) * dx)
	flux_ns1 = MEAN(((flux_o3_bottom/flux_air_bottom) - (flux_o3_top/flux_air_top)), /NAN); / ((dim[1]-11) * dx)
	flux_ew = TOTAL(((flux_o3_left/flux_air_left) - (flux_o3_right/flux_air_right)), /NAN); / ((dim[0]-11) * dx)
	flux_ns = TOTAL(((flux_o3_bottom/flux_air_bottom) - (flux_o3_top/flux_air_top)), /NAN); / ((dim[1]-11) * dx)
	flux_ew2 = TOTAL(((flux_o3_left/flux_air_left) - (flux_o3_right/flux_air_right)), /NAN) / ((dim[0]-11) * dx)
	flux_ns2 = TOTAL(((flux_o3_bottom/flux_air_bottom) - (flux_o3_top/flux_air_top)), /NAN) / ((dim[1]-11) * dx)
	flux_ew3 = MEAN(((flux_o3_left/flux_air_left) - (flux_o3_right/flux_air_right)), /NAN) / ((dim[0]-11) * dx)
	flux_ns3 = MEAN(((flux_o3_bottom/flux_air_bottom) - (flux_o3_top/flux_air_top)), /NAN) / ((dim[1]-11) * dx)

	flux_tot = flux_ew3 + flux_ns3
	time_flux = [[[time_flux]],[[flux_tot]]]
  		
    ozone_array   = [ozone_array, MEAN(o3_perc_2d_copy4,/NAN)]
    ozone_array_conv   = [ozone_array_conv, MEAN(o3_perc_2d_conv,/NAN)]
    ozone_array_nconv   = [ozone_array_nconv, MEAN(o3_perc_2d_nconv,/NAN)]
    
    PRINT, MEAN(o3_perc_2d_copy4,/NAN)
    date_index = date_index + 1
ENDFOREACH

dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string

outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/plots/h2o_timeseries_new051219/'

epsfile = outdir + dom_string + '_' + date_string + '_%kg.eps'											;EPS filename
pdffile = outdir + dom_string + '_' + date_string + '_%kg.pdf'											;PDF filename
pngfile = outdir + dom_string + '_' + date_string + '_%kg.png'											;PNG filename

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

do3_dt = FLTARR(N_ELEMENTS(date_arr))
dflux_dt = FLTARR(N_ELEMENTS(date_arr))
num_sec = 3600.0
IF ((run EQ '20120530_ncar') AND (experiment EQ 'd03_30km')) THEN num_sec = 300.0
FOR tt = 0, N_ELEMENTS(date_arr) -3 DO BEGIN
	do3_dt[tt] = (ozone_array[tt+1] - ozone_array[tt]) / num_sec
	dflux_dt[tt] = (time_flux[tt+1] - time_flux[tt]) / num_sec
	PRINT, do3_dt[tt]
ENDFOR

net = SHIFT(do3_dt,1) - time_flux
ozone_vertflux = ozone_array+net*3600
ozone_vertflux_conv =  ozone_array_conv +net*3600
ozone_vertflux_nconv = ozone_array_nconv+net*3600

IF ((run EQ '20120530_ncar') AND (experiment EQ 'd02_30km')) THEN BEGIN 
    PLOT, SHIFT(do3_dt,1), $
    	YRANGE = [-4e-10,4e-10], $
    	YTITLE   = 'Ozone (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	TITLE    = 'Ozone % Mass of Layer: 2.5km, 20Z-05Z', $
    	CHARSIZE = 2, $
    	/NODATA
    
    AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [-4e-10,4e-10], $
    	YTITLE   = 'Ozone (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 2
    	
    OPLOT, SHIFT(do3_dt,1)*100.0, COLOR = COLOR_24('red'), THICK=3
    OPLOT, time_flux*100.0, COLOR = COLOR_24('blue'), THICK=3
    ;OPLOT, dflux_dt*100.0, COLOR = COLOR_24('blue'), THICK=3
    OPLOT, [0,250], [0,0], COLOR = COLOR_24('black'), THICK=3
    
    net = SHIFT(do3_dt,1) - time_flux
    OPLOT, net*100.0, COLOR = COLOR_24('darkgreen'), THICK=3
    
    AXIS, YAXIS = 1, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE   = [1.75E-5,3.0E-5], $
    	YTITLE   = 'ozone (% kg)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 2
    
    OPLOT, ozone_vertflux*100.0, COLOR = COLOR_24('gray50'), THICK=3
    OPLOT, ozone_vertflux_conv*100.0, COLOR = COLOR_24('gray30'), THICK=3
    OPLOT, ozone_vertflux_nconv*100.0, COLOR = COLOR_24('gray70'), THICK=3, LINESTYLE=2

    
    XYOUTS, 0.5, 2.90E-05, 'dO3/dt' , COLOR = COLOR_24('red'      ), CHARSIZE=2, /DATA
    XYOUTS, 0.5, 2.85E-05, 'O3 flux', COLOR = COLOR_24('blue'     ), CHARSIZE=2, /DATA
    XYOUTS, 0.5, 2.80E-05, 'Net'    , COLOR = COLOR_24('darkgreen'), CHARSIZE=2, /DATA
    XYOUTS, 0.5, 2.75E-05, 'o3'     , COLOR = COLOR_24('gray50'   ), CHARSIZE=2, /DATA
    XYOUTS, 0.5, 2.70E-05, 'o3-conv' , COLOR = COLOR_24('gray30'   ), CHARSIZE=2, /DATA
    XYOUTS, 0.5, 2.65E-05, 'o3-nconv', COLOR = COLOR_24('gray70'   ), CHARSIZE=2, /DATA
ENDIF

;IF ((run EQ '20120530_ncar') AND (experiment EQ 'd02_30km')) THEN BEGIN 
;    PLOT, SHIFT(do3_dt,1), $
;    	YRANGE = [-4e-11,4e-11], $
;    	YTITLE   = 'Ozone (kg/s)', $
;    	COLOR    = COLOR_24('black'), $
;    	TITLE    = 'Ozone % Mass of Layer: 2.5km, 20Z-05Z', $
;    	CHARSIZE = 2, $
;    	/NODATA
;    
;    AXIS, YAXIS = 0, $																								;Draw altitude axis
;    	SAVE     = 1, $
;    	YRANGE 	 = [-4e-11,4e-11], $
;    	YTITLE   = 'Ozone (kg/s)', $
;    	COLOR    = COLOR_24('black'), $
;    	YTICKS   = 1, $
;    	YSTYLE   = 1, $
;    	CHARSIZE = 2
;    	
;    OPLOT, SHIFT(do3_dt,1)*100.0, COLOR = COLOR_24('red'), THICK=3
;    OPLOT, time_flux*100.0, COLOR = COLOR_24('blue'), THICK=3
;    OPLOT, [0,250], [0,0], COLOR = COLOR_24('black'), THICK=3
;    
;    net = SHIFT(do3_dt,1) - time_flux
;    OPLOT, net*100.0, COLOR = COLOR_24('darkgreen'), THICK=3
;    
;    AXIS, YAXIS = 1, $																								;Draw altitude axis
;    	SAVE     = 1, $
;    	YRANGE   = [0,3.0E-6], $
;    	YTITLE   = 'ozone (kg)', $
;    	COLOR    = COLOR_24('black'), $
;    	YTICKS   = 1, $
;    	YSTYLE   = 1, $
;    	CHARSIZE = 2
;    
;    OPLOT, ozone_array*100.0, COLOR = COLOR_24('gray60'), THICK=3
;    
;    XYOUTS, 0.5, 2.80E-06, 'dO3/dt' , COLOR = COLOR_24('red'      ), CHARSIZE=2, /DATA
;    XYOUTS, 0.5, 2.65E-06, 'O3 flux', COLOR = COLOR_24('blue'     ), CHARSIZE=2, /DATA
;    XYOUTS, 0.5, 2.50E-06, 'Net'    , COLOR = COLOR_24('darkgreen'), CHARSIZE=2, /DATA
;    XYOUTS, 0.5, 2.35E-06, 'o3'     , COLOR = COLOR_24('gray60'   ), CHARSIZE=2, /DATA
;ENDIF

IF ((run EQ '20120530_ncar') AND (experiment EQ 'd03_30km')) THEN BEGIN 
    PLOT, SHIFT(do3_dt,1), $
    	YRANGE = [-1E-9,1e-9], $
    	YTITLE   = 'Ozone (kg/s)', $
 		XTICKINTERVAL = 12, $
	   	COLOR    = COLOR_24('black'), $
    	TITLE    = 'Ozone % Mass of Layer: 500m, 2130Z-0500Z', $
    	CHARSIZE = 2, $
    	/NODATA
    
    AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [-1E-9,1e-9], $
    	YTITLE   = 'Ozone (kg/s)', $
		XTICKINTERVAL = 12, $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 2
    	
    OPLOT, SHIFT(do3_dt,1)*100.0, COLOR = COLOR_24('red'), THICK=3
    OPLOT, time_flux*100.0, COLOR = COLOR_24('blue'), THICK=3
    OPLOT, [0,250], [0,0], COLOR = COLOR_24('black'), THICK=3
    
    net = SHIFT(do3_dt,1) - time_flux
	ozone_vertflux = ozone_array+net*3600
	ozone_vertflux_conv =  ozone_array_conv +net*3600
	ozone_vertflux_nconv = ozone_array_nconv+net*3600
    OPLOT, net*100.0, COLOR = COLOR_24('darkgreen'), THICK=3
    
    AXIS, YAXIS = 1, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE   = [1.75E-5,4.0E-5], $
    	YTITLE   = 'ozone (kg)', $
		XTICKINTERVAL = 12, $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 2
    
    OPLOT, ozone_vertflux*100.0, COLOR = COLOR_24('gray50'), THICK=3
    OPLOT, ozone_vertflux_conv*100.0, COLOR = COLOR_24('gray30'), THICK=3
    OPLOT, ozone_vertflux_nconv*100.0, COLOR = COLOR_24('gray70'), THICK=3, LINESTYLE=2
    
    XYOUTS, 5, 3.80E-05, 'dO3/dt' , COLOR = COLOR_24('red'      ), CHARSIZE=2, /DATA
    XYOUTS, 5, 3.70E-05, 'O3 flux', COLOR = COLOR_24('blue'     ), CHARSIZE=2, /DATA
    XYOUTS, 5, 3.60E-05, 'Net'    , COLOR = COLOR_24('darkgreen'), CHARSIZE=2, /DATA
    XYOUTS, 5, 3.50E-05, 'o3'     , COLOR = COLOR_24('gray50'   ), CHARSIZE=2, /DATA
    XYOUTS, 5, 3.40E-05, 'o3-conv' , COLOR = COLOR_24('gray30'   ), CHARSIZE=2, /DATA
    XYOUTS, 5, 3.30E-05, 'o3-nconv', COLOR = COLOR_24('gray70'   ), CHARSIZE=2, /DATA
ENDIF

IF (run EQ '20110518') THEN BEGIN 
    PLOT, SHIFT(do3_dt,1), $
    	YRANGE = [-3e-10,3e-10], $
    	YTITLE   = 'H2O (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	TITLE    = run, $
    	CHARSIZE = 2, $
    	/NODATA
    
    AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [-3e-10,3e-10], $
    	YTITLE   = 'H2O (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 2
    	
    OPLOT, SHIFT(do3_dt,1)*100.0, COLOR = COLOR_24('red'), THICK=3
    OPLOT, time_flux*100.0, COLOR = COLOR_24('blue'), THICK=3
    OPLOT, [0,250], [0,0], COLOR = COLOR_24('black'), THICK=3
    
    net = SHIFT(do3_dt,1) - time_flux
    OPLOT, net, COLOR = COLOR_24('darkgreen'), THICK=3
    
    AXIS, YAXIS = 1, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE   = [0,8.5E-8], $
    	YTITLE   = 'H2O (kg)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 2
    
    OPLOT, ozone_vertflux*100.0, COLOR = COLOR_24('gray50'), THICK=3
    OPLOT, ozone_vertflux_conv*100.0, COLOR = COLOR_24('gray30'), THICK=3
    OPLOT, ozone_vertflux_nconv*100.0, COLOR = COLOR_24('gray70'), THICK=3, LINESTYLE=2
    
    XYOUTS, 2, 140, 'dH2O/dt' , COLOR = COLOR_24('red'      ), CHARSIZE=2, /DATA
    XYOUTS, 2, 133, 'H2O flux', COLOR = COLOR_24('blue'     ), CHARSIZE=2, /DATA
    XYOUTS, 2, 126, 'Net'    , COLOR = COLOR_24('darkgreen'), CHARSIZE=2, /DATA
    XYOUTS, 2, 119, 'H2O'     , COLOR = COLOR_24('gray60'   ), CHARSIZE=2, /DATA
ENDIF

IF (run EQ '20130805') THEN BEGIN 
    PLOT, SHIFT(do3_dt,1), $
    	YRANGE = [-6e-3,6e-3], $
    	YTITLE   = 'H2O (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	TITLE    = run, $
    	CHARSIZE = 2, $
    	/NODATA
    
    AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [-6e-3,6e-3], $
    	YTITLE   = 'H2O (kg/s)', $
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
    	YTITLE   = 'H2O (kg)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 2
    
    OPLOT, ozone_array, COLOR = COLOR_24('gray60'), THICK=3
    
    XYOUTS, 2, 140, 'dH2O/dt' , COLOR = COLOR_24('red'   ), CHARSIZE=2, /DATA
    XYOUTS, 2, 133, 'H2O flux', COLOR = COLOR_24('blue'  ), CHARSIZE=2, /DATA
    XYOUTS, 2, 126, 'Net'    , COLOR = COLOR_24('green' ), CHARSIZE=2, /DATA
    XYOUTS, 2, 119, 'H2O'     , COLOR = COLOR_24('gray60'), CHARSIZE=2, /DATA
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

