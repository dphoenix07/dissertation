PRO MASS_LAYER, run, experiment, start_date, end_date, $
	DOMAIN        = domain, $
	REGION        = region, $
	WRAPPING      = wrapping, $
	CONVECTIVE 	  = convective, $
	NONCONVECTIVE = nonconvective, $
	PNG	          = png, $
	EPS   	      = eps


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

outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/plots/ozone_mass_layer/'
FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(run, experiment, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(run, experiment, start_date, end_date, /DATE)	


z = (WRF_READ_VAR('Z', date_arr[0], run, experiment, DOMAIN = domain, INDICES = region)).values
dim = SIZE(z,/DIMENSIONS)

mass_array		= [ ]
time_flux   	= [ ]
time_layer_flux = [ ]
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
    updrt = (WRF_READ_VAR('Updraft_tracer' , date, run, experiment, DOMAIN = domain, INDICES = region)).values
    press = (WRF_READ_VAR('P'	           , date, run, experiment, DOMAIN = domain, INDICES = region)).values							
    ztrop = (WRF_READ_VAR('Z_trop'         , date, run, experiment, DOMAIN = domain, INDICES = region)).values
    temp  = (WRF_READ_VAR('T'	           , date, run, experiment, DOMAIN = domain, INDICES = region)).values						
    theta =  WRF_READ_VAR('T'	           , date, run, experiment, DOMAIN = domain, INDICES = region)								;Read temperature variable from WRF output
    theta.values = ((1000.0/(WRF_READ_VAR('P', date, run, experiment, $										;Compute potential temperature
    		 						DOMAIN = domain, INDICES = region)).values)^(!Rair/!Cp))*(theta.values)

    ztrop    = MEDIAN(ztrop, 30)    
	trop_3d  = REBIN (ztrop, dim[0], dim[1], dim[2], /SAMPLE)

	molec_weight = 48.0
    r_star = 8.314
    overshoot  = 0.001*((MAX((cloud GE 1.0E-5)*z, DIM = 3, /NAN)) - trop_3d)						;Set map variable to cloud top altitude

	dx = 3000.0
	IF ((run EQ '20120530_ncar') AND (experiment EQ 'd03_30km')) THEN dx = 500.0
    ;Convert o3 from ppm to kg
    air_gm3     = ((press*1.0E2 * 28.97) / (r_star * temp))
    air_kg	    = ((press*1.0E2)*dx*dx)/9.81
	
	tot_tot_mass = FLTARR(dim[0],dim[1]) * !Values.F_NaN
	
	;Find index of tropopause at each x,y point, set layer top to be 5 grid points above
	flux_air_left   = FLTARR(1,dim[1],6)
	flux_air_right  = FLTARR(1,dim[1],6)
	flux_air_bottom = FLTARR(dim[0],1,6)
	flux_air_top	= FLTARR(dim[0],1,6)

	;total_mass 	  = FLTARR(dim[0],dim[1])
	total_mass = FLTARR(dim[2])
	total_mass_copy = FLTARR(dim[2])
	
	FOR ii = 5, dim[0]-6 DO BEGIN
		FOR jj = 5, dim[1]-6 DO BEGIN
			total_mass = FLTARR(dim[2])
			total_mass_copy = FLTARR(dim[2])
			PRINT, MEAN(total_mass)
			
			index2= MAX(VALUE_LOCATE(REFORM(z[ii,jj,*],dim[2],1,1),REFORM(trop_3d[ii,jj,*],dim[2],1,1)))
			index1 = index2-5
			PRINT, z[ii,jj,index1], z[ii,jj,index2]
	    	FOR kk = index1, index2 DO BEGIN
				total_mass[kk] = air_gm3[ii,jj,kk]*dx*dx*(z[ii,jj,kk+1]-z[ii,jj,kk])*1.0E-3									;total mass of layer in kg
			ENDFOR

			tot_tot_mass[ii,jj] = MEAN(total_mass[index1:index2],/NAN)
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
;Additionally, Mean =/= Total, so need to reconcile with flux calculation (5/2/19 uses Mean)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Do (O3 mass/Air mass)*wind for flux and then divide by dx--> (%kg/s)		
			;Calculate flux of o3 and flux of air
			;Then divide fluxes			
			IF (ii EQ 5) THEN BEGIN
				flux_air_left [*,jj,*] = (u[5,jj,index1:index2]*(total_mass[index1:index2]))
			ENDIF
			IF (ii EQ dim[0]-6) THEN BEGIN
				flux_air_right[*,jj,*] = (u[dim[0]-6,jj,index1:index2]*(total_mass[index1:index2]))
			ENDIF
			IF (jj EQ 5) THEN BEGIN
				flux_air_bottom[ii,*,*] = (v[ii,5,index1:index2]*(total_mass[index1:index2]))
			ENDIF
			IF (jj EQ dim[1]-6) THEN BEGIN
				flux_air_top[ii,*,*] = (v[ii,dim[1]-6,index1:index2]*(total_mass[index1:index2]))
			ENDIF
		ENDFOR
	ENDFOR

	flux_ew3 = MEAN(((flux_air_left) - (flux_air_right)), /NAN) / ((dim[0]-11) * dx)
	flux_ns3 = MEAN(((flux_air_bottom) - (flux_air_top)), /NAN) / ((dim[1]-11) * dx)

	flux_tot = flux_ew3 + flux_ns3
	time_flux = [[[time_flux]],[[flux_tot]]]
  		
    mass_array   = [mass_array, MEAN(tot_tot_mass,/NAN)]
    PRINT, MEAN(tot_tot_mass,/NAN)
    date_index = date_index + 1
ENDFOREACH

dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)								;Create date string

outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/plots/o3_timeseries/'

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

dmass_dt = FLTARR(N_ELEMENTS(date_arr))
dflux_dt = FLTARR(N_ELEMENTS(date_arr))
num_sec = 3600.0
IF ((run EQ '20120530_ncar') AND (experiment EQ 'd03_30km')) THEN num_sec = 300.0
FOR tt = 0, N_ELEMENTS(date_arr) -3 DO BEGIN
	dmass_dt[tt] = (mass_array[tt+1] - mass_array[tt]) / num_sec
	dflux_dt[tt] = (time_flux[tt+1] - time_flux[tt]) / num_sec
	PRINT, dmass_dt[tt]
ENDFOR

IF ((run EQ '20120530_ncar') AND (experiment EQ 'd02_30km')) THEN BEGIN 
    PLOT, SHIFT(dmass_dt,1), $
    	YRANGE = [-6000,6000], $
    	YTITLE   = 'Mass (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	TITLE    = 'Ozone % Mass of Layer: 2.5km, 20Z-05Z', $
    	CHARSIZE = 2, $
    	/NODATA
    
    AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [-6000,6000], $
    	YTITLE   = 'Mass (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 2
    	
    OPLOT, SHIFT(dmass_dt,1), COLOR = COLOR_24('red'), THICK=3
    OPLOT, time_flux, COLOR = COLOR_24('blue'), THICK=3
    ;OPLOT, dflux_dt*100.0, COLOR = COLOR_24('blue'), THICK=3
    OPLOT, [0,250], [0,0], COLOR = COLOR_24('black'), THICK=3
    
    net = SHIFT(dmass_dt,1) - time_flux
    OPLOT, net, COLOR = COLOR_24('darkgreen'), THICK=3
    
    AXIS, YAXIS = 1, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE   = [0,1.0E9], $
    	YTITLE   = 'mass (kg)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 2
    
    OPLOT, mass_array, COLOR = COLOR_24('gray60'), THICK=3
    
    XYOUTS, 0.5, 2.80E-04, 'dmass/dt' , COLOR = COLOR_24('red'      ), CHARSIZE=2, /DATA
    XYOUTS, 0.5, 2.65E-04, 'mass flux', COLOR = COLOR_24('blue'     ), CHARSIZE=2, /DATA
    XYOUTS, 0.5, 2.50E-04, 'Net'    , COLOR = COLOR_24('darkgreen'), CHARSIZE=2, /DATA
    XYOUTS, 0.5, 2.35E-04, 'mass'     , COLOR = COLOR_24('gray60'   ), CHARSIZE=2, /DATA
ENDIF

;IF ((run EQ '20120530_ncar') AND (experiment EQ 'd02_30km')) THEN BEGIN 
;    PLOT, SHIFT(dmass_dt,1), $
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
;    OPLOT, SHIFT(dmass_dt,1)*100.0, COLOR = COLOR_24('red'), THICK=3
;    OPLOT, time_flux*100.0, COLOR = COLOR_24('blue'), THICK=3
;    OPLOT, [0,250], [0,0], COLOR = COLOR_24('black'), THICK=3
;    
;    net = SHIFT(dmass_dt,1) - time_flux
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
;    OPLOT, mass_array*100.0, COLOR = COLOR_24('gray60'), THICK=3
;    
;    XYOUTS, 0.5, 2.80E-06, 'dmass/dt' , COLOR = COLOR_24('red'      ), CHARSIZE=2, /DATA
;    XYOUTS, 0.5, 2.65E-06, 'mass flux', COLOR = COLOR_24('blue'     ), CHARSIZE=2, /DATA
;    XYOUTS, 0.5, 2.50E-06, 'Net'    , COLOR = COLOR_24('darkgreen'), CHARSIZE=2, /DATA
;    XYOUTS, 0.5, 2.35E-06, 'mass'     , COLOR = COLOR_24('gray60'   ), CHARSIZE=2, /DATA
;ENDIF

IF ((run EQ '20120530_ncar') AND (experiment EQ 'd03_30km')) THEN BEGIN 
    PLOT, SHIFT(dmass_dt,1), $
    	YRANGE = [-8E-11,8e-11], $
    	YTITLE   = 'Ozone (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	TITLE    = 'Ozone % Mass of Layer: 500m, 20Z-05Z', $
    	CHARSIZE = 2, $
    	/NODATA
    
    AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [-8E-11,8e-11], $
    	YTITLE   = 'Ozone (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 2
    	
    OPLOT, SHIFT(dmass_dt,1)*100.0, COLOR = COLOR_24('red'), THICK=3
    OPLOT, time_flux*100.0, COLOR = COLOR_24('blue'), THICK=3
    OPLOT, [0,250], [0,0], COLOR = COLOR_24('black'), THICK=3
    
    net = SHIFT(dmass_dt,1) - time_flux
    OPLOT, net*100.0, COLOR = COLOR_24('darkgreen'), THICK=3
    
    AXIS, YAXIS = 1, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE   = [0,3.0E-6], $
    	YTITLE   = 'ozone (kg)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 2
    
    OPLOT, mass_array*100.0, COLOR = COLOR_24('gray60'), THICK=3
    
    XYOUTS, 5, 2.80E-06, 'dmass/dt' , COLOR = COLOR_24('red'      ), CHARSIZE=2, /DATA
    XYOUTS, 5, 2.65E-06, 'mass flux', COLOR = COLOR_24('blue'     ), CHARSIZE=2, /DATA
    XYOUTS, 5, 2.50E-06, 'Net'    , COLOR = COLOR_24('darkgreen'), CHARSIZE=2, /DATA
    XYOUTS, 5, 2.35E-06, 'mass'     , COLOR = COLOR_24('gray60'   ), CHARSIZE=2, /DATA
ENDIF

IF (run EQ '20110518') THEN BEGIN 
    PLOT, SHIFT(dmass_dt,1), $
    	YRANGE = [-3e-10,3e-10], $
    	YTITLE   = 'Ozone (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	TITLE    = run, $
    	CHARSIZE = 2, $
    	/NODATA
    
    AXIS, YAXIS = 0, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE 	 = [-3e-10,3e-10], $
    	YTITLE   = 'Ozone (kg/s)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 2
    	
    OPLOT, SHIFT(dmass_dt,1)*100.0, COLOR = COLOR_24('red'), THICK=3
    OPLOT, time_flux*100.0, COLOR = COLOR_24('blue'), THICK=3
    OPLOT, [0,250], [0,0], COLOR = COLOR_24('black'), THICK=3
    
    net = SHIFT(dmass_dt,1) - time_flux
    OPLOT, net, COLOR = COLOR_24('darkgreen'), THICK=3
    
    AXIS, YAXIS = 1, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE   = [0,8.5E-8], $
    	YTITLE   = 'ozone (kg)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 2
    
    OPLOT, mass_array*100.0, COLOR = COLOR_24('gray60'), THICK=3
    
    XYOUTS, 2, 140, 'dmass/dt' , COLOR = COLOR_24('red'      ), CHARSIZE=2, /DATA
    XYOUTS, 2, 133, 'mass flux', COLOR = COLOR_24('blue'     ), CHARSIZE=2, /DATA
    XYOUTS, 2, 126, 'Net'    , COLOR = COLOR_24('darkgreen'), CHARSIZE=2, /DATA
    XYOUTS, 2, 119, 'mass'     , COLOR = COLOR_24('gray60'   ), CHARSIZE=2, /DATA
ENDIF

IF (run EQ '20130805') THEN BEGIN 
    PLOT, SHIFT(dmass_dt,1), $
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
    	
    OPLOT, SHIFT(dmass_dt,1), COLOR = COLOR_24('red'), THICK=3
    OPLOT, time_layer_flux, COLOR = COLOR_24('blue'), THICK=3
    OPLOT, [0,250], [0,0], COLOR = COLOR_24('black'), THICK=3
    
    net = SHIFT(dmass_dt,1) - time_layer_flux
    OPLOT, net, COLOR = COLOR_24('green'), THICK=3
    
    AXIS, YAXIS = 1, $																								;Draw altitude axis
    	SAVE     = 1, $
    	YRANGE   = [0,360], $
    	YTITLE   = 'ozone (kg)', $
    	COLOR    = COLOR_24('black'), $
    	YTICKS   = 1, $
    	YSTYLE   = 1, $
    	CHARSIZE = 2
    
    OPLOT, mass_array, COLOR = COLOR_24('gray60'), THICK=3
    
    XYOUTS, 2, 140, 'dmass/dt' , COLOR = COLOR_24('red'   ), CHARSIZE=2, /DATA
    XYOUTS, 2, 133, 'mass flux', COLOR = COLOR_24('blue'  ), CHARSIZE=2, /DATA
    XYOUTS, 2, 126, 'Net'    , COLOR = COLOR_24('green' ), CHARSIZE=2, /DATA
    XYOUTS, 2, 119, 'mass'     , COLOR = COLOR_24('gray60'), CHARSIZE=2, /DATA
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

