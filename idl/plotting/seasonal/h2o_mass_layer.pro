PRO H2O_MASS_LAYER, run, experiment, start_date, end_date, $
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

time_flux_u     = [ ]
time_flux_v     = [ ]
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
	min_trop = MIN(trop_3d,/NAN)
    xyz_trop = FLTARR(dim[0],dim[1],dim[2]) + min_trop
    
	molec_weight = 18.0
    r_star = 8.314
	dx = 3000.0
		
	;;+ old flux calculation
 	;flux_u   = FLTARR(dim[2])
	;flux_v   = FLTARR(dim[2])
	;flux_tot = FLTARR(dim[2])

	;Estimate Flux of h2o in and out of domain
	;FOR k = 0, dim[2]-1 DO BEGIN
	;	flux_u  [k] = (MEAN((u[5,*,k]*h2o[5,*,k] - u[dim[0]-5,*,k]*h2o[dim[0]-5,*,k]),/NAN)) / ((dim[0] - 11) * dx)
	;	flux_v  [k] = (MEAN((v[*,5,k]*h2o[*,5,k] - v[*,dim[1]-5,k]*h2o[*,dim[1]-5,k]),/NAN)) / ((dim[1] - 11) * dx)
	;	flux_tot[k] = flux_u[k] + flux_v[k]
	;ENDFOR
;
	;time_flux = [[[time_flux]], [[flux_tot]]]
	;;- old flux calculation
	
	;;+To filter out cloud/non-cloud air;;
	;convect = WHERE(updrt GT 0.25, count, COMPLEMENT = nconv)
	;IF (KEYWORD_SET(WRAPPING)) THEN h2o [convect] = !Values.F_NaN
    ;h2o [convect] = !Values.F_NaN
    ;;-end filter
    
    overshoot  = 0.001*((MAX((cloud GE 1.0E-5)*z, DIM = 3, /NAN)) - trop_3d)						;Set map variable to cloud top altitude
    
    ;Convert h2o from ppm to kg
    air_kgm3   = ((press*1.0E2 * 28.97) / (r_star * temp))*1.0E-3
    air_kg	   = ((press*1.0E2)*dx*dx)/9.81
    h2o_ugm3   = (h2o * press*1.0E2 * molec_weight) / (r_star * temp)

	h2o_perc_2d= FLTARR(dim[0],dim[1]) * !Values.F_NaN
	h2o_ppm_2d = FLTARR(dim[0],dim[1]) * !Values.F_NaN
	
	;Find index of tropopause at each x,y point, set layer top to be 5 grid points above
	flux_h2o_left   = FLTARR(1,dim[1],6)
	flux_h2o_right  = FLTARR(1,dim[1],6)
	flux_h2o_bottom = FLTARR(dim[0],1,6)
	flux_h2o_top	= FLTARR(dim[0],1,6)
	
	flux_air_left   = FLTARR(1,dim[1],6)
	flux_air_right  = FLTARR(1,dim[1],6)
	flux_air_bottom = FLTARR(dim[0],1,6)
	flux_air_top	= FLTARR(dim[0],1,6)
	

	;total_mass = FLTARR(dim[0],dim[1])
	total_mass = FLTARR(dim[2])
	h2o_kg	   = FLTARR(dim[2])
	
	FOR ii = 5, dim[0]-6 DO BEGIN
		FOR jj = 5, dim[1]-6 DO BEGIN
			index1= MAX(VALUE_LOCATE(REFORM(z[ii,jj,*],dim[2],1,1),REFORM(trop_3d[ii,jj,*],dim[2],1,1)))
			index2 = index1+5
	    	FOR kk = index1, index2 DO BEGIN
	    	    h2o_kg [kk]    = h2o_ugm3[ii,jj,kk]*dx*dx*(z[ii,jj,kk+1]-z[ii,jj,kk])*1.0E-9 				;convert h2o in ug/m3 to kg using dz of layer 
				total_mass[kk] = air_kgm3[ii,jj,kk]*dx*dx*(z[ii,jj,kk+1]-z[ii,jj,kk])*1.0E-3									;total mass of layer in kg
			ENDFOR
	    	;h2o_kg = h2o_ugm3[ii,jj,index1:index2]*dx*dx*(z[ii,jj,index2]-z[ii,jj,index1])*1.0E-9 				;convert h2o in ug/m3 to kg using dz of layer 
			;total_mass[ii,jj] = air_kgm3[ii,jj,index1:index2]*dx*dx*(z[ii,jj,index2]-z[ii,jj,index1])*1.0E-3									;total mass of layer in kg			
			h2o_perc_2d [ii,jj] = ((TOTAL(h2o_kg,/NAN))/(TOTAL(air_kg[ii,jj,index1:index2],/NAN)))
;			h2o_perc_2d [ii,jj] = ((TOTAL(h2o_kg)/(TOTAL(total_mass))))
			
			;Calculate flux of h2o and flux of air
			;Then divide fluxes
			IF (ii EQ 5) THEN BEGIN
				flux_h2o_left  [*,jj,*] = (u[5,jj,index1:index2]*(h2o_kg	[index1:index2]))
				flux_air_left  [*,jj,*] = (u[5,jj,index1:index2]*(total_mass[index1:index2]))
			ENDIF
			IF (ii EQ dim[0]-6) THEN BEGIN
				flux_h2o_right [*,jj,*] = (u[dim[0]-6,jj,index1:index2]*(h2o_kg	 [index1:index2]))
				flux_air_right [*,jj,*] = (u[dim[0]-6,jj,index1:index2]*(total_mass[index1:index2]))
			ENDIF
			IF (jj EQ 5) THEN BEGIN
				flux_h2o_bottom[ii,*,*] = (v[ii,5,index1:index2]*(h2o_kg	[index1:index2]))
				flux_air_bottom[ii,*,*] = (v[ii,5,index1:index2]*(total_mass[index1:index2]))
			ENDIF
			IF (jj EQ dim[1]-6) THEN BEGIN
				flux_h2o_top [ii,*,*] = (v[ii,dim[1]-6,index1:index2]*(h2o_kg    [index1:index2]))
				flux_air_top [ii,*,*] = (v[ii,dim[1]-6,index1:index2]*(total_mass[index1:index2]))
			ENDIF
		ENDFOR
	ENDFOR

	flux_ew = MEAN(((flux_h2o_left/flux_air_left) - (flux_h2o_right/flux_air_right)), /NAN) / ((dim[0]-11) * dx)
	flux_ns = MEAN(((flux_h2o_bottom/flux_air_bottom) - (flux_h2o_top/flux_air_top)), /NAN) / ((dim[1]-11) * dx)
	flux_tot = flux_ew + flux_ns
	time_flux = [[[time_flux]],[[flux_tot]]]

    h2o_array   = [h2o_array, MEAN(h2o_perc_2d,/NAN)]
    PRINT, MEAN(h2o_perc_2d,/NAN)
    date_index = date_index + 1
ENDFOREACH

dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string

outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/plots/h2o_timeseries/'

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

num_sec = 3600.0
IF (experiment) EQ '20120530_ncar'  THEN num_sec = 300.0

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
		YRANGE   = [-6.0E-7,6.0E-7], $		
		YTITLE   = 'h2o (% kg/s)', $
		COLOR    = COLOR_24('black'), $
		CHARSIZE = 2, $
		/NODATA

	AXIS, YAXIS = 0, $																								;Draw altitude axis
		SAVE     = 1, $
		YRANGE   = [-6.0E-7,6.0E-7], $
		YTITLE   = 'h2o (% kg/s)', $
		COLOR    = COLOR_24('black'), $
		YTICKS   = 1, $
		YSTYLE   = 1, $
		CHARSIZE = 2
	
	OPLOT, SHIFT(dh2o_dt,1)*100.0, COLOR = COLOR_24('red'),  THICK=3
	OPLOT, time_flux*100.0, COLOR = COLOR_24('blue'), THICK=3
	OPLOT, [0,250], [0,0]  , COLOR = COLOR_24('black'),THICK=3

	net = SHIFT(dh2o_dt,1) - time_flux
	OPLOT, net*100.0, COLOR = COLOR_24('darkgreen'), THICK=3

	AXIS, YAXIS = 1, $																								;Draw altitude axis
		SAVE     = 1, $
		YRANGE   = [0.0,2.0E-5], $
		YTITLE   = 'h2o (% kg)', $
		COLOR    = COLOR_24('black'), $
		YTICKS   = 1, $
		YSTYLE   = 1, $
		CHARSIZE = 2

	OPLOT, h2o_array*100.0, COLOR = COLOR_24('gray60'), THICK=3

	XYOUTS, 215, 190, 'dh2o/dt' , COLOR = COLOR_24('red'   ), CHARSIZE=2, /DATA
	XYOUTS, 215, 183, 'h2o flux', COLOR = COLOR_24('blue'  ), CHARSIZE=2, /DATA
	XYOUTS, 215, 176, 'Net'     , COLOR = COLOR_24('green' ), CHARSIZE=2, /DATA
	XYOUTS, 215, 169, 'h2o'     , COLOR = COLOR_24('gray60'), CHARSIZE=2, /DATA
ENDIF

IF (run EQ '20130805') THEN BEGIN 
	PLOT, SHIFT(dh2o_dt,1), $
		TITLE    = run, $
		YRANGE   = [-6.0e-4,6.0e-4], $
		YTITLE   = 'h2o (% kg/s)', $
		COLOR    = COLOR_24('black'), $
		CHARSIZE = 2, $
		/NODATA

	AXIS, YAXIS = 0, $																								;Draw altitude axis
		SAVE     = 1, $
		YRANGE   = [-6.0e-4,6.0e-4], $
		YTITLE   = 'h2o (% kg/s)', $
		COLOR    = COLOR_24('black'), $
		YTICKS   = 1, $
		YSTYLE   = 1, $
		CHARSIZE = 2
	
	OPLOT, SHIFT(dh2o_dt,1), COLOR = COLOR_24('red'),  THICK=3
	OPLOT, time_flux , COLOR = COLOR_24('blue'), THICK=3
	OPLOT, [0,250], [0,0]  , COLOR = COLOR_24('black'),THICK=3

	net = SHIFT(dh2o_dt,1) - time_flux
	OPLOT, net, COLOR = COLOR_24('darkgreen'), THICK=3

	AXIS, YAXIS = 1, $																								;Draw altitude axis
		SAVE     = 1, $
		YRANGE   = [10,20], $
		YTITLE   = 'h2o (% kg)', $
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

