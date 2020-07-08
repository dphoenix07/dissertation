PRO TIME_HEIGHT_STATIC_STABILITY, event, scheme, start_date, end_date, $
	DOMAIN   = domain, $
	REGION   = region, $
	TROPICS  = tropics,$
	MID_LAT  = mid_lat, $
	ALT	     = alt, $
	PNG	     = png, $
	EPS   	 = eps


;+
; Name:
;		TIME_HEIGHT_STATIC_STABILITY
; Purpose:
;		Calculates the profile of the mean potential temperature gradient and plots a 
;		curtain in time. 
; Calling sequence:
;		TIME_HEIGHT_STATIC_STABILITY, run, scheme, start_date, end_date
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		scheme 	   : Model initial state. Typically 'morrison' or 'nssl'.
;		start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;		end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
;		isen	   : Isentrope at which to map ozone tracer values 
; Output:
;		Time-height plot of static stability
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		TRACER  : Ozone tracer to use (UTLS_tracer is default).
; Author and history:
;		Daniel B. Phoenix	    2018-02-26. 
;								
;								
;-

COMPILE_OPT IDL2																				;Set compile options

IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain

outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/plots/static_stability/'
FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(event, scheme, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(event, scheme, start_date, end_date, /DATE)	


map_plot = [ ]
ralt_arr = [ ]
FOREACH date, date_arr DO BEGIN
	PRINT, date
	
 	R 	    =  WRF_READ_VAR('REFL'           , date, event, scheme, DOMAIN=domain, INDICES=region)
	cloud   =  WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, DOMAIN=domain, INDICES=region)
    y       = (WRF_READ_VAR('Latitude'       , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    x       = (WRF_READ_VAR('Longitude'      , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    z       = (WRF_READ_VAR('Z'		         , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E-3
    ztrop1  = (WRF_READ_VAR('Z_trop'	     , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E-3	
	theta   = (WRF_READ_VAR('T'			     , date, event, scheme, DOMAIN=domain, INDICES=region)).values					
	theta   = ((1000.0/(WRF_READ_VAR('P', date, event, scheme, DOMAIN = domain, INDICES=region)).values)^(!Rair/!Cp))*(theta)

	ztrop1   = MEDIAN(ztrop1, 100)
    dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)

    xyz_trop = REBIN(ztrop1, dim[0], dim[1], dim[2], /SAMPLE)

	count = 0
 	IF (KEYWORD_SET(tropics)) THEN igood = WHERE(xyz_trop GT 15000.0, count, COMPLEMENT = ibad)
 	IF (KEYWORD_SET(mid_lat)) THEN igood = WHERE(xyz_trop LT 15000.0, count, COMPLEMENT = ibad)

	IF (count GT 0) THEN BEGIN
        z       [ibad] = !Values.F_NaN
        xyz_trop[ibad] = !Values.F_NaN
		theta   [ibad] = !Values.F_NaN
	ENDIF
 
    ralt   = (z - xyz_trop) 
    IF (KEYWORD_SET(ALT)) THEN ralt1 = z 

	theta_c  = FLTARR(dim[0],dim[1],dim[2])
	theta_nc = FLTARR(dim[0],dim[1],dim[2])
 	dthdz    = FLTARR(dim[0],dim[1],dim[2]-2) 
 	 
	FOR ii = 0, dim[0]-1 DO BEGIN
		FOR jj = 0, dim[1]-1 DO BEGIN
			FOR kk = 1, dim[2]-3 DO BEGIN
				dthdz[ii,jj,kk] = (theta[ii,jj,kk+1]-theta[ii,jj,kk])/(z[ii,jj,kk+1]-z[ii,jj,kk])
			ENDFOR
		ENDFOR
	ENDFOR


      stab      = REFORM(dthdz,dim[0]*dim[1],dim[2]-2)
	  stab_ave  = [ ]
	  stab_ave1 = [ ]
	  ralt      = REFORM(ralt,dim[0]*dim[1],dim[2])
	  ralt_ave  = [ ]
	  ralt_ave1 = [ ]
 	  
	FOR k = 1, dim[2]-3 DO BEGIN
 		stab_ave  = MEAN(stab[*,k],/NAN)
		stab_ave1 = [stab_ave1, stab_ave]
	
	    ralt_ave  = MEAN(ralt[*,k],/NAN) 
	    PRINT, ralt_ave
        ralt_ave1 = [ralt_ave1, ralt_ave]
	ENDFOR
	
	map_plot = [map_plot, stab_ave1]
	ralt_arr = [ralt_arr, ralt_ave1]
ENDFOREACH ;date

map_plot = REFORM(map_plot,dim[2]-3,N_ELEMENTS(date_arr))
ralt_arr = REFORM(ralt_arr,dim[2]-3,N_ELEMENTS(date_arr))

dd = FLTARR(dim[2]-3,N_ELEMENTS(date_arr))
FOR ii = 0, N_ELEMENTS(date_arr)-1 DO BEGIN
	dd[*,ii] = ii
ENDFOR

map_bar_title1 = 'Layer-Mean Static Stability (K/km)'													;Set color bar title
map_bar_min    = 0.0																				;Set echo top minimum
map_bar_max    = 20.0																				;Set echo top maximum
map_bar_ticks  = 4																						;Set number of color bar ticks
map_table1     = [HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])]                                                  ;Set color table
map_levels1    = [FINDGEN(20)]

map_pos1 = [0.05, 0.20, 0.95, 0.95]																			;Set map position
bar_pos1 = [0.25, 0.09, 0.75, 0.11]																			;Set color bar position

CONTOUR, map_plot, dd, ralt_arr, /NODATA, $																	;Contour values
    FILL      = 1, $
    LEVELS    = map_levels1, $
    C_COLOR   = map_table1, $
    POSITION  = map_pos1

CONTOUR, map_plot, dd, ralt_arr, $																	;Contour values
       FILL      = 1, $
       OVERPLOT  = 1, $
       LEVELS    = map_levels1, $
       C_COLOR   = map_table1, $ 
       POSITION  = map_pos1


COLOR_BAR_24_KPB, map_table1[1:*], OVER = map_table1[-1], UNDER = map_table1[0], $						;Draw map color bar
	TICKS = map_bar_ticks, $
	RANGE = [0, 20], $
	TITLE = map_bar_title1, $
	POSIT = bar_pos1


END