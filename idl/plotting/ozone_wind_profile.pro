PRO OZONE_WIND_PROFILE, event, scheme, start_date, end_date, $
		DOMAIN 	   = domain, $
		REGION 	   = region, $
		LEV	   	   = lev, $
		TIMESERIES = timeseries, $
		TABLE  	   = table, $
		PLT    	   = plt

;+
; Name:
;		OZONE_WIND_PROFILE
; Purpose:
;		This is a procedure to plot wind and ozone profiles in the lowest 2 km.
; Calling sequence:
;		OZONE_WIND_PROFILE
; Input:
;		date     : analysis date (e.g., MAKE_DATE(2012,5,19,22,00))
;		event    : string of case date (e.g., '20120519')
;		scheme   : microphysics scheme (e.g., 'morrison')
; Output:
;		plots the vertical profile of ozone and wind
; Keywords:
;		REGION	  : Region to average wind and ozone over.
;		EPS       : If set, output to PostScript.
;		PDF       : If set, output to PDF.
;		PNG       : If set, write PNG image.
; Author and history:
;		Daniel B. Phoenix	2016-04-08.
;-

COMPILE_OPT IDL2																			;Set compile options

IF (N_ELEMENTS(event     ) EQ 0) THEN event      = '20120519'
IF (N_ELEMENTS(scheme	 ) EQ 0) THEN scheme	 = 'morrison'
IF (N_ELEMENTS(domain	 ) EQ 0) THEN domain	 = 2
;IF (N_ELEMENTS(region	 ) EQ 0) THEN region	 = [74,129,84,139]							;Default region is grid over Marysville, KS
IF (N_ELEMENTS(lev		 ) EQ 0) THEN lev		 = 20	

marysville_d01 = [92,79,95,83]
marysville_d02 = [76,135,77,136]

;start_date = '20150630T1900Z'
;end_date   = '20150701T1000Z'
date   = MK_DATE_ARR(event, scheme, start_date, end_date, /DATE)											;Create array of CDATEs 
ndate  = N_ELEMENTS(date)

wind_time = [ ]
o3_time   = [ ]
nox_time  = [ ]
z_time    = [ ]
times     = INDGEN(ndate)



IF KEYWORD_SET(timeseries) THEN BEGIN
	FOR i = 0, ndate- 1 DO BEGIN
		x    = (WRF_READ_VAR('Longitude', date[i], event, scheme, DOMAIN = domain, INDICES = region)).values				;Read variables
		y    = (WRF_READ_VAR('Latitude' , date[i], event, scheme, DOMAIN = domain, INDICES = region)).values
		z    = (WRF_READ_VAR('Z'	    , date[i], event, scheme, DOMAIN = domain, INDICES = region)).values
;		z    = [[[z_time]], [[z]]]
		hgt  = (WRF_READ_VAR('Z0'	    , date[i], event, scheme, DOMAIN = domain, INDICES = region)).values
		o    = (WRF_READ_VAR('O3'  	    , date[i], event, scheme, DOMAIN = domain, INDICES = region)).values * 1.0E3
		nox  = (WRF_READ_VAR('NO' 	    , date[i], event, scheme, DOMAIN = domain, INDICES = region)).values * 1.0E6
;		nox += (WRF_READ_VAR('NO2' 	    , date[i], event, scheme, DOMAIN = domain, INDICES = region)).values * 1.0E6 
		u    = (WRF_READ_VAR('u' 	    , date[i], event, scheme, DOMAIN = domain, INDICES = region)).values
		v    = (WRF_READ_VAR('v'        , date[i], event, scheme, DOMAIN = domain, INDICES = region)).values

		dim = SIZE(u, /DIMENSION)

		hgt = REBIN (hgt, dim[0],dim[1],dim[2], /SAMPLE)

		model_lev = INDGEN(20)																		;For plotting model levels
		wind 	  = SQRT(u^2 + v^2)																	;Horizontal wind
		z = z - hgt																					;Height AGL

		wind_mean = [ ]																				;Create arrays for averaging
		o3_mean   = [ ]
		nox_mean  = [ ]
		z_mean    = [ ]
	
		FOR k = 0, 19 DO BEGIN 																		;Compute means
			wind_mean = [wind_mean, MEAN(wind[*,*,k])] 
			o3_mean   = [o3_mean  , MEAN(o   [*,*,k])] 
			nox_mean  = [nox_mean , MEAN(nox [*,*,k])]
			z_mean    = [z_mean   , MEAN(z [*,*,k])]
		ENDFOR

		PLOT, o3_mean, times, $
			NOERASE = 1

;		wind_time = [wind_time, wind_mean]
;		o3_time   = [o3_time  , o3_mean  ]
;		nox_time  = [nox_time , nox_mean ]
;		z_time    = [z_time   , z_mean   ]

	ENDFOR

;	wind_time = REBIN(wind_time, 20, ndate-1, /SAMPLE)
;	o3_time   = REBIN(o3_time  , 20, ndate-1, /SAMPLE)
;	nox_time  = REBIN(nox_time , 20, ndate-1, /SAMPLE)
;	z_time    = REBIN(z_time   , 20, ndate-1, /SAMPLE)
;	x_3d = REBIN(x_3d, 11, 11, ndate-1, /SAMPLE)
;	HELP, x_3d
ENDIF



;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;; 
nhours = 6
ndates = N_ELEMENTS(date)
date_int = 12

date = [date[0], date[date_int], date[2*date_int], date[3*date_int], date[4*date_int], $
		date[5*date_int]]


!P.MULTI = [0, 3, 1]
wnd_pos  = [0.100, 0.22, 0.350, 0.92]																		;Set map position
o3_pos   = [0.400, 0.22, 0.650, 0.92]
nox_pos  = [0.700, 0.22, 0.950, 0.92]
bar_pos  = [0.450, 0.05, 0.550, 0.15]

FOR i = 0, 5 DO BEGIN
	
	x    = WRF_READ_VAR('Longitude', date[i], event, scheme, DOMAIN = domain, INDICES = region)				;Read variables
	y    = WRF_READ_VAR('Latitude' , date[i], event, scheme, DOMAIN = domain, INDICES = region)
	z    = (WRF_READ_VAR('Z'	   , date[i], event, scheme, DOMAIN = domain, INDICES = region)).values
	hgt  = (WRF_READ_VAR('Z0'	   , date[i], event, scheme, DOMAIN = domain, INDICES = region)).values
	o    = (WRF_READ_VAR('O3'  	   , date[i], event, scheme, DOMAIN = domain, INDICES = region)).values * 1.0E3
	nox  = (WRF_READ_VAR('NO' 	   , date[i], event, scheme, DOMAIN = domain, INDICES = region)).values * 1.0E6
;	nox += (WRF_READ_VAR('NO2' 	   , date[i], event, scheme, DOMAIN = domain, INDICES = region)).values * 1.0E6 
	u    = (WRF_READ_VAR('u' 	   , date[i], event, scheme, DOMAIN = domain, INDICES = region)).values
	v    = (WRF_READ_VAR('v'       , date[i], event, scheme, DOMAIN = domain, INDICES = region)).values

	dim = SIZE(u, /DIMENSION)

	hgt = REBIN (hgt, dim[0],dim[1],dim[2], /SAMPLE)

	model_lev = INDGEN(20)																		;For plotting model levels
	wind 	  = SQRT(u^2 + v^2)																	;Horizontal wind
	z = z - hgt																					;Height AGL

	wind_mean = [ ]																				;Create arrays for averaging
	o3_mean   = [ ]
	nox_mean  = [ ]
	z_mean    = [ ]
	
	FOR k = 0, 19 DO BEGIN 																		;Compute means
		wind_mean = [wind_mean, MEAN(wind[*,*,k])] 
		o3_mean   = [o3_mean  , MEAN(o   [*,*,k])] 
		nox_mean  = [nox_mean , MEAN(nox [*,*,k])]
		z_mean    = [z_mean   , MEAN(z [*,*,k])]
	ENDFOR


	IF KEYWORD_SET(table) THEN BEGIN															;Print profiles in table format
		PRINT, FORMAT = '("Height (m, AGL)", 5X, "Wind Speed (m/s)", 5X, "Ozone (ppb)", 8X, "NOx (ppt)")'
		PRINT, '====================================================================='

		FOR k = 0, 19 DO BEGIN 
			wind_mean = [wind_mean, MEAN(wind[*,*,k])] 
			o3_mean   = [o3_mean  , MEAN(o[*,*,k])] 
			nox_mean  = [nox_mean , MEAN(nox [*,*,k])]
			z_mean    = [z_mean   , MEAN(z [*,*,k])]
			PRINT, FORMAT = '(F10.2, 14X, F10.2, 9X, F10.2, 9X, F10.2)', MEAN(z [*,*,k]), $
							MEAN(wind[*,*,k]), MEAN(o[*,*,k]), MEAN(nox [*,*,k])
		ENDFOR
	ENDIF

	IF KEYWORD_SET(plt) THEN BEGIN
		PLOT, wind_mean, z_mean, $
				TITLE	  = 'Wind Profile', $
				XTITLE    = 'Wind Speed (m/s)', $
				YTITLE    = 'HGT AGL (m)', $
				LINESTYLE = i, $
				PSYM	  = i, $
				NOERASE   = 1, $
				XRANGE    = [2, 17] , $
				YRANGE    = [0, 2000], $
				FONT	  = -1, $
				POSITION  = wnd_pos
	
		PLOT, o3_mean, z_mean, $
				TITLE	  = 'Ozone Profile', $
				XTITLE    = 'Ozone Concentration (ppb)', $
				LINESTYLE = i, $
				PSYM	  = i, $
				NOERASE   = 1, $
				XRANGE    = [10, 80], $
				YRANGE    = [0, 2000], $
				FONT	  = -1, $
				POSITION  = o3_pos

		PLOT, nox_mean, z_mean, $
				TITLE	  = 'NOx Profile', $
				XTITLE    = 'NOx Concentration (ppt)', $
				LINESTYLE = i, $
				PSYM	  = i, $
				NOERASE   = 1, $
				XRANGE    = [0, 200], $
				YRANGE    = [0, 2000], $
				FONT	  = -1, $
				POSITION  = nox_pos	
	ENDIF
	
ENDFOR


!P.MULTI = 0																										;Reset multiple plots
!P.POSITION = 0



END