PRO PBL_VERT_PROFILE, event, start_date, k_top, $
		AIR_POL    = air_pol, $
		scheme     = scheme, $
		DOMAIN 	   = domain, $
		REGION 	   = region, $
		LEV	   	   = lev, $
		VAR 	   = var, $
		TABLE  	   = table, $
		PLT    	   = plt

;+
; Name:
;		PBL_VERT_PROFILE
; Purpose:
;		This is a procedure to plot wind and ozone profiles in the lowest 2 km.
; Calling sequence:
;		PBL_VERT_PROFILE
; Input:
;		event    	: string of case date (e.g., '20120519')
;		start_date  : start time to plot, CDATE
;		dt			: time increment
; Output:
;		plots the vertical profiles of user selected variables at three times
; Keywords:
;		REGION	  : Region to average wind and ozone over.
;		EPS       : If set, output to PostScript.
;		PDF       : If set, output to PDF.
;		PNG       : If set, write PNG image.
; Author and history:
;		Daniel B. Phoenix	2016-04-08.
;							2016-08-03.	Added keyword option to use code written for
;										air pollution project (O3, WSP, NOx profiles for
;										six different times) and added section to compare
;										PBL schemes at three different times.
;							2017-03-01. NOTE: this code was edited to show temperature
;										profiles in UTLS for BMPs and was not changed back.
;-

COMPILE_OPT IDL2																			;Set compile options

ERASE 

IF (N_ELEMENTS(event     ) EQ 0) THEN event      = '20120519'
IF (N_ELEMENTS(domain	 ) EQ 0) THEN domain	 = 2
IF (N_ELEMENTS(region	 ) EQ 0) THEN region	 = [98,147,103,152]							;Default region 
IF (N_ELEMENTS(lev		 ) EQ 0) THEN lev		 = 20	
IF (N_ELEMENTS(k_top	 ) EQ 0) THEN k_top		 = 19

;region = [50, 50, 250, 190]

IF (KEYWORD_SET(air_pol)) THEN BEGIN															;Keyword to execute program designed for air pollution project
	marysville_d01 = [92,79,95,83]
	marysville_d02 = [76,135,77,136]

	;start_date = '20120519T1900Z'
	;end_date   = '20120520T1000Z'
	date   = MK_DATE_ARR(event, scheme, start_date, end_date, /DATE)											;Create array of CDATEs 
	ndate  = N_ELEMENTS(date)

	times     = INDGEN(ndate)

	nhours = 6
	ndates = N_ELEMENTS(date)
	date_int = 6

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
		nox += (WRF_READ_VAR('NO2' 	   , date[i], event, scheme, DOMAIN = domain, INDICES = region)).values * 1.0E6 
		u    = (WRF_READ_VAR('u' 	   , date[i], event, scheme, DOMAIN = domain, INDICES = region)).values
		v    = (WRF_READ_VAR('v'       , date[i], event, scheme, DOMAIN = domain, INDICES = region)).values
		
		dim = SIZE(u, /DIMENSION)

		hgt = REBIN (hgt, dim[0],dim[1],dim[2], /SAMPLE)

		model_lev = INDGEN(k_top+1)																		;For plotting model levels
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
					CHARSIZE  = 3, $
					POSITION  = wnd_pos
	
			PLOT, o3_mean, z_mean, $
					TITLE	  = 'Ozone Profile', $
					XTITLE    = 'Ozone Concentration (ppb)', $
					LINESTYLE = i, $
					PSYM	  = i, $
					NOERASE   = 1, $
					XRANGE    = [10, 80], $
					YRANGE    = [0, 2000], $
					CHARSIZE  = 3, $
					POSITION  = o3_pos

			PLOT, nox_mean, z_mean, $
					TITLE	  = 'NOx Profile', $
					XTITLE    = 'NOx Concentration (ppt)', $
					LINESTYLE = i, $
					PSYM	  = i, $
					NOERASE   = 1, $
					XRANGE    = [0, 2000], $
					YRANGE    = [0, 2000], $
					CHARSIZE  = 3, $
					POSITION  = nox_pos	
		ENDIF
	
	ENDFOR


	!P.MULTI = 0																										;Reset multiple plots
	!P.POSITION = 0
ENDIF

end_date = MAKE_DATE(2012,5,20,12)

dt = TIME_DIFF(end_date,start_date)
nt = dt/1800
nt = nt/3

;scheme   	= ['nssl_ysu','nssl_qnse','nssl_acm2']
scheme 		= ['morrison_ysu','nssl_ysu','milbyau_ysu']
color 	   = [COLOR_24_DP('RED'), COLOR_24_DP('BLUE'), COLOR_24_DP('gray50')]


!P.MULTI = [0, 3, 1]
t1_pos   = [0.100, 0.22, 0.350, 0.92]																		;Set map position
t2_pos   = [0.400, 0.22, 0.650, 0.92]
t3_pos   = [0.700, 0.22, 0.950, 0.92]
bar_pos  = [0.450, 0.05, 0.550, 0.15]

var_2d = [ ]
var_3d = FLTARR(k_top+1,3,3)
z_2d   = [ ]
z_3d   = FLTARR(k_top+1,3,3)

FOR s = 0, nt DO BEGIN
	FOR i = 0, 2 DO BEGIN
		x    = WRF_READ_VAR('Longitude', TIME_INC(start_date,s*1800), event, scheme[i], DOMAIN = domain, INDICES = region)				;Read variables
		y    = WRF_READ_VAR('Latitude' , TIME_INC(start_date,s*1800), event, scheme[i], DOMAIN = domain, INDICES = region)
		z    = (WRF_READ_VAR('Z'	   , TIME_INC(start_date,s*1800), event, scheme[i], DOMAIN = domain, INDICES = region)).values
		hgt  = (WRF_READ_VAR('Z0'	   , TIME_INC(start_date,s*1800), event, scheme[i], DOMAIN = domain, INDICES = region)).values
		pblh = (WRF_READ_VAR('PBL'	   , TIME_INC(start_date,s*1800), event, scheme[i], DOMAIN = domain, INDICES = region)).values
		u    = (WRF_READ_VAR('u' 	   , TIME_INC(start_date,s*1800), event, scheme[i], DOMAIN = domain, INDICES = region)).values
		v    = (WRF_READ_VAR('v'       , TIME_INC(start_date,s*1800), event, scheme[i], DOMAIN = domain, INDICES = region)).values
		dim  = SIZE(u, /DIMENSION)
		wind = SQRT(u^2 + v^2)																						;Horizontal wind


		CASE var OF
			"o3"  : BEGIN 
				plt_var  = (WRF_READ_VAR('O3'  	, TIME_INC(start_date,s*1800), event, scheme[i], DOMAIN = domain, INDICES = region)).values * 1.0E3
				xtitle   = 'Ozone Concentration (ppb)'
				xrange   = [45, 70]
			END

			"co"  : BEGIN 
				plt_var  = (WRF_READ_VAR('CO'  	, TIME_INC(start_date,s*1800), event, scheme[i], DOMAIN = domain, INDICES = region)).values * 1.0E3
				xtitle   = 'Carbon Monoxide Concentration (ppb)'
				xrange   = [100, 200]
			END


			"nox" : BEGIN
				plt_var  = (WRF_READ_VAR('NO' 	, TIME_INC(start_date,s*1800), event, scheme[i], DOMAIN = domain, INDICES = region)).values * 1.0E6
				plt_var += (WRF_READ_VAR('NO2' 	, TIME_INC(start_date,s*1800), event, scheme[i], DOMAIN = domain, INDICES = region)).values * 1.0E6 
				xtitle   = 'NOx Concentration (ppt)'
				xrange   = [0, 1000]
			END
	
			"wind" : BEGIN
				plt_var  = wind
				xtitle   = 'Wind Speed (m/s)'
				xrange   = [5, 20]
			END
		
			"theta" : BEGIN
				plt_var = (WRF_READ_VAR('T', TIME_INC(start_date,s*1800), event, scheme[i], DOMAIN = domain, INDICES = region)).values						;Read temperature variable from WRF output
		 		plt_var = ((1000.0/(WRF_READ_VAR('P', TIME_INC(start_date,s*1800), event, scheme[i], DOMAIN = domain, $
		 					INDICES = region)).values)^(!Rair/!Cp))*(plt_var)
				xtitle   = 'Potential Temperature (K)'
				xrange   = [304, 316]

			END
		
			"h2o"	: BEGIN
				plt_var  = (WRF_READ_VAR('H2O'  , TIME_INC(start_date,s*1800), event, scheme[i], DOMAIN = domain, INDICES = region)).values * 1.0E3
				xtitle   = 'Water Vapor (g kg-1)'
				xrange   = [0, 12]
			END

			"T"	: BEGIN
				plt_var  = (WRF_READ_VAR('T'  , TIME_INC(start_date,s*1800), event, scheme[i], DOMAIN = domain, INDICES = region)).values 
				xtitle   = 'Air Temperature (K)'
	;;			xrange   = [273, 310]
				xrante   = [MIN(var), MAX(var)]
			END

			"Td"	: BEGIN
				plt_var  = (WRF_READ_VAR('Td'  , TIME_INC(start_date,s*1800), event, scheme[i], DOMAIN = domain, INDICES = region)).values
				xtitle   = 'Dew Point Temperature (F)'
				xrange   = [70, 100]
			END
		ENDCASE

		hgt = REBIN (hgt, dim[0],dim[1],dim[2], /SAMPLE)

		model_lev = INDGEN(k_top+1)																	;For plotting model levels
		z = z - hgt																					;Height AGL

		var_mean = [ ]																				;Create arrays for averaging
		z_mean   = [ ]
		pbl_mean = [ ]
		
		FOR k = 0, k_top DO BEGIN 																		;Compute means
			var_mean = [var_mean, MEAN(plt_var[*,*,k])] 
			z_mean   = [z_mean  , MEAN(z  	  [*,*,k])]
		ENDFOR
		z_mean = z_mean * 1.0E-3
		pbl_mean = [pbl_mean, MEAN(pblh   [*,*])]

		IF (s EQ 0) THEN BEGIN
			IF (i EQ 0) THEN BEGIN
				var_mean1 = var_mean
				pbl1	  = pbl_mean
				z_mean1 = z_mean
			ENDIF
			IF (i EQ 1) THEN BEGIN 
				var_mean2 = var_mean
				pbl2	  = pbl_mean
				z_mean2 = z_mean
			ENDIF
			IF (i EQ 2) THEN BEGIN
				var_mean3 = var_mean
				pbl3 	  = pbl_mean
				z_mean3 = z_mean
			ENDIF
		ENDIF
		
		IF (s EQ 1) THEN BEGIN
			IF (i EQ 0) THEN BEGIN
				var_mean4 = var_mean
				pbl4      = pbl_mean
			ENDIF
			IF (i EQ 1) THEN BEGIN
				var_mean5 = var_mean
				pbl5      = pbl_mean
			ENDIF
			IF (i EQ 2) THEN BEGIN
				var_mean6 = var_mean
				pbl6      = pbl_mean
			ENDIF
			z_mean2 = z_mean
		ENDIF

		IF (s EQ 2) THEN BEGIN
			IF (i EQ 0) THEN BEGIN
				var_mean7 = var_mean
				pbl7	  = pbl_mean
			ENDIF
			IF (i EQ 1) THEN BEGIN 
				var_mean8 = var_mean
				pbl8	  = pbl_mean
			ENDIF
			IF (i EQ 2) THEN BEGIN
				var_mean9 = var_mean
				pbl9	  = pbl_mean
			ENDIF
			z_mean3 = z_mean
		ENDIF
		
;		PLOT, var_mean, z_mean, $
;				TITLE	  = title, $
;				XTITLE    = xtitle, $
;				LINESTYLE = i, $
;				PSYM	  = i, $
;				NOERASE   = 1, $
;				XRANGE    = xrange, $
;				YRANGE    = [z_mean[0],z_mean[k-1]], $
;				CHARSIZE  = 3, $
;				POSITION  = t1_pos
;
;		PLOT, var_2d[*,*,1], z_2d[*,*,1], $
;				TITLE	  = title, $
;				XTITLE    = xtitle, $
;				LINESTYLE = i, $
;				PSYM	  = i, $
;				NOERASE   = 1, $
;				XRANGE    = xrange, $
;				YRANGE    = [z_mean[0],z_mean[k-1]], $
;				CHARSIZE  = 3, $
;				POSITION  = t2_pos
;	
;		PLOT, var_2d[*,*,2], z_2d[*,*,2], $
;				TITLE	  = title, $
;				XTITLE    = xtitle, $
;				LINESTYLE = i, $
;				PSYM	  = i, $
;				NOERASE   = 1, $
;				XRANGE    = xrange, $
;				YRANGE    = [z_mean[0],z_mean[k-1]], $
;				CHARSIZE  = 3, $
;				POSITION  = t3_pos
	ENDFOR

	
;		POSITION  = [pos1[s],pos2[s],pos3[s],pos4[s]]

ENDFOR

p1=	PLOT( var_mean, z_mean, /NODATA, $
			TITLE	  = STRMID(MAKE_ISO_DATE_STRING(TIME_INC(start_date,0*1800)),11,8) + " UTC", $
			XTITLE    = xtitle, $
			YTITLE    = 'Altitude (km)', $
;			NOERASE   = 1, $
;			XRANGE    = xrange, $
			XRANGE	  = [215,235], $
			YRANGE    = [8, 16], $
;			CHARSIZE  = 3, $
;			POSITION  = t1_pos, $
			OVERPLOT  = 1)

	t2 = TEXT(555,445,  'MOR', FONT_SIZE=10, /DEVICE, FONT_COLOR = !COLOR.blue)
	t3 = TEXT(555,430,  'NSSL',FONT_SIZE=10, /DEVICE, FONT_COLOR = !COLOR.red)
	t4 = TEXT(555,415,  'MY',  FONT_SIZE=10, /DEVICE, FONT_COLOR = !COLOR.gray)

	XYOUTS, 110.0, 455.0, 'YSU  [---]', COLOR = COLOR_24('red'), /DEVICE, CHARSIZE = 1.5
	XYOUTS, 110.0, 430.0, 'QNSE [ . . ]', COLOR = COLOR_24('red'), /DEVICE, CHARSIZE = 1.5   
	XYOUTS, 110.0, 405.0, 'ACM2 [- -]', COLOR = COLOR_24('red'), /DEVICE, CHARSIZE = 1.5   

p2=	PLOT( var_mean1, z_mean1, COLOR = color[0], THICK = 3, /OVERPLOT)	
p3=	PLOT( var_mean2, z_mean1, COLOR = color[1], THICK = 3, /OVERPLOT)	
p4=	PLOT( var_mean3, z_mean1, COLOR = color[2], THICK = 3, /OVERPLOT)	
;	OPLOT, xrange, [pbl1, pbl1], LINESTYLE=0
;	OPLOT, xrange, [pbl2, pbl2], LINESTYLE=1
;	OPLOT, xrange, [pbl3, pbl3], LINESTYLE=2
	
STOP
p5=	PLOT( var_mean, z_mean, /NODATA, $
			TITLE	  = STRMID(MAKE_ISO_DATE_STRING(TIME_INC(start_date,2*1800)),11,8) + " UTC", $
			XTITLE    = xtitle, $
;			NOERASE   = 1, $
			XRANGE    = xrange, $
;			CHARSIZE  = 3, $
			LAYOUT = [3,1,2], $
			OVERPLOT  = 1)

p6=	PLOT( var_mean4, z_mean2, LINESTYLE=0, LAYOUT = [3,1,2]);, /OVERPLOT)
p7=	PLOT( var_mean5, z_mean2, LINESTYLE=1, LAYOUT = [3,1,2]);, /OVERPLOT)
p8=	PLOT( var_mean6, z_mean2, LINESTYLE=2, LAYOUT = [3,1,2]);, /OVERPLOT)
;	OPLOT, xrange, [pbl4, pbl4], LINESTYLE=0
;	OPLOT, xrange, [pbl5, pbl5], LINESTYLE=1
;	OPLOT, xrange, [pbl6, pbl6], LINESTYLE=2

p9=	PLOT( var_mean, z_mean, /NODATA, $
			TITLE	  = STRMID(MAKE_ISO_DATE_STRING(TIME_INC(start_date,4*1800)),11,8) + " UTC", $
			XTITLE    = xtitle, $
;			NOERASE   = 1, $
			XRANGE    = xrange, $
;			CHARSIZE  = 3, $
			LAYOUT = [3,1,3],$
			OVERPLOT  = 1)

p10=	PLOT( var_mean7, z_mean3, LINESTYLE=0, LAYOUT = [3,1,3]);, /OVERPLOT)
p11=	PLOT( var_mean8, z_mean3, LINESTYLE=1, LAYOUT = [3,1,3]);, /OVERPLOT)
p12=	PLOT( var_mean9, z_mean3, LINESTYLE=2, LAYOUT = [3,1,3]);, /OVERPLOT)
;	OPLOT, xrange, [pbl7, pbl7], LINESTYLE=0
;	OPLOT, xrange, [pbl8, pbl8], LINESTYLE=1
;	OPLOT, xrange, [pbl9, pbl9], LINESTYLE=2

!P.MULTI = 0																										;Reset multiple plots
!P.POSITION = 0

END