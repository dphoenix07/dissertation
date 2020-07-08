PRO ANVIL_STATS, event, date, $
		MELT_LVL = melt_lvl, $
		PNG	     = png, $
		EPS      = eps

;+
; Name:
;		ANVIL_STATS
; Purpose:
;		This is a procedure to calculate the top and bottom altitude of the anvil 
;		as well as the mean and median altitude. 
; Calling sequence:
;		ANVIL_STATS, event, scheme, date
; Input:
;		event  : String variable of the date (e.g., '20120519')
;		scheme : String variable of the scheme (e.g., 'morrison')
;		date   : Analysis date {CDATE}.
; Output:
;		A table of the top, bottom, mean and median altitude of the anvil
; Keywords:
;		PNG : If set, write PNG image.
;		EPS : If set, output to PostScript.
; Author and history:
;		Daniel B. Phoenix	2016-03-02.
;-

COMPILE_OPT IDL2																				;Set compile options

IF (N_ELEMENTS (date	) EQ 0) THEN date 	  =  MAKE_DATE(2012,5,19,22,00)
IF (N_ELEMENTS (event	) EQ 0) THEN event    = '20120519'
IF (N_ELEMENTS (melt_lvl) EQ 0) THEN melt_lvl = 7.0

IF (event EQ '20120519') THEN region = [50, 50, 250, 190]

IF KEYWORD_SET(eps) THEN BEGIN	
	PS_ON, FILENAME = epsfile, PAGE_SIZE = [6.0, 4.0], MARGIN = 0.0, /INCHES					;Switch to Postscript device
	DEVICE, /ENCAPSULATED
	!P.FONT     = 0																				;Hardware fonts
	!P.CHARSIZE = 0.8
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS																		;Load basic color definitions
ENDIF ELSE BEGIN
	SET_PLOT, 'X'
	WINDOW, XSIZE = 1200, YSIZE = 800															;Open graphics window
	!P.COLOR      = COLOR_24('black')															;Foreground color
	!P.BACKGROUND = COLOR_24('white')															;Background color
	!P.CHARSIZE   = 3.1		
	!P.FONT       = -1																			;Use Hershey fonts
ENDELSE
!P.MULTI = [0,3,2]

schemes  = ['morrison', 'milbyau', 'nssl']

FOREACH scheme, schemes DO BEGIN 
	u_wind = (WRF_READ_VAR('u'				, date, event, scheme, DOMAIN = 2, INDICES = region)).values 
	v_wind = (WRF_READ_VAR('v'				, date, event, scheme, DOMAIN = 2, INDICES = region)).values 
	w_wind = (WRF_READ_VAR('w'				, date, event, scheme, DOMAIN = 2, INDICES = region)).values 
	cloud  = (WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, DOMAIN = 2, INDICES = region)).values 
	Z	   = (WRF_READ_VAR('Z'			    , date, event, scheme, DOMAIN = 2, INDICES = region)).values
	cloud  = cloud * 1.0E9
	wind   = SQRT(u_wind^2 + v_wind^2 + w_wind^2)


	dim = SIZE(Z, /DIMENSIONS)
	
	icloud = ((cloud GE 0.1)*Z*0.001)
	izero  = WHERE(icloud EQ 0, count)
	IF (count GT 0) THEN BEGIN
		icloud [izero] = !Values.F_NaN	
		wind   [izero] = !Values.F_NaN
	ENDIF
	
	top	   = MAX(icloud, DIM = 3, /NAN)
	bot    = MIN(icloud, DIM = 3, /NAN)
	wind_t = WHERE(icloud EQ top, t_count, COMPLEMENT = t_zero)
	wind_b = WHERE(icloud EQ bot, b_count, COMPLEMENT = b_zero)
	IF ((t_count NE 0) AND (b_count NE 0)) THEN BEGIN
		wind [wind_t] = wind [wind_t]
		wind [wind_b] = wind [wind_b]
	ENDIF ELSE BEGIN
		wind [t_zero] = 0.0
		wind [b_zero] = 0.0
	ENDELSE
	
	imelt  = WHERE(bot LT melt_lvl, count)
	IF (count GT 0) THEN BEGIN
		top  [imelt] = !Values.F_NaN
		bot  [imelt] = !Values.F_NaN
		wind [imelt] = !Values.F_NaN
	ENDIF
			
	hist = HISTOGRAM(wind, MIN = 1.0, BINSIZE = 1, NBINS = 100)								;Calculate density
	cloud = HISTOGRAM(cloud, MIN = 0.1, BINSIZE = 0.01, NBINS = 500)
	
	PLOT, ((top - bot) + bot), $														
		TITLE  = 'Anvil Cloud Altitude: ' + scheme, $
		XTITLE = 'Data Points', $
		YRANGE = [0, 20], $
		YTITLE = 'Altitude (km)'

	IF (scheme EQ 'morrison') THEN BEGIN 
		mo_cld  = cloud
		mo_wind = hist
		mo_mean = MEAN(top - bot, /NAN)
		mo_max  = MAX(top, /NAN)
		mo_min  = MIN(bot, /NAN)
	ENDIF ELSE IF (scheme EQ 'milbyau') THEN BEGIN
	    my_cld  = cloud
		my_wind = hist
		my_mean = MEAN(top - bot, /NAN)
		my_max  = MAX(top, /NAN)
		my_min  = MIN(bot, /NAN)
	ENDIF ELSE IF (scheme EQ 'nssl') THEN BEGIN
	    ns_cld  = cloud
		ns_wind = hist
		ns_mean = MEAN(top - bot, /NAN)
		ns_max  = MAX(top, /NAN)
		ns_min  = MIN(bot, /NAN)
	ENDIF
ENDFOREACH

PLOT, mo_wind, $														
	TITLE  = 'Anvil Cloud Wind Speed (m/s)', $
	XTITLE = 'Wind Speed', $
	XRANGE = [5, 80], $
	YRANGE = [0, 5.0E4], $
	YTITLE = 'Count'
OPLOT, my_wind, LINESTYLE = 1
OPLOT, ns_wind, LINESTYLE = 5

PLOT, mo_wind - ns_wind, TITLE = 'Horizontal Wind Speed Difference (MO - NSSL)'
PLOT, mo_wind - my_wind, TITLE = 'Horizontal Wind Speed Difference (MO - MY)'

;PLOT,  mo_cld, TITLE = 'Morrison';, YRANGE = [0, 6000]
;OPLOT, my_cld, LINESTYLE = 1
;OPLOT, ns_cld, LINESTYLE = 4


PRINT, FORMAT = '("MP Scheme", 5X, "Mean Cloud (km)", 5X, "Max Cloud (km)", 5X, "Min Cloud (km)")'
PRINT, '====================================================================='
PRINT, FORMAT = '(A8, 6X, F10.2, 9X, F10.2, 9X, F10.2)', 'morrison', mo_mean, mo_max, mo_min
PRINT, FORMAT = '(A8, 6X, F10.2, 9X, F10.2, 9X, F10.2)', 'milbyau' , my_mean, my_max, my_min
PRINT, FORMAT = '(A8, 6X, F10.2, 9X, F10.2, 9X, F10.2)', 'nssl'    , ns_mean, ns_max, ns_min



END