PRO PLOT_NOAA_SONDE_EVAL, $
	PNG=png, $
	EPS=eps

;+
; Name:
;		PLOT_NOAA_SONDE_EVAL
; Purpose:
;		This is a function to read wmo ozonesonde files. 
; Calling sequence:
;		PLOT_NOAA_SONDE_EVAL
; Inputs:
;		None.
; Output:
;		Vertical profile of difference in difference between WRF and sonde
;		in LRT height
; Keywords:
;		None.
; Author and history:
;		Cameron R. Homeyer  2013-03-19.
;		Daniel B. Phoenix	2020-01-15.  Expanded PLOT_NOAA_SONDE to do multiple times
;										 in same plot.
;-

COMPILE_OPT IDL2																									;Set Compile Options


; Change Station Name and lat/lon (line 102-103)
indir = !WRF_DIRECTORY + '20110518/radiosondes/KGSO/' 

color = ['black','red','blue','darkgreen','orange','cyan']

IF KEYWORD_SET(eps) THEN BEGIN	
	PS_ON, FILENAME = epsfile, PAGE_SIZE = [4.0, 4.0], MARGIN = 0.0, /INCHES					;Switch to Postscript device
	DEVICE, /ENCAPSULATED
	!P.FONT     = 0																				;Hardware fonts
	!P.CHARSIZE = 0.8
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS																		;Load basic color definitions
ENDIF ELSE BEGIN
	SET_PLOT, 'X'
	WINDOW, XSIZE = 800, YSIZE = 800															;Open graphics window
	!P.COLOR      = COLOR_24('black')															;Foreground color
	!P.BACKGROUND = COLOR_24('white')															;Background color
	!P.CHARSIZE   = 2.9		
	!P.FONT       = -1																			;Use Hershey fonts
ENDELSE

nf=0
FOR nf = 0, 4 DO BEGIN
	IF (nf EQ 0) THEN BEGIN
		infile = indir+'20110519T00Z.txt'				

		wrf_lon  = (WRF_READ_VAR('Longitude',MAKE_DATE(2011,5,19,00), '20110518', 'seasonal_final/bigger_domain', DOMAIN = 1)).values			;Read variables
		wrf_lat  = (WRF_READ_VAR('Latitude', MAKE_DATE(2011,5,19,00), '20110518', 'seasonal_final/bigger_domain', DOMAIN = 1)).values
		wrf_alt  = (WRF_READ_VAR('Z'       , MAKE_DATE(2011,5,19,00), '20110518', 'seasonal_final/bigger_domain', DOMAIN = 1)).values
		;wrf_o3   = (WRF_READ_VAR('O3'	   , MAKE_DATE(2011,5,19,00), '20110518', 'seasonal_final/bigger_domain', DOMAIN = 1)).values
		wrf_trop = (WRF_READ_VAR('Z_trop'  , MAKE_DATE(2011,5,19,00), '20110518', 'seasonal_final/bigger_domain', DOMAIN = 1)).values
	ENDIF

	IF (nf EQ 1) THEN BEGIN
		infile = indir+'20110520T00Z.txt'				

		wrf_lon  = (WRF_READ_VAR('Longitude',MAKE_DATE(2011,5,20,00), '20110518', 'seasonal_final/bigger_domain', DOMAIN = 1)).values			;Read variables
		wrf_lat  = (WRF_READ_VAR('Latitude', MAKE_DATE(2011,5,20,00), '20110518', 'seasonal_final/bigger_domain', DOMAIN = 1)).values
		wrf_alt  = (WRF_READ_VAR('Z'       , MAKE_DATE(2011,5,20,00), '20110518', 'seasonal_final/bigger_domain', DOMAIN = 1)).values
		;wrf_o3   = (WRF_READ_VAR('O3'	   , MAKE_DATE(2011,5,20,00), '20110518', 'seasonal_final/bigger_domain', DOMAIN = 1)).values
		wrf_trop = (WRF_READ_VAR('Z_trop'  , MAKE_DATE(2011,5,20,00), '20110518', 'seasonal_final/bigger_domain', DOMAIN = 1)).values
	ENDIF

	IF (nf EQ 2) THEN BEGIN
		infile = indir+'20110522T00Z.txt'				

		wrf_lon  = (WRF_READ_VAR('Longitude',MAKE_DATE(2011,5,22,00), '20110518', 'seasonal_final/bigger_domain', DOMAIN = 1)).values			;Read variables
		wrf_lat  = (WRF_READ_VAR('Latitude', MAKE_DATE(2011,5,22,00), '20110518', 'seasonal_final/bigger_domain', DOMAIN = 1)).values
		wrf_alt  = (WRF_READ_VAR('Z'       , MAKE_DATE(2011,5,22,00), '20110518', 'seasonal_final/bigger_domain', DOMAIN = 1)).values
		;wrf_o3   = (WRF_READ_VAR('O3'	   , MAKE_DATE(2011,5,22,00), '20110518', 'seasonal_final/bigger_domain', DOMAIN = 1)).values
		wrf_trop = (WRF_READ_VAR('Z_trop'  , MAKE_DATE(2011,5,22,00), '20110518', 'seasonal_final/bigger_domain', DOMAIN = 1)).values
	ENDIF

	IF (nf EQ 3) THEN BEGIN
		infile = indir+'20110524T00Z.txt'				

		wrf_lon  = (WRF_READ_VAR('Longitude',MAKE_DATE(2011,5,24,00), '20110518', 'seasonal_final/bigger_domain', DOMAIN = 1)).values			;Read variables
		wrf_lat  = (WRF_READ_VAR('Latitude', MAKE_DATE(2011,5,24,00), '20110518', 'seasonal_final/bigger_domain', DOMAIN = 1)).values
		wrf_alt  = (WRF_READ_VAR('Z'       , MAKE_DATE(2011,5,24,00), '20110518', 'seasonal_final/bigger_domain', DOMAIN = 1)).values
		;wrf_o3   = (WRF_READ_VAR('O3'	   , MAKE_DATE(2011,5,24,00), '20110518', 'seasonal_final/bigger_domain', DOMAIN = 1)).values
		wrf_trop = (WRF_READ_VAR('Z_trop'  , MAKE_DATE(2011,5,24,00), '20110518', 'seasonal_final/bigger_domain', DOMAIN = 1)).values
	ENDIF

	IF (nf EQ 4) THEN BEGIN
		infile = indir+'20110526T00Z.txt'				

		wrf_lon  = (WRF_READ_VAR('Longitude',MAKE_DATE(2011,5,26,00), '20110518', 'seasonal_final/bigger_domain', DOMAIN = 1)).values			;Read variables
		wrf_lat  = (WRF_READ_VAR('Latitude', MAKE_DATE(2011,5,26,00), '20110518', 'seasonal_final/bigger_domain', DOMAIN = 1)).values
		wrf_alt  = (WRF_READ_VAR('Z'       , MAKE_DATE(2011,5,26,00), '20110518', 'seasonal_final/bigger_domain', DOMAIN = 1)).values
		;wrf_o3   = (WRF_READ_VAR('O3'	   , MAKE_DATE(2011,5,26,00), '20110518', 'seasonal_final/bigger_domain', DOMAIN = 1)).values
		wrf_trop = (WRF_READ_VAR('Z_trop'  , MAKE_DATE(2011,5,26,00), '20110518', 'seasonal_final/bigger_domain', DOMAIN = 1)).values
	ENDIF

		
	sonde_lon = -79.95
	sonde_lat = 36.08

	epsfile = indir + '.eps'	
	pdffile = indir + '.pdf'	
	pngfile = indir + '.png'	
	
	IF (nf EQ 0) THEN sTemplate = ASCII_TEMPLATE(infile)
	sonde = READ_ASCII(infile, TEMPLATE = sTemplate)

	sonde_ztrop  = TROPOPAUSE(sonde.temp, sonde.alt*1.0E-3, sonde.press)

	wrf_trop_lon = INDEX_OF_NEAREST(sonde_lon, wrf_lon)
	wrf_trop_lat = INDEX_OF_NEAREST(sonde_lat, wrf_lat)

	iwrf_latlon_trop = WHERE(MIN((SQRT((sonde_lon - wrf_lon)^2 + (sonde_lat - wrf_lat)^2))) EQ $
						(SQRT((sonde_lon - wrf_lon)^2 + (sonde_lat - wrf_lat)^2)))

	wrf_trop_lon = wrf_lon[iwrf_latlon_trop]
	wrf_trop_lat = wrf_lat[iwrf_latlon_trop]

	wrf_ztrop = wrf_trop[iwrf_latlon_trop]

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	dim 	   = SIZE(wrf_alt, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
	wrf_lon_3d = REBIN(wrf_lon, dim[0], dim[1], dim[2], /SAMPLE)
	wrf_lat_3d = REBIN(wrf_lat, dim[0], dim[1], dim[2], /SAMPLE)

	ralt = (sonde.alt*1.0E-3) - sonde_ztrop
	wrf_ralt_trop = (wrf_ztrop*1.0E-3) - sonde_ztrop

	PRINT, sonde_ztrop, wrf_ztrop*1.0E-3

;	nlines = SIZE(wrf_o3_sondepath,/DIMENSIONS)

;	IF (nf EQ 0) THEN PLOT , (wrf_o3_sondepath[0:nlines-49:50]-sonde_o3[0:nlines-49:50]), ralt[0:nlines-49:50], THICK = 3, TITLE = location, XRANGE = [-300,200], COLOR = COLOR_24(color[0])
;	IF (nf GE 1) THEN OPLOT, (wrf_o3_sondepath[0:nlines-49:50]-sonde_o3[0:nlines-49:50]), ralt[0:nlines-49:50], THICK = 3, COLOR = COLOR_24(color[1])

	IF (nf EQ 0) THEN PLOT, [-300,200],[wrf_ralt_trop,wrf_ralt_trop], THICK = 3, LINESTYLE=2, COLOR=COLOR_24(color[nf]), YRANGE = [-2,2], YTITLE = 'Relative Altitude (km)'
	IF (nf GE 1) THEN OPLOT, [-300,200],[wrf_ralt_trop,wrf_ralt_trop], THICK = 3, LINESTYLE=2, COLOR=COLOR_24(color[nf])
	PRINT, wrf_ralt_trop
ENDFOR


OPLOT, [-300,200],[0.0  ,0.0]
;OPLOT, [ 0.0,0.0],[-20.0,5.0]

IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN $
		PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS													;Convert eps to pdf
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file

STOP
END
