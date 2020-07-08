PRO READ_SEACRS_O3SONDE, location, $
	PNG=png, $
	EPS=eps

;+
; Name:
;		READ_SONDE
; Purpose:
;		This is a function to read sonde files. 
; Calling sequence:
;		READ_SONDE
; Inputs:
;		None.
; Output:
;		A structure of tropical cyclone track information for the Atlantic.
; Keywords:
;		None.
; Author and history:
;		Cameron R. Homeyer  2013-03-19.
;-

COMPILE_OPT IDL2																									;Set Compile Options

indir = !WRF_DIRECTORY + '20130805/sondes/' + location + '/'
infile = FILE_SEARCH(indir + '*.ict', COUNT = nfile)							;Set input filepath

;infile = indir + 'seac4rs-stlouis_SONDES_2013081418_R1_L1.ict'
;infile = indir + 'seac4rs-tallahassee_SONDES_2013081017_R1_L1.ict'
;infile = indir + 'seac4rs-huntsville_SONDES_2013080918_R1_L1.ict'
;infile = indir + 'seac4rs-boulder_SONDES_2013081219_R1_L1.ict'

color = ['black','red','blue','darkgreen','orange','cyan']

epsfile = indir + location + '.eps'	
pdffile = indir + location + '.pdf'	
pngfile = indir + location + '.png'	

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

FOR nf = 0, nfile-1 DO BEGIN ;N_ELEMENTS(infile)-1 DO BEGIN
	PRINT, infile[nf]
	
	nlines = FILE_LINES(infile[nf])
	data   = STRARR(nlines)

	OPENR, iunit, infile[nf], /GET_LUN
	READF, iunit, data
	FREE_LUN, iunit

	sonde_utc  = FLTARR(nlines-48)
	sonde_pres = FLTARR(nlines-48)
	sonde_alt  = FLTARR(nlines-48)
	sonde_temp = FLTARR(nlines-48)
	sonde_o3   = FLTARR(nlines-48)
	sonde_lon  = FLTARR(nlines-48)
	sonde_lat  = FLTARR(nlines-48)
	sonde_gpsz = FLTARR(nlines-48)

	FOR i = 48, nlines-1 DO BEGIN
		sonde_utc [i-48] = STRMID(data[i],0,8)
		sonde_pres[i-48] = STRMID(data[i],17,11)
		sonde_alt [i-48] = STRMID(data[i],29,11)
		sonde_temp[i-48] = STRMID(data[i],41,11)
		sonde_o3  [i-48] = STRMID(data[i],77,11)
		sonde_lon [i-48] = STRMID(data[i],149,11)
		sonde_lat [i-48] = STRMID(data[i],161,11)
		sonde_gpsz[i-48] = STRMID(data[i],173,11)
	ENDFOR

	inan = WHERE((sonde_temp LT  -90000.000), nan_count)
	sonde_temp[inan] = !Values.F_NaN
	sonde_o3  [inan] = !Values.F_NaN
	sonde_pres[inan] = !Values.F_NaN
	sonde_alt [inan] = !Values.F_NaN
	sonde_lon [inan] = !Values.F_NaN 
	sonde_lat [inan] = !Values.F_NaN 
	sonde_gpsz[inan] = !Values.F_NaN 

	sonde_ztrop  = TROPOPAUSE(sonde_temp, sonde_gpsz, sonde_pres)
	sonde_iztrop = INDEX_OF_NEAREST(sonde_ztrop, sonde_gpsz) 

	sonde_trop_o3 = sonde_o3[sonde_iztrop]*1.0E3
	sonde_trop_lat = sonde_lat[sonde_iztrop]
	sonde_trop_lon = sonde_lon[sonde_iztrop]

	wrf_lon  = (WRF_READ_VAR('Longitude',MAKE_DATE(2013,8,12,19), '20130805', 'nest_final', DOMAIN = 1)).values			;Read variables
	wrf_lat  = (WRF_READ_VAR('Latitude', MAKE_DATE(2013,8,12,19), '20130805', 'nest_final', DOMAIN = 1)).values
	wrf_alt  = (WRF_READ_VAR('Z'       , MAKE_DATE(2013,8,12,19), '20130805', 'nest_final', DOMAIN = 1)).values
	wrf_o3   = (WRF_READ_VAR('O3'	   , MAKE_DATE(2013,8,12,19), '20130805', 'nest_final', DOMAIN = 1)).values
	wrf_trop = (WRF_READ_VAR('Z_trop'  , MAKE_DATE(2013,8,12,19), '20130805', 'nest_final', DOMAIN = 1)).values

	wrf_trop_lon = INDEX_OF_NEAREST(sonde_trop_lon, wrf_lon)
	wrf_trop_lat = INDEX_OF_NEAREST(sonde_trop_lat, wrf_lat)

	iwrf_latlon_trop = WHERE(MIN((SQRT((sonde_trop_lon - wrf_lon)^2 + (sonde_trop_lat - wrf_lat)^2))) EQ $
						(SQRT((sonde_trop_lon - wrf_lon)^2 + (sonde_trop_lat - wrf_lat)^2)))

	wrf_trop_lon = wrf_lon[iwrf_latlon_trop]
	wrf_trop_lat = wrf_lat[iwrf_latlon_trop]

	wrf_ztrop = wrf_trop[iwrf_latlon_trop]

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	sonde_gpsz = sonde_gpsz*1.0E3

	dim 	   = SIZE(wrf_alt, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
	wrf_lon_3d = REBIN(wrf_lon, dim[0], dim[1], dim[2], /SAMPLE)
	wrf_lat_3d = REBIN(wrf_lat, dim[0], dim[1], dim[2], /SAMPLE)

	wrf_o3_sondepath = FLTARR(nlines-47)
	sonde_loc = FLTARR(nlines-47)

	FOR tt = 0, sonde_iztrop+1000, 50 DO BEGIN
		sonde_loc[tt] = WHERE(MIN((SQRT((sonde_lon[tt] - wrf_lon_3d)^2 + (sonde_lat[tt] - wrf_lat_3d)^2 + (sonde_gpsz[tt] - wrf_alt)^2 ))) EQ $
						(SQRT((sonde_lon[tt] - wrf_lon_3d)^2 + (sonde_lat[tt] - wrf_lat_3d)^2 + (sonde_gpsz[tt] - wrf_alt)^2)))
		wrf_o3_sondepath [tt] = wrf_o3[sonde_loc[tt]]*1.0E3
		PRINT, tt, wrf_o3_sondepath[tt], sonde_o3[tt]*1.0E3
	ENDFOR

	ralt = (sonde_gpsz[0:nlines-49:50]*1.0E-3) - sonde_ztrop
	wrf_ralt_trop = (wrf_ztrop*1.0E-3) - sonde_ztrop

	PRINT, sonde_ztrop, wrf_ztrop*1.0E-3

	IF (nf EQ 0) THEN PLOT , (wrf_o3_sondepath[0:nlines-49:50])-sonde_o3[0:nlines-49:50]*1.0E3, ralt, THICK = 3, TITLE = location, XRANGE = [-300,100], COLOR = COLOR_24(color[nf]);, POSITION = position1	
	IF (nf GE 1) THEN OPLOT, (wrf_o3_sondepath[0:nlines-49:50])-sonde_o3[0:nlines-49:50]*1.0E3, ralt, THICK = 3, COLOR = COLOR_24(color[nf])

	OPLOT, [-300,100],[wrf_ralt_trop,wrf_ralt_trop], THICK = 3, LINESTYLE=2, COLOR=COLOR_24(color[nf])

	;plot, wrf_o3_sondepath[0:nlines-49:50],sonde_gpsz[0:nlines-49:50],thick=3, title = 'boulder_SONDES_2013081219'
	;oplot,sonde_o3[0:nlines-49:50]*1.0E3,sonde_gpsz[0:nlines-49:50]  ,thick=3,color=color_24('red')
	;oplot, [0,5000],[sonde_ztrop*1.0E3,sonde_ztrop*1.0E3], color=color_24('red') 
	;oplot, [0,5000],[wrf_ztrop,wrf_ztrop], linestyle=2

ENDFOR

OPLOT, [-300,100],[0.0  ,0.0]
OPLOT, [ 0.0,0.0],[-20.0,5.0]

;plot, wrf_o3_sondepath[0:nlines-49:50],sonde_gpsz[0:nlines-49:50],thick=3, title = 'boulder_SONDES_2013081219'
;oplot,sonde_o3[0:nlines-49:50]*1.0E3,sonde_gpsz[0:nlines-49:50]  ,thick=3,color=color_24('red')
;oplot, [0,5000],[sonde_ztrop*1.0E3,sonde_ztrop*1.0E3], color=color_24('red') 
;oplot, [0,5000],[wrf_ztrop,wrf_ztrop], linestyle=2

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
