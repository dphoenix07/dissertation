PRO PLOT_TMATRIX_NEXRAD_ALTS, event, date, date1, $
	PBL = pbl, $
	EPS = eps, $
	PNG = png

;+
; Name:
;		PLOT_TMATRIX_NEXRAD_ALTS
; Purpose:
;		This is a procedure to plot histograms of storm altitudes from NEXRAD
;		obeservations and WRF resolution testing simulations. 
; Calling sequence:
;		PLOT_TMATRIX_NEXRAD_ALTS, date
; Input:
;		date  : Analysis date {CDATE}.
; Output:
;		A histogram plot of storm altitude frequencies for NEXRAD observations,
;		WRF reflectivity, and WRF cloud top.
; Keywords:
;		EPS  : If set, output to PostScript.
;		PNG  : If set, write PNG image.
; Author and history:
;		Cameron R. Homeyer  2014-08-04.
;		Daniel B. Phoenix	2016-06-04.		Edited original to read in tmatrix files.
;-

COMPILE_OPT IDL2																															;Set compile options

dbz_thresh = 10.0

z0   = -0.5																																	;Set altitude histogram parameters
z1   = 15.5
dz   =  1.0
nbin = LONG((z1-z0)/dz)
zbin = z0 + 0.5*dz + dz*FINDGEN(nbin)

IF (N_ELEMENTS(date1) EQ 0) THEN date1 = date

time1 = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)
time2 = MAKE_ISO_DATE_STRING(date1, PREC='MINUTE', /COMPACT, /UTC)

dt = TIME_DIFF(date1,date)
nt = dt/1800
Znexrad_mean  = 0.0
Znexrad_max   = 0.0
Znexrad_count = 0
FOR s = 0, nt DO BEGIN
	date_string = MAKE_ISO_DATE_STRING(TIME_INC(date,s*1800), PREC='MINUTE', /COMPACT, /UTC)
	nexfile     = !NEXRAD_DIRECTORY + event + '/' + date_string + '.nc'

	nexrad = NEXRAD_READ_LEVEL2_2(nexfile)													;Read NEXRAD composite
	nexrad = NEXRAD_FILTER(nexrad)

	Znexrad = MAX((nexrad.dBZ.values GE dbz_thresh)*REBIN(REFORM(nexrad.z.values, 1, 1, nexrad.z.n), $				;Compute maximum altitude where NEXRAD reflectivities meet or exceed threshold
							 nexrad.x.n, nexrad.y.n, nexrad.z.n, /SAMPLE), DIM = 3, /NAN)

	inexnan  = WHERE((Znexrad EQ 0), nnexnan)																						;Search for points without reflectivity
	IF (nnexnan  GT 0) THEN  Znexrad[inexnan ] = !Values.F_NaN																	;Set points without reflectivity to NaNs

CASE event OF
	'20120519' : BEGIN 
		lon1 = WHERE(nexrad.x.values GE 259.0)												;If focusing on a sub-domain
		lon1 = lon1[0]
		lon2 = WHERE(nexrad.x.values GE 262.1)
		lon2 = lon2 [0]
		lat1 = WHERE(nexrad.y.values EQ 34.6)
		lat2 = WHERE(nexrad.y.values EQ 36.7)

		Znexrad = Znexrad[lon1:lon2,lat1:lat2]

		i0 = 75																				;WRF indices determined by 
		i1 = 250																			;Mapping and guess and check	
		j0 = 100
		j1 = 190
		indices  = [75,100,190,190]

	END
	
	'20120529' : BEGIN
;		lon1 = WHERE(nexrad.x.values GE 259.7)
;		lon1 = lon1[0]
;		lon2 = WHERE(nexrad.x.values GE 264.6)
;		lon2 = lon2 [0]
;		lat1 = WHERE(nexrad.y.values EQ 34.8)
;		lat2 = WHERE(nexrad.y.values EQ 39.3)
;
;		Znexrad = Znexrad[lon1:lon2,lat1:lat2]
	
		Znexrad  = Znexrad[0:248,0:226]														;Use whole NEXRAD domain

		i0 = 50
		i1 = 230
		j0 = 115
		j1 = 270
	END
	
ENDCASE


	indices  = [75,100,190,190]

	inexrad = WHERE(((Znexrad  GE z0) AND (Znexrad  LE z1) AND FINITE(Znexrad )), nnexrad)

	Znexrad_mean  += nnexrad*MEAN(Znexrad[inexrad])
	Znexrad_count += nnexrad
	Znexrad_max    = (Znexrad_max > MAX(Znexrad[inexrad]))

	IF (s EQ 0) THEN $
		hist_nexrad  = HISTOGRAM( Znexrad[inexrad], MIN = z0, NBIN = nbin, BINSIZE = dz) ELSE $
		hist_nexrad += HISTOGRAM( Znexrad[inexrad], MIN = z0, NBIN = nbin, BINSIZE = dz)
ENDFOR

PRINT, 'NEXRAD stats: mean - ' + STRTRIM(Znexrad_mean/Znexrad_count,2) + ', max - ' + STRTRIM(Znexrad_max,2) 

epsfile = !WRF_DIRECTORY + event + '/histograms/wrf_storm_alt_tmatrix_mp.eps'
pngfile = !WRF_DIRECTORY + event + '/histograms/wrf_storm_alt_tmatrix_mp.png'

IF KEYWORD_SET(eps) THEN BEGIN	
	PS_ON, FILENAME = epsfile, PAGE_SIZE = [6.45, 8.25], MARGIN = 0.0, /INCHES											;Switch to Postscript device
	DEVICE, /ENCAPSULATED
	!P.FONT     = 0																														;Hardware fonts
	!P.CHARSIZE = 1.25	
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS																													;Load basic color definitions
ENDIF ELSE BEGIN
	SET_PLOT, 'X'
	WINDOW, XSIZE = 645, YSIZE = 825																								;Open graphics window
	!P.COLOR      = COLOR_24('black')																								;Foreground color
	!P.BACKGROUND = COLOR_24('white')																								;Background color
	!P.CHARSIZE   = 1.8		
	!P.FONT       = -1																													;Use Hershey fonts
ENDELSE
!P.MULTI = [0,2,3]

wrf_scheme  = ['morrison','nssl','milbyau']
tmat_scheme = ['morrison_ysu','nssl_ysu','milbyau_ysu']
title       = ['morrison','nssl','milbyau']

IF KEYWORD_SET(pbl) THEN BEGIN 
	wrf_scheme  = ['nssl','qnse','acm2']
	tmat_scheme = ['nssl_ysu','nssl_qnse','nssl_acm2']
	title       = ['ysu','qnse','acm2']

	epsfile = !WRF_DIRECTORY + event + '/histograms/wrf_storm_alt_tmatrix_pbl.eps'
	pngfile = !WRF_DIRECTORY + event + '/histograms/wrf_storm_alt_tmatrix_pbl.png'

ENDIF

color = [COLOR_24('red'), COLOR_24('gray50'), COLOR_24('blue')]

tmatrix_dir = !WRF_DIRECTORY + event + '/tmatrix/' + tmat_scheme[0] + '/'
CD, tmatrix_dir
tfile = FILE_SEARCH('wrfout*', COUNT = ntfile)		

file1 = WHERE('wrfout_d02_'+ STRING(time1) EQ tfile)
file2 = WHERE('wrfout_d02_'+ STRING(time2) EQ tfile)
file1 = file1[0] 
file2 = file2[0]

times = [ ]
FOR i = file1, file2 DO BEGIN
	time  = tfile[i]
	times = [times, time]
ENDFOR
PRINT, times

FOR i = 0, 2 DO BEGIN
	hist_wrf    = LONARR(nbin, 3)
	hist_wrfcld = LONARR(nbin, 3)
	Zwrfecho_mean  = 0.0
	Zwrfecho_max   = 0.0
	Zwrfecho_count = 0		
	Zwrfcld_mean   = 0.0
	Zwrfcld_max    = 0.0
	Zwrfcld_count  = 0		
		
	FOR s = 0, nt DO BEGIN
;		tmatrix_dir = !WRF_DIRECTORY + event + '/tmatrix/' + tmat_scheme[i] + '/'

;		CD, tmatrix_dir
;		tfile = FILE_SEARCH('wrfout*', COUNT = ntfile)														;Get files

		CD, tmatrix_dir + '/' + tfile[s]
		infile = FILE_SEARCH('*.nc', COUNT = nfile)


		R 	 = [ ]
		refl = [ ]

		FOR t = 0, nfile-1 DO BEGIN  
			refl    = FLOAT((TMATRIX_READ_VAR('data', event, tmat_scheme[i], times[s], t, $
						DOMAIN = dom, INDICES = indices)).values)
			R 		= [[[R]],[[refl]]]
		ENDFOR

		
		cloud  = WRF_READ_VAR('CLOUD_MIX_TOTAL', TIME_INC(date,s*1800), event, wrf_scheme[i], $
					 DOMAIN = dom, INDICES = indices)
		Z      = WRF_READ_VAR('Z', 	             TIME_INC(date,s*1800), event, wrf_scheme[i], $
					 DOMAIN = dom, INDICES = indices)

		Z.values = 0.001*Z.values																												;Convert WRF geopotential heights to km
	
		Zwrf     = MAX((R GE dbz_thresh)*(Z.values), DIM = 3)															;Compute maximum altitude where WRF
		Zwrf_cld = MAX((cloud.values GT 0.0)*(Z.values), DIM = 3)																	;Get WRF cloud maximum altitudes

		iwrfnan  = WHERE( (Zwrf    EQ 0), nwrfnan)
		iwrfcnan = WHERE(((Zwrf    EQ 0) OR (Zwrf_cld EQ 0)), nwrfcnan)

		IF (nwrfnan  GT 0) THEN     Zwrf[iwrfnan ] = !Values.F_NaN
		IF (nwrfcnan GT 0) THEN Zwrf_cld[iwrfcnan] = !Values.F_NaN

		iwrf    = WHERE(((Zwrf     GE z0) AND (Zwrf     LE z1) AND FINITE(Zwrf    )), nwrf   )
		iwrfcld = WHERE(((Zwrf_cld GE z0) AND (Zwrf_cld LE z1) AND FINITE(Zwrf_cld)), nwrfcld)

		Zwrfecho_mean  += TOTAL(FINITE(Zwrf))*MEAN(Zwrf,/NAN)
		Zwrfecho_count += TOTAL(FINITE(Zwrf))
		Zwrfecho_max    = (Zwrfecho_max > MAX(Zwrf,/NAN))

		Zwrfcld_mean  += TOTAL(FINITE(Zwrf_cld))*MEAN(Zwrf_cld,/NAN)
		Zwrfcld_count += TOTAL(FINITE(Zwrf_cld))
		Zwrfcld_max    = (Zwrfcld_max > MAX(Zwrf_cld,/NAN))
	
		IF (s EQ 0) THEN BEGIN
			hist_wrf[*,0]    = HISTOGRAM(    Zwrf[iwrf   ], MIN = z0, NBIN = nbin, BINSIZE = dz)
			hist_wrfcld[*,0] = HISTOGRAM(Zwrf_cld[iwrfcld], MIN = z0, NBIN = nbin, BINSIZE = dz)
		ENDIF ELSE BEGIN
			hist_wrf[*,0]    += HISTOGRAM(    Zwrf[iwrf   ], MIN = z0, NBIN = nbin, BINSIZE = dz)
			hist_wrfcld[*,0] += HISTOGRAM(Zwrf_cld[iwrfcld], MIN = z0, NBIN = nbin, BINSIZE = dz)
		ENDELSE
	ENDFOR

	PRINT, 'WRF Echo stats: mean - ' + STRTRIM(Zwrfecho_mean/Zwrfecho_count,2) + ', max - ' + STRTRIM(Zwrfecho_max,2) 
	PRINT, 'WRF Cloud stats: mean - ' + STRTRIM(Zwrfcld_mean/Zwrfcld_count,2) + ', max - ' + STRTRIM(Zwrfcld_max,2) 

	PLOT, zbin, zbin, /NODATA, $
		XRANGE = [6,15], $	;[z0 + 0.5*dz, z1 - 0.5*dz], $
		XSTYLE = 1, $
		XTITLE = 'Storm Altitude (km)', $
		YRANGE = [0, 40], $
		YSTYLE = 1, $
		YTITLE = 'Frequency (%)', $
		TITLE  = 'Echo Top ' + title[i]

	OPLOT, zbin, 100.0*(FLOAT(hist_nexrad)/TOTAL(hist_nexrad)), THICK = 4, PSYM = 10
	OPLOT, zbin, 100.0*(FLOAT(hist_wrf[*,0])/TOTAL(hist_wrf[*,0])), THICK = 2, PSYM = 10, COLOR = color[0]

	PLOT, zbin, zbin, /NODATA, $
		XRANGE = [6,15], $	;[z0 + 0.5*dz, z1 - 0.5*dz], $
		XSTYLE = 1, $
		XTITLE = 'Storm Altitude (km)', $
		YRANGE = [0, 85], $
		YSTYLE = 1, $
		YTITLE = 'Frequency (%)', $
		TITLE  = 'Cloud Top ' + title[i]

	OPLOT, zbin, 100.0*(FLOAT(hist_nexrad)/TOTAL(hist_nexrad)), THICK = 4, PSYM = 10
	OPLOT, zbin, 100.0*(FLOAT(hist_wrfcld[*,0])/TOTAL(hist_wrfcld[*,0])), THICK = 2, PSYM = 10, COLOR = color[0]

	IF (i EQ 0) THEN BEGIN
		XYOUTS, 7.0, 75.0, 'NEXRAD Echo Top', /DATA
		XYOUTS, 7.0, 67.0, 'dx = 2 km',     COLOR = color[0], /DATA
	ENDIF
ENDFOR

!P.MULTI = 0
IF KEYWORD_SET(eps) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																										;Reset color table to linear ramp
	PS_OFF																																	;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																								;Write PNG file

END
