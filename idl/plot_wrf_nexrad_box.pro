PRO PLOT_WRF_NEXRAD_BOX, event, date, date1, $
	PBL 	  = pbl, $
	RES_TEST  = res_test, $
	CHEM      = chem, $
	EPS 	  = eps, $
	PNG 	  = png

;+
; Name:
;		PLOT_WRF_NEXRAD_BOX
; Purpose:
;		This is a procedure to plot boxplots of storm altitudes from NEXRAD
;		obeservations and WRF resolution testing simulations. 
; Calling sequence:
;		PLOT_WRF_NEXRAD_BOX, date
; Input:
;		date  : Analysis date {CDATE}.
; Output:
;		A boxplot of storm altitude frequencies for NEXRAD observations,
;		WRF reflectivity, and WRF cloud top.
; Keywords:
;		EPS  : If set, output to PostScript.
;		PNG  : If set, write PNG image.
; Author and history:
;		Cameron R. Homeyer  2014-08-04.
;		DBP added 'ROUND(Z.values)' 3/28/17 

;-

COMPILE_OPT IDL2																															;Set compile options

dbz_thresh = 10.0

z0   = -0.5																																	;Set altitude histogram parameters
z1   = 15.5
dz   =  1.0
nbin = LONG((z1-z0)/dz)
zbin = z0 + 0.5*dz + dz*FINDGEN(nbin)

IF (N_ELEMENTS(date1) EQ 0) THEN date1 = date

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
	
	'20120601' : BEGIN
		lon1 = WHERE(nexrad.x.values GE 256.3)
		lon1 = lon1[0]
		lon2 = WHERE(nexrad.x.values GE 262.0) 
		lon2 = lon2[0]
		lat1 = WHERE(nexrad.y.values EQ 33.26)
		lat2 = WHERE(nexrad.y.values EQ 38.0)

		Znexrad = Znexrad[lon1:lon2,lat1:lat2]
		
		i0 = 50
		i1 = 300
		j0 = 100
		j1 = 270
	END
ENDCASE



	inexrad = WHERE(((Znexrad  GE z0) AND (Znexrad  LE z1) AND FINITE(Znexrad )), nnexrad)

	Znexrad_mean  += nnexrad*MEAN(Znexrad[inexrad])
	Znexrad_count += nnexrad
	Znexrad_max    = (Znexrad_max > MAX(Znexrad[inexrad]))

	IF (s EQ 0) THEN $
		hist_nexrad  = HISTOGRAM( Znexrad[inexrad], MIN = z0, NBIN = nbin, BINSIZE = dz) ELSE $
		hist_nexrad += HISTOGRAM( Znexrad[inexrad], MIN = z0, NBIN = nbin, BINSIZE = dz)
ENDFOR


Znex1 = Znexrad[inexrad]
Znex2 = Znex1[SORT(Znex1)]

IF (N_ELEMENTS(Znex2) MOD 2 EQ 0) THEN BEGIN
	nZnex 	= N_ELEMENTS(Znex2)
	nex_med = (Znex2[(nZnex/2)-1] + Znex2[(nZnex/2)]) / 2.0
	lower_half = Znex2[0:(nZnex/2)-1]
	upper_half = Znex2[(nZnex/2):(nZnex-1)]
ENDIF ELSE BEGIN
	nZnex 	= N_ELEMENTS(Znex2)
	nex_med = Znex2[(nZnex/2)] 
	lower_half = Znex2[0:(nZnex/2)-1]
	upper_half = Znex2[(nZnex/2):(nZnex-1)]
ENDELSE

quartile_25 = MEDIAN(lower_half, /EVEN)
quartile_75 = MEDIAN(upper_half, /EVEN)
quartile_05 = Znex2[0.05*nZnex]
quartile_95 = Znex2[0.95*nZnex]

Znex = [quartile_05, quartile_25, nex_med, quartile_75,quartile_95]


PRINT, 'NEXRAD stats: mean - ' + STRTRIM(Znexrad_mean/Znexrad_count,2) + ', max - ' + STRTRIM(Znexrad_max,2) 


;IF KEYWORD_SET(eps) THEN BEGIN
;            PS_ON, FILENAME = epsfile, PAGE_SIZE = [8.0, 6.0], MARGIN = 0.0, /INCHES          						;Switch to Postscript device
;            DEVICE, /ENCAPSULATED
;            !P.FONT     = 0                                               													;Hardware fonts
;            !P.CHARSIZE = 1.0
;            IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
;              LOAD_BASIC_COLORS                                             												;Load basic color definitions
;ENDIF ELSE BEGIN
;            SET_PLOT, 'X'
;            WINDOW, XSIZE = 1024, YSIZE = 768                                   											;Open graphics window
;            !P.COLOR      = COLOR_24('black')                                   											;Foreground color
;            !P.BACKGROUND = COLOR_24('white')                                   											;Background color
;            !P.CHARSIZE   = 2.0 
;            !P.FONT       = -1                                                  											;Use Hershey fonts
;ENDELSE



IF (event EQ '20120519') THEN BEGIN
	scheme = ['morrison_ysu','nssl_ysu','milbyau_ysu','nssl_qnse','nssl_ysu','nssl_acm2', $
				'cbmz_noaerosols', 'nssl_ysu', 'mozcart_new']

	title  = ['MOR','NSSL','MY','QNSE','YSU','ACM2', $
				'CBMZ', 'RACM', 'MOZ', 'OBS']
ENDIF

IF (event EQ '20120529') THEN BEGIN
	scheme = ['morrison','nssl','milbyau','qnse','nssl','nssl', $
				'cbmz', 'nssl', 'mozcart']

	title  = ['MOR','NSSL','MY','QNSE','YSU','N/A', $
				'CBMZ', 'RACM', 'MOZ', 'OBS']
ENDIF

IF (event EQ '20120601') THEN BEGIN
	scheme = ['morrison','nssl','milbyau','qnse','nssl','acm2', $
				'cbmz', 'nssl', 'mozcart']

	title  = ['MOR','NSSL','MY','QNSE','YSU','ACM2', $
				'CBMZ', 'RACM', 'MOZ', 'OBS']
ENDIF

outdir  = !WRF_DIRECTORY + event + '/boxplot/'
epsfile = !WRF_DIRECTORY + event + '/boxplot/wrf_storm_box.eps'
pngfile = !WRF_DIRECTORY + event + '/boxplot/wrf_storm_box.png'
FILE_MKDIR, outdir


IF KEYWORD_SET(res_test) THEN BEGIN
    scheme = ['15-3km_fix']
    title = scheme
    i0 = 50
    i1 = 210 
    j0 = 90
    j1 = 190
    epsfile = !WRF_DIRECTORY + event + '/boxplot/wrf_storm_box_RES.eps'
	pngfile = !WRF_DIRECTORY + event + '/boxplot/wrf_storm_box_RES.png'
ENDIF


IF KEYWORD_SET(eps) THEN BEGIN	
	PS_ON, FILENAME = epsfile, PAGE_SIZE = [6.45, 8.25], MARGIN = 0.0, /INCHES											;Switch to Postscript device
	DEVICE, /ENCAPSULATED
	!P.FONT     = 0																														;Hardware fonts
	!P.CHARSIZE = 1.25	
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS																													;Load basic color definitions
ENDIF; ELSE BEGIN
;	SET_PLOT, 'X'
;	WINDOW, XSIZE = 645, YSIZE = 825																								;Open graphics window
;	!P.COLOR      = COLOR_24('black')																								;Foreground color
;	!P.BACKGROUND = COLOR_24('white')																								;Background color
;	!P.CHARSIZE   = 1.8		
;	!P.FONT       = -1																													;Use Hershey fonts
;ENDELSE
;!P.MULTI = [0,2,4]


color = [COLOR_24('red'), COLOR_24('gray50'), COLOR_24('blue')]

wrf_data  = [ ]
Cwrf_data = [ ]  
Zwrf_dt   = [ ]
Cwrf_dt   = [ ]

FOR i = 0, N_ELEMENTS(scheme)-1 DO BEGIN
	Zwrf_dt   = [ ]
	Cwrf_dt   = [ ]

	hist_wrf    = LONARR(nbin, 3)
	hist_wrfcld = LONARR(nbin, 3)
	Zwrfecho_mean  = 0.0
	Zwrfecho_max   = 0.0
	Zwrfecho_count = 0		
	Zwrfcld_mean   = 0.0
	Zwrfcld_max    = 0.0
	Zwrfcld_count  = 0		
		FOR s = 0, nt DO BEGIN
			refl   = WRF_READ_VAR('REFL', 			 TIME_INC(date,s*1800), event, scheme[i], $				; Read relevant variables
						 DOMAIN = dom, INDICES = [i0[0],j0[0],i1[0],j1[0]])
			cloud  = WRF_READ_VAR('CLOUD_MIX_TOTAL', TIME_INC(date,s*1800), event, scheme[i], $
						 DOMAIN = dom, INDICES = [i0[0],j0[0],i1[0],j1[0]])
			Z      = WRF_READ_VAR('Z', 	             TIME_INC(date,s*1800), event, scheme[i], $
						 DOMAIN = dom, INDICES = [i0[0],j0[0],i1[0],j1[0]])

			Z.values = 0.001*Z.values																		;Convert WRF geopotential heights to km

;;; DBP added 'ROUND(Z.values) 3/28/17 ;;;;

;			Zwrf     = MAX((refl.values GE dbz_thresh)*(ROUND(Z.values)), DIM = 3)									;Compute maximum altitude where WRF
;			Zwrf_cld = MAX((cloud.values GT 0.0)*(ROUND(Z.values)), DIM = 3)										;Get WRF cloud maximum altitudes

			Zwrf     = MAX((refl.values GE dbz_thresh)*(Z.values), DIM = 3)									;Compute maximum altitude where WRF
			Zwrf_cld = MAX((cloud.values GT 0.0)*(Z.values), DIM = 3)										;Get WRF cloud maximum altitudes

;;; DBP added 'ROUND(Z.values) 3/28/17 ;;;;

			iwrfnan  = WHERE( (Zwrf    EQ 0), nwrfnan)														; Find NAN index
			iwrfcnan = WHERE(((Zwrf    EQ 0) OR (Zwrf_cld EQ 0)), nwrfcnan)

			IF (nwrfnan  GT 0) THEN     Zwrf[iwrfnan ] = !Values.F_NaN										; Assign "NAN" to NAN index
			IF (nwrfcnan GT 0) THEN Zwrf_cld[iwrfcnan] = !Values.F_NaN

			iwrf    = WHERE(((Zwrf     GE z0) AND (Zwrf     LE z1) AND FINITE(Zwrf    )), nwrf   )			; Find index of values between limits
			iwrfcld = WHERE(((Zwrf_cld GE z0) AND (Zwrf_cld LE z1) AND FINITE(Zwrf_cld)), nwrfcld)

			Zwrfecho_mean  += TOTAL(FINITE(Zwrf))*MEAN(Zwrf,/NAN)											; Compute mean and max
			Zwrfecho_count += TOTAL(FINITE(Zwrf))
			Zwrfecho_max    = (Zwrfecho_max > MAX(Zwrf,/NAN))

			Zwrfcld_mean  += TOTAL(FINITE(Zwrf_cld))*MEAN(Zwrf_cld,/NAN)
			Zwrfcld_count += TOTAL(FINITE(Zwrf_cld))
			Zwrfcld_max    = (Zwrfcld_max > MAX(Zwrf_cld,/NAN))
	
			IF (s EQ 0) THEN BEGIN																			; Compute histograms
				hist_wrf[*,0]    = HISTOGRAM(    Zwrf[iwrf   ], MIN = z0, NBIN = nbin, BINSIZE = dz)
				hist_wrfcld[*,0] = HISTOGRAM(Zwrf_cld[iwrfcld], MIN = z0, NBIN = nbin, BINSIZE = dz)
			ENDIF ELSE BEGIN
				hist_wrf[*,0]    += HISTOGRAM(    Zwrf[iwrf   ], MIN = z0, NBIN = nbin, BINSIZE = dz)
				hist_wrfcld[*,0] += HISTOGRAM(Zwrf_cld[iwrfcld], MIN = z0, NBIN = nbin, BINSIZE = dz)
			ENDELSE

			Zwrf1 = Zwrf[iwrf]
			Zwrf_dt = [Zwrf_dt,Zwrf1]

			Cwrf1 = Zwrf_cld[iwrfcld]
			Cwrf_dt = [Cwrf_dt,Cwrf1]

		ENDFOR

		PRINT, 'WRF Echo stats: mean - ' + STRTRIM(Zwrfecho_mean/Zwrfecho_count,2) + ', max - ' + STRTRIM(Zwrfecho_max,2) 
		PRINT, 'WRF Cloud stats: mean - ' + STRTRIM(Zwrfcld_mean/Zwrfcld_count,2) + ', max - ' + STRTRIM(Zwrfcld_max,2) 

		Zwrf2 = Zwrf_dt[SORT(Zwrf_dt)]

		IF (N_ELEMENTS(Zwrf2) MOD 2 EQ 0) THEN BEGIN
			nZ		 	= N_ELEMENTS(Zwrf2)
			wrf_med 	= (Zwrf2[(nZ/2)-1] + Zwrf2[(nZ/2)]) / 2.0
			lower_half  = Zwrf2[0:(nZ/2)-1]
			upper_half  = Zwrf2[(nZ/2):(nZ-1)]
		ENDIF ELSE BEGIN
			nZ		 	= N_ELEMENTS(Zwrf2)
			wrf_med 	= Zwrf2[(nZ/2)] 
			lower_half  = Zwrf2[0:(nZ/2)-1]
			upper_half 	= Zwrf2[(nZ/2):(nZ-1)]
		ENDELSE

		quartile_25 = MEDIAN(lower_half, /EVEN)
		quartile_75 = MEDIAN(upper_half, /EVEN)
		quartile_05 = Zwrf2[0.05*nZ]
		quartile_95 = Zwrf2[0.95*nZ]

		Zwrf3 = [quartile_05, quartile_25, wrf_med, quartile_75,quartile_95]
		wrf_data = [[wrf_data],[Zwrf3]]


		Cwrf2 = Cwrf_dt[SORT(Cwrf_dt)]

		IF (N_ELEMENTS(Cwrf2) MOD 2 EQ 0) THEN BEGIN
			nC		 	= N_ELEMENTS(Cwrf2)
			Cwrf_med 	= (Cwrf2[(nC/2)-1] + Cwrf2[(nC/2)]) / 2.0
			lower_half  = Cwrf2[0:(nC/2)-1]
			upper_half  = Cwrf2[(nC/2):(nC-1)]
		ENDIF ELSE BEGIN
			nC		 	= N_ELEMENTS(Cwrf2)
			Cwrf_med 	= Cwrf2[(nC/2)] 
			lower_half  = Cwrf2[0:(nC/2)-1]
			upper_half 	= Cwrf2[(nC/2):(nC-1)]
		ENDELSE

		quartile_25 = MEDIAN(lower_half, /EVEN)
		quartile_75 = MEDIAN(upper_half, /EVEN)
		quartile_05 = Cwrf2[0.05*nC]
		quartile_95 = Cwrf2[0.95*nC]

		Cwrf3 = [quartile_05, quartile_25, Cwrf_med, quartile_75,quartile_95]
		Cwrf_data = [[Cwrf_data],[Cwrf3]]


ENDFOR

data  = [[wrf_data ],[Znex]]
Cdata = [[Cwrf_data], [Znex]]

boxes = BOXPLOT(data, $
		XRANGE 		= [1,16], $
		YRANGE 		= [-1, 10], $
		XTITLE 		= 'Echo Top Height (km)', $
		YTICKNAME 	= title, $
		YTICKVALUES = INDGEN(N_ELEMENTS(title)), $
		FONT_SIZE   = 20, $
		HORIZONTAL	= 1)

boxes = BOXPLOT(Cdata, $
		XRANGE 		= [1,16], $
		YRANGE 		= [-1, 10], $
		XTITLE 		= 'WRF Cloud Top Height (km) / NEXRAD Echo Top', $
		YTICKNAME 	= title, $
		YTICKVALUES = INDGEN(N_ELEMENTS(title)), $	
		FONT_SIZE	= 20, $	
		HORIZONTAL	= 1)

!P.MULTI = 0

IF (N_ELEMENTS(pngfile) NE 0) THEN print, pngfile

IF KEYWORD_SET(eps) THEN BEGIN
    IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
	      LOAD_BASIC_COLORS, /RESET                                           												;Reset color table to linear ramp
   	   PS_OFF                                                                											;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
         WRITE_PNG, pngfile, TVRD(TRUE = 1)                                    											;Write PNG file

END
