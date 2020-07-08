PRO TROPOPAUSE_HISTOGRAM, run, experiment, start_date, end_date, $
	DOMAIN        = domain, $
	REGION        = region, $
	TROPICS		  = tropics, $
	MIDLATS   	  = midlats, $
	PNG	     	  = png, $
	EPS   	 	  = eps


;+
; Name:
;		TROPOPAUSE_HISTOGRAM
; Purpose:
;		Calculates mass (kg) of O3 in some layer
; Calling sequence:
;		TROPOPAUSE_HISTOGRAM, run, scheme, start_date, end_date
;		TROPOPAUSE_HISTOGRAM, '20120530_ncar','d03_30km','20120530T2200Z','20120531T0500Z'
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

IF (run EQ '20110518') THEN midlats = 1
IF (run EQ '20130805') THEN tropics = 1

domain = 1

outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/plots/tropopause_histogram/'
FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(run, experiment, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(run, experiment, start_date, end_date, /DATE)	

z = (WRF_READ_VAR('Z', date_arr[0], run, experiment, DOMAIN = domain, INDICES = region)).values
dim = SIZE(z,/DIMENSIONS)

z0   = 9.5																																	;Set altitude histogram parameters
z1   = 17.5
dz   =  0.25
nbin = LONG((z1-z0)/dz)
zbin = z0 + 0.5*dz + dz*FINDGEN(nbin)

date_index = 0
index1 = FLTARR(dim[0],dim[1])*!Values.F_NaN

Ztrop_mean  = 0
Ztrop_count = 0
Zfilt_mean  = 0
Zfilt_count = 0

ztrop_max = 0
trop_filtered_max = 0

FOREACH date, date_arr DO BEGIN
    PRINT, date
    
    x     = WRF_READ_VAR('Longitude'       , date, run, experiment, DOMAIN = domain, INDICES = region)		;Read variables
    y     = WRF_READ_VAR('Latitude'        , date, run, experiment, DOMAIN = domain, INDICES = region)
    z     = (WRF_READ_VAR('Z' 	           , date, run, experiment, DOMAIN = domain, INDICES = region)).values
    ztrop = (WRF_READ_VAR('Z_trop'         , date, run, experiment, DOMAIN = domain, INDICES = region)).values
    
    dim = SIZE(z, /DIMENSIONS)
    ztrop    = MEDIAN(ztrop, 30)    
	trop_filtered = ztrop
	
	count = 0
	
	IF (KEYWORD_SET(tropics)) THEN igood = WHERE(ztrop GE 15000.0, count, COMPLEMENT = ibad)
	IF (KEYWORD_SET(midlats)) THEN igood = WHERE(ztrop LT 14000.0, count, COMPLEMENT = ibad)

	IF (count GT 0) THEN trop_filtered [ibad] = !Values.F_NaN
	   		
	ztrop = ztrop*1.0E-3
	trop_filtered = trop_filtered*1.0E-3
	
	ztrop_sort = SORT(ztrop)
	ztrop_finite = WHERE(FINITE(ztrop[ztrop_sort]))
	ztrop1 = ztrop[ztrop_sort[ztrop_finite]]

	trop_filtered_sort = SORT(trop_filtered)
	trop_filtered_finite = WHERE(FINITE(trop_filtered[trop_filtered_sort]))
	trop_filtered1 = trop_filtered[trop_filtered_sort[trop_filtered_finite]]

	iztrop1 = WHERE(((ztrop1 GE z0) AND (ztrop1 LE z1) AND FINITE(ztrop1)), nztrop1)
	ifilt  = WHERE(((trop_filtered1 GE z0) AND (trop_filtered1 LE z1) AND FINITE(trop_filtered1)), nfilt)

	ztrop_mean  += TOTAL(FINITE(ztrop1))*MEAN(ztrop1,/NAN)
	ztrop_count += TOTAL(FINITE(ztrop1))
	ztrop_max    = (ztrop_max > MAX(ztrop1,/NAN))

	Zfilt_mean  += TOTAL(FINITE(trop_filtered1))*MEAN(trop_filtered1,/NAN)
	Zfilt_count += TOTAL(FINITE(trop_filtered1))
	Zfilt_max    = (trop_filtered_max > MAX(trop_filtered1,/NAN))

	
	IF date_index EQ 0 THEN BEGIN
		hist_trop  = HISTOGRAM(ztrop1[iztrop1]		    , MIN = z0, NBIN = nbin, BINSIZE = dz) 
		hist_filt  = HISTOGRAM(trop_filtered1[ifilt]	, MIN = z0, NBIN = nbin, BINSIZE = dz) 
		trop_95    = ztrop1[PERCENTILE(ztrop1,95,/NAN)]
		trop_5     = ztrop1[PERCENTILE(ztrop1,5 ,/NAN)]
		index0     = (MEAN(ztrop1,/NAN))
		ftrop_95   = trop_filtered1[PERCENTILE(trop_filtered1,95,/NAN)]
		ftrop_5    = trop_filtered1[PERCENTILE(trop_filtered1, 5,/NAN)]
		findex0    = (MEAN(trop_filtered1,/NAN))
	ENDIF ELSE BEGIN
		hist_trop  += HISTOGRAM(ztrop1[iztrop1]		 , MIN = z0, NBIN = nbin, BINSIZE = dz) 
		hist_filt  += HISTOGRAM(trop_filtered1[ifilt], MIN = z0, NBIN = nbin, BINSIZE = dz) 
		trop_95    = [trop_95  , ztrop1[PERCENTILE(ztrop1,95,/NAN)]]
		trop_5     = [trop_5   , ztrop1[PERCENTILE(ztrop1, 5,/NAN)]]
		index0     = [index0   , (MEAN(ztrop1,/NAN))]
		ftrop_95   = [ftrop_95 , trop_filtered1[PERCENTILE(trop_filtered1,95,/NAN)]]
		ftrop_5    = [ftrop_5  , trop_filtered1[PERCENTILE(trop_filtered1, 5,/NAN)]]
		findex0    = [findex0  , (MEAN(trop_filtered1,/NAN))]
	ENDELSE

	date_index += 1
ENDFOREACH

hist_mean  = HISTOGRAM(index0				, MIN = z0, NBIN = nbin, BINSIZE = dz) 
hist_5th   = HISTOGRAM(trop_5				, MIN = z0, NBIN = nbin, BINSIZE = dz) 
hist_95th  = HISTOGRAM(trop_95				, MIN = z0, NBIN = nbin, BINSIZE = dz)  
hist_fmean = HISTOGRAM(findex0				, MIN = z0, NBIN = nbin, BINSIZE = dz) 
hist_f5th  = HISTOGRAM(ftrop_5				, MIN = z0, NBIN = nbin, BINSIZE = dz) 
hist_f95th = HISTOGRAM(ftrop_95				, MIN = z0, NBIN = nbin, BINSIZE = dz)  

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

!P.MULTI = [0,3,2]

IF (run EQ '20110518') THEN yrange = [0,15]
IF (run EQ '20130805') THEN yrange = [0,30]

PLOT, zbin, zbin, /NODATA, $
	XRANGE = [z0 + 0.5*dz, z1 - 0.5*dz], $
	XSTYLE = 1, $
	XTITLE = 'Tropopause Altitude (km)', $
	YRANGE = yrange, $
	YSTYLE = 1, $
	YTITLE = 'Frequency (%)', $
	TITLE  = run

OPLOT, zbin, 100.0*(FLOAT(hist_trop)/TOTAL(hist_trop)), THICK = 4, PSYM = 10
OPLOT, zbin, 100.0*(FLOAT(hist_filt)/TOTAL(hist_filt)), THICK = 4, PSYM = 10, COLOR = COLOR_24('red')

IF (run EQ '20110518') THEN yrange = [0,75]
IF (run EQ '20130805') THEN yrange = [0,50]

PLOT, zbin, zbin, /NODATA, $
	XRANGE = [z0 + 0.5*dz, z1 - 0.5*dz], $
	XSTYLE = 1, $
	XTITLE = 'Mean Tropopause Altitude (km)', $
	YRANGE = yrange, $
	YSTYLE = 1, $
	YTITLE = 'Frequency (%)', $
	TITLE  = run

OPLOT, zbin, 100.0*(FLOAT(hist_mean)/TOTAL(hist_mean)), THICK = 4, PSYM = 10
OPLOT, zbin, 100.0*(FLOAT(hist_fmean)/TOTAL(hist_fmean)), THICK = 4, PSYM = 10, COLOR = COLOR_24('red')

IF (run EQ '20110518') THEN yrange = [0,40]
IF (run EQ '20130805') THEN yrange = [0,30]

PLOT, zbin, zbin, /NODATA, $
	XRANGE = [z0 + 0.5*dz, z1 - 0.5*dz], $
	XSTYLE = 1, $
	XTITLE = '95th % Tropopause Altitude (km)', $
	YRANGE = yrange, $
	YSTYLE = 1, $
	YTITLE = 'Frequency (%)', $
	TITLE  = run

OPLOT, zbin, 100.0*(FLOAT(hist_95th)/TOTAL(hist_95th)), THICK = 4, PSYM = 10
OPLOT, zbin, 100.0*(FLOAT(hist_f95th)/TOTAL(hist_f95th)), THICK = 4, PSYM = 10, COLOR = COLOR_24('red')

IF (run EQ '20110518') THEN yrange = [0,30]
IF (run EQ '20130805') THEN yrange = [0,40]

PLOT, zbin, zbin, /NODATA, $
	XRANGE = [z0 + 0.5*dz, z1 - 0.5*dz], $
	XSTYLE = 1, $
	XTITLE = '5th % Tropopause Altitude (km)', $
	YRANGE = yrange, $
	YSTYLE = 1, $
	YTITLE = 'Frequency (%)', $
	TITLE  = run

OPLOT, zbin, 100.0*(FLOAT(hist_5th)/TOTAL(hist_5th)), THICK = 4, PSYM = 10
OPLOT, zbin, 100.0*(FLOAT(hist_f5th)/TOTAL(hist_f5th)), THICK = 4, PSYM = 10, COLOR = COLOR_24('red')

STOP
END
