PRO LRT_THETA_HISTOGRAM, run, experiment, start_date, end_date, $
	DOMAIN        = domain, $
	REGION        = region, $
	TROPICS		  = tropics, $
	MIDLATS   	  = midlats, $
	PNG	     	  = png, $
	EPS   	 	  = eps


;+
; Name:
;		LRT_THETA_HISTOGRAM
; Purpose:
;		Calculates mass (kg) of O3 in some layer
; Calling sequence:
;		LRT_THETA_HISTOGRAM, run, scheme, start_date, end_date
;		LRT_THETA_HISTOGRAM, '20120530_ncar','d03_30km','20120530T2200Z','20120531T0500Z'
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

outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/plots/lrt_theta_histogram/'
FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(run, experiment, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(run, experiment, start_date, end_date, /DATE)	

z = (WRF_READ_VAR('Z', date_arr[0], run, experiment, DOMAIN = domain, INDICES = region)).values
dim = SIZE(z,/DIMENSIONS)

epsfile = outdir + run + '_lrt_theta_hist.eps'											;EPS filename
pdffile = outdir + run + '_lrt_theta_hist.pdf'											;PDF filename
pngfile = outdir + run + '_lrt_theta_hist.png'											;PNG filename

z0   = 310.																																	;Set altitude histogram parameters
z1   = 390.
dz   =  5.0
nbin = LONG((z1-z0)/dz)
zbin = z0 + 0.5*dz + dz*FINDGEN(nbin)

date_index = 0
index1 = FLTARR(dim[0],dim[1])*!Values.F_NaN

theta1_mean  = 0
theta1_count = 0
theta1_max = 0

FOREACH date, date_arr DO BEGIN
    PRINT, date
    
    x            = WRF_READ_VAR('Longitude'  , date, run, experiment, DOMAIN = domain, INDICES = region)		;Read variables
    y            = WRF_READ_VAR('Latitude'   , date, run, experiment, DOMAIN = domain, INDICES = region)
    z            = (WRF_READ_VAR('Z' 	     , date, run, experiment, DOMAIN = domain, INDICES = region)).values
    ztrop        = (WRF_READ_VAR('Z_trop'    , date, run, experiment, DOMAIN = domain, INDICES = region)).values
   	temp	   	 = (WRF_READ_VAR('T'         , date, run, experiment, DOMAIN = domain, INDICES = region)).values							;Read temperature variable from WRF output
 	theta	   	 =  WRF_READ_VAR('T'         , date, run, experiment, DOMAIN = domain, INDICES = region)							;Read temperature variable from WRF output
	theta.values = ((1000.0/(WRF_READ_VAR('P', date, run, experiment, DOMAIN = domain)).values)^(!Rair/!Cp))*(theta.values)
  
    dim = SIZE(z, /DIMENSIONS)
    ztrop    = MEDIAN(ztrop, 30)    
	trop_filtered = ztrop
	
	count = 0
	
	IF (KEYWORD_SET(tropics)) THEN igood = WHERE(ztrop GE 14000.0, count, COMPLEMENT = ibad)
	IF (KEYWORD_SET(midlats)) THEN igood = WHERE(ztrop LT 14000.0, count, COMPLEMENT = ibad)

	IF (count GT 0) THEN trop_filtered [ibad] = !Values.F_NaN
	trop_3d  = REBIN (trop_filtered, dim[0], dim[1], dim[2], /SAMPLE)

	;Find theta at tropopause 	 
	theta1 = FLTARR(dim[0],dim[1])*!Values.F_NaN
	FOR ii = 0, dim[0]-1 DO BEGIN
		FOR jj = 0, dim[1]-1 DO BEGIN
			index1 = VALUE_LOCATE(REFORM(z[ii,jj,*],dim[2],1,1),REFORM(trop_3d[ii,jj,*],dim[2],1,1))
			theta1[ii,jj] = theta.values[ii,jj,index1[0]]
		ENDFOR
	ENDFOR
	   		
	theta1_sort = SORT(theta1)
	theta1_finite = WHERE(FINITE(theta1[theta1_sort]))
	theta11 = theta1[theta1_sort[theta1_finite]]

	itheta11 = WHERE(((theta11 GE z0) AND (theta11 LE z1) AND FINITE(theta11)), ntheta11)

	theta1_mean  += TOTAL(FINITE(theta11))*MEAN(theta11,/NAN)
	theta1_count += TOTAL(FINITE(theta11))
	theta1_max    = (theta1_max > MAX(theta11,/NAN))
	
	IF date_index EQ 0 THEN BEGIN
		hist_trop  = HISTOGRAM(theta11[itheta11]		    , MIN = z0, NBIN = nbin, BINSIZE = dz) 
		trop_95    = theta11[PERCENTILE(theta11,95,/NAN)]
		trop_5     = theta11[PERCENTILE(theta11,5 ,/NAN)]
		index0     = (MEAN(theta11,/NAN))
	ENDIF ELSE BEGIN
		hist_trop  += HISTOGRAM(theta11[itheta11]		 , MIN = z0, NBIN = nbin, BINSIZE = dz) 
		trop_95    = [trop_95  , theta11[PERCENTILE(theta11,95,/NAN)]]
		trop_5     = [trop_5   , theta11[PERCENTILE(theta11, 5,/NAN)]]
		index0     = [index0   , (MEAN(theta11,/NAN))]
	ENDELSE

	date_index += 1
ENDFOREACH

hist_mean  = HISTOGRAM(index0				, MIN = z0, NBIN = nbin, BINSIZE = dz) 
hist_5th   = HISTOGRAM(trop_5				, MIN = z0, NBIN = nbin, BINSIZE = dz) 
hist_95th  = HISTOGRAM(trop_95				, MIN = z0, NBIN = nbin, BINSIZE = dz)  

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

IF (run EQ '20110518') THEN yrange = [0,30]
IF (run EQ '20130805') THEN yrange = [0,30]

PLOT, zbin, zbin, /NODATA, $
	XRANGE   = [z0 + 0.5*dz, z1 - 0.5*dz], $
	XSTYLE   = 1, $
	XTITLE   = 'Theta at LRT (K)', $
	YRANGE   = yrange, $
	YSTYLE   = 1, $
	YTITLE   = 'Frequency (%)', $
	TITLE    = run, $
	CHARSIZE = 2

OPLOT, zbin, 100.0*(FLOAT(hist_trop)/TOTAL(hist_trop)), THICK = 4, PSYM = 10

IF (run EQ '20110518') THEN yrange = [0,75]
IF (run EQ '20130805') THEN yrange = [0,100]

PLOT, zbin, zbin, /NODATA, $
	XRANGE = [z0 + 0.5*dz, z1 - 0.5*dz], $
	XSTYLE = 1, $
	XTITLE = 'Mean Theta at LRT (K)', $
	YRANGE = yrange, $
	YSTYLE = 1, $
	YTITLE = 'Frequency (%)', $
	TITLE  = run

OPLOT, zbin, 100.0*(FLOAT(hist_mean)/TOTAL(hist_mean)), THICK = 4, PSYM = 10

IF (run EQ '20110518') THEN yrange = [0,40]
IF (run EQ '20130805') THEN yrange = [0,100]

PLOT, zbin, zbin, /NODATA, $
	XRANGE = [z0 + 0.5*dz, z1 - 0.5*dz], $
	XSTYLE = 1, $
	XTITLE = '95th % Theta at LRT (K)', $
	YRANGE = yrange, $
	YSTYLE = 1, $
	YTITLE = 'Frequency (%)', $
	TITLE  = run

OPLOT, zbin, 100.0*(FLOAT(hist_95th)/TOTAL(hist_95th)), THICK = 4, PSYM = 10

IF (run EQ '20110518') THEN yrange = [0,30]
IF (run EQ '20130805') THEN yrange = [0,100]

PLOT, zbin, zbin, /NODATA, $
	XRANGE = [z0 + 0.5*dz, z1 - 0.5*dz], $
	XSTYLE = 1, $
	XTITLE = '5th % Theta at LRT (K)', $
	YRANGE = yrange, $
	YSTYLE = 1, $
	YTITLE = 'Frequency (%)', $
	TITLE  = run

OPLOT, zbin, 100.0*(FLOAT(hist_5th)/TOTAL(hist_5th)), THICK = 4, PSYM = 10

IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file

STOP
END
