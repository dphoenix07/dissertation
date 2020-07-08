PRO W_TIMESERIES, run, experiment, start_date, end_date, $
	DOMAIN   = domain, $
	REGION   = region, $
	WRAPPING = wrapping, $
	PNG	     = png, $
	EPS   	 = eps


;+
; Name:
;		W_TIMESERIES
; Purpose:
;		Calculates mass (kg) of H2O in some layer
; Calling sequence:
;		W_TIMESERIES, run, scheme, start_date, end_date
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		scheme 	   : Model initial state. Typically 'morrison' or 'nssl'.
;		start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;		end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
; Output:
;		Timeseries of dH2O/dt, H2O flux along boundaries, and net change in H2O
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel B. Phoenix	    2018-11-22. 
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain

outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/plots/w_timeseries/'
FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(run, experiment, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(run, experiment, start_date, end_date, /DATE)	


z = (WRF_READ_VAR('Z', date_arr[0], run, experiment, DOMAIN = domain, INDICES = region)).values
dim = SIZE(z,/DIMENSIONS)

wmax_array 	 = [ ]
wmin_array 	 = [ ]
w_area_total = [ ]
d_area_total = [ ]
date_index = 0
index1 = FLTARR(dim[0],dim[1])*!Values.F_NaN

FOREACH date, date_arr DO BEGIN
	PRINT, date
	
    x     = WRF_READ_VAR('Longitude'       , date, run, experiment, DOMAIN = domain, INDICES = region)		;Read variables
    y     = WRF_READ_VAR('Latitude'        , date, run, experiment, DOMAIN = domain, INDICES = region)
    z     = (WRF_READ_VAR('Z' 	           , date, run, experiment, DOMAIN = domain, INDICES = region)).values
 	w     = (WRF_READ_VAR('w'              , date, run, experiment, DOMAIN = domain, INDICES = region)).values
  	cloud = (WRF_READ_VAR('CLOUD_MIX_TOTAL', date, run, experiment, DOMAIN = domain, INDICES = region)).values
    updrt = (WRF_READ_VAR('Updraft_tracer' , date, run, experiment, DOMAIN = domain, INDICES = region)).values
    ztrop = (WRF_READ_VAR('Z_trop'         , date, run, experiment, DOMAIN = domain, INDICES = region)).values

    ztrop     = MEDIAN(ztrop, 30)    
	trop_3d   = REBIN (ztrop, dim[0], dim[1], dim[2], /SAMPLE)
	cloud_air = WHERE(cloud GT 1.0E-5, cloud_count, COMPLEMENT = cloud_free)
	dx = 500.0

	IF (date_index EQ 0) THEN BEGIN 
		;Find min vertical level at tropopause for initial time only
		PRINT, 'Find minimum level of tropopause'
		FOR ii = 0, dim[0]-1 DO BEGIN
			FOR jj = 0, dim[1]-1 DO BEGIN
				index1[ii,jj] = MEAN(VALUE_LOCATE(REFORM(z[ii,jj,*],dim[2],1,1),REFORM(trop_3d[ii,jj,*],dim[2],1,1)))
			ENDFOR
		ENDFOR
		PRINT, 'Done finding minimum level of tropopause' 

		index_nan = WHERE(index1 LT 0)
		index1[index_nan] = !Values.F_NaN
	
		index = FLOOR(MEAN(index1,/NAN))
		index0 = index-10
	ENDIF
	
	IF (cloud_count) GT 0 THEN BEGIN
		w[cloud_air ] = w[cloud_air]
		w[cloud_free] = !Values.F_NaN
	ENDIF
	
	;Find columns where w > 3 m/s
	w_thres = FLTARR(dim[0],dim[1])*!Values.F_NaN
	d_thres = FLTARR(dim[0],dim[1])*!Values.F_NaN
	FOR ii = 0, dim[0]-1 DO BEGIN
		FOR jj = 0, dim[1]-1 DO BEGIN
			IF (MAX(w[ii,jj,index0:index] GT  3.0)) THEN w_thres[ii,jj] = 1.0
			IF (MIN(w[ii,jj,index0:index] LT -3.0)) THEN d_thres[ii,jj] = 1.0
		ENDFOR
	ENDFOR
	
	;Sum total number of columns where w > 3 m/s and calculate total area
	w_area = TOTAL(w_thres, /NAN)*dx*dx
	d_area = TOTAL(d_thres, /NAN)*dx*dx
	
	;Create time array of w area
	w_area_total = [w_area_total, w_area]
	d_area_total = [d_area_total, d_area]
	
	;Calculate maximum w over time
	w_max   = MAX(w[*,*,index0:index],/NAN)
	w_min   = MIN(w[*,*,index0:index],/NAN)
    wmax_array = [wmax_array, w_max]
    wmin_array = [wmin_array, w_min]
    
    date_index = date_index + 1
ENDFOREACH


dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string

outdir  = !WRF_DIRECTORY + run + '/' + experiment + '/plots/w_timeseries/'

epsfile = outdir + experiment + '_' + date_string + '.eps'						        ;EPS filename
pdffile = outdir + experiment + '_' + date_string + '.pdf'						        ;PDF filename
pngfile = outdir + experiment + '_' + date_string + '.png'						        ;PNG filename

FILE_MKDIR, outdir																								;Create output directory, if necessary
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
		PS_ON, FILENAME = epsfile, PAGE_SIZE = [8.0,4.0], MARGIN = 0.0, /INCHES;PAGE_SIZE = 0.001*dim*wfactor			;Switch to Postscript device
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


PLOT, wmax_array, $
	YRANGE   = [20.0,70.0], $
	YTITLE   = 'wmax (m/s)', $
	;COLOR    = COLOR_24('black'), $
	XTICKINTERVAL = 12, $
	TITLE    = run, $
	CHARSIZE = 2

PLOT, wmin_array, $
	YRANGE   = [0.0,-50.0], $
	YTITLE   = 'wmin (m/s)', $
	;COLOR    = COLOR_24('black'), $
	XTICKINTERVAL = 12, $
	TITLE    = run, $
	CHARSIZE = 2

PLOT, w_area_total, $
	YRANGE   = [1.0E9,1.0E10], $
	YTITLE   = 'Updraft Prevalence (m2)', $
;	COLOR    = COLOR_24('black'), $
	XTICKINTERVAL = 12, $
	TITLE    = run, $
	CHARSIZE = 2
	
PLOT, d_area_total, $
	YRANGE   = [1.0E9,1.0E10], $
	YTITLE   = 'Downdraft Prevalence (m2)', $
;	COLOR    = COLOR_24('black'), $
	XTICKINTERVAL = 12, $
	TITLE    = run, $
	CHARSIZE = 2
	
IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file


STOP
END

