PRO PLOT_TRACER_TRACER_FILE, event, scheme, start_date, end_date, $
	ZRELATIVE    = zrelative, $
	POTEN_TEMP   = poten_temp, $
	CLOUD        = cloud, $
	TRACER_COLOR = tracer_color, $
	TEMP_COLOR   = temp_color, $
	RALT_COLOR   = ralt_color, $
	CLOUD_COLOR  = cloud_color, $
	GROUP2       = group2, $
	UPDRAFT_TR   = updraft_tr, $
	DIFFERENCE   = difference, $
	DOMAIN       = domain, $
	XLOG	     = xlog, $
	BINNED       = binned, $
	EPS		     = eps, $
	PNG	         = png


;+
; Name:
;		PLOT_TRACER_TRACER_FILE
; Purpose:
;		Currently set up to combine multiple time steps and produce scatter plots of 
;		trace gases using WRF_TRACER_TRACER.
;		(e.g., Similar to 'PLOT_WRF_TRACER_TRACER', but can do multiple time steps)
; Calling sequence:
;		PLOT_MULTI_TRACER, run, scheme, start_date, end_date
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		scheme 	   : Model initial state. Typically 'morrison' or 'nssl'.
;		start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;		end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
; Output:
;		Scatterplots of trace gases from multiple timesteps.
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel B. Phoenix	    2016-02-18. 
;								2018-03-15.	Use PLOT_MULTI_TRACER_TEST if difference not
;											desired. Some problem with this routine not 
;											coloring by potential temp, cloud, etc.
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain
IF (N_ELEMENTS(filter	 ) EQ 0) THEN filter 	 = 0
IF (N_ELEMENTS(nxbin     ) EQ 0) THEN nxbin      = 100
IF (N_ELEMENTS(nybin     ) EQ 0) THEN nybin      = 100
IF (N_ELEMENTS(binned	 ) EQ 0) THEN binned 	 = 1

IF KEYWORD_SET(updraft_tr) THEN filter = 1

IF (event EQ '20120519') THEN region = [50, 50, 250, 190]

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(event, scheme, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(event, scheme, start_date, end_date, /DATE)	

outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/plots/tracer_tracer/'
epsfile = outdir + event + 'h2o_o3_ralt.eps'	
pdffile = outdir + event + 'h2o_o3_ralt.pdf'	
pngfile = outdir + event + 'h2o_o3_ralt.png'	

IF (KEYWORD_SET(group2)) THEN BEGIN
	epsfile = outdir + event + 'co_o3_ralt.eps'	
	pdffile = outdir + event + 'co_o3_ralt.pdf'	
	pngfile = outdir + event + 'co_o3_ralt.png'	
ENDIF

FILE_MKDIR, outdir

tracer1_conv   = [ ]
tracer2_conv   = [ ]
tracer3_conv   = [ ]
ralt_arr_conv  = [ ]
temparr_conv   = [ ]

tracer1_nconv   = [ ]
tracer2_nconv   = [ ]
tracer3_nconv   = [ ]
ralt_arr_nconv  = [ ]
temparr_nconv   = [ ]

hist_conv   = FLTARR(100,100)
hist_nconv  = FLTARR(100,100)

col_conv   = FLTARR(100,100)
col_nconv  = FLTARR(100,100)


FOREACH date, date_arr DO BEGIN
	PRINT, date

	IF KEYWORD_SET(updraft_tr) THEN BEGIN
		hist1_conv	= (WRF_READ_VAR('HIST_UPCONV'    , date, event, scheme, DOMAIN = domain, INDICES = region, TR_TR=1)).values
		hist1_nconv	= (WRF_READ_VAR('HIST_UPNCONV'   , date, event, scheme, DOMAIN = domain, INDICES = region, TR_TR=1)).values
	
		IF KEYWORD_SET(tracer_color) THEN BEGIN
			col1_conv	= (WRF_READ_VAR('TR_UPCONV'    , date, event, scheme, DOMAIN = domain, INDICES = region, TR_TR=1)).values
			col1_nconv	= (WRF_READ_VAR('TR_UPNCONV'   , date, event, scheme, DOMAIN = domain, INDICES = region, TR_TR=1)).values
		ENDIF

		IF KEYWORD_SET(ralt_color) THEN BEGIN
			col1_conv	= (WRF_READ_VAR('RALT_UPCONV'    , date, event, scheme, DOMAIN = domain, INDICES = region, TR_TR=1)).values
			col1_nconv	= (WRF_READ_VAR('RALT_UPNCONV'   , date, event, scheme, DOMAIN = domain, INDICES = region, TR_TR=1)).values
		ENDIF

		IF KEYWORD_SET(temp_color) THEN BEGIN
			col1_conv	= (WRF_READ_VAR('TEMP_UPCONV'    , date, event, scheme, DOMAIN = domain, INDICES = region, TR_TR=1)).values
			col1_nconv	= (WRF_READ_VAR('TEMP_UPNCONV'   , date, event, scheme, DOMAIN = domain, INDICES = region, TR_TR=1)).values
		ENDIF

		IF KEYWORD_SET(cloud_color) THEN BEGIN
			col1_conv	= (WRF_READ_VAR('CLOUD_UPCONV'    , date, event, scheme, DOMAIN = domain, INDICES = region, TR_TR=1)).values
			col1_nconv	= (WRF_READ_VAR('CLOUD_UPNCONV'   , date, event, scheme, DOMAIN = domain, INDICES = region, TR_TR=1)).values
		ENDIF
	ENDIF ELSE BEGIN
		hist1_conv	= (WRF_READ_VAR('HIST_CLDCONV'    , date, event, scheme, DOMAIN = domain, INDICES = region, TR_TR=1)).values
		hist1_nconv	= (WRF_READ_VAR('HIST_CLDNCONV'   , date, event, scheme, DOMAIN = domain, INDICES = region, TR_TR=1)).values
		
		IF KEYWORD_SET(tracer_color) THEN BEGIN
			col1_conv	= (WRF_READ_VAR('TR_CLDCONV'    , date, event, scheme, DOMAIN = domain, INDICES = region, TR_TR=1)).values
			col1_nconv	= (WRF_READ_VAR('TR_CLDNCONV'   , date, event, scheme, DOMAIN = domain, INDICES = region, TR_TR=1)).values
		ENDIF

		IF KEYWORD_SET(ralt_color) THEN BEGIN
			col1_conv	= (WRF_READ_VAR('RALT_CLDCONV'    , date, event, scheme, DOMAIN = domain, INDICES = region, TR_TR=1)).values
			col1_nconv	= (WRF_READ_VAR('RALT_CLDNCONV'   , date, event, scheme, DOMAIN = domain, INDICES = region, TR_TR=1)).values
		ENDIF

		IF KEYWORD_SET(temp_color) THEN BEGIN
			col1_conv	= (WRF_READ_VAR('TEMP_CLDCONV'    , date, event, scheme, DOMAIN = domain, INDICES = region, TR_TR=1)).values
			col1_nconv	= (WRF_READ_VAR('TEMP_CLDNCONV'   , date, event, scheme, DOMAIN = domain, INDICES = region, TR_TR=1)).values
		ENDIF

		IF KEYWORD_SET(cloud_color) THEN BEGIN
			col1_conv	= (WRF_READ_VAR('CLOUD_CLDCONV'    , date, event, scheme, DOMAIN = domain, INDICES = region, TR_TR=1)).values
			col1_nconv	= (WRF_READ_VAR('CLOUD_CLDNCONV'   , date, event, scheme, DOMAIN = domain, INDICES = region, TR_TR=1)).values
		ENDIF
		
	ENDELSE

	var_arr = ['O3','H2O','CO']
	factor = 1.0E6
	cfactr = 1.0E3
	xlog   = 1
	xrange = [1, 20000]
	yrange = [0,   600]
	zrange = [0,   200]
	xtitle = 'H2O (ppmv)'

	IF KEYWORD_SET(group2) THEN BEGIN
		xtitle = 'CO (ppbv)'
		xrange = [0, 200]
		yrange = [0, 600]
		factor = 1.0E3
		cfactr = 1.0E3
		xlog   = 0
	ENDIF

	title = ['Non-Conv.(cloud)','Conv.(cloud)']
	IF KEYWORD_SET(updraft_tr) THEN title = ['Non-Conv.(updraft)','Conv.(updraft)']

    ;tracer1_conv   = [tracer1_conv,  var1_arr_conv]
    ;tracer2_conv   = [tracer2_conv,  var2_arr_conv]
    ;tracer3_conv   = [tracer3_conv,  var3_arr_conv]	
    ;ralt_arr_conv  = [ralt_arr_conv, 	 ralt_conv]	
    ;temparr_conv   = [temparr_conv , temp_arr_conv]

    ;tracer1_nconv   = [tracer1_nconv , var1_arr_nconv]
    ;tracer2_nconv   = [tracer2_nconv , var2_arr_nconv]
    ;tracer3_nconv   = [tracer3_nconv , var3_arr_nconv]	
    ;ralt_arr_nconv  = [ralt_arr_nconv,     ralt_nconv]	
    ;temparr_nconv   = [temparr_nconv , temp_arr_nconv]
    
    ;hist1_conv	+= hist1_conv
	;hist1_nconv	+= hist1_nconv

    hist_conv	= hist_conv  + hist1_conv
	hist_nconv	= hist_nconv + hist1_nconv

    col_conv	= col_conv  + col1_conv
	col_nconv	= col_nconv + col1_nconv

	HELP, hist1_conv
	PRINT, TOTAL(hist1_conv,/NAN)
ENDFOREACH

conv_total = TOTAL(hist_conv)
c_nan = WHERE((hist_conv/conv_total)*100 LT 1.0E-5, cnan)
hist_conv[c_nan] = 0.0

nconv_total = TOTAL(hist_nconv)
nc_nan = WHERE((hist_nconv/nconv_total)*100 LT 1.0E-5, ncnan)
hist_nconv[nc_nan] = 0.0

IF KEYWORD_SET(zrelative) THEN BEGIN
	ztracer = ralt_arr_conv
	zrange  = [-2.5, 2.5]
	ztitle  = 'Tropopause Relative (km)'
	name    = 'zrelative'
ENDIF ELSE IF KEYWORD_SET(poten_temp) THEN BEGIN
	ztrsort = ralt_arr_conv
	ztracer = temparr_conv
	zrange  = [300,400]
	ztitle  = 'Potential Temperature (K)'
	name    = 'temp'
ENDIF

symsize = 1.0 - 0.25*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf))

xlog_scl = nxbin/(ALOG10(xrange[1]/xrange[0]))																	;Set bin parameters for logaritmic scale
dx       = FLOAT(xrange[1] - xrange[0])/nxbin																	;Set bin parameters for regular scale
xbin     = 0.5*dx + dx*FINDGEN(nxbin) + xrange[0]
ylog_scl = nybin/(ALOG10(yrange[1]/yrange[0]))
dy       = FLOAT(yrange[1] - yrange[0])/nybin
ybin     = 0.5*dy + yrange[0] + dy*FINDGEN(nybin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;For convective																		 	 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
layer_total_conv = REBIN(REFORM(TOTAL(hist_conv,1),1,nybin),nxbin,nybin)
freq_conv 		 = 100.0 * (FLOAT(hist_conv) / layer_total_conv)
inan  	         = WHERE(freq_conv  LT 1.0)
freq_conv[inan] = 0.0

pmax  = 100.0*(LONG(MEAN(hist_conv) + 2*STDDEV(hist_conv))/100 + 1)
table = GRAYSCALE_24(40, 0.85, 0.0, PS = ps)																		;Color table for plotting 
col_conv1   = COLOR_LOOKUP_24(freq_conv, table, MIN_VALUE = 0.0, MAX_VALUE = 15.0, MISSING = table[-1])
none  = WHERE((hist_conv EQ 0), none_count)
IF (none_count GT 0) THEN col_conv1[none] = COLOR_24('white')														;Set counts of zero to white

IF (KEYWORD_SET(tracer_color)) THEN BEGIN
;	tr_min = 30.0
;	tr_max = 180.0
	tr_min = 1.0
	tr_max = 500.0
	table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])
	col_conv1 = COLOR_LOOKUP_24(col_conv, table, MIN_VALUE = tr_min, MAX_VALUE = tr_max, MISSING = table[-1])
	none   = WHERE(col_conv EQ 0, none_count)
	IF (none_count GT 0) THEN col_conv1[none] = COLOR_24('white')		
ENDIF

IF (KEYWORD_SET(temp_color)) THEN BEGIN
	tr_min = 240.0
	tr_max = 420.0
	table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])
	col_conv1 = COLOR_LOOKUP_24(col_conv, table, MIN_VALUE = tr_min, MAX_VALUE = tr_max, MISSING = table[-1])
	none   = WHERE(col_conv EQ 0, none_count)
	IF (none_count GT 0) THEN col_conv1[none] = COLOR_24('white')		
ENDIF

IF (KEYWORD_SET(ralt_color)) THEN BEGIN
	tr_min = -15.0
	tr_max = 15.0
	table  = BLUE_RED_24(20, 0.1, 1.0, PS = ps)
;	table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])
	col_conv1 = COLOR_LOOKUP_24(col_conv, table, MIN_VALUE = tr_min, MAX_VALUE = tr_max, MISSING = table[-1])
	none   = WHERE(col_conv EQ 0, none_count)
	IF (none_count GT 0) THEN col_conv1[none] = COLOR_24('white')		
ENDIF

IF (KEYWORD_SET(cloud_color)) THEN BEGIN
	tr_min = 0.0001
	tr_max = 0.01
	table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])
	col_conv1 = COLOR_LOOKUP_24(col_conv, table, MIN_VALUE = tr_min, MAX_VALUE = tr_max, MISSING = table[-1])
	none   = WHERE(col_conv EQ 0, none_count)
	IF (none_count GT 0) THEN col_conv1[none] = COLOR_24('white')		
ENDIF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End convective																		 	 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;For non-convective																		 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
layer_total_nconv = REBIN(REFORM(TOTAL(hist_nconv,1),1,nybin),nxbin,nybin)
freq_nconv 	   	  = 100.0 * (FLOAT(hist_nconv) / layer_total_nconv)
inan  	          = WHERE(freq_nconv  LT 1.0)
freq_nconv[inan]  = 0.0

pmax  = 100.0*(LONG(MEAN(hist_nconv) + 2*STDDEV(hist_nconv))/100 + 1)
table = GRAYSCALE_24(40, 0.85, 0.0, PS = ps)																		;Color table for plotting 
col_nconv1   = COLOR_LOOKUP_24(freq_nconv, table, MIN_VALUE = 0.0, MAX_VALUE = 15.0, MISSING = table[-1])

none  = WHERE((hist_nconv EQ 0), none_count)
IF (none_count GT 0) THEN col_nconv1[none] = COLOR_24('white')														;Set counts of zero to white

IF (KEYWORD_SET(tracer_color)) THEN BEGIN
;	tr_min = 30.0
;	tr_max = 180.0
	tr_min = 1.0
	tr_max = 500.0
	table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])
	col_nconv1    = COLOR_LOOKUP_24(col_nconv, table, MIN_VALUE = tr_min, MAX_VALUE = tr_max, MISSING = table[-1])
	none   = WHERE(col_nconv EQ 0, none_count)
	IF (none_count GT 0) THEN col_nconv1[none] = COLOR_24('white')		
ENDIF	

IF (KEYWORD_SET(temp_color)) THEN BEGIN
	tr_min = 240.0
	tr_max = 420.0
	table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])
	col_nconv1    = COLOR_LOOKUP_24(col_nconv, table, MIN_VALUE = tr_min, MAX_VALUE = tr_max, MISSING = table[-1])
	none   = WHERE(col_nconv EQ 0, none_count)
	IF (none_count GT 0) THEN col_nconv1[none] = COLOR_24('white')		
ENDIF	

IF (KEYWORD_SET(ralt_color)) THEN BEGIN
	tr_min = -15.0
	tr_max = 15.0
	table  = BLUE_RED_24(20, 0.1, 1.0, PS = ps)
;	table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])
	col_nconv1    = COLOR_LOOKUP_24(col_nconv, table, MIN_VALUE = tr_min, MAX_VALUE = tr_max, MISSING = table[-1])
	none   = WHERE(col_nconv EQ 0, none_count)
	IF (none_count GT 0) THEN col_nconv1[none] = COLOR_24('white')		
ENDIF	

IF (KEYWORD_SET(cloud_color)) THEN BEGIN
	tr_min = 0.0000001
	tr_max = 0.1
	table  = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])
	col_nconv1    = COLOR_LOOKUP_24(col_nconv, table, MIN_VALUE = tr_min, MAX_VALUE = tr_max, MISSING = table[-1])
	none   = WHERE(col_nconv EQ 0, none_count)
	IF (none_count GT 0) THEN col_nconv1[none] = COLOR_24('white')		
ENDIF	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End non-convective																		 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


IF KEYWORD_SET(eps) THEN BEGIN	
	PS_ON, FILENAME = epsfile, PAGE_SIZE = [8.0, 3.0], MARGIN = 0.0, /INCHES					;Switch to Postscript device
	DEVICE, /ENCAPSULATED
	!P.FONT     = 0																				;Hardware fonts
	!P.CHARSIZE = 0.8
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS																		;Load basic color definitions
ENDIF ELSE BEGIN
	SET_PLOT, 'X'
	WINDOW, XSIZE = 1800, YSIZE = 600															;Open graphics window
	!P.COLOR      = COLOR_24('black')															;Foreground color
	!P.BACKGROUND = COLOR_24('white')															;Background color
	!P.CHARSIZE   = 2.9		
	!P.FONT       = -1																			;Use Hershey fonts
ENDELSE
!P.MULTI = [0, 3, 1]

xtracer2 = 0
ytracer2 = 0

TRACER_TRACER_DIFFERENCE, xtracer2, ytracer2, col_conv1, $
	TITLE      = title[1], $
	ZTRSORT    = ztrsort, $
	XRANGE     = xrange, $
	XTITLE     = xtitle, $
	YRANGE     = yrange, $
	YTITLE     = 'Ozone (ppbv)', $
	ZRANGE     = zrange, $
	ZTITLE     = ztitle, $
	NXBIN	   = nxbin, $
	NYBIN	   = nybin, $
    XBIN 	   = xbin, $
    YBIN	   = ybin, $
	DX		   = dx, $
	DY		   = dy, $
	BINNED     = 1, $
	XLOG	   = xlog, $
	TABLE	   = table, $
	CREVERSE   = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $		
	SYMSIZE    = symsize, $
	CHARSIZE   = 1.5, $
	NOWINDOW   = 1

;COLOR_BAR_24_KPB, table, UNDER = table[0], OVER = table[-1], $
;	RANGE = [0, 15], $
;	TITLE = 'Frequency (%)', $
;	MINOR = 1, $
;	POSIT = [0.07,0.08,0.27,0.1]
;!P.POSITION = 0

TRACER_TRACER_DIFFERENCE, xtracer2, ytracer2, col_nconv1, $
	TITLE      = title[0], $
	ZTRSORT    = ztrsort, $
	XRANGE     = xrange, $
	XTITLE     = xtitle, $
	YRANGE     = yrange, $
	YTITLE     = 'Ozone (ppbv)', $
	ZRANGE     = zrange, $
	ZTITLE     = ztitle, $
	NXBIN	   = nxbin, $
	NYBIN	   = nybin, $
    XBIN 	   = xbin, $
    YBIN	   = ybin, $
	DX		   = dx, $
	DY		   = dy, $
	BINNED     = 1, $
	XLOG	   = xlog, $
	TABLE	   = table, $
	CREVERSE   = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $		
	SYMSIZE    = symsize, $
	CHARSIZE   = 1.5, $
	NOWINDOW   = 1

;COLOR_BAR_24_KPB, table, UNDER = table[0], OVER = table[-1], $
;	RANGE = [0, 15], $
;	TITLE = 'Frequency (%)', $
;	MINOR = 1, $
;	POSIT = [0.415,0.08,0.615,0.1]
;!P.POSITION = 0

hist_diff = freq_conv - freq_nconv
hist_sum  = freq_conv + freq_nconv
	
layer_total = REBIN(REFORM(TOTAL(hist_sum,1),1,nybin),nxbin,nybin)
freq 		= 100.0 * (FLOAT(hist_diff) / layer_total)

pmax   = 25.0*(LONG(MEAN(hist_diff) + 2*STDDEV(hist_diff))/25 + 1)											;Calculate maximum count for each gas concentration w/in 2 STD DEV												
; table  = BLUE_RED_24(1000, 0.25, 1.0, PS = ps)													;Color table for plotting 		
table  = BLUE_RED_24(1000, 0.1, 1.0, PS = ps)
col    = COLOR_LOOKUP_24(freq, table, MIN_VALUE = -15.0, MAX_VALUE = 15.0, MISSING = table[-1])
        
none   = WHERE((hist_diff EQ 0), none_count)
IF (none_count GT 0) THEN col[none] = COLOR_24('white')											;Set counts of zero to white


TRACER_TRACER_DIFFERENCE, xtracer2, ytracer2, col, $
	TITLE      = "Frequency Difference", $
	ZTRSORT    = ztrsort, $
	XRANGE     = xrange, $
	XTITLE     = xtitle, $
	YRANGE     = yrange, $
	YTITLE     = 'Ozone (ppbv)', $
	ZRANGE     = zrange, $
	ZTITLE     = ztitle, $
	NXBIN	   = nxbin, $
	NYBIN	   = nybin, $
	XBIN 	   = xbin, $
 	YBIN	   = ybin, $
	DX		   = dx, $
	DY		   = dy, $
	BINNED     = 1, $
	XLOG	   = xlog, $
	TABLE	   = table, $
	CREVERSE   = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $		
	SYMSIZE    = symsize, $
	CHARSIZE   = 1.5, $
	NOWINDOW   = 1

;COLOR_BAR_24_KPB, table, UNDER = table[0], OVER = table[-1], $
;	RANGE = [-15, 15], $
;	TITLE = 'Frequency Difference (%)', $
;	MINOR = 1, $
;	POSIT = [0.755,0.08,0.955,0.1]
;!P.POSITION = 0
    

!P.MULTI = 0
IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN $
		PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS													;Convert eps to pdf
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file

END
