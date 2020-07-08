PRO WRITE_TRACER_TRACER_FILE, event, scheme, start_date, end_date, $
	ZRELATIVE  = zrelative, $
	POTEN_TEMP = poten_temp, $
	CLOUD      = cloud, $
	TR_COLOR   = tr_color, $
	GROUP2     = group2, $
	UPDRAFT_TR = updraft_tr, $
	FILTER	   = filter, $
	DIFFERENCE = difference, $
	DOMAIN     = domain, $
	XLOG	   = xlog, $
	TROPICS    = tropics, $
	MIDLAT     = midlat, $
	BINNED     = binned, $
	PNG	       = png


;+
; Name:
;		PLOT_MULTI_TRACER
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
IF (event EQ '20130805') THEN tropics = 1
IF (event EQ '20120519') THEN region = [50, 50, 250, 190]

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(event, scheme, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(event, scheme, start_date, end_date, /DATE)	

;IF KEYWORD_SET(eps) THEN BEGIN	
;	PS_ON, FILENAME = epsfile, PAGE_SIZE = [6.0, 4.0], MARGIN = 0.0, /INCHES					;Switch to Postscript device
;	DEVICE, /ENCAPSULATED
;	!P.FONT     = 0																				;Hardware fonts
;	!P.CHARSIZE = 0.8
;	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
;		LOAD_BASIC_COLORS																		;Load basic color definitions
;ENDIF ELSE BEGIN
;	SET_PLOT, 'X'
;	WINDOW, XSIZE = 1200, YSIZE = 800															;Open graphics window
;	!P.COLOR      = COLOR_24('black')															;Foreground color
;	!P.BACKGROUND = COLOR_24('white')															;Background color
;	!P.CHARSIZE   = 2.9		
;	!P.FONT       = -1																			;Use Hershey fonts
;ENDELSE
;!P.MULTI = [0, 2, 1]


z_arr 	    = [ ]																				;allocate arrays
xyz_troparr = [ ]
var1_arr    = [ ] 
var1_arr    = [ ] 
cld_arr	    = [ ]
cld_tr_arr  = [ ]
updraft_arr = [ ]
colorarr    = [ ]
temparr     = [ ]

var_arr = ['O3','H2O','CO']
factor = 1.0E6
cfactr = 1.0E3
xlog   = 1
xrange = [1, 20000]
yrange = [0,   600]
zrange = [0,   200]
xtitle = 'H2O (ppmv)'
;xtitle = 'CO (ppbv)'
;xrange = [0, 200]
;yrange = [0, 600]
;factor = 1.0E3
;cfactr = 1.0E3
;xlog   = 0

IF KEYWORD_SET(group2) THEN BEGIN
	xtitle = 'CO (ppbv)'
	xrange = [0, 200]
	zrange = [0, 400]
	factor = 1.0E3
	cfactr = 1.0E6
	xlog   = 0
ENDIF

title = ['Non-Conv.(cloud)','Conv.(cloud)']
IF KEYWORD_SET(updraft_tr) THEN title = ['Non-Conv.(updraft)','Conv.(updraft)']
IF (filter EQ 0) THEN title = [' ', 'All data points']

tracer1_upconv   = [ ]
tracer2_upconv   = [ ]
tracer3_upconv   = [ ]
ralt_arr_upconv  = [ ]
temparr_upconv   = [ ]

tracer1_upnconv   = [ ]
tracer2_upnconv   = [ ]
tracer3_upnconv   = [ ]
ralt_arr_upnconv  = [ ]
temparr_upnconv   = [ ]

tracer1_cldconv   = [ ]
tracer2_cldconv   = [ ]
tracer3_cldconv   = [ ]
ralt_arr_cldconv  = [ ]
temparr_cldconv   = [ ]

tracer1_cldnconv   = [ ]
tracer2_cldnconv   = [ ]
tracer3_cldnconv   = [ ]
ralt_arr_cldnconv  = [ ]
temparr_cldnconv   = [ ]

FOREACH date, date_arr DO BEGIN
	PRINT, date
	
	z		 = (WRF_READ_VAR('Z'     , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read height values
	dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
	z_trop   = (WRF_READ_VAR('Z_trop', date, event, scheme, DOMAIN = domain, INDICES = region)).values
	z_trop   = MEDIAN(z_trop, 30) 												;Filter tropopause values
	xyz_trop = REBIN(z_trop, dim[0], dim[1], dim[2], /SAMPLE)

 	z_arr       = z       														
	xyz_troparr = xyz_trop	

	cld_arr			= ((((WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, DOMAIN = domain)).values) * 1.0E3) GT 0.01)
	  	
	
	var1_arr    = (WRF_READ_VAR(var_arr[0]      , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values    
	var3_arr	= (WRF_READ_VAR(var_arr[1]		, date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read carbon monoxide data
   	var2_arr	= (WRF_READ_VAR(var_arr[2]		, date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read water vapor data 

    cld_tr_arr  = (WRF_READ_VAR('Cloud_tracer'	, date, event, scheme, DOMAIN = domain, INDICES = region)).values
    updraft_arr = (WRF_READ_VAR('Updraft_tracer', date, event, scheme, DOMAIN = domain, INDICES = region)).values
    	
	temp_arr    = (WRF_READ_VAR('T', date, event, scheme, DOMAIN = domain, INDICES = region)).values 				
	temp_arr    = ((1000.0/(WRF_READ_VAR('P', date, event, scheme, $						;Compute potential temperature
	 					DOMAIN = domain)).values)^(!Rair/!Cp))*(temp_arr)
	ralt = 0.001*(z_arr - xyz_troparr)

	count = 0
 	IF (KEYWORD_SET(tropics)) THEN igood = WHERE(xyz_troparr GE 14000.0, count, COMPLEMENT = ibad)
 	IF (KEYWORD_SET(mid_lat)) THEN igood = WHERE(xyz_troparr LT 14000.0, count, COMPLEMENT = ibad)

	IF (count GT 0) THEN BEGIN
        z           [ibad] = !Values.F_NaN
        ralt        [ibad] = !Values.F_NaN
        var1_arr    [ibad] = !Values.F_NaN
        var2_arr    [ibad] = !Values.F_NaN
        var3_arr    [ibad] = !Values.F_NaN
        cld_arr     [ibad] = !Values.F_NaN
        cld_tr_arr  [ibad] = !Values.F_NaN
        updraft_arr [ibad] = !Values.F_NaN
        temp_arr    [ibad] = !Values.F_NaN
    ENDIF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Partition convective and non-convective 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		

   conv_values = WHERE(updraft_arr GT 0.1, conv_count, COMPLEMENT = non_conv, $							;Find values in cloud 
                    	NCOMPLEMENT = nconv_count)
                    
   IF (conv_count GT 0) THEN BEGIN
		var1_arr_upconv = var1_arr[conv_values]
    	var2_arr_upconv = var2_arr[conv_values]
    	var3_arr_upconv = var3_arr[conv_values]
    	ralt_upconv     = ralt    [conv_values]
   	  	temp_arr_upconv = temp_arr[conv_values] 
        cld_arr_upconv  = cld_arr [conv_values]
   ENDIF
   IF (nconv_count GT 0) THEN BEGIN
       var1_arr_upnconv  = var1_arr[non_conv]
       var2_arr_upnconv  = var2_arr[non_conv]
       var3_arr_upnconv  = var3_arr[non_conv]
       ralt_upnconv      = ralt    [non_conv]
       temp_arr_upnconv  = temp_arr[non_conv] 
       cld_arr_upnconv   = cld_arr [non_conv]
   ENDIF
              
	cld_tr_values =  WHERE(cld_tr_arr GT 0.1, cld_count, COMPLEMENT = non_cld, $							;Find values in cloud 
                    			NCOMPLEMENT = ncld_count)
                    					
    IF (cld_count GT 0) THEN BEGIN
        var1_arr_cldconv = var1_arr[cld_tr_values]
        var2_arr_cldconv = var2_arr[cld_tr_values]
        var3_arr_cldconv = var3_arr[cld_tr_values]
        ralt_cldconv     = ralt    [cld_tr_values]
   	    temp_arr_cldconv = temp_arr[cld_tr_values] 
        cld_arr_cldconv  = cld_arr [cld_tr_values]
    ENDIF
    IF (ncld_count GT 0) THEN BEGIN
        var1_arr_cldnconv = var1_arr[non_cld]
   		var2_arr_cldnconv = var2_arr[non_cld]
   		var3_arr_cldnconv = var3_arr[non_cld]
        ralt_cldnconv     = ralt    [non_cld]
   	    temp_arr_cldnconv = temp_arr[non_cld] 
        cld_arr_cldnconv  = cld_arr [non_cld]
   ENDIF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End partition convective and non-convective 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Histogram for Updraft-Convective 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
xtracer2 = var2_arr_upconv*1.0E3
ytracer2 = var1_arr_upconv*1.0E3
ztracer2 = var3_arr_upconv*1.0E6

xlog_scl = nxbin/(ALOG10(xrange[1]/xrange[0]))																	;Set bin parameters for logaritmic scale
dx       = FLOAT(xrange[1] - xrange[0])/nxbin																	;Set bin parameters for regular scale
xbin     = 0.5*dx + dx*FINDGEN(nxbin) + xrange[0]
ylog_scl = nybin/(ALOG10(yrange[1]/yrange[0]))
dy       = FLOAT(yrange[1] - yrange[0])/nybin
ybin     = 0.5*dy + yrange[0] + dy*FINDGEN(nybin)

IF KEYWORD_SET(xlog) AND KEYWORD_SET(ylog) THEN $
	bin = LONG(xlog_scl*ALOG10(xtracer2/xrange[0])) + nxbin*LONG(ylog_scl*ALOG10(ytracer2/yrange[0])) $		;Bin data for histogram
ELSE IF KEYWORD_SET(xlog) THEN $
	bin = LONG(xlog_scl*ALOG10(xtracer2/xrange[0])) + nxbin*LONG((ytracer2-yrange[0])/dy) $
ELSE IF KEYWORD_SET(ylog) THEN $
	bin = LONG((xtracer2-xrange[0])/dx) + nxbin*LONG(ylog_scl*ALOG10(ytracer2/yrange[0])) $
ELSE $
	bin = LONG((xtracer2-xrange[0])/dx) + nxbin*LONG((ytracer2-yrange[0])/dy)

ztracer_bin = FLTARR(10000)
temp_bin    = FLTARR(10000)
ralt_bin    = FLTARR(10000)
cld_bin     = FLTARR(10000)
FOR ii = 0, N_ELEMENTS(xtracer2)-1 DO IF ((bin[ii] GE 0) AND (bin[ii] LE 10000)) THEN BEGIN
	IF (FINITE(ztracer2[ii]        )) THEN ztracer_bin[bin[ii]] += ztracer2[ii]
	IF (FINITE(temp_arr_upconv[ii] )) THEN temp_bin   [bin[ii]] += temp_arr_upconv[ii]
	IF (FINITE(ralt_upconv[ii]     )) THEN ralt_bin   [bin[ii]] += ralt_upconv[ii]
	IF (FINITE(cld_arr_upconv[ii]  )) THEN cld_bin    [bin[ii]] += cld_arr_upconv[ii]
ENDIF
	
hist = HISTOGRAM(bin, BINSIZE = 1, MIN = 0, MAX = (nxbin*nybin -1))										;Calculate density
hist_upconv = REFORM(hist, nxbin, nybin)

ztracer_mean_upconv = ztracer_bin/hist
izero = WHERE(FINITE(ztracer_mean_upconv,/NAN))
ztracer_mean_upconv[izero] = 0.0
ztracer_mean_upconv = REFORM(ztracer_mean_upconv, nxbin, nybin)

temp_mean_upconv = temp_bin/hist
izero = WHERE(FINITE(temp_mean_upconv,/NAN))
temp_mean_upconv[izero] = 0.0
temp_mean_upconv = REFORM(temp_mean_upconv, nxbin, nybin)

ralt_mean_upconv = ralt_bin/hist
izero = WHERE(FINITE(ralt_mean_upconv,/NAN))
ralt_mean_upconv[izero] = 0.0
ralt_mean_upconv = REFORM(ralt_mean_upconv, nxbin, nybin)

cld_mean_upconv = cld_bin/hist
izero = WHERE(FINITE(cld_mean_upconv,/NAN))
cld_mean_upconv[izero] = 0.0
cld_mean_upconv = REFORM(cld_mean_upconv, nxbin, nybin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End Histogram for Updraft-Convective 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Histogram for Updraft-NonConvective 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
xtracer2 = var2_arr_upnconv*1.0E3
ytracer2 = var1_arr_upnconv*1.0E3
ztracer2 = var3_arr_upnconv*1.0E6

xlog_scl = nxbin/(ALOG10(xrange[1]/xrange[0]))																	;Set bin parameters for logaritmic scale
dx       = FLOAT(xrange[1] - xrange[0])/nxbin																	;Set bin parameters for regular scale
xbin     = 0.5*dx + dx*FINDGEN(nxbin) + xrange[0]
ylog_scl = nybin/(ALOG10(yrange[1]/yrange[0]))
dy       = FLOAT(yrange[1] - yrange[0])/nybin
ybin     = 0.5*dy + yrange[0] + dy*FINDGEN(nybin)

IF KEYWORD_SET(xlog) AND KEYWORD_SET(ylog) THEN $
	bin = LONG(xlog_scl*ALOG10(xtracer2/xrange[0])) + nxbin*LONG(ylog_scl*ALOG10(ytracer2/yrange[0])) $		;Bin data for histogram
ELSE IF KEYWORD_SET(xlog) THEN $
	bin = LONG(xlog_scl*ALOG10(xtracer2/xrange[0])) + nxbin*LONG((ytracer2-yrange[0])/dy) $
ELSE IF KEYWORD_SET(ylog) THEN $
	bin = LONG((xtracer2-xrange[0])/dx) + nxbin*LONG(ylog_scl*ALOG10(ytracer2/yrange[0])) $
ELSE $
	bin = LONG((xtracer2-xrange[0])/dx) + nxbin*LONG((ytracer2-yrange[0])/dy)

ztracer_bin = FLTARR(10000)
temp_bin    = FLTARR(10000)
ralt_bin    = FLTARR(10000)
cld_bin     = FLTARR(10000)
FOR ii = 0, N_ELEMENTS(xtracer2)-1 DO IF ((bin[ii] GE 0) AND (bin[ii] LE 10000)) THEN BEGIN
	IF (FINITE(ztracer2[ii]         )) THEN ztracer_bin[bin[ii]] += ztracer2[ii]
	IF (FINITE(temp_arr_upnconv[ii] )) THEN temp_bin   [bin[ii]] += temp_arr_upnconv[ii]
	IF (FINITE(ralt_upnconv[ii]     )) THEN ralt_bin   [bin[ii]] += ralt_upnconv[ii]
	IF (FINITE(cld_arr_upnconv[ii]  )) THEN cld_bin    [bin[ii]] += cld_arr_upnconv[ii]
ENDIF

hist = HISTOGRAM(bin, BINSIZE = 1, MIN = 0, MAX = (nxbin*nybin -1))										;Calculate density
hist_upnconv = REFORM(hist, nxbin, nybin)

ztracer_mean_upnconv = ztracer_bin/hist
izero = WHERE(FINITE(ztracer_mean_upnconv,/NAN))
ztracer_mean_upnconv[izero] = 0.0
ztracer_mean_upnconv = REFORM(ztracer_mean_upnconv, nxbin, nybin)

temp_mean_upnconv = temp_bin/hist
izero = WHERE(FINITE(temp_mean_upnconv,/NAN))
temp_mean_upnconv[izero] = 0.0
temp_mean_upnconv = REFORM(temp_mean_upnconv, nxbin, nybin)

ralt_mean_upnconv = ralt_bin/hist
izero = WHERE(FINITE(ralt_mean_upnconv,/NAN))
ralt_mean_upnconv[izero] = 0.0
ralt_mean_upnconv = REFORM(ralt_mean_upnconv, nxbin, nybin)

cld_mean_upnconv = cld_bin/hist
izero = WHERE(FINITE(cld_mean_upnconv,/NAN))
cld_mean_upnconv[izero] = 0.0
cld_mean_upnconv = REFORM(cld_mean_upnconv, nxbin, nybin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End Histogram for Updraft-NonConvective 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Histogram for Cloud-Convective 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
xtracer2 = var2_arr_cldconv*1.0E3
ytracer2 = var1_arr_cldconv*1.0E3
ztracer2 = var3_arr_cldconv*1.0E6

xlog_scl = nxbin/(ALOG10(xrange[1]/xrange[0]))																	;Set bin parameters for logaritmic scale
dx       = FLOAT(xrange[1] - xrange[0])/nxbin																	;Set bin parameters for regular scale
xbin     = 0.5*dx + dx*FINDGEN(nxbin) + xrange[0]
ylog_scl = nybin/(ALOG10(yrange[1]/yrange[0]))
dy       = FLOAT(yrange[1] - yrange[0])/nybin
ybin     = 0.5*dy + yrange[0] + dy*FINDGEN(nybin)

IF KEYWORD_SET(xlog) AND KEYWORD_SET(ylog) THEN $
	bin = LONG(xlog_scl*ALOG10(xtracer2/xrange[0])) + nxbin*LONG(ylog_scl*ALOG10(ytracer2/yrange[0])) $		;Bin data for histogram
ELSE IF KEYWORD_SET(xlog) THEN $
	bin = LONG(xlog_scl*ALOG10(xtracer2/xrange[0])) + nxbin*LONG((ytracer2-yrange[0])/dy) $
ELSE IF KEYWORD_SET(ylog) THEN $
	bin = LONG((xtracer2-xrange[0])/dx) + nxbin*LONG(ylog_scl*ALOG10(ytracer2/yrange[0])) $
ELSE $
	bin = LONG((xtracer2-xrange[0])/dx) + nxbin*LONG((ytracer2-yrange[0])/dy)

ztracer_bin = FLTARR(10000)
temp_bin    = FLTARR(10000)
ralt_bin    = FLTARR(10000)
cld_bin     = FLTARR(10000)
FOR ii = 0, N_ELEMENTS(xtracer2)-1 DO IF ((bin[ii] GE 0) AND (bin[ii] LE 10000)) THEN BEGIN
	IF (FINITE(ztracer2[ii]         )) THEN ztracer_bin[bin[ii]] += ztracer2[ii]
	IF (FINITE(temp_arr_cldconv[ii] )) THEN temp_bin   [bin[ii]] += temp_arr_cldconv[ii]
	IF (FINITE(ralt_cldconv[ii]     )) THEN ralt_bin   [bin[ii]] += ralt_cldconv[ii]
	IF (FINITE(cld_arr_cldconv[ii]  )) THEN cld_bin    [bin[ii]] += cld_arr_cldconv[ii]
ENDIF

hist = HISTOGRAM(bin, BINSIZE = 1, MIN = 0, MAX = (nxbin*nybin -1))										;Calculate density
hist_cldconv = REFORM(hist, nxbin, nybin)

ztracer_mean_cldconv = ztracer_bin/hist
izero = WHERE(FINITE(ztracer_mean_cldconv,/NAN))
ztracer_mean_cldconv[izero] = 0.0
ztracer_mean_cldconv = REFORM(ztracer_mean_cldconv, nxbin, nybin)

temp_mean_cldconv = temp_bin/hist
izero = WHERE(FINITE(temp_mean_cldconv,/NAN))
temp_mean_cldconv[izero] = 0.0
temp_mean_cldconv = REFORM(temp_mean_cldconv, nxbin, nybin)

ralt_mean_cldconv = ralt_bin/hist
izero = WHERE(FINITE(ralt_mean_cldconv,/NAN))
ralt_mean_cldconv[izero] = 0.0
ralt_mean_cldconv = REFORM(ralt_mean_cldconv, nxbin, nybin)

cld_mean_cldconv = cld_bin/hist
izero = WHERE(FINITE(cld_mean_cldconv,/NAN))
cld_mean_cldconv[izero] = 0.0
cld_mean_cldconv = REFORM(cld_mean_cldconv, nxbin, nybin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End Histogram for Cloud-Convective 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Histogram for Cloud-NonConvective 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
xtracer2 = var2_arr_cldnconv*1.0E3
ytracer2 = var1_arr_cldnconv*1.0E3
ztracer2 = var3_arr_cldnconv*1.0E6

xlog_scl = nxbin/(ALOG10(xrange[1]/xrange[0]))																	;Set bin parameters for logaritmic scale
dx       = FLOAT(xrange[1] - xrange[0])/nxbin																	;Set bin parameters for regular scale
xbin     = 0.5*dx + dx*FINDGEN(nxbin) + xrange[0]
ylog_scl = nybin/(ALOG10(yrange[1]/yrange[0]))
dy       = FLOAT(yrange[1] - yrange[0])/nybin
ybin     = 0.5*dy + yrange[0] + dy*FINDGEN(nybin)

IF KEYWORD_SET(xlog) AND KEYWORD_SET(ylog) THEN $
	bin = LONG(xlog_scl*ALOG10(xtracer2/xrange[0])) + nxbin*LONG(ylog_scl*ALOG10(ytracer2/yrange[0])) $		;Bin data for histogram
ELSE IF KEYWORD_SET(xlog) THEN $
	bin = LONG(xlog_scl*ALOG10(xtracer2/xrange[0])) + nxbin*LONG((ytracer2-yrange[0])/dy) $
ELSE IF KEYWORD_SET(ylog) THEN $
	bin = LONG((xtracer2-xrange[0])/dx) + nxbin*LONG(ylog_scl*ALOG10(ytracer2/yrange[0])) $
ELSE $
	bin = LONG((xtracer2-xrange[0])/dx) + nxbin*LONG((ytracer2-yrange[0])/dy)

ztracer_bin = FLTARR(10000)
temp_bin    = FLTARR(10000)
ralt_bin    = FLTARR(10000)
cld_bin     = FLTARR(10000)
FOR ii = 0, N_ELEMENTS(xtracer2)-1 DO IF ((bin[ii] GE 0) AND (bin[ii] LE 10000)) THEN BEGIN
	 IF (FINITE(ztracer2[ii]          )) THEN ztracer_bin[bin[ii]] += ztracer2[ii]
	 IF (FINITE(temp_arr_cldnconv[ii] )) THEN temp_bin   [bin[ii]] += temp_arr_cldnconv[ii]
	 IF (FINITE(ralt_cldnconv[ii]     )) THEN ralt_bin   [bin[ii]] += ralt_cldnconv[ii]
	 IF (FINITE(cld_arr_cldnconv[ii]  )) THEN cld_bin    [bin[ii]] += cld_arr_cldnconv[ii]
ENDIF

hist = HISTOGRAM(bin, BINSIZE = 1, MIN = 0, MAX = (nxbin*nybin -1))										;Calculate density
hist_cldnconv = REFORM(hist, nxbin, nybin)

ztracer_mean_cldnconv = ztracer_bin/hist
izero = WHERE(FINITE(ztracer_mean_cldnconv,/NAN))
ztracer_mean_cldnconv[izero] = 0.0
ztracer_mean_cldnconv = REFORM(ztracer_mean_cldnconv, nxbin, nybin)

temp_mean_cldnconv = temp_bin/hist
izero = WHERE(FINITE(temp_mean_cldnconv,/NAN))
temp_mean_cldnconv[izero] = 0.0
temp_mean_cldnconv = REFORM(temp_mean_cldnconv, nxbin, nybin)

ralt_mean_cldnconv = ralt_bin/hist
izero = WHERE(FINITE(ralt_mean_cldnconv,/NAN))
ralt_mean_cldnconv[izero] = 0.0
ralt_mean_cldnconv = REFORM(ralt_mean_cldnconv, nxbin, nybin)

cld_mean_cldnconv = cld_bin/hist
izero = WHERE(FINITE(cld_mean_cldnconv,/NAN))
cld_mean_cldnconv[izero] = 0.0
cld_mean_cldnconv = REFORM(cld_mean_cldnconv, nxbin, nybin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End Histogram for Cloud-NonConvective 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Write to file here for all variables and non-conv, conv, % diff here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
PRINT, 'start writing file'
;;Write values to file for later plotting (using plot_overshoot_relationships.pro)
IF (domain EQ 1) THEN domain1 = 'd01'
time    = MAKE_ISO_DATE_STRING(date, PRECISION='minute', /COMPACT, /UTC)
infile  = !WRF_DIRECTORY + event + '/' + scheme + '/reduced/' + domain1 + '_' + time + '.nc'						;Set input filepath
outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/reduced/tracer_tracer_files_co_o3/'
outfile = STRMID(infile, 16, /REVERSE_OFFSET)														;Set output file name
FILE_MKDIR, outdir

iid = NCDF_OPEN(infile)																						;Open input file for reading
NCDF_ATTGET, iid, 'DX', dx, /GLOBAL																		;Read grid resolution
NCDF_ATTGET, iid, 'DT', dt, /GLOBAL																		;Read grid resolution

dim = SIZE(hist_upconv, /DIMENSIONS)																			;Get grid dimension sizes
CATCH, error_status																								;Catch any errors with netcdf control or file creation

IF (error_status NE 0) THEN BEGIN
	NCDF_CLOSE, oid																								;Close previous failed file
	oid = NCDF_CREATE(outdir + outfile, CLOBBER = 1, /NETCDF3_64BIT)								;Create output file for writing
ENDIF ELSE $
	oid = NCDF_CREATE(outdir + outfile, CLOBBER = 1, /NETCDF3_64BIT)								;Create output file for writing

xid  = NCDF_DIMDEF(oid, 'x', dim[0])
yid  = NCDF_DIMDEF(oid, 'y', dim[1])
tid  = NCDF_DIMDEF(oid, 't', 14    )	

vid = NCDF_VARDEF(oid, 'Time', [tid], /CHAR)																;Define the time variable
NCDF_ATTPUT, oid, 'Time', 'long_name', 'ISO Date String'												;Name attribute
NCDF_ATTPUT, oid, 'Time', 'units',     'YYYYMMDD_HHMM_'												;Units attribute

vid = NCDF_VARDEF(oid, 'HIST_UPCONV', [xid, yid], /FLOAT)													;Define the latitude variable
NCDF_ATTPUT, oid, 'HIST_UPCONV', 'long_name', 'HIST_UPCONV'						;Name attribute
NCDF_ATTPUT, oid, 'HIST_UPCONV', 'units',     'Counts (#)'												;Units attribute

vid = NCDF_VARDEF(oid, 'HIST_UPNCONV', [xid, yid], /FLOAT)													;Define the latitude variable
NCDF_ATTPUT, oid, 'HIST_UPNCONV', 'long_name', 'HIST_UPNCONV'						;Name attribute
NCDF_ATTPUT, oid, 'HIST_UPNCONV', 'units',     'Counts (#)'												;Units attribute

vid = NCDF_VARDEF(oid, 'HIST_CLDCONV', [xid, yid], /FLOAT)													;Define the latitude variable
NCDF_ATTPUT, oid, 'HIST_CLDCONV', 'long_name', 'HIST_CLDCONV'						;Name attribute
NCDF_ATTPUT, oid, 'HIST_CLDCONV', 'units',     'Counts (#)'												;Units attribute

vid = NCDF_VARDEF(oid, 'HIST_CLDNCONV', [xid, yid], /FLOAT)													;Define the latitude variable
NCDF_ATTPUT, oid, 'HIST_CLDNCONV', 'long_name', 'HIST_CLDNCONV'						;Name attribute
NCDF_ATTPUT, oid, 'HIST_CLDNCONV', 'units',     'Counts (#)'												;Units attribute

vid = NCDF_VARDEF(oid, 'TR_UPCONV', [xid, yid], /FLOAT)													;Define the latitude variable
NCDF_ATTPUT, oid, 'TR_UPCONV', 'long_name', 'TR_UPCONV'						;Name attribute
NCDF_ATTPUT, oid, 'TR_UPCONV', 'units',     'Tracer Mean Value'												;Units attribute

vid = NCDF_VARDEF(oid, 'TR_UPNCONV', [xid, yid], /FLOAT)													;Define the latitude variable
NCDF_ATTPUT, oid, 'TR_UPNCONV', 'long_name', 'TR_UPNCONV'						;Name attribute
NCDF_ATTPUT, oid, 'TR_UPNCONV', 'units',     'Tracer Mean Value'												;Units attribute

vid = NCDF_VARDEF(oid, 'TR_CLDCONV', [xid, yid], /FLOAT)													;Define the latitude variable
NCDF_ATTPUT, oid, 'TR_CLDCONV', 'long_name', 'TR_CLDCONV'						;Name attribute
NCDF_ATTPUT, oid, 'TR_CLDCONV', 'units',     'Tracer Mean Value'												;Units attribute

vid = NCDF_VARDEF(oid, 'TR_CLDNCONV', [xid, yid], /FLOAT)													;Define the latitude variable
NCDF_ATTPUT, oid, 'TR_CLDNCONV', 'long_name', 'TR_CLDNCONV'						;Name attribute
NCDF_ATTPUT, oid, 'TR_CLDNCONV', 'units',     'Tracer Mean Value'												;Units attribute

vid = NCDF_VARDEF(oid, 'TEMP_UPCONV', [xid, yid], /FLOAT)													;Define the latitude variable
NCDF_ATTPUT, oid, 'TEMP_UPCONV', 'long_name', 'TEMP_UPCONV'						;Name attribute
NCDF_ATTPUT, oid, 'TEMP_UPCONV', 'units',     'Potential Temperature Mean Value (K)'												;Units attribute

vid = NCDF_VARDEF(oid, 'TEMP_UPNCONV', [xid, yid], /FLOAT)													;Define the latitude variable
NCDF_ATTPUT, oid, 'TEMP_UPNCONV', 'long_name', 'TEMP_UPNCONV'						;Name attribute
NCDF_ATTPUT, oid, 'TEMP_UPNCONV', 'units',     'Potential Temperature Mean Value (K)'												;Units attribute

vid = NCDF_VARDEF(oid, 'TEMP_CLDCONV', [xid, yid], /FLOAT)													;Define the latitude variable
NCDF_ATTPUT, oid, 'TEMP_CLDCONV', 'long_name', 'TEMP_CLDCONV'						;Name attribute
NCDF_ATTPUT, oid, 'TEMP_CLDCONV', 'units',     'Potential Temperature Mean Value (K)'												;Units attribute

vid = NCDF_VARDEF(oid, 'TEMP_CLDNCONV', [xid, yid], /FLOAT)													;Define the latitude variable
NCDF_ATTPUT, oid, 'TEMP_CLDNCONV', 'long_name', 'TEMP_CLDNCONV'						;Name attribute
NCDF_ATTPUT, oid, 'TEMP_CLDNCONV', 'units',     'Potential Temperature Mean Value (K)'												;Units attribute

vid = NCDF_VARDEF(oid, 'RALT_UPCONV', [xid, yid], /FLOAT)													;Define the latitude variable
NCDF_ATTPUT, oid, 'RALT_UPCONV', 'long_name', 'RALT_UPCONV'						;Name attribute
NCDF_ATTPUT, oid, 'RALT_UPCONV', 'units',     'Tropopause-Relative Altitude Mean Value (km)'												;Units attribute

vid = NCDF_VARDEF(oid, 'RALT_UPNCONV', [xid, yid], /FLOAT)													;Define the latitude variable
NCDF_ATTPUT, oid, 'RALT_UPNCONV', 'long_name', 'RALT_UPNCONV'						;Name attribute
NCDF_ATTPUT, oid, 'RALT_UPNCONV', 'units',     'Tropopause-Relative Altitude Mean Value (km)'												;Units attribute

vid = NCDF_VARDEF(oid, 'RALT_CLDCONV', [xid, yid], /FLOAT)													;Define the latitude variable
NCDF_ATTPUT, oid, 'RALT_CLDCONV', 'long_name', 'RALT_CLDCONV'						;Name attribute
NCDF_ATTPUT, oid, 'RALT_CLDCONV', 'units',     'Tropopause-Relative Altitude Mean Value (km)'												;Units attribute

vid = NCDF_VARDEF(oid, 'RALT_CLDNCONV', [xid, yid], /FLOAT)													;Define the latitude variable
NCDF_ATTPUT, oid, 'RALT_CLDNCONV', 'long_name', 'RALT_CLDNCONV'						;Name attribute
NCDF_ATTPUT, oid, 'RALT_CLDNCONV', 'units',     'Tropopause-Relative Altitude Mean Value (km)'												;Units attribute

vid = NCDF_VARDEF(oid, 'CLOUD_UPCONV', [xid, yid], /FLOAT)													;Define the latitude variable
NCDF_ATTPUT, oid, 'CLOUD_UPCONV', 'long_name', 'CLOUD_UPCONV'						;Name attribute
NCDF_ATTPUT, oid, 'CLOUD_UPCONV', 'units',     'Cloud Concentration Mean Value (g/kg)'												;Units attribute

vid = NCDF_VARDEF(oid, 'CLOUD_UPNCONV', [xid, yid], /FLOAT)													;Define the latitude variable
NCDF_ATTPUT, oid, 'CLOUD_UPNCONV', 'long_name', 'CLOUD_UPNCONV'						;Name attribute
NCDF_ATTPUT, oid, 'CLOUD_UPNCONV', 'units',     'Cloud Concentration Mean Value (g/kg)'												;Units attribute

vid = NCDF_VARDEF(oid, 'CLOUD_CLDCONV', [xid, yid], /FLOAT)													;Define the latitude variable
NCDF_ATTPUT, oid, 'CLOUD_CLDCONV', 'long_name', 'CLOUD_CLDCONV'						;Name attribute
NCDF_ATTPUT, oid, 'CLOUD_CLDCONV', 'units',     'Cloud Concentration Mean Value (g/kg)'												;Units attribute

vid = NCDF_VARDEF(oid, 'CLOUD_CLDNCONV', [xid, yid], /FLOAT)													;Define the latitude variable
NCDF_ATTPUT, oid, 'CLOUD_CLDNCONV', 'long_name', 'CLOUD_CLDNCONV'						;Name attribute
NCDF_ATTPUT, oid, 'CLOUD_CLDNCONV', 'units',     'Cloud Concentration Mean Value (g/kg)'												;Units attribute

NCDF_ATTPUT, oid, 'DX', dx, /GLOBAL
NCDF_ATTPUT, oid, 'DT', dt, /GLOBAL

PRINT, 'Done creating variables'
NCDF_CONTROL, oid, /ENDEF

PRINT, 'Output file closed successfully' 

NCDF_VARPUT, oid, 'Time', STRMID(outfile, 0, 14)														;Write date string to file
NCDF_VARPUT, oid, 'HIST_UPCONV'   , hist_upconv
NCDF_VARPUT, oid, 'HIST_UPNCONV'  , hist_upnconv
NCDF_VARPUT, oid, 'HIST_CLDCONV'  , hist_cldconv
NCDF_VARPUT, oid, 'HIST_CLDNCONV' , hist_cldnconv

NCDF_VARPUT, oid, 'TR_UPCONV'   , ztracer_mean_upconv
NCDF_VARPUT, oid, 'TR_UPNCONV'  , ztracer_mean_upnconv
NCDF_VARPUT, oid, 'TR_CLDCONV'  , ztracer_mean_cldconv
NCDF_VARPUT, oid, 'TR_CLDNCONV' , ztracer_mean_cldnconv

NCDF_VARPUT, oid, 'TEMP_UPCONV'   , temp_mean_upconv
NCDF_VARPUT, oid, 'TEMP_UPNCONV'  , temp_mean_upnconv
NCDF_VARPUT, oid, 'TEMP_CLDCONV'  , temp_mean_cldconv
NCDF_VARPUT, oid, 'TEMP_CLDNCONV' , temp_mean_cldnconv

NCDF_VARPUT, oid, 'RALT_UPCONV'   , ralt_mean_upconv
NCDF_VARPUT, oid, 'RALT_UPNCONV'  , ralt_mean_upnconv
NCDF_VARPUT, oid, 'RALT_CLDCONV'  , ralt_mean_cldconv
NCDF_VARPUT, oid, 'RALT_CLDNCONV' , ralt_mean_cldnconv

NCDF_VARPUT, oid, 'CLOUD_UPCONV'   , cld_mean_upconv
NCDF_VARPUT, oid, 'CLOUD_UPNCONV'  , cld_mean_upnconv
NCDF_VARPUT, oid, 'CLOUD_CLDCONV'  , cld_mean_cldconv
NCDF_VARPUT, oid, 'CLOUD_CLDNCONV' , cld_mean_cldnconv

;NCDF_VARPUT, oid, 'O3_UPCONV'   , var2_arr_upconv
;NCDF_VARPUT, oid, 'O3_UPNCONV'  , var2_arr_upnconv
;NCDF_VARPUT, oid, 'O3_CLDCONV'  , var2_arr_cldconv
;NCDF_VARPUT, oid, 'O3_CLDNCONV' , var2_arr_cldnconv

;NCDF_VARPUT, oid, 'CO_UPCONV'   , var3_arr_upconv
;NCDF_VARPUT, oid, 'CO_UPNCONV'  , var3_arr_upnconv
;NCDF_VARPUT, oid, 'CO_CLDCONV'  , var3_arr_cldconv
;NCDF_VARPUT, oid, 'CO_CLDNCONV' , var3_arr_cldnconv

;NCDF_VARPUT, oid, 'RALT_UPCONV'   , ralt_upconv
;NCDF_VARPUT, oid, 'RALT_UPNCONV'  , ralt_upnconv
;NCDF_VARPUT, oid, 'RALT_CLDCONV'  , ralt_cldconv
;NCDF_VARPUT, oid, 'RALT_CLDNCONV' , ralt_cldnconv

;NCDF_VARPUT, oid, 'THETA_UPCONV'   , temp_arr_upconv
;NCDF_VARPUT, oid, 'THETA_UPNCONV'  , temp_arr_upnconv
;NCDF_VARPUT, oid, 'THETA_CLDCONV'  , temp_arr_cldconv
;NCDF_VARPUT, oid, 'THETA_CLDNCONV' , temp_arr_cldnconv


PRINT, 'done writing variables'

NCDF_CLOSE, oid																									;Close output file
NCDF_CLOSE, iid																									;Close input file

PRINT, 'File ' + infile + ' processed.' 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End write file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		

ENDFOREACH

STOP
TRACER_TRACER_DIFFERENCE, xtracer2, ytracer2, col, $
	TITLE      = title[i], $
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
	BINNED     = binned, $
	XLOG	   = xlog, $
	TABLE	   = table, $
	CREVERSE   = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $		
	SYMSIZE    = symsize, $
	NOWINDOW   = 1

IF KEYWORD_SET(difference) THEN BEGIN
	IF (i EQ 0) THEN BEGIN   			
		freq_nc    = freq
		xtracer_nc = xtracer2
		ytracer_nc = ytracer2
		ralt_nc    = ralt_arr
	ENDIF
	IF (i EQ 1) THEN BEGIN
		freq_c    = freq
		xtracer_c = xtracer2
		ytracer_c = ytracer2
		ralt_c    = ralt_arr
	ENDIF
ENDIF

IF KEYWORD_SET(difference) THEN BEGIN 
	hist_diff = freq_c - freq_nc
	hist_sum  = freq_c + freq_nc
	xtracers  = xtracer_c + xtracer_nc
	ytracers  = ytracer_c + ytracer_nc
	ralts     = ralt_c + ralt_nc

    		
    layer_total = REBIN(REFORM(TOTAL(hist_sum,1),1,nybin),nxbin,nybin)
    freq 		= 100.0 * (FLOAT(hist_diff) / layer_total)
   
    pmax   = 25.0*(LONG(MEAN(hist_diff) + 2*STDDEV(hist_diff))/25 + 1)											;Calculate maximum count for each gas concentration w/in 2 STD DEV												
;    table  = BLUE_RED_24(1000, 0.25, 1.0, PS = ps)													;Color table for plotting 		
    table  = BLUE_RED_24(1000, 0.1, 1.0, PS = ps)
    col    = COLOR_LOOKUP_24(freq, table, MIN_VALUE = -15.0, MAX_VALUE = 15.0, MISSING = table[-1])
            
    none   = WHERE((hist_diff EQ 0), none_count)
    IF (none_count GT 0) THEN col[none] = COLOR_24('white')											;Set counts of zero to white

	TRACER_TRACER_DIFFERENCE, xtracers, ytracers, col, $
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
		BINNED     = binned, $
		XLOG	   = xlog, $
		TABLE	   = table, $
		CREVERSE   = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $		
		SYMSIZE    = symsize, $
		NOWINDOW   = 1
    
ENDIF

;outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/plots/chemistry_tracer/'
;epsfile = outdir + scheme + '_' + end_date + '_' + name + '.eps'						;EPS filename
;pdffile = outdir + scheme + '_' + end_date + '_' + name + '.pdf'						;PDF filename
;pngfile = outdir + scheme + '_' + end_date + '_' + name + '.png'						;PNG filename

;FILE_MKDIR, outdir


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
