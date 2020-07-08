PRO PLOT_MULTI_TRACER_TEST, event, scheme, start_date, end_date, $
	ZRELATIVE  = zrelative, $
	POTEN_TEMP = poten_temp, $
	CLOUD      = cloud, $
	TR_COLOR   = tr_color, $
	GROUP2     = group2, $
	UPDRAFT_TR = updraft_tr, $
	FILTER	   = filter, $
	DOMAIN     = domain, $
	XLOG	   = xlog, $
	BINNED     = binned, $
	PERCENT95  = percent95, $
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
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain
IF (N_ELEMENTS(filter	 ) EQ 0) THEN filter 	 = 0
IF (N_ELEMENTS(percent95 ) EQ 0) THEN percent95  = 0

IF KEYWORD_SET(updraft_tr) THEN filter = 1

IF (event EQ '20120519') THEN region = [50, 50, 250, 190]

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(event, scheme, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(event, scheme, start_date, end_date, /DATE)	

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
	!P.CHARSIZE   = 2.9		
	!P.FONT       = -1																			;Use Hershey fonts
ENDELSE
!P.MULTI = [0, 2, 1]

z_arr 	    = [ ]																				;allocate arrays
xyz_troparr = [ ]
var1_arr    = [ ] 
var1_arr    = [ ] 
cld_arr	    = [ ]
cld_tr_arr  = [ ]
updraft_arr = [ ]
tracer1     = [ ]
tracer2     = [ ]
ralt_arr	= [ ]
colorarr    = [ ]
temparr     = [ ]

IF (KEYWORD_SET(PERCENT95)) THEN BEGIN
	var1_conv    = [ ]
	var2_conv    = [ ] 
	var1_ncon    = [ ]
	var2_ncon    = [ ]
	temp_conv    = [ ]
	temp_ncon    = [ ]
	tracer1_conv = [ ]
    tracer2_conv = [ ]
    tracer3_conv = [ ]
    tracer1_ncon = [ ]
    tracer2_ncon = [ ]
    tracer3_ncon = [ ]
ENDIF

var_arr = ['O3','H2O','CO']
factor = 1.0E6
cfactr = 1.0E3
xlog   = 1
xrange = [1, 20000]
zrange = [0,   200]
xtitle = 'H2O (ppmv)'

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

FOR i = 0,1 DO BEGIN
	IF (filter EQ 0) THEN i = 1
	convective = i	

    FOREACH date, date_arr DO BEGIN
    	PRINT, date
    	
    	z		 = (WRF_READ_VAR('Z'     , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read height values
    	dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
    	z_trop   = (WRF_READ_VAR('Z_trop', date, event, scheme, DOMAIN = domain, INDICES = region)).values
		z_trop	 = MEDIAN(z_trop,100)
;    	z_trop   = CALC_TROP_MODE(z_trop, scheme, threshold) 												;Filter tropopause values
    	xyz_trop = REBIN(z_trop, dim[0], dim[1], dim[2], /SAMPLE)
;    	filt     = WHERE(xyz_trop EQ 999999 , filt_count, COMPLEMENT = good, NCOMPLEMENT = good_count)

good_count = 1   
    	IF (good_count GT 0) THEN BEGIN
     		z_arr       = z       ;[good]														;concatenate arrays for each time
    		xyz_troparr = xyz_trop;[good]	
    
    		var			= ((((WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, DOMAIN = domain)).values) * 1.0E3) GT 0.01)
    		cld_arr  	= var;[good]	
    		
    		var       	= (WRF_READ_VAR(var_arr[0]      , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values
    		var1_arr    = var;[good]	
    
    		var	    	= (WRF_READ_VAR(var_arr[1]		, date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read carbon monoxide data
    		var2_arr    = var;[good]
            
            var      	= (WRF_READ_VAR('Cloud_tracer'	, date, event, scheme, DOMAIN = domain, INDICES = region)).values
        	cld_tr_arr  = var;[good]
         				       
            var      	= (WRF_READ_VAR('Updraft_tracer', date, event, scheme, DOMAIN = domain, INDICES = region)).values
        	updraft_arr = var;[good]
    		
    		IF KEYWORD_SET(poten_temp) THEN BEGIN
    		    var      = (WRF_READ_VAR('T', date, event, scheme, DOMAIN = domain, INDICES = region)).values 				
				var      = ((1000.0/(WRF_READ_VAR('P', date, event, scheme, $						;Compute potential temperature
		 						DOMAIN = domain)).values)^(!Rair/!Cp))*(var)
		 		temp_arr = var;[good]
		 	ENDIF

    		IF KEYWORD_SET(group2) THEN BEGIN 
    			var	 	 = (WRF_READ_VAR(var_arr[2]		, date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read water vapor data 
    			var2_arr = var;[good]		
    			IF KEYWORD_SET(tr_color) THEN BEGIN
    				PRINT, 'setting h2o as color'
    				var     = (WRF_READ_VAR('H2O'   	, date, event, scheme, DOMAIN = domain, INDICES = region)).values
    				trc_arr = var;[good]
		    		ztitle  = 'Water Vapor (ppmv)'
    			ENDIF
    		ENDIF ELSE BEGIN
    			IF KEYWORD_SET(tr_color) THEN BEGIN
	    			PRINT, 'setting co as color'
    				var     = (WRF_READ_VAR('CO'   		, date, event, scheme, DOMAIN = domain, INDICES = region)).values
    				trc_arr = var;[good]
	    			ztitle  = 'Carbon Monoxide (ppbv)'	
    			ENDIF
    		ENDELSE
    	ENDIF	 											
    
    	ralt = 0.001*(z_arr - xyz_troparr)

		IF KEYWORD_SET(filter) THEN BEGIN
          IF KEYWORD_SET(updraft_tr) THEN BEGIN
            conv_values = WHERE(updraft_arr GT 0.1, conv_count, COMPLEMENT = non_conv, $							;Find values in cloud 
                             		NCOMPLEMENT = nconv_count)
            
            IF (KEYWORD_SET(percent95)) THEN BEGIN
            	i = 1
            	var1_conv = var1_arr;[conv_values]
            	var2_conv = var2_arr;[conv_values]
            	temp_conv = temp_arr;[conv_values]
            	var1_ncon = var1_arr[non_conv   ]
            	var2_ncon = var2_arr[non_conv   ]
				temp_ncon = temp_arr[non_conv   ]
			ENDIF
                 
           	IF KEYWORD_SET(convective) THEN BEGIN																;Sort values in cloud vs out of cloud
            	   IF (conv_count GT 0) THEN BEGIN
             	  	 var1_arr = var1_arr[conv_values]
             	  	 var2_arr = var2_arr[conv_values]
               	     ralt     = ralt    [conv_values]
            	  	 IF KEYWORD_SET(tr_color  ) THEN trc_arr  = trc_arr  [conv_values] 
            	  	 IF KEYWORD_SET(poten_temp) THEN temp_arr = temp_arr [conv_values] 
             	  ENDIF
       	   	ENDIF ELSE BEGIN
               	IF (nconv_count GT 0) THEN BEGIN
                   	var1_arr = var1_arr[non_conv]
             	  	var2_arr = var2_arr[non_conv]
                    ralt     = ralt    [non_conv]
           	  		IF KEYWORD_SET(tr_color)   THEN trc_arr  = trc_arr  [non_conv] 
             	  	IF KEYWORD_SET(poten_temp) THEN temp_arr = temp_arr [non_conv] 
               	ENDIF
           	ENDELSE
            			           
           ENDIF ELSE BEGIN
               cld_tr_values =  WHERE(cld_tr_arr GT 0.1, cld_count, COMPLEMENT = non_cld, $							;Find values in cloud 
                             			NCOMPLEMENT = ncld_count)
                             					
               IF KEYWORD_SET(convective) THEN BEGIN																;Sort values in cloud vs out of cloud
                   IF (cld_count GT 0) THEN BEGIN
                       var1_arr = var1_arr[cld_tr_values]
                       var2_arr = var2_arr[cld_tr_values]
                       ralt     = ralt    [cld_tr_values]
            	  	   IF KEYWORD_SET(tr_color  ) THEN trc_arr  = trc_arr  [cld_tr_values] 
            	  	   IF KEYWORD_SET(poten_temp) THEN temp_arr = temp_arr [cld_tr_values] 
                   ENDIF
               ENDIF ELSE BEGIN
                   IF (ncld_count GT 0) THEN BEGIN
                       var1_arr = var1_arr[non_cld]
    	   			   var2_arr = var2_arr[non_cld]
                       ralt     = ralt    [non_cld]
            	  	   IF KEYWORD_SET(tr_color  ) THEN trc_arr  = trc_arr  [non_cld] 
            	  	   IF KEYWORD_SET(poten_temp) THEN temp_arr = temp_arr [non_cld] 
                   ENDIF
               ENDELSE
           ENDELSE
                                            	
           IF KEYWORD_SET(dbl_trop) THEN BEGIN																;If secondary tropopause is present, filter out those values
               prim_trop = WHERE (xyz_trop LT upper_trop, trop_count)
                       	
               IF (trop_count GT 0) THEN BEGIN
                   var1_arr = var1_arr[prim_trop]
    	   		   var2_arr = var2_arr[prim_trop]
                   ralt     = ralt    [prim_trop]
                ENDIF
            ENDIF	
                    			
    ;    good = WHERE(((var_arr GE xrange[0]) AND $												;Use specified data range
    ;                (var_arr LE xrange[1]) 		 AND $												;Only used if filtering not desired
    ;                (ralt    GE yrange[0])   	 AND $
    ;                (ralt    LE yrange[1])), good_count)
    ;                
    ;    empty_plot = 0
    ;    IF (good_count GT 0) THEN BEGIN																	;Save good data points
    ;        var_arr  = var_arr[good]
    ;        ralt 	 = ralt   [good] 
    ;    ENDIF ELSE empty_plot = 1
        ENDIF
        		
        tracer1   = [tracer1 , var1_arr]
        tracer2   = [tracer2 , var2_arr]	
        ralt_arr  = [ralt_arr, ralt    ]	
        IF KEYWORD_SET(tr_color  ) THEN colorarr = [colorarr, trc_arr ]
        IF KEYWORD_SET(poten_temp) THEN temparr  = [temparr , temp_arr]

		IF (KEYWORD_SET(percent95)) THEN BEGIN
			tracer1_conv = [tracer1_conv, var1_conv]
			tracer2_conv = [tracer2_conv, var2_conv]
			tracer3_conv = [tracer3_conv, temp_conv]
			tracer1_ncon = [tracer1_ncon, var1_ncon]
			tracer2_ncon = [tracer2_ncon, var2_ncon]
			tracer3_ncon = [tracer3_ncon, temp_ncon]
		ENDIF
    ENDFOREACH 

IF KEYWORD_SET(zrelative) THEN BEGIN
	ztracer = ralt_arr
	zrange  = [-2.5, 2.5]
	ztitle  = 'Tropopause Relative (km)'
	name    = 'zrelative'
ENDIF ELSE IF KEYWORD_SET(cloud) THEN BEGIN	
	ztrsort = ralt_arr
	ztracer = cld_tr_arr
	zrange  = [0, 1]
	ztitle  = 'Cloud Particles (#)'
	name    = 'cloud'
ENDIF ELSE IF KEYWORD_SET(tr_color) THEN BEGIN
	ztrsort = ralt_arr
	ztracer = colorarr
	ztracer = ztracer*cfactr
	zrange  = zrange
	ztitle  = ztitle
	name    = 'tr_color'
ENDIF ELSE IF KEYWORD_SET(poten_temp) THEN BEGIN
	ztrsort = ralt_arr
	ztracer = temparr
	ztracer = ztracer
	zrange  = [300,420]
	ztitle  = 'Potential Temperature (K)'
	name    = 'temp'
ENDIF

symsize = 1.0 - 0.25*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf))

IF (KEYWORD_SET(percent95)) THEN BEGIN
    PLOT_TRACER_TRACER_P95, tracer2_conv*factor, tracer2_ncon*factor, tracer1_conv*1.0E3, $
    						tracer1_ncon*1.0E3, tracer3_conv, tracer3_ncon, $
    	TITLE      = title[i], $
    	XRANGE     = xrange, $
    	XTITLE     = xtitle, $
    	YRANGE     = [0, 600], $
    	YTITLE     = 'Ozone (ppbv)', $
		ZRANGE     = [300,420], $
		ZTITLE     = 'Potential Temperature (K)', $
    	BINNED     = binned, $
    	XLOG	   = xlog, $
    	SYMSIZE    = symsize, $
    	NOWINDOW   = 1
ENDIF ELSE BEGIN	

    PLOT_TRACER_TRACER_DP, tracer2*factor, tracer1*1.0E3, ztracer, $
    	TITLE      = title[i], $
    	ZTRSORT    = ztrsort, $
    	XRANGE     = xrange, $
    	XTITLE     = xtitle, $
    	YRANGE     = [0, 600], $
    	YTITLE     = 'Ozone (ppbv)', $
    	ZRANGE     = zrange, $
    	ZTITLE     = ztitle, $
    	BINNED     = binned, $
    	PERCENT95  = percent95, $
    	IFLAG      = i, $
    	XLOG	   = xlog, $
    	CREVERSE   = (KEYWORD_SET(cloud) OR KEYWORD_SET(water)), $		
    	SYMSIZE    = symsize, $
    	NOWINDOW   = 1
ENDELSE

ENDFOR

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
