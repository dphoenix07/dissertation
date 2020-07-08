PRO CFAD_FREQ_DIFFERENCE, event, scheme, start_date, end_date, $
	DOMAIN     = domain, $
	BINNED     = binned, $
	UPDRAFT_TR = updraft_tr, $
	REGION     = region, $
	DIFFERENCE = difference, $
	ALT		   = alt, $
	PNG	       = png, $
	EPS   	   = eps


;+
; Name:
;		CONV_TRANSPORT_O3_H2O
; Purpose:
;		Look at bulk convective transport of o3 and h2o
; Calling sequence:
;		CONV_TRANSPORT_O3_H2O, run, scheme, start_date, end_date
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
;		Daniel B. Phoenix	    2017-07-25. 
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20110518T1200Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20110520T1200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain
IF (N_ELEMENTS(in_cloud  ) EQ 0) THEN in_cloud   = 1
IF (N_ELEMENTS(nxbin 	 ) EQ 0) THEN nxbin      = 50
IF (N_ELEMENTS(nybin 	 ) EQ 0) THEN nybin      = 50
IF (N_ELEMENTS(updraft_tr) EQ 0) THEN updraft_tr = 0

outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/plots/chemistry_scatter/'
epsfile = outdir + scheme + '_' + end_date + '.eps'						;EPS filename
pdffile = outdir + scheme + '_' + end_date + '.pdf'						;PDF filename
pngfile = outdir +  'nssl_racm-esrl_' + end_date + '.png'						;PNG filename

FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(event, scheme, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(event, scheme, start_date, end_date, /DATE)	

IF KEYWORD_SET(eps) THEN BEGIN	
	PS_ON, FILENAME = epsfile, PAGE_SIZE = [10.0, 6.0], MARGIN = 0.0, /INCHES					;Switch to Postscript device
	DEVICE, /ENCAPSULATED
	!P.FONT     = 0																				;Hardware fonts
	!P.CHARSIZE = 1.5
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS																		;Load basic color definitions
ENDIF ELSE BEGIN
	SET_PLOT, 'X'
	WINDOW, XSIZE = 900, YSIZE = 1200															;Open graphics window
	!P.COLOR      = COLOR_24('black')															;Foreground color
	!P.BACKGROUND = COLOR_24('white')															;Background color
	!P.CHARSIZE   = 3.1		
	!P.FONT       = -1																			;Use Hershey fonts
ENDELSE
!P.MULTI = [0, 3, 3]

title = ['Non-Conv.(cloud)','Conv.(cloud)']
IF KEYWORD_SET(updraft_tr) THEN title = ['Non-Conv.(updraft)','Conv.(updraft)']

var_set     = ['H2O','O3', 'CO']

z = (WRF_READ_VAR('Z', date_arr[0], event, scheme, DOMAIN = domain)).values					;Read height values
dim = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)

convective = 0

FOR i = 0,1 DO BEGIN
	convective = i
    FOREACH var, var_set DO BEGIN
    
    	CASE var OF
         	'O3' : BEGIN
         		factor     = 1.0E3
         		XRANGE 	   = [0, 400]
         		XTITLE     = 'Ozone (ppbv)'   
         	END
         	
         	'H2O' : BEGIN
         		factor     = 1.0E6
         		XRANGE 	   = [1, 500]
         		XTITLE     = 'Water Vapor (ppmv)'     
         	END
         	
         	'CO' : BEGIN
         		factor     = 1.0E3
         		XRANGE     = [0, 200]
         		XTITLE     = 'Carbon Monoxide (ppbv)'
        	END
         ENDCASE
    
        z_arr 	    = [ ]																				;allocate arrays
        xyz_troparr = [ ]
        var_arr     = [ ] 
        cld_tr_arr  = [ ]
        updraft_arr = [ ] 
    	tracer	    = [ ]
    	ralt_arr    = [ ]
    	tracers	    = [ ]
    	
        FOREACH date, date_arr DO BEGIN
        	PRINT, date
    
        	    z		 = (WRF_READ_VAR('Z'     , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read height values
        	    dim1 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
        	    z_trop   = (WRF_READ_VAR('Z_trop', date, event, scheme, DOMAIN = domain, INDICES = region)).values
        	    z_trop   = CALC_TROP_MODE(z_trop, scheme, threshold) 												;Filter tropopause values
        	    xyz_trop = REBIN(z_trop, dim1[0], dim1[1], dim1[2], /SAMPLE)
        	    filt     = WHERE(xyz_trop EQ 999999 , filt_count, COMPLEMENT = good, NCOMPLEMENT = good_count)
           
        	    IF (good_count GT 0) THEN BEGIN
    				z_arr		= z[good]
     				xyz_troparr = xyz_trop[good]
     				       
        	    	var1      	= (WRF_READ_VAR(var      		  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values
    				var_arr	    = var1[good]
    			
        	    	var1      	= (WRF_READ_VAR('Cloud_tracer'	  , date, event, scheme, DOMAIN = domain, INDICES = region)).values
     				cld_tr_arr  = var1[good]
     				       
        	    	var1      	= (WRF_READ_VAR('Updraft_tracer'  , date, event, scheme, DOMAIN = domain, INDICES = region)).values
    				updraft_arr = var1[good]
        	    ENDIF
        	     
        	     var_arr = var_arr*factor
        
    		    IF KEYWORD_SET(alt) THEN BEGIN
    		        ytitle = 'Altitude (km)'
        	    	yrange = [0, 20]
    		    	ralt   = 0.001*(z_arr)
    		    ENDIF ELSE BEGIN
    		    	ytitle = 'Relative Altitude (km)'
    		    	yrange = [-15, 5]
    		    	ralt   = 0.001*(z_arr-xyz_troparr)
    		    ENDELSE
    
                IF KEYWORD_SET(updraft_tr) THEN BEGIN
                      conv_values = WHERE(updraft_arr GT 0.1, conv_count, COMPLEMENT = non_conv, $							;Find values in cloud 
                      					NCOMPLEMENT = nconv_count)
                      
                      IF KEYWORD_SET(convective) THEN BEGIN																;Sort values in cloud vs out of cloud
                      	IF (conv_count GT 0) THEN BEGIN
                      		var_arr = var_arr[conv_values]
                      		ralt    = ralt   [conv_values]
                      	ENDIF
                      ENDIF ELSE BEGIN
                      	IF (nconv_count GT 0) THEN BEGIN
                      		var_arr = var_arr[non_conv]
                      		ralt    = ralt   [non_conv]
                      	ENDIF
                      ENDELSE
                
                ENDIF ELSE BEGIN
                      cld_tr_values =  WHERE(cld_tr_arr GT 0.1, cld_count, COMPLEMENT = non_cld, $							;Find values in cloud 
                      					NCOMPLEMENT = ncld_count)
                      					
                      IF KEYWORD_SET(convective) THEN BEGIN																;Sort values in cloud vs out of cloud
                      	IF (cld_count GT 0) THEN BEGIN
                      		var_arr = var_arr[cld_tr_values]
                      		ralt    = ralt   [cld_tr_values]
                      	ENDIF
                      ENDIF ELSE BEGIN
                      	IF (ncld_count GT 0) THEN BEGIN
                      		var_arr = var_arr[non_cld]
                      		ralt    = ralt   [non_cld]
                      	ENDIF
                      ENDELSE
                ENDELSE
                	
                IF KEYWORD_SET(dbl_trop) THEN BEGIN																;If secondary tropopause is present, filter out those values
                	prim_trop = WHERE (xyz_trop LT upper_trop, trop_count)
                	
                	IF (trop_count GT 0) THEN BEGIN
                		var_arr = var_arr[prim_trop]
                		ralt    = ralt   [prim_trop]
                	ENDIF
                ENDIF	
                			
                good = WHERE(((var_arr GE xrange[0]) AND $												;Use specified data range
                		(var_arr LE xrange[1]) 		 AND $												;Only used if filtering not desired
                		(ralt    GE yrange[0])   	 AND $
                		(ralt    LE yrange[1])), good_count)
                
                empty_plot = 0
                IF (good_count GT 0) THEN BEGIN																	;Save good data points
                	var_arr  = var_arr[good]
                	ralt 	 = ralt   [good] 
                ENDIF ELSE empty_plot = 1
    			
    			tracer   = [tracer  , var_arr]	
    			ralt_arr = [ralt_arr, ralt   ]	

    			PRINT, MEAN(ralt_arr,/NAN)
    			
    	 ENDFOREACH ;date 
    	 
                dy   = FLOAT(yrange[1] - yrange[0])/nybin														;Compute y bin spacing
                ybin = 0.5*dy + yrange[0] + dy*FINDGEN(nybin)
                
                dx      = FLOAT(xrange[1] - xrange[0])/nxbin													;Set bin parameters for regular scale
                xbin    = 0.5*dx + dx*FINDGEN(nxbin) + xrange[0]
                bin     = LONG((tracer-xrange[0])/dx) + nxbin*(LONG((ralt_arr-yrange[0])/dy))
                
                hist = HISTOGRAM(bin, BINSIZE = 1, MIN = 0, MAX = (nxbin*nybin -1))								;Calculate density
                hist = REFORM(hist, nxbin, nybin)
                
                layer_total = REBIN(REFORM(TOTAL(hist,1),1,nybin),nxbin,nybin)
                freq 		= 100.0 * (FLOAT(hist) / layer_total)
     
                pmax   = 25.0*(LONG(MEAN(hist) + 2*STDDEV(hist))/25 + 1)											;Calculate maximum count for each gas concentration w/in 2 STD DEV												
                table  = GRAY_LOGSCALE_24(20, 0.75, 0.0, PS = ps)													;Color table for plotting 
                col    = COLOR_LOOKUP_24(freq, table, MIN_VALUE = 0.0, MAX_VALUE = 20.0, MISSING = table[-1])
                        
                none   = WHERE((hist EQ 0), none_count)
                IF (none_count GT 0) THEN col[none] = COLOR_24('white')											;Set counts of zero to white
        
    		
    IF (convective EQ 0) THEN BEGIN    
    	hist_nc = hist
        PLOT_CONV_TRANSPORT_TEST, tracer, ralt_arr, col, $									;Call PLOT_CONV_TRANSPORT to produce scatterplots
        	TITLE      = title[i], $
        	XRANGE     = xrange, $
        	XTITLE     = xtitle, $
        	YTITLE     = ytitle, $
        	YRANGE     = yrange, $
        	BINNED     = 1, $
        	FILTERING  = filtering, $
           	NXBIN 	   = nxbin, $
           	NYBIN	   = nybin, $
        	XBIN 	   = xbin, $
        	YBIN	   = ybin, $
        	DX		   = dx, $
        	DY 		   = dy, $
        	TABLE	   = table, $
        	EMPTY_PLOT = empty_plot, $
    		CONVECTIVE = convective, $
        	NOERASE	   = 1, $
            NOWINDOW   = 1
    ENDIF ELSE BEGIN
    	hist_c = hist
        PLOT_CONV_TRANSPORT_TEST, tracer, ralt_arr, col, $												;Call PLOT_CONV_TRANSPORT to produce scatterplots
           	TITLE      = title[i], $
           	XRANGE     = xrange, $
           	XTITLE     = xtitle, $
           	YTITLE     = ytitle, $
           	YRANGE     = yrange, $
           	BINNED     = 1, $
           	NXBIN 	   = nxbin, $
           	NYBIN	   = nybin, $
        	XBIN 	   = xbin, $
        	YBIN	   = ybin, $
        	DX		   = dx, $
        	DY 		   = dy, $
        	TABLE	   = table, $
        	EMPTY_PLOT = empty_plot, $
           	FILTERING  = filtering, $
           	CONVECTIVE = convective, $
           	NOERASE	   = 1, $
           	NOWINDOW   = 1
    ENDELSE

	IF KEYWORD_SET(difference) THEN BEGIN
   		IF (i EQ 0) THEN BEGIN   			
   			IF var EQ 'H2O' THEN BEGIN
   				h2o_hist_nc = freq
   				h2o_nc      = tracer
   				h2o_ralt_nc = ralt_arr
   			ENDIF
   			IF var EQ 'O3' THEN BEGIN
   				o3_hist_nc = freq
   				o3_nc      = tracer
   				o3_ralt_nc = ralt_arr
   			ENDIF
   			IF var EQ 'CO' THEN BEGIN
   				co_hist_nc = freq
   				co_nc      = tracer
   				co_ralt_nc = ralt_arr
   			ENDIF
   		ENDIF
   		IF (i EQ 1) THEN BEGIN
   			IF var EQ 'H2O' THEN BEGIN
   				h2o_hist_c = freq
   				h2o_c      = tracer
   				h2o_ralt_c = ralt_arr
   			ENDIF
   			IF var EQ 'O3' THEN BEGIN
   				o3_hist_c = freq
   				o3_c      = tracer
   				o3_ralt_c = ralt_arr
   			ENDIF
   			IF var EQ 'CO' THEN BEGIN
   				co_hist_c = freq
   				co_c      = tracer
   				co_ralt_c = ralt_arr
   			ENDIF
   		ENDIF
    ENDIF
    
;    	ENDFOR
    ENDFOREACH ;var

ENDFOR
    
IF KEYWORD_SET(difference) THEN BEGIN 
	FOREACH var, var_set DO BEGIN
		CASE var OF 
			'H2O': BEGIN
				hist_diff = h2o_hist_c - h2o_hist_nc
				hist_sum  = h2o_hist_c + h2o_hist_nc
				tracers   = h2o_c + h2o_nc
				ralts	  = h2o_ralt_c + h2o_ralt_nc
				XTITLE    = 'Water Vapor (ppmv)'   

			END
			
			'O3' : BEGIN
				hist_diff = o3_hist_c - o3_hist_nc
				hist_sum  = o3_hist_c + o3_hist_nc
				tracers   = o3_c + o3_nc
				ralts	  = o3_ralt_c + o3_ralt_nc
				XTITLE    = 'Ozone (ppbv)'   
			END
	
			'CO' : BEGIN
				hist_diff = co_hist_c - co_hist_nc
				hist_sum  = co_hist_c + co_hist_nc
				tracers   = co_c + co_nc
				ralts	  = co_ralt_c + co_ralt_nc
				XTITLE    = 'Carbon Monoxide (ppbv)'   
    		END
    	ENDCASE
    		
   		layer_total = REBIN(REFORM(TOTAL(hist_sum,1),1,nybin),nxbin,nybin)
    	freq 		= 100.0 * (FLOAT(hist_diff) / layer_total)
    
        pmax   = 25.0*(LONG(MEAN(hist_diff) + 2*STDDEV(hist_diff))/25 + 1)											;Calculate maximum count for each gas concentration w/in 2 STD DEV												
        table  = BLUE_RED_24(1000, 0.25, 1.0, PS = ps)													;Color table for plotting 
        col    = COLOR_LOOKUP_24(freq, table, MIN_VALUE = -20.0, MAX_VALUE = 20.0, MISSING = table[-1])
            
        none   = WHERE((hist_diff EQ 0), none_count)
        IF (none_count GT 0) THEN col[none] = COLOR_24('white')											;Set counts of zero to white
    
 	    PLOT_CONV_TRANSPORT_TEST, tracers, ralts, col, $												;Call PLOT_CONV_TRANSPORT to produce scatterplots
        	TITLE      = 'Difference', $
        	XRANGE     = xrange, $
        	XTITLE     = xtitle, $
        	YTITLE     = ytitle, $
        	YRANGE     = yrange, $
        	BINNED     = 1, $
        	NXBIN 	   = nxbin, $
        	NYBIN	   = nybin, $
 	    	XBIN 	   = xbin, $
 	    	YBIN	   = ybin, $
 	    	DX		   = dx, $
 	    	DY 		   = dy, $
 	    	TABLE	   = table, $
 	    	EMPTY_PLOT = empty_plot, $
        	FILTERING  = filtering, $
        	CONVECTIVE = convective, $
        	DIFFERENCE = 1, $
        	NOERASE	   = 1, $
        	NOWINDOW   = 1
    ENDFOREACH
ENDIF
STOP
!P.MULTI = 0
IF KEYWORD_SET(eps) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																;Reset color table to linear ramp
	PS_OFF																						;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)															;Write PNG file

END