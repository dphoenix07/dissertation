PRO TIME_HEIGHT_CONVECTIVE_INFLUENCE1, event, start_date, end_date, $
	DOMAIN         = domain, $
	PERCENTILE     = percentile, $
	PERCENT_DIFF   = percent_diff, $
	TRACER		   = tracer, $
	ALT		       = alt, $
	REGION         = region, $
	NONCONV        = nonconv, $
	PNG	           = png, $
	EPS   	       = eps


;+
; Name:
;		TIME_HEIGHT_CONVECTIVE_INFLUENCE1
; Purpose:
;		Computes a time-height diagram of the percent difference in mean profiles of 
;		a user supplied tracer (O3,H2O,CO,T) between convective and non-convective
;		areas.
; Calling sequence:
;		TIME_HEIGHT_CONVECTIVE_INFLUENCE1, run, start_date, end_date
; Example: TIME_HEIGHT_CONVECTIVE_INFLUENCE1,'20110518','20110518T1300Z','20110527T1200Z', tracer = 'O3'
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
;		Daniel B. Phoenix	    2018-02-14.	Same as TIME_HEIGHT_CONVECTIVE_INFLUENCE,
;											but trying to make more efficient 
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain
IF (N_ELEMENTS(in_cloud  ) EQ 0) THEN in_cloud   = 1
IF (N_ELEMENTS(threshold ) EQ 0) THEN threshold  = 1000.0
IF (N_ELEMENTS(xrange	 ) EQ 0) THEN xrange     = [  0, 800]
IF (N_ELEMENTS(xtitle	 ) EQ 0) THEN xtitle     = 'Ozone (ppbv)'
IF (N_ELEMENTS(yrange	 ) EQ 0) THEN yrange     = [-5,   5]
IF (N_ELEMENTS(ytitle	 ) EQ 0) THEN ytitle     = 'Relative Altitude'
IF (N_ELEMENTS(nxbin 	 ) EQ 0) THEN nxbin      = 50
IF (N_ELEMENTS(nybin 	 ) EQ 0) THEN nybin      = 50
IF (N_ELEMENTS(var2		 ) EQ 0) THEN var2		 = 0

CASE event OF
	'20110518' : BEGIN
		scheme = ['seasonal_final/corrected']
		label  = ['racm-yellowstone']
		key    = 'test'
		END
ENDCASE

IF (KEYWORD_SET(ALT)) THEN BEGIN
	yrange = [0, 18]
	ytitle = 'Altitude'
ENDIF

IF (N_ELEMENTS(region) EQ 0) THEN BEGIN
	IF (event EQ '20120519') THEN region = [50, 50, 250, 190]
ENDIF

outdir  = !WRF_DIRECTORY + event + '/paper/plots/time_height/'
epsfile = outdir + key + '_' + end_date + '.eps'						;EPS filename
pdffile = outdir + key + '_' + end_date + '.pdf'						;PDF filename
pngfile = outdir + key + '_' + end_date + '.png'						;PNG filename

FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(event, scheme, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(event, scheme, start_date, end_date, /DATE)	

c = 0 
in_cloud = 0
in_cloud = c

var_arr  = [tracer]
map_plot = [ ]
ralt_arr = [ ]

range1 = [-10,10]
FOREACH var, var_arr DO BEGIN 
   IF var EQ 'H2O'  THEN BEGIN
   	xtitle   = 'H2O (ppmv)'
   	IF KEYWORD_SET(percent_diff) THEN xtitle   = 'H2O (%)'
   	xrange   = [0, 200]
   	map_bar_min   = -5.0																					
	map_bar_max   = 5.0																					
;      	xrange0  = [0, 500]
    ENDIF
    IF var EQ 'O3'   THEN BEGIN
    	xtitle   = 'O3 (ppbv)'
    	IF KEYWORD_SET(percent_diff) THEN xtitle   = 'O3 (%)'
		xrange   = [0   , 400]
    	xrange1  = [-200, 200]
	   	map_bar_min   = -100.0																					
		map_bar_max   = 100.0																					
 ;     	xrange0  = [0   , 800]
	ENDIF
    IF var EQ 'CO'   THEN BEGIN
	  	xtitle   = 'CO (ppbv)'
      	IF KEYWORD_SET(percent_diff) THEN xtitle   = 'CO (%)'
      	xrange   = [0, 200]
   		map_bar_min   = -10.0																					
		map_bar_max   = 10.0																					
 ;     	xrange0  = [0, 200]
    ENDIF
                      
    color 	   = [COLOR_24_DP('RED'), COLOR_24_DP('BLUE'), COLOR_24_DP('DARKGREEN')]			
      
    tracer_obs = (WRF_READ_VAR('O3', date_arr[0], event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values
    ralt_obs   = (WRF_READ_VAR('Z' , date_arr[0], event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values
    
    i = 0
 		IF KEYWORD_SET(percent_diff) THEN xrange1 = [-200,200]        
    yrange = [-5, 5]
    IF (KEYWORD_SET(ALT)) THEN yrange = [0, 18] 
      
;      figure = PLOT(tracer_obs, ralt_obs, /NODATA, $
;      		XRANGE = xrange1, $
;      		YRANGE = yrange, $
;      		LAYOUT = position, $
;      		/CURRENT)
;      
;      figure = PLOT(tracer_obs, ralt_obs, /NODATA, $
;      		XRANGE = xrange1, $
;      		YRANGE = yrange, $
;      		LAYOUT = position, $
;      		/OVERPLOT)
      
    plt_var1 = [ ] 
    FOREACH date, date_arr DO BEGIN
		PRINT, "Processing date: ", date
    	z		 = (WRF_READ_VAR('Z'     , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read height values
    	dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
    	z_trop   = (WRF_READ_VAR('Z_trop', date, event, scheme, DOMAIN = domain, INDICES = region)).values
    	z_trop   = CALC_TROP_MODE(z_trop, scheme, threshold) 												;Filter tropopause values
    
    	precip   = (WRF_READ_VAR('PRECIP'	     , date, event, scheme, DOMAIN = domain, INDICES = region)).values
    	w	     = (WRF_READ_VAR('w' 	 	     , date, event, scheme, DOMAIN = domain, INDICES = region)).values
    	cld_tr   = (WRF_READ_VAR('Cloud_tracer'  , date, event, scheme, DOMAIN = domain, INDICES = region)).values
    	updraft  = (WRF_READ_VAR('Updraft_tracer', date, event, scheme, DOMAIN = domain, INDICES = region)).values

    
    	xyz_trop = REBIN(z_trop, dim[0], dim[1], dim[2], /SAMPLE)
    	filt     = WHERE(xyz_trop EQ 999999 , filt_count, COMPLEMENT = good, NCOMPLEMENT = good_count)
    
    	z[filt] 	    = !Values.F_NaN
    	xyz_trop[filt]  = !Values.F_NaN
    
    	cloud   		= (WRF_READ_VAR('CLOUD_MIX_TOTAL' , date, event, scheme, DOMAIN = domain, INDICES = region)).values * 1.0E3			;Read in cloud mixing ratio values
    	cloud[filt]     = !Values.F_NaN
    
    	plt_var      	= (WRF_READ_VAR(var      		  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values
    	plt_var[filt]   = !Values.F_NaN
    	plt_var1		= [[[plt_var1]], [[plt_var]]]													

     	;Create regular 200 m vertically spaced grid
;     	z_new = FINDGEN(120)*200
;    	iz = FLTARR(dim[0],dim[1],N_ELEMENTS(z_new))
;    	var_interp = FLTARR(dim[0],dim[1],N_ELEMENTS(z_new))
;    	xyz_trop_interp = FLTARR(dim[0],dim[1],N_ELEMENTS(z_new))
;    
;        ;;+Interpolate z array
;     	;Create regular 200 m vertically spaced grid
;        z_new = FINDGEN(120)*200
;        iz = FLTARR(dim[0],dim[1],N_ELEMENTS(z_new))
;        z2 = FLTARR(dim[0],dim[1],N_ELEMENTS(z_new))
;        
;        FOR ii=0, dim[0]-1 DO BEGIN
;        	FOR jj=0, dim[1]-1 DO BEGIN
;        		iz[ii,jj,*] = INTERPOL(FINDGEN(N_ELEMENTS(z[ii,jj,*])),z[ii,jj,*],z_new)
;        	ENDFOR
;        ENDFOR
;        
;        FOR ii=0, dim[0]-1 DO BEGIN
;        	FOR jj=0, dim[1]-1 DO BEGIN
;        		z2[ii,jj,*] = INTERPOLATE(z[ii,jj,*],iz[ii,jj,*])
;     			var_interp[ii,jj,*] = INTERPOLATE(plt_var1[ii,jj,*],iz[ii,jj,*])
;     			xyz_trop_interp[ii,jj,*] = INTERPOLATE(xyz_trop[ii,jj,*],iz[ii,jj,*])
;        	ENDFOR
;        ENDFOR		
;
;		dim = SIZE(z2, /DIMENSIONS)
    	
    	IF var EQ 'T'	 THEN factor = 1.0
    	IF var EQ 'O3'   THEN factor = 1.0E3
    	IF var EQ 'H2O'  THEN factor = 1.0E6
    	IF var EQ 'CO'   THEN factor = 1.0E3
    	IF var EQ 'HNO3' THEN factor = 1.0E6
    	IF var EQ 'NO2'  THEN factor = 1.0E6
    	IF var EQ 'SO2'  THEN factor = 1.0E6
    	IF var EQ 'HCHO' THEN factor = 1.0E6
    	
    	
;    	tracer = var_interp * factor
;   	ralt   = (z2 - xyz_trop_interp) * 1.0E-3
;    	ralt1  = (z2 - xyz_trop_interp) * 1.0E-3
    	tracer = plt_var * factor
    	ralt   = (z - xyz_trop) * 1.0E-3
    	ralt1  = (z - xyz_trop) * 1.0E-3
    
    	tracer_nc = FLTARR(dim[0],dim[1],dim[2])
    	ralt_nc   = FLTARR(dim[0],dim[1],dim[2])
    	
    	IF (KEYWORD_SET(ALT)) THEN ralt1 = z2 * 1.0E-3 ;ralt1 = z * 1.0E-3
    		
      
      ;;DBP 7/23/2017 removed this block about convective influence for seasonal anaylsis
      ;		FOR ii=0,dim[0]-1 DO BEGIN
      ;			FOR jj=0,dim[1]-1 DO BEGIN
      ;				IF (MAX(w[ii,jj,*]) GE 5.0) THEN BEGIN
      ;					tracer[ii,jj,*] = !Values.F_NaN
      ;					ralt  [ii,jj,*] = !Values.F_NaN
      ;					cloud [ii,jj,*] = !Values.F_NaN
      ;				ENDIF
      ;			ENDFOR
      ;		ENDFOR
      ;
      ;		precip = REBIN(precip, dim[0], dim[1], dim[2], /SAMPLE)
      ;		p_updraft = WHERE((precip GE 1.0), up_count, COMPLEMENT = out_count, $
      ;						NCOMPLEMENT = nout_count)	
      ;				
      ;		IF (up_count GT 0) THEN BEGIN
      ;			tracer [p_updraft] = !Values.F_NaN
      ;			ralt   [p_updraft] = !Values.F_NaN
      ;			cloud  [p_updraft] = !Values.F_NaN
      ;		ENDIF				
      ;;end
      

      	IF (c EQ 0) THEN BEGIN
         	   conv_values = WHERE(updraft GT 0.1, conv_count, COMPLEMENT = non_conv, $							;Find values in cloud 
            					NCOMPLEMENT = nconv_count)
            
            	IF (conv_count GT 0) THEN BEGIN
            		tracer   [conv_values] = tracer[conv_values]
            		ralt     [conv_values] = ralt  [conv_values]
            		tracer_nc[conv_values] = !Values.F_NAN
            		ralt_nc  [conv_values] = !Values.F_NAN
            	ENDIF

            	IF (nconv_count GT 0) THEN BEGIN
            		tracer_nc[non_conv] = tracer[non_conv]
            		ralt_nc  [non_conv] = ralt  [non_conv]
      				tracer   [non_conv] = !Values.F_NAN
      				ralt     [non_conv]	= !Values.F_NAN
            	ENDIF

    
      	ENDIF ELSE BEGIN
            	cld_tr_values =  WHERE(cld_tr GT 0.1, cld_count, COMPLEMENT = non_cld, $							;Find values in cloud 
            					NCOMPLEMENT = ncld_count)
            					
            	IF (cld_count GT 0) THEN BEGIN
            		tracer   [cld_tr_values] = tracer[cld_tr_values]
            		ralt     [cld_tr_values] = ralt  [cld_tr_values]
            		tracer_nc[cld_tr_values] = !Values.F_NAN
            		ralt_nc  [cld_tr_values] = !Values.F_NAN
            	ENDIF
            	IF (ncld_count GT 0) THEN BEGIN
            		tracer_nc[non_cld] = tracer[non_cld]
            		ralt_nc  [non_cld] = ralt  [non_cld]
      				tracer   [non_cld] = !Values.F_NAN
      				ralt     [non_cld] = !Values.F_NAN
            	ENDIF
      	ENDELSE
      
      ;		cloud_values = WHERE(cloud GE 0.1, cld_count, COMPLEMENT = non_cloud, $							;Find values in cloud 
      ;							NCOMPLEMENT = ncld_count)
      ;
      ;		IF KEYWORD_SET(in_cloud) THEN BEGIN																;Sort values in cloud vs out of cloud
      ;			IF (cld_count GT 0) THEN BEGIN
      ;				tracer[non_cloud] = !Values.F_NaN
      ;				ralt  [non_cloud] = !Values.F_NaN
      ;			ENDIF
      ;		ENDIF ELSE BEGIN
      ;			IF (ncld_count GT 0) THEN BEGIN
      ;				tracer [cloud_values] = !Values.F_NaN
      ;				ralt   [cloud_values] = !Values.F_NaN
      ;			ENDIF
      ;		ENDELSE
      
      		
;    		good = WHERE(((tracer GE xrange[0]) 	AND $												;Use specified data range
;    						(tracer LE xrange[1]) 	AND $												;Only used if filtering not desired
;    						(ralt   GE yrange[0])   AND $
;    						(ralt   LE yrange[1])), good_count, COMPLEMENT=out, NCOMPLEMENT = nout)
;           				  
;    		IF (nout GT 0) THEN BEGIN																	;Save good data points
;    			tracer    [out] = !Values.F_NaN
;    			ralt      [out] = !Values.F_NaN 
;    		ENDIF
      
      	    tracer    = REFORM(tracer	,dim[0],dim[1],dim[2])
      	    ralt      = REFORM(ralt  	,dim[0],dim[1],dim[2])
      	    tracer_nc = REFORM(tracer_nc,dim[0],dim[1],dim[2])
      	    ralt_nc   = REFORM(ralt_nc  ,dim[0],dim[1],dim[2])
      	    ralt1	  = REFORM(ralt1    ,dim[0],dim[1],dim[2])
          
      	    tracer 	  = REFORM(tracer	,dim[0]*dim[1],dim[2])
      	    ralt   	  = REFORM(ralt  	,dim[0]*dim[1],dim[2])
      	    tracer_nc = REFORM(tracer_nc,dim[0]*dim[1],dim[2])
      	    ralt_nc   = REFORM(ralt_nc  ,dim[0]*dim[1],dim[2])
      	    ralt1     = REFORM(ralt1  	,dim[0]*dim[1],dim[2])
                	
      	    tr_ave1		 = [ ]
      	    trnc_ave1	 = [ ] 
      	    ralt_mean1   = [ ]
      	    raltnc_mean1 = [ ]
      	    ralt1_mean1  = [ ]
      	    
      	    FOR k = 0, dim[2]-1 DO BEGIN
      	    	lev = SORT(tracer[*,k])
      	    	tr1 = tracer[lev,k]
          
      	    	levnc = SORT(tracer_nc[*,k])
      	    	trnc1 = tracer_nc[levnc,k]
          
      	    	tr_ave   = MEAN(tr1  ,/NAN)
      	    	trnc_ave = MEAN(trnc1,/NAN)
      	    	
      	    	;total rel. alt
      	    	ralt1_mean  = MEAN(ralt1[*,k],/NAN) 
      	    	ralt1_mean1 = [ralt1_mean1, ralt1_mean]
          
      	    	ralt_mean  = MEAN(ralt[*,k],/NAN) 
      	    	ralt_mean1 = [ralt_mean1, ralt_mean]
          
      	    	raltnc_mean  = MEAN(ralt_nc[*,k],/NAN) 
      	    	raltnc_mean1 = [raltnc_mean1, raltnc_mean]
      	    
      	    	tr_ave1   = [tr_ave1  ,tr_ave  ]
      	    	trnc_ave1 = [trnc_ave1,trnc_ave]
       	    ENDFOR
 		    	
 		    	IF KEYWORD_SET(percent_diff) THEN BEGIN
      	    	tr_ave1_diff = ((tr_ave1 - trnc_ave1) / (trnc_ave1))*100.0
 		    	ENDIF ELSE BEGIN        			
      	    	tr_ave1_diff = tr_ave1 - trnc_ave1
      	    ENDELSE
		    	
		    	PRINT, MIN(tr_ave1_diff,/NAN), MAX(tr_ave1_diff,/NAN)
		    	map_plot = [map_plot,tr_ave1_diff]
		    	ralt_arr = [ralt_arr, ralt1_mean1]
      		numlev = [0:dim[2]-1]
        ENDFOREACH	;date
   ENDFOREACH ;var

dd = FLTARR(dim[2],N_ELEMENTS(date_arr))
FOR ii = 0, N_ELEMENTS(date_arr)-1 DO BEGIN
	dd[*,ii] = ii
ENDFOR

map_plot = REFORM(map_plot,dim[2],N_ELEMENTS(date_arr))
ralt_arr = REFORM(ralt_arr,dim[2],N_ELEMENTS(date_arr))

map_pos = [0.05, 0.15, 0.95, 0.95]																			;Set map position
bar_pos = [0.25, 0.07, 0.75, 0.09]																			;Set color bar position

map_bar_title = 'Tracer = ' + STRING(var) 													;Set color bar title
map_bar_min   = map_bar_min																					;Set echo top minimum
map_bar_max   = map_bar_max																					;Set echo top maximum
map_bar_ticks = 4																						;Set number of color bar ticks
;map_table     = [HCL_COLOR_TABLE(41, HUE_RANGE = [100.0, 300.0])]                                                  ;Set color table
map_levels    = [MAKEN(map_bar_min,map_bar_max,41)]
map_table     = BLUE_RED_24(N_ELEMENTS(map_levels))

CONTOUR, map_plot, dd, ralt_arr, $																	;Contour values
       FILL      = 1, $
       LEVELS    = map_levels, $
       C_COLOR   = map_table, $ 
       POSITION  = map_pos


COLOR_BAR_24_KPB, map_table[1:*], OVER = map_table[-1], UNDER = map_table[0], $						;Draw map color bar
	TICKS = map_bar_ticks, $
	RANGE = [map_bar_min, map_bar_max], $
	TITLE = map_bar_title, $
	POSIT = bar_pos

!P.MULTI = 0
IF KEYWORD_SET(eps) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																;Reset color table to linear ramp
	PS_OFF																						;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)															;Write PNG file

STOP
END