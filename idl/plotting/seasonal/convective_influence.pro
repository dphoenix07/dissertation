PRO CONVECTIVE_INFLUENCE, event, start_date, end_date, $
	DOMAIN      = domain, $
	PERCENTILE  = percentile, $
	ISENTROPE   = isentrope, $
	VAR2        = var2, $
	ALT		    = alt, $
	TROPICS	    = tropics, $
	MID_LAT     = mid_lat, $
	CLOUD_IN    = cloud_in, $
	REGION      = region, $
	NONCONV     = nonconv, $
	PNG	        = png, $
	EPS   	    = eps


;+
; Name:
;		CONVECTIVE_INFLUENCE
; Purpose:
;		Computes min/median/max profiles of O3,H2O,CO and compares those profiles with
;		convective influence (using updraft and cloud-at-trop tracers) to those without.
; Calling sequence:
;		CONVECTIVE_INFLUENCE, run, scheme, start_date, end_date
; Example: CONVECTIVE_INFLUENCE,'20120519','20120520T0000Z','20120520T0300Z'
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
;		Daniel B. Phoenix	    2017-07-26. 
;								2018-01-29.	Edited to use theta surfaces as vertical
;											coordinate.
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain
IF (N_ELEMENTS(threshold ) EQ 0) THEN threshold  = 1000.0
IF (N_ELEMENTS(xrange	 ) EQ 0) THEN xrange     = [  0, 800]
IF (N_ELEMENTS(xtitle	 ) EQ 0) THEN xtitle     = 'Ozone (ppbv)'
IF (N_ELEMENTS(yrange	 ) EQ 0) THEN yrange     = [-5,   5]
IF (N_ELEMENTS(ytitle	 ) EQ 0) THEN ytitle     = 'Relative Altitude'
IF (N_ELEMENTS(nxbin 	 ) EQ 0) THEN nxbin      = 50
IF (N_ELEMENTS(nybin 	 ) EQ 0) THEN nybin      = 50
IF (N_ELEMENTS(var2		 ) EQ 0) THEN var2		 = 0


IF (KEYWORD_SET(cloud_in)) THEN BEGIN 
	cloud_in = 1
ENDIF ELSE BEGIN
	cloud_in = 0
ENDELSE

CASE event OF
	'20110518' : BEGIN
		scheme = ['seasonal_final/bigger_domain']
		label  = ['racm-yellowstone']
		key    = 'test'
		END
	
	'20130805' : BEGIN
		scheme = ['nest_final']
		key	   = 'test'
		END
ENDCASE

IF (KEYWORD_SET(ALT)) THEN BEGIN
	yrange = [0, 18]
	ytitle = 'Altitude'
ENDIF

IF (KEYWORD_SET(isentrope)) THEN BEGIN
	yrange = [300, 450]
	ytitle = 'Potential Temperature'
ENDIF

IF (N_ELEMENTS(region) EQ 0) THEN BEGIN
	IF (event EQ '20120519') THEN region = [50, 50, 250, 190]
ENDIF

outdir  = !WRF_DIRECTORY + event + '/paper/plots/chemistry_profiles/'
epsfile = outdir + key + '_' + end_date + '.eps'						;EPS filename
pdffile = outdir + key + '_' + end_date + '.pdf'						;PDF filename
pngfile = outdir + key + '_' + end_date + '.png'						;PNG filename

FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(event, scheme, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(event, scheme, start_date, end_date, /DATE)	

in_cloud = 0
c = 0
FOR c = 0, 1 DO BEGIN
	 in_cloud = c

     var_arr = ['H2O', 'O3', 'CO']
     
     IF (KEYWORD_SET(var2)) THEN var_arr = ['HNO3', 'SO2', 'HCHO']
     ;IF (KEYWORD_SET(var2)) THEN var_arr = ['HNO3', 'SO2', 'NO2']
          
     FOREACH var, var_arr DO BEGIN 
        IF c EQ 0 THEN BEGIN
        	IF var EQ 'H2O'  THEN BEGIN
        		position = [3,2,1]
        		xtitle   = 'H2O (ppmv)'
        		xrange   = [0, 200]
        	ENDIF
        	IF var EQ 'O3'   THEN BEGIN
        		position = [3,2,2]
        		xtitle   = 'O3 (ppbv)'
        		xrange   = [0, 400]
			ENDIF
        	IF var EQ 'CO'   THEN BEGIN
        		position = [3,2,3]
        		xtitle   = 'CO (ppbv)'
        		xrange   = [0, 200]
        	ENDIF
        ENDIF 
        IF c EQ 1 THEN BEGIN
        	IF var EQ 'H2O'  THEN BEGIN
        		position = [3,2,4]
        		xtitle   = 'H2O (ppmv)'
        		xrange   = [0, 200]
        	ENDIF
        	IF var EQ 'O3'   THEN BEGIN
        		position = [3,2,5]
        		xtitle   = 'O3 (ppbv)'
        		xrange   = [0, 400]
			ENDIF
        	IF var EQ 'CO'   THEN BEGIN
        		position = [3,2,6]
        		xtitle   = 'CO (ppbv)'
        		xrange   = [0, 200]
        	ENDIF
        ENDIF
        
        IF (KEYWORD_SET(var2)) THEN BEGIN
        	IF c EQ 0 THEN BEGIN
        		IF var EQ 'HNO3'  THEN position = [3,2,1]
        		IF var EQ 'SO2'   THEN position = [3,2,2]
        		IF var EQ 'HCHO'  THEN position = [3,2,3]
        ;		IF var EQ 'NO2'   THEN position = [3,2,3]
        	ENDIF 
        	IF c EQ 1 THEN BEGIN
        		IF var EQ 'HNO3'  THEN position = [3,2,4]
        		IF var EQ 'SO2'   THEN position = [3,2,5]
        		IF var EQ 'HCHO'  THEN position = [3,2,6]
        ;		IF var EQ 'NO2'   THEN position = [3,2,6]
        	ENDIF
        ENDIF
        
        
        color 	   = [COLOR_24_DP('RED'), COLOR_24_DP('BLUE')]			
        
        tracer_obs = (WRF_READ_VAR('O3', date_arr[0], event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values
        ralt_obs   = (WRF_READ_VAR('Z' , date_arr[0], event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values
        
        IF (KEYWORD_SET(isentrope)) THEN ralt_obs = (WRF_READ_VAR('theta', date_arr[0], event, scheme, DOMAIN = domain, INDICES = region)).values 
        
        i = 0
        yrange = [-5, 5]
        IF (KEYWORD_SET(ALT)) THEN yrange = [0, 18] 
        IF (KEYWORD_SET(isentrope)) THEN yrange = [300, 450]
        
        figure = PLOT(tracer_obs, ralt_obs, /NODATA, $
        		XRANGE = xrange, $
        		YRANGE = yrange, $
        ;		YTITLE = 'Tropopause Relative (km)', $
        		LAYOUT = position, $
        		/CURRENT)
        
        figure = PLOT(tracer_obs, ralt_obs, /NODATA, $
        		XRANGE = xrange, $
        		YRANGE = yrange, $
        		LAYOUT = position, $
        		/OVERPLOT)
        
        plt_var1 = [ ] 
        	FOREACH date, date_arr DO BEGIN
				PRINT, "Processing date: ", date
        		z		= (WRF_READ_VAR('Z'     , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read height values
        		dim 	= SIZE(z, /DIMENSIONS)																						;Get dimensions (to reform arrays later)
        		z_trop  = (WRF_READ_VAR('Z_trop', date, event, scheme, DOMAIN = domain, INDICES = region)).values
			  	z_trop   = MEDIAN(z_trop, 100)
      
				theta   = (WRF_READ_VAR('T'			     , date, event, scheme, DOMAIN = domain, INDICES = region)).values					
				theta   = ((1000.0/(WRF_READ_VAR('P'     , date, event, scheme, DOMAIN = domain, INDICES = region)).values)^(!Rair/!Cp))*(theta)
        		precip  = (WRF_READ_VAR('PRECIP'	     , date, event, scheme, DOMAIN = domain, INDICES = region)).values
        		w	    = (WRF_READ_VAR('w' 	 	     , date, event, scheme, DOMAIN = domain, INDICES = region)).values
        		cld_tr  = (WRF_READ_VAR('Cloud_tracer'   , date, event, scheme, DOMAIN = domain, INDICES = region)).values
        		updraft = (WRF_READ_VAR('Updraft_tracer' , date, event, scheme, DOMAIN = domain, INDICES = region)).values
        
        		xyz_trop = REBIN(z_trop, dim[0], dim[1], dim[2], /SAMPLE)

 	            IF (KEYWORD_SET(tropics)) THEN igood = WHERE(xyz_trop GT 15000.0, count, COMPLEMENT = filt)
 	            IF (KEYWORD_SET(mid_lat)) THEN igood = WHERE(xyz_trop LT 15000.0, count, COMPLEMENT = filt)
        
        		cloud   		= (WRF_READ_VAR('CLOUD_MIX_TOTAL' , date, event, scheme, DOMAIN = domain, INDICES = region)).values * 1.0E3			;Read in cloud mixing ratio values
        		plt_var      	= (WRF_READ_VAR(var      		  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values
 
         		IF (N_ELEMENTS(filt) GT 0) THEN BEGIN
         		    z[filt] 	    = !Values.F_NaN
        		    xyz_trop[filt]  = !Values.F_NaN
        		    cloud[filt]     = !Values.F_NaN
        		    plt_var[filt]   = !Values.F_NaN
				ENDIF
				
        		plt_var1		= [[[plt_var1]], [[plt_var]]]													
        		
        		IF var EQ 'O3'   THEN factor = 1.0E3
        		IF var EQ 'H2O'  THEN factor = 1.0E6
        		IF var EQ 'CO'   THEN factor = 1.0E3
        		IF var EQ 'HNO3' THEN factor = 1.0E6
        		IF var EQ 'NO2'  THEN factor = 1.0E6
        		IF var EQ 'SO2'  THEN factor = 1.0E6
        		IF var EQ 'HCHO' THEN factor = 1.0E6
        		
        		tracer = plt_var * factor
        		ralt   = (z - xyz_trop) * 1.0E-3
        		IF (KEYWORD_SET(isentrope)) THEN ralt = theta
        
        		tracer_nc = FLTARR(dim[0],dim[1],dim[2])
        		ralt_nc   = FLTARR(dim[0],dim[1],dim[2])
        		
        		IF (KEYWORD_SET(ALT)) THEN ralt = z * 1.0E-3
        
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
              	IF (cloud_in EQ 0) THEN BEGIN
              	    PRINT,'cloud_tracer'
              	    
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
              	 ENDIF ELSE BEGIN
 			    	IF (KEYWORD_SET(cloud_in)) THEN BEGIN       
        	    	    PRINT, 'in_cloud'
        	    	    cloud_values = WHERE(cloud GE 0.1, cld_count, COMPLEMENT = non_cloud, $							;Find values in cloud 
        	    	    					NCOMPLEMENT = ncld_count)
                
        	    	    IF (cld_count GT 0) THEN BEGIN
        	    	    	tracer   [cloud_values] = tracer[cloud_values]
        	    	    	ralt     [cloud_values] = ralt  [cloud_values]
        	    	    	tracer_nc[cloud_values] = !Values.F_NaN
        	    	    	ralt_nc  [cloud_values] = !Values.F_NaN
        	    	    ENDIF
        	    	    IF (ncld_count GT 0) THEN BEGIN
        	    	    	tracer_nc [non_cloud] = tracer[non_cloud]
        	    	    	ralt_nc	  [non_cloud] = ralt  [non_cloud]
        	    	    	tracer    [non_cloud] = !Values.F_NaN
        	    	    	ralt      [non_cloud] = !Values.F_NaN
        	    	    ENDIF
        	    	ENDIF
        	      ENDELSE	
        	ENDELSE
        		
      		good = WHERE(((tracer GE xrange[0]) 	AND $												;Use specified data range
      						(tracer LE xrange[1]) 	AND $												;Only used if filtering not desired
      						(ralt   GE yrange[0])   AND $
      						(ralt   LE yrange[1])), good_count, COMPLEMENT=out, NCOMPLEMENT = nout)
             				  
      		IF (nout GT 0) THEN BEGIN																	;Save good data points
      			tracer    [out] = !Values.F_NaN
      			ralt      [out] = !Values.F_NaN 
      		ENDIF
        
        		tracer    = REFORM(tracer	,dim[0],dim[1],dim[2])
        		ralt      = REFORM(ralt  	,dim[0],dim[1],dim[2])
        		tracer_nc = REFORM(tracer_nc,dim[0],dim[1],dim[2])
        		ralt_nc   = REFORM(ralt_nc  ,dim[0],dim[1],dim[2])
        
        		tracer 	  = REFORM(tracer	,dim[0]*dim[1],dim[2])
        		ralt   	  = REFORM(ralt  	,dim[0]*dim[1],dim[2])
        		tracer_nc = REFORM(tracer_nc,dim[0]*dim[1],dim[2])
        		ralt_nc   = REFORM(ralt_nc  ,dim[0]*dim[1],dim[2])
        
        	ENDFOREACH	;date
        	
        		tr_med1   	 = [ ]
        		tr_min1   	 = [ ]
        		tr_max1    	 = [ ] 
        		tr_ave1	     = [ ]
        		trnc_med1    = [ ]
        		trnc_min1    = [ ]
        		trnc_max1    = [ ]
        		trnc_ave1    = [ ] 
        		ralt_mean1   = [ ]
        		raltnc_mean1 = [ ]
        		
        		FOR k = 0, dim[2]-1 DO BEGIN
        			lev = SORT(tracer[*,k])
        			tr1 = tracer[lev,k]
        
        			levnc = SORT(tracer_nc[*,k])
        			trnc1 = tracer_nc[levnc,k]
 
 					iconv = WHERE(FINITE(tr1,/NAN),conv_count, COMPLEMENT=jconv, NCOMPLEMENT=jconv_count)
 					inonc = WHERE(FINITE(trnc1,/NAN),nonc_count, COMPLEMENT=jnconv, NCOMPLEMENT=jnonc_count)
 					PRINT, k, jconv_count, jnonc_count
 					
        			tr_med = MEDIAN(tr1)
        			tr_min = MIN(tr1,/NAN)
        			tr_max = MAX(tr1,/NAN)
        			tr_ave = MEAN(tr1,/NAN)
  
	    			trnc_med = MEDIAN(trnc1)
        			trnc_min = MIN(trnc1,/NAN)
        			trnc_max = MAX(trnc1,/NAN)
        			trnc_ave = MEAN(trnc1,/NAN)
 
  					IF (N_ELEMENTS(percentile) GT 0) THEN BEGIN
 						tr_min   = tr1  [PERCENTILE(tr1  ,percentile[0],/NOSORT,/NAN)]
						tr_max   = tr1  [PERCENTILE(tr1  ,percentile[1],/NOSORT,/NAN)]
 						trnc_min = trnc1[PERCENTILE(trnc1,percentile[0],/NOSORT,/NAN)]
						trnc_max = trnc1[PERCENTILE(trnc1,percentile[1],/NOSORT,/NAN)]
       				ENDIF
       				
        			ralt_mean  = MEAN(ralt[*,k],/NAN) 
        			ralt_mean1 = [ralt_mean1, ralt_mean]
        
        			raltnc_mean  = MEAN(ralt_nc[*,k],/NAN) 
        			raltnc_mean1 = [raltnc_mean1, raltnc_mean]
        	
        			tr_ave1 = [tr_ave1,tr_ave]
        			tr_med1 = [tr_med1,tr_med]
        			tr_min1 = [tr_min1,tr_min]
        			tr_max1 = [tr_max1,tr_max]
        
        			trnc_ave1 = [trnc_ave1,trnc_ave]
        			trnc_med1 = [trnc_med1,trnc_med]
        			trnc_min1 = [trnc_min1,trnc_min]
        			trnc_max1 = [trnc_max1,trnc_max]
        
        		ENDFOR
        
        		numlev = [0:dim[2]-1]
        
        		figure3   = PLOT(tr_ave1  , ralt_mean1, LINESTYLE =0, XMINOR =1, COLOR = color[0], THICK = 2, /OVERPLOT, LAYOUT = position)		
        		figure3nc = PLOT(trnc_ave1, ralt_mean1, LINESTYLE =0, XMINOR =1, COLOR = color[1], THICK = 2, /OVERPLOT, LAYOUT = position)		
        		figure4   = PLOT(tr_max1  , ralt_mean1, LINESTYLE =2, XMINOR =1, COLOR = color[0], THICK = 2, /OVERPLOT, LAYOUT = position)
        		figure4nc = PLOT(trnc_max1, ralt_mean1, LINESTYLE =2, XMINOR =1, COLOR = color[1], THICK = 2, /OVERPLOT, LAYOUT = position)
        		
        		IF KEYWORD_SET(~var2) THEN BEGIN
        			figure5   = PLOT(tr_min1  , ralt_mean1, LINESTYLE =2, XMINOR =1, COLOR = color[0], THICK = 2, /OVERPLOT, LAYOUT = position)
        			figure5nc = PLOT(trnc_min1, ralt_mean1, LINESTYLE =2, XMINOR =1, COLOR = color[1], THICK = 2, /OVERPLOT, LAYOUT = position)
        		ENDIF
        		        
        figure6 = PLOT(xrange,[0.0,0.0], /OVERPLOT, LAYOUT = position, XTITLE = xtitle)

        	i = i + 1  
     
     ENDFOREACH ;var

ENDFOR

t2 = TEXT(545,455,  'Conv'   , FONT_SIZE=10, /DEVICE, FONT_COLOR = !COLOR.blue)
t3 = TEXT(545,440,  'No-conv', FONT_SIZE=10, /DEVICE, FONT_COLOR = !COLOR.red )

!P.MULTI = 0
IF KEYWORD_SET(eps) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																;Reset color table to linear ramp
	PS_OFF																						;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)															;Write PNG file

STOP
END