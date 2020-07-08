PRO CONVECTIVE_INFLUENCE_DIFFERENCE, event, start_date, end_date, $
	DOMAIN       = domain, $
	PERCENTILE   = percentile, $
	PERCENT_DIFF = percent_diff, $
	VAR2         = var2, $
	ALT		     = alt, $
	REGION       = region, $
	NONCONV      = nonconv, $
	PNG	         = png, $
	EPS   	     = eps


;+
; Name:
;		CONVECTIVE_INFLUENCE_DIFFERENCE
; Purpose:
;		Computes difference in min/median/max profiles of O3,H2O,CO between compares those
;		profiles to areas with convective influence (using updraft and cloud-at-trop tracers) 
; 		to those without.
; Calling sequence:
;		CONVECTIVE_INFLUENCE_DIFFERENCE, run, scheme, start_date, end_date
; Example: CONVECTIVE_INFLUENCE_DIFFERENCE,'20120519','20120520T0000Z','20120520T0300Z', /BMP, /ALT
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
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
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
		scheme = ['seasonal_final/bigger_domain']
		label  = ['racm-yellowstone']
		key    = 'test'
		END

	'20130805' : BEGIN
		scheme = ['nest_final']
		label  = ['racm-yellowstone']
		key    = 'test'
		END
ENDCASE

IF (KEYWORD_SET(ALT)) THEN BEGIN
	yrange = [0, 25]
	ytitle = 'Altitude'
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
  
  	xrange1 = [-400,400]
     FOREACH var, var_arr DO BEGIN 
        IF c EQ 0 THEN BEGIN
        	IF var EQ 'H2O'  THEN BEGIN
        		position = [3,2,1]
        		xtitle   = 'H2O (ppmv)'
        		IF KEYWORD_SET(percent_diff) THEN xtitle   = 'H2O (%)'
        		xrange   = [0, 200]
;        		xrange0  = [0, 500]
        	ENDIF
        	IF var EQ 'O3'   THEN BEGIN
        		position = [3,2,2]
        		xtitle   = 'O3 (ppbv)'
        		IF KEYWORD_SET(percent_diff) THEN xtitle   = 'O3 (%)'
 				xrange   = [0   , 400]
        		xrange1  = [-200, 200]
 ;       		xrange0  = [0   , 800]

			ENDIF
        	IF var EQ 'CO'   THEN BEGIN
        		position = [3,2,3]
        		xtitle   = 'CO (ppbv)'
        		IF KEYWORD_SET(percent_diff) THEN xtitle   = 'CO (%)'
        		xrange   = [0, 200]
        		xrange1  = [-200, 200]
 ;       		xrange0  = [0, 200]
        	ENDIF
        ENDIF 
        IF c EQ 1 THEN BEGIN
        	IF var EQ 'H2O'  THEN BEGIN
        		position = [3,2,4]
        		xtitle   = 'H2O (ppmv)'
        		IF KEYWORD_SET(percent_diff) THEN xtitle   = 'H2O (%)'
	       		xrange   = [0, 200]
 ;       		xrange0  = [0, 500]
        	ENDIF
        	IF var EQ 'O3'   THEN BEGIN
        		position = [3,2,5]
        		xtitle   = 'O3 (ppbv)'
        		IF KEYWORD_SET(percent_diff) THEN xtitle   = 'O3 (%)'
				xrange   = [0   , 400]
       			xrange1  = [-200, 200]
;        		xrange0  = [0   , 500]

			ENDIF
        	IF var EQ 'CO'   THEN BEGIN
        		position = [3,2,6]
        		xtitle   = 'CO (ppbv)'
        		IF KEYWORD_SET(percent_diff) THEN xtitle   = 'CO (%)'
        		xrange   = [0, 200]
;        		xrange0  = [0, 500]
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
        
        
        color 	   = [COLOR_24_DP('RED'), COLOR_24_DP('BLUE'), COLOR_24_DP('DARKGREEN')]			
        
        tracer_obs = (WRF_READ_VAR('O3', date_arr[0], event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values
        ralt_obs   = (WRF_READ_VAR('Z' , date_arr[0], event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values
        
        i = 0
 	  	;IF KEYWORD_SET(percent_diff) THEN xrange1 = [-200,200]        
        yrange = [-5, 5]
        IF (KEYWORD_SET(ALT)) THEN yrange = [0, 25] 
        
        figure = PLOT(tracer_obs, ralt_obs, /NODATA, $
        		XRANGE = xrange1, $
        		YRANGE = yrange, $
        		LAYOUT = position, $
        		/CURRENT)
        
        figure = PLOT(tracer_obs, ralt_obs, /NODATA, $
        		XRANGE = xrange1, $
        		YRANGE = yrange, $
        		LAYOUT = position, $
        		/OVERPLOT)
        
        plt_var1 = [ ] 
        	FOREACH date, date_arr DO BEGIN
				PRINT, "Processing date: ", date
        		z		 = (WRF_READ_VAR('Z'     , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read height values
        		dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
        		z_trop   = (WRF_READ_VAR('Z_trop', date, event, scheme, DOMAIN = domain, INDICES = region)).values
        		z_trop   = MEDIAN(z_trop, 30)												;Filter tropopause values
        
        		precip   = (WRF_READ_VAR('PRECIP'	     , date, event, scheme, DOMAIN = domain, INDICES = region)).values
        		w	     = (WRF_READ_VAR('w' 	 	     , date, event, scheme, DOMAIN = domain, INDICES = region)).values
        		cld_tr   = (WRF_READ_VAR('Cloud_tracer'  , date, event, scheme, DOMAIN = domain, INDICES = region)).values
        		updraft  = (WRF_READ_VAR('Updraft_tracer', date, event, scheme, DOMAIN = domain, INDICES = region)).values
        
        		xyz_trop = REBIN(z_trop, dim[0], dim[1], dim[2], /SAMPLE)
        		cloud    = (WRF_READ_VAR('CLOUD_MIX_TOTAL' , date, event, scheme, DOMAIN = domain, INDICES = region)).values * 1.0E3			;Read in cloud mixing ratio values
       
        		plt_var  = (WRF_READ_VAR(var      		  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values
        		plt_var1 = [[[plt_var1]], [[plt_var]]]													
        		
        		IF var EQ 'O3'   THEN factor = 1.0E3
        		IF var EQ 'H2O'  THEN factor = 1.0E6
        		IF var EQ 'CO'   THEN factor = 1.0E3
        		IF var EQ 'HNO3' THEN factor = 1.0E6
        		IF var EQ 'NO2'  THEN factor = 1.0E6
        		IF var EQ 'SO2'  THEN factor = 1.0E6
        		IF var EQ 'HCHO' THEN factor = 1.0E6
        		
        		tracer = plt_var * factor
        		ralt   = (z - xyz_trop) * 1.0E-3
        
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
        
        		
;      		good = WHERE(((tracer GE xrange[0]) 	AND $												;Use specified data range
;      						(tracer LE xrange[1]) 	AND $												;Only used if filtering not desired
;      						(ralt   GE yrange[0])   AND $
;      						(ralt   LE yrange[1])), good_count, COMPLEMENT=out, NCOMPLEMENT = nout)
;             				  
;      		IF (nout GT 0) THEN BEGIN																	;Save good data points
;      			tracer    [out] = !Values.F_NaN
;      			ralt      [out] = !Values.F_NaN 
;      		ENDIF
        
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
        		tr_ave1		 = [ ]
        		trnc_med1    = [ ]
        		trnc_min1    = [ ]
        		trnc_max1    = [ ]
        		trnc_ave1	 = [ ] 
        		ralt_mean1   = [ ]
        		raltnc_mean1 = [ ]
        		
        		FOR k = 0, dim[2]-1 DO BEGIN
        			lev = SORT(tracer[*,k])
        			tr1 = tracer[lev,k]
       ; STOP
        			levnc = SORT(tracer_nc[*,k])
        			trnc1 = tracer_nc[levnc,k]
        
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
        	
        			tr_med1 = [tr_med1,tr_med]
        			tr_min1 = [tr_min1,tr_min]
        			tr_max1 = [tr_max1,tr_max]
        			tr_ave1 = [tr_ave1,tr_ave]
        
        			trnc_med1 = [trnc_med1,trnc_med]
        			trnc_min1 = [trnc_min1,trnc_min]
        			trnc_max1 = [trnc_max1,trnc_max]
        			trnc_ave1 = [trnc_ave1,trnc_ave]
         		ENDFOR
 				
 				IF KEYWORD_SET(percent_diff) THEN BEGIN
         			tr_med1_diff = ((tr_med1 - trnc_med1) / (trnc_med1))*100.0
        			tr_max1_diff = ((tr_max1 - trnc_max1) / (trnc_max1))*100.0
        			tr_min1_diff = ((tr_min1 - trnc_min1) / (trnc_min1))*100.0
        			tr_ave1_diff = ((tr_ave1 - trnc_ave1) / (trnc_ave1))*100.0
 				ENDIF ELSE BEGIN        			
        			tr_med1_diff = tr_med1 - trnc_med1
        			tr_max1_diff = tr_max1 - trnc_max1
        			tr_min1_diff = tr_min1 - trnc_min1
        			tr_ave1_diff = tr_ave1 - trnc_ave1
        		ENDELSE
				
				PRINT, MIN(tr_min1_diff), MAX(tr_max1_diff)
        		numlev = [0:dim[2]-1]
        
        		figure3   = PLOT(tr_med1_diff, ralt_mean1, LINESTYLE =0, XMINOR =1, COLOR = color[0], THICK = 2, /OVERPLOT, LAYOUT = position)		
;        		figure4   = PLOT(tr_max1_diff, ralt_mean1, LINESTYLE =2, XMINOR =1, COLOR = color[1], THICK = 2, /OVERPLOT, LAYOUT = position)
        		figure5   = PLOT(tr_ave1_diff, ralt_mean1, LINESTYLE =2, XMINOR =1, COLOR = color[1], THICK = 2, /OVERPLOT, LAYOUT = position)

        		IF KEYWORD_SET(~var2) THEN $
;       		figure6   = PLOT(tr_min1_diff, ralt_mean1, LINESTYLE =2, XMINOR =1, COLOR = color[2], THICK = 2, /OVERPLOT, LAYOUT = position)
        
        
        figure7 = PLOT(xrange1,[0.0,0.0], /OVERPLOT, LAYOUT = position, XTITLE = xtitle)
        figure8 = PLOT([0.0,0.0], yrange, /OVERPLOT, LAYOUT = position, XTITLE = xtitle)

	
        	i = i + 1        
     ENDFOREACH ;var

ENDFOR


t2 = TEXT(545,455,  'Median', FONT_SIZE=10, /DEVICE, FONT_COLOR = !COLOR.blue )
t2 = TEXT(545,440,  'Mean'  , FONT_SIZE=10, /DEVICE, FONT_COLOR = !COLOR.red )

;t3 = TEXT(545,440,  'Max'   , FONT_SIZE=10, /DEVICE, FONT_COLOR = !COLOR.red  )
;t4 = TEXT(545,425,  'Min'   , FONT_SIZE=10, /DEVICE, FONT_COLOR = !COLOR.green)

!P.MULTI = 0
IF KEYWORD_SET(eps) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																;Reset color table to linear ramp
	PS_OFF																						;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)															;Write PNG file

END