PRO TIME_HEIGHT_CONVECTIVE_INFLUENCE_NEW, event, start_date, end_date, $
	DOMAIN         = domain, $
	PERCENT_DIFF   = percent_diff, $
	ALT		       = alt, $
	TROPICS		   = tropics, $
	MID_LAT	  	   = mid_lat, $
	DOUBLE_TROP    = double_trop, $
	CLD_COVER	   = cld_cover, $
	OVERWORLD	   = overworld, $
	COLUMN		   = column, $
	REGION         = region, $
	NONCONV        = nonconv, $
	PNG	           = png, $
	EPS   	       = eps


;+
; Name:
;		TIME_HEIGHT_CONVECTIVE_INFLUENCE_NEW
; Purpose:
;		Computes a time-height diagram of the percent difference in mean profiles of 
;		a user supplied tracer (O3,H2O,CO,T) between convective and non-convective
;		areas.
; Calling sequence:
;		TIME_HEIGHT_CONVECTIVE_INFLUENCE_NEW, run, start_date, end_date
; Example: TIME_HEIGHT_CONVECTIVE_INFLUENCE_NEW,'20110518','20110518T1300Z','20110527T1200Z', tracer = 'O3'
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
;		Daniel B. Phoenix	    2018-02-15.	Implements new features of script, including
;											agressive median filter of the tropopause,
;											separates extra-tropical and tropical 
;											environments, single vs. double tropopause
;											environments, and tracks convective intensity.	 
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
		key    = 'time_hgt'
		END

	'20120519' : BEGIN
		scheme = ['lightnox_test2']
		key	   = 'time_hgt'
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
	!P.CHARSIZE   = 1.5		
	!P.FONT       = -1																			;Use Hershey fonts
ENDELSE
!P.MULTI = [0, 2, 2]

tracer_obs = (WRF_READ_VAR('O3', date_arr[0], event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values
ralt_obs   = (WRF_READ_VAR('Z' , date_arr[0], event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values

c = 0
in_cloud = 0

map_plot   = [ ]
cont_plot  = [ ]
T_plot	   = [ ]
conv_plot  = [ ]
conv_plot1 = [ ] 
ralt_arr   = [ ]

range1 = [-10,10]
FOREACH date, date_arr DO BEGIN
	PRINT, "Processing date: ", date
    z		 = (WRF_READ_VAR('Z'     , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read height values
    dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
    z_trop   = (WRF_READ_VAR('Z_trop', date, event, scheme, DOMAIN = domain, INDICES = region)).values
 ;   z_trop   = CALC_TROP_MODE(z_trop, scheme, threshold) 												;Filter tropopause values
	z_trop   = MEDIAN(z_trop, 100)
    
    precip   = (WRF_READ_VAR('PRECIP'	     , date, event, scheme, DOMAIN = domain, INDICES = region)).values
    w	     = (WRF_READ_VAR('w' 	 	     , date, event, scheme, DOMAIN = domain, INDICES = region)).values
    cld_tr   = (WRF_READ_VAR('Cloud_tracer'  , date, event, scheme, DOMAIN = domain, INDICES = region)).values
    updraft  = (WRF_READ_VAR('Updraft_tracer', date, event, scheme, DOMAIN = domain, INDICES = region)).values
    
    xyz_trop = REBIN(z_trop, dim[0], dim[1], dim[2], /SAMPLE)

	count = 0
 	IF (KEYWORD_SET(tropics)) THEN igood = WHERE(xyz_trop GT 15000.0, count, COMPLEMENT = ibad)
 	IF (KEYWORD_SET(mid_lat)) THEN igood = WHERE(xyz_trop LT 15000.0, count, COMPLEMENT = ibad)

    cloud   	= (WRF_READ_VAR('CLOUD_MIX_TOTAL' , date, event, scheme, DOMAIN = domain, INDICES = region)).values * 1.0E3			;Read in cloud mixing ratio values
    plt_var_o3  = (WRF_READ_VAR('O3'       	      , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values
    plt_var_h2o = (WRF_READ_VAR('H2O'      	      , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values
    plt_var_T   = (WRF_READ_VAR('T'        	      , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values

	IF (KEYWORD_SET (overworld)) THEN plt_var_o3 = (WRF_READ_VAR('ST_tracer', date, event, scheme, DOMAIN = domain, INDICES = region)).values

	IF (count GT 0) THEN BEGIN
        z[ibad] 	      = !Values.F_NaN
        xyz_trop[ibad]    = !Values.F_NaN
        cloud[ibad]       = !Values.F_NaN
        plt_var_o3[ibad]  = !Values.F_NaN
        plt_var_h2o[ibad] = !Values.F_NaN
        plt_var_T[ibad]   = !Values.F_NaN
    ENDIF
    
    factor1 = 1.0
    factor2 = 1.0E3
    factor3 = 1.0E6
    
    tracerT	  = plt_var_T   * factor1
    tracero3  = plt_var_o3  * factor2
    tracerh2o = plt_var_h2o * factor3 
    ralt   = (z - xyz_trop) * 1.0E-3
    ralt1  = (z - xyz_trop) * 1.0E-3

    tracerT_nc   = FLTARR(dim[0],dim[1],dim[2])    
    tracero3_nc  = FLTARR(dim[0],dim[1],dim[2])
    tracerh2o_nc = FLTARR(dim[0],dim[1],dim[2])
    ralt_nc      = FLTARR(dim[0],dim[1],dim[2])
  
    IF (KEYWORD_SET(ALT)) THEN ralt1 = z * 1.0E-3
      
      ;;DBP 7/23/2017 removed this block about convective influence for seasonal anaylsis
      ;		FOR ii=0,dim[0]-1 DO BEGIN
      ;			FOR jj=0,dim[1]-1 DO BEGIN
      ;				IF (MAX(w[ii,jj,*]) GE 5.0) THEN BEGIN
      ;					tracero3[ii,jj,*] = !Values.F_NaN
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
      ;			tracero3 [p_updraft] = !Values.F_NaN
      ;			ralt   [p_updraft] = !Values.F_NaN
      ;			cloud  [p_updraft] = !Values.F_NaN
      ;		ENDIF				
      ;;end
      

   IF (c EQ 0) THEN BEGIN
    	   conv_values = WHERE(updraft GT 0.5, conv_count, COMPLEMENT = non_conv, $							;Find values in cloud 
       					NCOMPLEMENT = nconv_count)
       		PRINT, 'Convective_count = ', conv_count, ' Non-convective = ', nconv_count
       	IF (conv_count GT 0) THEN BEGIN
       		tracero3    [conv_values] = tracero3 [conv_values]
       		tracerh2o   [conv_values] = tracerh2o[conv_values]
       		tracerT     [conv_values] = tracerT  [conv_values]
       		ralt        [conv_values] = ralt     [conv_values]
       		tracero3_nc [conv_values] = !Values.F_NAN
       		tracerh2o_nc[conv_values] = !Values.F_NAN
       		tracerT_nc  [conv_values] = !Values.F_NAN
       		ralt_nc     [conv_values] = !Values.F_NAN
       	ENDIF

       	IF (nconv_count GT 0) THEN BEGIN
       		tracero3_nc [non_conv] = tracero3 [non_conv]
       		tracerh2o_nc[non_conv] = tracerh2o[non_conv]
       		tracerT_nc  [non_conv] = tracerT  [non_conv]
       		ralt_nc     [non_conv] = ralt     [non_conv]
   			tracero3    [non_conv] = !Values.F_NAN
   			tracerh2o   [non_conv] = !Values.F_NAN
   			tracerT     [non_conv] = !Values.F_NAN
   			ralt        [non_conv] = !Values.F_NAN
       	ENDIF

   
   ENDIF ELSE BEGIN
       	cld_tr_values =  WHERE(cld_tr GT 0.1, cld_count, COMPLEMENT = non_cld, $							;Find values in cloud 
       					NCOMPLEMENT = ncld_count)
       					
       	IF (cld_count GT 0) THEN BEGIN
       		tracero3    [cld_tr_values] = tracero3 [cld_tr_values]
       		tracerh2o   [cld_tr_values] = tracerh2o[cld_tr_values]
       		tracerT     [cld_tr_values] = tracerT  [cld_tr_values]
       		ralt        [cld_tr_values] = ralt     [cld_tr_values]
       		tracero3_nc [cld_tr_values] = !Values.F_NAN
       		tracerh2o_nc[cld_tr_values] = !Values.F_NAN
       		tracerT_nc  [cld_tr_values] = !Values.F_NAN
       		ralt_nc     [cld_tr_values] = !Values.F_NAN
       	ENDIF
       	IF (ncld_count GT 0) THEN BEGIN
       		tracero3_nc [non_cld] = tracero3 [non_cld]
       		tracerh2o_nc[non_cld] = tracerh2o[non_cld]
       		tracerT_nc  [non_cld] = tracerT  [non_cld]
       		ralt_nc     [non_cld] = ralt     [non_cld]
   			tracero3    [non_cld] = !Values.F_NAN
   			tracerh2o   [non_cld] = !Values.F_NAN
   			tracerT     [non_cld] = !Values.F_NAN
   			ralt        [non_cld] = !Values.F_NAN
       	ENDIF
   ENDELSE
   
      ;		cloud_values = WHERE(cloud GE 0.1, cld_count, COMPLEMENT = non_cloud, $							;Find values in cloud 
      ;							NCOMPLEMENT = ncld_count)
      ;
      ;		IF KEYWORD_SET(in_cloud) THEN BEGIN																;Sort values in cloud vs out of cloud
      ;			IF (cld_count GT 0) THEN BEGIN
      ;				tracero3[non_cloud] = !Values.F_NaN
      ;				ralt  [non_cloud] = !Values.F_NaN
      ;			ENDIF
      ;		ENDIF ELSE BEGIN
      ;			IF (ncld_count GT 0) THEN BEGIN
      ;				tracero3 [cloud_values] = !Values.F_NaN
      ;				ralt   [cloud_values] = !Values.F_NaN
      ;			ENDIF
      ;		ENDELSE
      
      		
;    		good = WHERE(((tracero3 GE xrange[0]) 	AND $												;Use specified data range
;    						(tracero3 LE xrange[1]) 	AND $												;Only used if filtering not desired
;    						(ralt   GE yrange[0])   AND $
;    						(ralt   LE yrange[1])), good_count, COMPLEMENT=out, NCOMPLEMENT = nout)
;           				  
;    		IF (nout GT 0) THEN BEGIN																	;Save good data points
;    			tracero3    [out] = !Values.F_NaN
;    			ralt      [out] = !Values.F_NaN 
;    		ENDIF
      
      	tracero3     = REFORM(tracero3	  ,dim[0],dim[1],dim[2])
      	tracerh2o    = REFORM(tracerh2o	  ,dim[0],dim[1],dim[2])
      	tracerT      = REFORM(tracerT	  ,dim[0],dim[1],dim[2])
      	ralt         = REFORM(ralt  	  ,dim[0],dim[1],dim[2])
      	tracero3_nc  = REFORM(tracero3_nc ,dim[0],dim[1],dim[2])
      	tracerh2o_nc = REFORM(tracerh2o_nc,dim[0],dim[1],dim[2])
      	tracerT_nc   = REFORM(tracerT_nc  ,dim[0],dim[1],dim[2])
      	ralt_nc      = REFORM(ralt_nc     ,dim[0],dim[1],dim[2])
      	ralt1	     = REFORM(ralt1       ,dim[0],dim[1],dim[2])
       
      	tracero3 	 = REFORM(tracero3	  ,dim[0]*dim[1],dim[2])
      	tracerh2o    = REFORM(tracerh2o   ,dim[0]*dim[1],dim[2])
      	tracerT  	 = REFORM(tracerT 	  ,dim[0]*dim[1],dim[2])
      	ralt   	     = REFORM(ralt  	  ,dim[0]*dim[1],dim[2])
      	tracero3_nc  = REFORM(tracero3_nc ,dim[0]*dim[1],dim[2])
      	tracerh2o_nc = REFORM(tracerh2o_nc,dim[0]*dim[1],dim[2])
      	tracerT_nc   = REFORM(tracerT_nc  ,dim[0]*dim[1],dim[2])
      	ralt_nc      = REFORM(ralt_nc     ,dim[0]*dim[1],dim[2])
      	ralt1        = REFORM(ralt1  	  ,dim[0]*dim[1],dim[2])
      	plt_var_o3   = REFORM(plt_var_o3  ,dim[0]*dim[1],dim[2])
       
      	o3_ave1		 = [ ]
      	o3nc_ave1	 = [ ] 
      	h2o_ave1	 = [ ]
      	h2onc_ave1	 = [ ] 
      	T_ave1		 = [ ]
      	Tnc_ave1	 = [ ] 
      	ralt_mean1   = [ ]
      	raltnc_mean1 = [ ]
      	ralt1_mean1  = [ ]
      	c_count 	 = [ ]
      	dbl_o3	     = [ ] 
      	dbl_o31	     = [ ]
      	
      	FOR k = 0, dim[2]-1 DO BEGIN
      		levo3  = SORT(tracero3[*,k])
      		o31    = tracero3[levo3,k]
      		levh2o = SORT(tracerh2o[*,k])
      		h2o1   = tracerh2o[levh2o,k]
      		levT   = SORT(tracerT[*,k])
      		T1     = tracerT[levT,k]
       
      		levo3nc  = SORT(tracero3_nc[*,k])
      		o3nc1    = tracero3_nc[levo3nc,k]
      		levh2onc = SORT(tracerh2o_nc[*,k])
      		h2onc1   = tracerh2o_nc[levh2onc,k]
      		levTnc  = SORT(tracerT_nc[*,k])
      		Tnc1    = tracerT_nc[levTnc,k]
       
			IF (KEYWORD_SET(cld_cover)) THEN BEGIN
				cld_count   = WHERE(cloud[*,*,k] GT 0.0, count)
 				jconv_count = (FLOAT(count) / FLOAT(dim[0]*dim[1])) * 100.0 				
	  			c_count     = [c_count , jconv_count]
; 				PRINT, k, jconv_count, c_count[k] 			
			ENDIF ELSE BEGIN
			  	iconv = WHERE(FINITE(o31,/NAN),conv_count, COMPLEMENT=jconv, NCOMPLEMENT=jconv_count)				
 				jconv_count = (FLOAT(jconv_count) / FLOAT(dim[0]*dim[1])) * 100.0 				
  				c_count  = [c_count , jconv_count]
  			ENDELSE

			IF (KEYWORD_SET(double_trop)) THEN BEGIN
				dbl_o3  = MEAN(plt_var_o3[*,k], /NAN)
				dbl_o31 = [dbl_o31, dbl_o3] 
;			 	PRINT, k, dbl_o3
			ENDIF

      		o3_ave   = MEAN(o31  ,/NAN)
      		o3nc_ave = MEAN(o3nc1,/NAN)

      		h2o_ave   = MEAN(h2o1  ,/NAN)
      		h2onc_ave = MEAN(h2onc1,/NAN)

      		T_ave   = MEAN(T1  ,/NAN)
      		Tnc_ave = MEAN(Tnc1,/NAN)
      		
      		;total rel. alt
      		ralt1_mean  = MEAN(ralt1[*,k],/NAN) 
      		ralt1_mean1 = [ralt1_mean1, ralt1_mean]
       
      		ralt_mean  = MEAN(ralt[*,k],/NAN) 
      		ralt_mean1 = [ralt_mean1, ralt_mean]
       
      		raltnc_mean  = MEAN(ralt_nc[*,k],/NAN) 
      		raltnc_mean1 = [raltnc_mean1, raltnc_mean]
      	
      		o3_ave1   = [o3_ave1  ,o3_ave  ]
      		o3nc_ave1 = [o3nc_ave1,o3nc_ave]

      		h2o_ave1   = [h2o_ave1  ,h2o_ave  ]
      		h2onc_ave1 = [h2onc_ave1,h2onc_ave]

      		T_ave1   = [T_ave1  ,T_ave  ]
      		Tnc_ave1 = [Tnc_ave1,Tnc_ave]
       ENDFOR
 			
 		IF KEYWORD_SET(percent_diff) THEN BEGIN
      		o3_ave1_diff  = (( o3_ave1 -  o3nc_ave1) / ( o3nc_ave1))*100.0
      		h2o_ave1_diff = ((h2o_ave1 - h2onc_ave1) / (h2onc_ave1))*100.0
      		T_ave1_diff  = (( T_ave1 -  Tnc_ave1) / ( Tnc_ave1))*100.0
 		ENDIF ELSE BEGIN        			
      		o3_ave1_diff = o3_ave1 - o3nc_ave1
      		h2o_ave1_diff = h2o_ave1 - h2onc_ave1
      		T_ave1_diff = T_ave1 - Tnc_ave1
      	ENDELSE
			      	
      	IF (KEYWORD_SET(column)) THEN BEGIN
			iconv = WHERE(FINITE(o31,/NAN),conv_count, COMPLEMENT=jconv, NCOMPLEMENT=jconv_count)				
 			jconv_count = (FLOAT(jconv_count) / FLOAT(dim[0]*dim[1])) * 100.0 				

			IF (KEYWORD_SET(cld_cover)) THEN BEGIN
				cld_count   = WHERE(cloud GT 0.0, count)
 				jconv_count = (FLOAT(count) / FLOAT(dim[0]*dim[1]*dim[2])) * 100.0 
 			ENDIF
			c_count  = FLTARR(dim[2]) + jconv_count
		ENDIF
		
		map_plot   = [map_plot  , o3_ave1_diff]
		cont_plot  = [cont_plot ,h2o_ave1_diff] 
		T_plot     = [T_plot    ,  T_ave1_diff] 
		conv_plot  = [conv_plot ,      c_count] 
		ralt_arr   = [ralt_arr  ,  ralt1_mean1]
		conv_plot1 = [conv_plot1,      dbl_o31]
      	numlev = [0:dim[2]-1]

ENDFOREACH	;date
;   ENDFOREACH ;var

map_plot   = REFORM(map_plot  ,dim[2],N_ELEMENTS(date_arr))
cont_plot  = REFORM(cont_plot ,dim[2],N_ELEMENTS(date_arr))
T_plot     = REFORM(T_plot    ,dim[2],N_ELEMENTS(date_arr))
ralt_arr   = REFORM(ralt_arr  ,dim[2],N_ELEMENTS(date_arr))
conv_plot  = REFORM(conv_plot ,dim[2],N_ELEMENTS(date_arr))

IF (KEYWORD_SET(double_trop)) THEN conv_plot1 = REFORM(conv_plot1,dim[2],N_ELEMENTS(date_arr))

dd = FLTARR(dim[2],N_ELEMENTS(date_arr))
FOR ii = 0, N_ELEMENTS(date_arr)-1 DO BEGIN
	dd[*,ii] = ii
ENDFOR

map_pos1 = [0.05, 0.62, 0.47, 0.95]																			;Set map position
bar_pos1 = [0.10, 0.55, 0.40, 0.57]																			;Set color bar position


IF (KEYWORD_SET(double_trop)) THEN BEGIN
	map_bar_title1 = 'Ozone Concentration (ppbv)'													;Set color bar title
	map_bar_min    = 0.0																				;Set echo top minimum
	map_bar_max    = 1000.0																				;Set echo top maximum
	map_bar_ticks  = 4																						;Set number of color bar ticks
	map_table1     = [HCL_COLOR_TABLE(50, HUE_RANGE = [100.0, 300.0])]                                                  ;Set color table
	map_levels1    = [FINDGEN(50)*20.0]

	CONTOUR, conv_plot1*1.0E3, dd, ralt_arr, /NODATA, $																	;Contour values
    	FILL      = 1, $
    	LEVELS    = map_levels1, $
    	C_COLOR   = map_table1, $ 
  		YRANGE 	  = [-10, 5], $
		XRANGE    = [0, 24], $
  		CHARSIZE  = 1.5, $
	  	POSITION  = map_pos1


	CONTOUR, conv_plot1*1.0E3, dd, ralt_arr, $																	;Contour values
	       FILL      = 1, $
	       OVERPLOT  = 1, $
	       LEVELS    = map_levels1, $
	       C_COLOR   = map_table1, $ 
	  	   YRANGE 	 = [-10, 5], $
		   XRANGE    = [0, 24], $
		   CHARSIZE  = 1.5, $
	       POSITION  = map_pos1

	COLOR_BAR_24_KPB, map_table1[1:*], OVER = map_table1[-1], UNDER = map_table1[0], $						;Draw map color bar
		TICKS     = map_bar_ticks, $
		RANGE     = [0, 400], $
		TITLE     = map_bar_title1, $
		CHARSIZE  = 1.25, $
		XCHARSIZE = 1.25, $
		POSIT 	  = bar_pos1
ENDIF ELSE BEGIN
	map_bar_title1 = 'Convective fraction (%)'													;Set color bar title
	map_bar_min    = 0.0																				;Set echo top minimum
	map_bar_max    = 100.0																				;Set echo top maximum
	map_bar_ticks  = 4																						;Set number of color bar ticks
	map_table1     = [HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])]                                                  ;Set color table
	map_levels1    = [FINDGEN(20)*5.0]

	IF (KEYWORD_SET(TROPICS)) THEN BEGIN
		map_bar_max    = 25.0																				;Set echo top maximum
		map_table1     = [HCL_COLOR_TABLE(25, HUE_RANGE = [100.0, 300.0])]                                                  ;Set color table
		map_levels1    = [FINDGEN(25)]
	ENDIF
	
	CONTOUR, conv_plot, dd, ralt_arr, /NODATA, $																	;Contour values
	    FILL      = 1, $
	    LEVELS    = map_levels1, $
	    C_COLOR   = map_table1, $ 
  		YRANGE 	  = [-10, 5], $
		XRANGE    = [0, 24], $
		CHARSIZE  = 1.5, $
	    POSITION  = map_pos1
	
	CONTOUR, conv_plot, dd, ralt_arr, $																	;Contour values
	       FILL      = 1, $
	       OVERPLOT  = 1, $
	       LEVELS    = map_levels1, $
	       C_COLOR   = map_table1, $ 
	  	   YRANGE 	 = [-10, 5], $
		   XRANGE    = [0, 24], $
		   CHARSIZE  = 1.5, $
	       POSITION  = map_pos1
	
	COLOR_BAR_24_KPB, map_table1[1:*], OVER = map_table1[-1], UNDER = map_table1[0], $						;Draw map color bar
		TICKS     = map_bar_ticks, $
		RANGE     = [0, 100], $
		TITLE     = map_bar_title1, $
		CHARSIZE  = 1.25, $
		XCHARSIZE = 1.25, $
		POSIT     = bar_pos1
ENDELSE


;map_plot = REFORM(map_plot,dim[2],N_ELEMENTS(date_arr))
;ralt_arr = REFORM(ralt_arr,dim[2],N_ELEMENTS(date_arr))

map_pos2 = [0.05, 0.15, 0.47, 0.47]																			;Set map position
bar_pos2 = [0.10, 0.07, 0.40, 0.09]																			;Set color bar position

map_bar_title2 = 'O3 % Difference' 													;Set color bar title
map_bar_min2   = -75.0																					;Set echo top minimum
map_bar_max2   = 75.0																					;Set echo top maximum
map_bar_ticks = 4																						;Set number of color bar ticks
map_table2 	  = [BLUE_RED_24(42)]
map_levels2   = [MAKEN(map_bar_min2,map_bar_max2,41)]

CONTOUR, map_plot, dd, ralt_arr, /NODATA, $																	;Contour values
    FILL      = 1, $
    LEVELS    = map_levels2, $
    C_COLOR   = map_table2, $ 
  	YRANGE 	  = [-10, 5], $
	XRANGE    = [0, 24], $
	CHARSIZE  = 1.5, $
    POSITION  = map_pos2

CONTOUR, map_plot, dd, ralt_arr, $																	;Contour values
       FILL      = 1, $
       OVERPLOT  = 1, $
       LEVELS    = map_levels2, $
       C_COLOR   = map_table2, $ 
  	   YRANGE 	 = [-10, 5], $
	   XRANGE    = [0, 24], $
	   CHARSIZE  = 1.5, $
       POSITION  = map_pos2

COLOR_BAR_24_KPB, map_table2, OVER = map_table2[-1], UNDER = map_table2[0], $						;Draw map color bar
	TICKS     = map_bar_ticks, $
	RANGE     = [map_bar_min2, map_bar_max2], $
	TITLE     = map_bar_title2, $
	CHARSIZE  = 1.25, $
	XCHARSIZE = 1.25, $
	POSIT     = bar_pos2

map_pos3 = [0.52, 0.15, 0.95, 0.47]																			;Set map position
bar_pos3 = [0.58, 0.07, 0.88, 0.09]																			;Set color bar position

map_bar_title3 = 'H2O % Difference'  													;Set color bar title
map_bar_min3   = -200.0																					;Set echo top minimum
map_bar_max3   = 200.0																					;Set echo top maximum
map_bar_ticks  = 4																						;Set number of color bar ticks
map_table3 	  = [BLUE_RED_24(42)]
map_levels3   = [MAKEN(map_bar_min3,map_bar_max3,41)]

CONTOUR, cont_plot, dd, ralt_arr, /NODATA, $																	;Contour values
    FILL      = 1, $
    LEVELS    = map_levels3, $
    C_COLOR   = map_table3, $ 
  	YRANGE 	  = [-10, 5], $
	XRANGE    = [0, 24], $
	CHARSIZE  = 1.5, $
    POSITION  = map_pos3

CONTOUR, cont_plot, dd, ralt_arr, $
		FILL  	 = 1, $
		OVERPLOT = 1, $
		LEVELS   = map_levels3, $
        C_COLOR  = map_table3, $ 
  		YRANGE 	 = [-10, 5], $
		XRANGE   = [0, 24], $
		CHARSIZE = 1.5, $
		POSITION = map_pos3
		
COLOR_BAR_24_KPB, map_table3, OVER = map_table3[-1], UNDER = map_table3[0], $						;Draw map color bar
	TICKS     = map_bar_ticks, $
	RANGE     = [map_bar_min3, map_bar_max3], $
	TITLE     = map_bar_title3, $
	CHARSIZE  = 1.25, $
	XCHARSIZE = 1.25, $
	POSIT	  = bar_pos3

map_pos4 = [0.52, 0.62, 0.95, 0.95]																			;Set map position
bar_pos4 = [0.58, 0.55, 0.88, 0.57]																			;Set color bar position

map_bar_title4 = 'T % Difference'  													;Set color bar title
map_bar_min4   = -10.0																					;Set echo top minimum
map_bar_max4   = 10.0																					;Set echo top maximum
map_bar_ticks  = 4																						;Set number of color bar ticks
map_table4 	  = [BLUE_RED_24(42)]
map_levels4   = [MAKEN(map_bar_min4,map_bar_max4,41)]

CONTOUR, T_plot, dd, ralt_arr, /NODATA, $																	;Contour values
    FILL      = 1, $
    LEVELS    = map_levels4, $
    C_COLOR   = map_table4, $ 
  	YRANGE 	  = [-10, 5], $
	XRANGE    = [0, 24], $
 	CHARSIZE  = 1.5, $
    POSITION  = map_pos4

CONTOUR, T_plot, dd, ralt_arr, $
		FILL  	 = 1, $
		OVERPLOT = 1, $
		LEVELS   = map_levels4, $
        C_COLOR  = map_table4, $ 
  		YRANGE 	 = [-10, 5], $
		XRANGE   = [0, 24], $
		CHARSIZE = 1.5, $
		POSITION = map_pos4
		
COLOR_BAR_24_KPB, map_table4, OVER = map_table4[-1], UNDER = map_table4[0], $						;Draw map color bar
	TICKS     = map_bar_ticks, $
	RANGE     = [map_bar_min4, map_bar_max4], $
	TITLE     = map_bar_title4, $
	CHARSIZE  = 1.25, $
	XCHARSIZE = 1.25, $
	POSIT 	  = bar_pos4

!P.MULTI = 0
IF KEYWORD_SET(eps) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																;Reset color table to linear ramp
	PS_OFF																						;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)															;Write PNG file

END