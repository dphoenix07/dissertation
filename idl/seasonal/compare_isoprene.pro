PRO COMPARE_ISOPRENE, event, date, $
	DOMAIN      = domain, $
	PERCENTILE  = percentile, $
	ISENTROPE   = isentrope, $
	VAR2        = var2, $
	ALT		    = alt, $
	REGION      = region, $
	NONCONV     = nonconv, $
	PNG	        = png, $
	EPS   	    = eps


;+
; Name:
;		COMPARE_ISOPRENE
; Purpose:
;		Computes min/median/max profile of isoprene and compares those profiles with
;		convective influence (using updraft and cloud-at-trop tracers) to those without.
; Calling sequence:
;		COMPARE_ISOPRENE, run, date
; Example: COMPARE_ISOPRENE,'20110808','20110808T1300Z'
; Input:
;		event : Model simulation name. (e.g., '20110808')
;		date  : String variable of the date requested (e.g., '20110808T1300Z')
; Output:
;		Scatterplots of trace gases from multiple timesteps.
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel B. Phoenix	    2018-02-19.	
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain
IF (N_ELEMENTS(in_cloud  ) EQ 0) THEN in_cloud   = 1
IF (N_ELEMENTS(threshold ) EQ 0) THEN threshold  = 1000.0
IF (N_ELEMENTS(xrange	 ) EQ 0) THEN xrange     = [175, 275]
IF (N_ELEMENTS(xtitle	 ) EQ 0) THEN xtitle     = 'Temperature (K)'
IF (N_ELEMENTS(yrange	 ) EQ 0) THEN yrange     = [-5,   5]
IF (N_ELEMENTS(ytitle	 ) EQ 0) THEN ytitle     = 'Relative Altitude'
IF (N_ELEMENTS(nxbin 	 ) EQ 0) THEN nxbin      = 50
IF (N_ELEMENTS(nybin 	 ) EQ 0) THEN nybin      = 50
IF (N_ELEMENTS(var2		 ) EQ 0) THEN var2		 = 0

scheme_arr = ['d02_ndown_d01chem', 'bio1', 'bio2']
label  	   = ['MEGAN', 'GUNTHER', 'BEIS']
key    	   = 'test'

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

var = ['ISO']

color 	   = [COLOR_24_DP('RED'), COLOR_24_DP('BLUE'), COLOR_24_DP('GREEN')]			

tracer_obs = (WRF_READ_VAR('T', date[0], event, scheme_arr[0], DOMAIN = domain, INDICES = region)).values					;Read ozone values
ralt_obs   = (WRF_READ_VAR('Z', date[0], event, scheme_arr[0], DOMAIN = domain, INDICES = region)).values					;Read ozone values

IF (KEYWORD_SET(isentrope)) THEN ralt_obs = (WRF_READ_VAR('theta', date_arr[0], event, scheme, DOMAIN = domain, INDICES = region)).values 

i = 0
yrange = [-5, 5]
IF (KEYWORD_SET(ALT)) THEN yrange = [0, 18] 
IF (KEYWORD_SET(isentrope)) THEN yrange = [300, 450]
        		
plt_var1 = [ ] 
FOREACH scheme, scheme_arr DO BEGIN
	PRINT, "Processing date: ", date
	z		 = (WRF_READ_VAR('Z'     , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read height values
	dim 	 = SIZE(z, /DIMENSIONS)																						;Get dimensions (to reform arrays later)
	z_trop   = (WRF_READ_VAR('Z_trop', date, event, scheme, DOMAIN = domain, INDICES = region)).values
	z_trop   = CALC_TROP_MODE(z_trop, scheme, threshold) 																;Filter tropopause values

	theta   = (WRF_READ_VAR('T'			     , date, event, scheme, DOMAIN = domain, INDICES = region)).values					
	theta   = ((1000.0/(WRF_READ_VAR('P'     , date, event, scheme, DOMAIN = domain, INDICES = region)).values)^(!Rair/!Cp))*(theta)
	precip   = (WRF_READ_VAR('PRECIP'	     , date, event, scheme, DOMAIN = domain, INDICES = region)).values
	w	     = (WRF_READ_VAR('w' 	 	     , date, event, scheme, DOMAIN = domain, INDICES = region)).values
	cld_tr   = (WRF_READ_VAR('Cloud_tracer'  , date, event, scheme, DOMAIN = domain, INDICES = region)).values
	updraft  = (WRF_READ_VAR('Updraft_tracer', date, event, scheme, DOMAIN = domain, INDICES = region)).values

	xyz_trop = REBIN(z_trop, dim[0], dim[1], dim[2], /SAMPLE)
	filt     = WHERE(FINITE(xyz_trop, /NAN), filt_count, COMPLEMENT = good, NCOMPLEMENT = good_count)

    z[filt] 	    = !Values.F_NaN
    xyz_trop[filt]  = !Values.F_NaN

	cloud   		= (WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, DOMAIN = domain, INDICES = region)).values * 1.0E3			
	cloud[filt]     = !Values.F_NaN

	plt_var      	= (WRF_READ_VAR(var      		 , date, event, scheme, DOMAIN = domain, INDICES = region)).values					
	plt_var[filt]   = !Values.F_NaN

	tracer = plt_var 
	ralt   = (z - xyz_trop) * 1.0E-3
	IF (KEYWORD_SET(isentrope)) THEN ralt = theta

	tracer_nc = FLTARR(dim[0],dim[1],dim[2])
	ralt_nc   = FLTARR(dim[0],dim[1],dim[2])
	
	IF (KEYWORD_SET(ALT)) THEN ralt = z * 1.0E-3

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
	
		tr_med1 = [tr_med1,tr_med]
		tr_min1 = [tr_min1,tr_min]
		tr_max1 = [tr_max1,tr_max]
 		tr_ave1 = [tr_ave1,tr_ave]

		trnc_med1 = [trnc_med1,trnc_med]
		trnc_min1 = [trnc_min1,trnc_min]
		trnc_max1 = [trnc_max1,trnc_max]
		trnc_ave1 = [trnc_ave1,trnc_ave]

 	   	tr_med1_pdiff = ((tr_med1 - trnc_med1) / (trnc_med1))*100.0
	   	tr_max1_pdiff = ((tr_max1 - trnc_max1) / (trnc_max1))*100.0
	   	tr_min1_pdiff = ((tr_min1 - trnc_min1) / (trnc_min1))*100.0
	   	tr_ave1_pdiff = ((tr_ave1 - trnc_ave1) / (trnc_ave1))*100.0

	   	tr_med1_diff = tr_med1 - trnc_med1
	   	tr_max1_diff = tr_max1 - trnc_max1
	   	tr_min1_diff = tr_min1 - trnc_min1
	   	tr_ave1_diff = tr_ave1 - trnc_ave1
	
	ENDFOR

	numlev = [0:dim[2]-1]

 	figure1 = PLOT(tracer_obs, ralt_obs, /NODATA, $
		XRANGE = xrange, $
		YRANGE = yrange, $
		LAYOUT = position1, $
		/CURRENT)

	 figure1 = PLOT(tracer_obs, ralt_obs, /NODATA, $
		XRANGE = xrange, $
		YRANGE = yrange, $
		LAYOUT = position1, $
		/OVERPLOT)
     
	figure3   = PLOT(tr_med1  , ralt_mean1, LINESTYLE =0, XMINOR =1, COLOR = color[0], THICK = 2, /OVERPLOT, LAYOUT = position1)		
	figure3nc = PLOT(trnc_med1, ralt_mean1, LINESTYLE =0, XMINOR =1, COLOR = color[1], THICK = 2, /OVERPLOT, LAYOUT = position1)		
	figure4   = PLOT(tr_max1  , ralt_mean1, LINESTYLE =2, XMINOR =1, COLOR = color[0], THICK = 2, /OVERPLOT, LAYOUT = position1)
	figure4nc = PLOT(trnc_max1, ralt_mean1, LINESTYLE =2, XMINOR =1, COLOR = color[1], THICK = 2, /OVERPLOT, LAYOUT = position1)
	figure5   = PLOT(tr_min1  , ralt_mean1, LINESTYLE =2, XMINOR =1, COLOR = color[0], THICK = 2, /OVERPLOT, LAYOUT = position1)
	figure5nc = PLOT(trnc_min1, ralt_mean1, LINESTYLE =2, XMINOR =1, COLOR = color[1], THICK = 2, /OVERPLOT, LAYOUT = position1)      		        
	figure6   = PLOT(xrange,[0.0,0.0], /OVERPLOT, LAYOUT = position1, XTITLE = xtitle)

	figure2 = PLOT(tracer_obs, ralt_obs, /NODATA, $
			XRANGE = [-20,20], $
			YRANGE = yrange, $
			LAYOUT = position2, $
			/CURRENT)
	
	figure2 = PLOT(tracer_obs, ralt_obs, /NODATA, $
			XRANGE = [-20,20], $
			YRANGE = yrange, $
			LAYOUT = position2, $
			/OVERPLOT)
	
	figure7   = PLOT(tr_med1_diff, ralt_mean1, LINESTYLE =0, XMINOR =1, COLOR = color[0], THICK = 2, /OVERPLOT, LAYOUT = position2)		
	figure8   = PLOT(tr_max1_diff, ralt_mean1, LINESTYLE =2, XMINOR =1, COLOR = color[0], THICK = 2, /OVERPLOT, LAYOUT = position2)
	figure9   = PLOT(tr_min1_diff, ralt_mean1, LINESTYLE =2, XMINOR =1, COLOR = color[0], THICK = 2, /OVERPLOT, LAYOUT = position2)
	figure10  = PLOT(tr_ave1_diff, ralt_mean1, LINESTYLE =2, XMINOR =1, COLOR = color[0], THICK = 2, /OVERPLOT, LAYOUT = position2)
	figure11  = PLOT([-20,20],[0.0,0.0], /OVERPLOT, LAYOUT = position2, XTITLE = xtitle)
	figure12  = PLOT([0.0,0.0],yrange, /OVERPLOT, LAYOUT = position2, XTITLE = xtitle)

	figure13 = PLOT(tracer_obs, ralt_obs, /NODATA, $
			XRANGE = [-5,5], $
			YRANGE = yrange, $
			LAYOUT = position3, $
			/CURRENT)
	
	figure13 = PLOT(tracer_obs, ralt_obs, /NODATA, $
			XRANGE = [-5,5], $
			YRANGE = yrange, $
			LAYOUT = position3, $
			/OVERPLOT)
	
	figure14  = PLOT(tr_med1_pdiff, ralt_mean1, LINESTYLE =0, XMINOR =1, COLOR = color[0], THICK = 2, /OVERPLOT, LAYOUT = position2)		
	figure15  = PLOT(tr_max1_pdiff, ralt_mean1, LINESTYLE =2, XMINOR =1, COLOR = color[0], THICK = 2, /OVERPLOT, LAYOUT = position2)
	figure16  = PLOT(tr_min1_pdiff, ralt_mean1, LINESTYLE =2, XMINOR =1, COLOR = color[0], THICK = 2, /OVERPLOT, LAYOUT = position2)
	figure17  = PLOT(tr_ave1_pdiff, ralt_mean1, LINESTYLE =2, XMINOR =1, COLOR = color[0], THICK = 2, /OVERPLOT, LAYOUT = position2)
	figure18  = PLOT([-20,20],[0.0,0.0], /OVERPLOT, LAYOUT = position2, XTITLE = xtitle)
	figure19  = PLOT([0.0,0.0],yrange, /OVERPLOT, LAYOUT = position2, XTITLE = xtitle)


	i = i + 1  

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

END