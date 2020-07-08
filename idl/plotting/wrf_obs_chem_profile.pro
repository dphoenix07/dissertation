PRO WRF_OBS_CHEM_PROFILE, event, start_date, end_date, $
	DOMAIN   = domain, $
	IN_CLOUD = in_cloud, $
	BMP		 = bmp, $
	PBL		 = pbl, $
	CHEM	 = chem, $
	VAR2     = var2, $
	REGION   = region, $
	ALT		 = alt, $
	PNG	     = png, $
	EPS   	 = eps


;+
; Name:
;		PLOT_WRF_OBS_SCATTER
; Purpose:
;		Currently set up to combine multiple time steps and produce scatter plots of 
;		trace gases using WRF_TRACER_RALT.
;		(e.g., Similar to 'PLOT_WRF_TRACER_RALT', but can do multiple time steps)
; Calling sequence:
;		PLOT_WRF_OBS_SCATTER, run, scheme, start_date, end_date
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
;		Daniel B. Phoenix	    2016-10-07. 
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 2											;Set default domain
IF (N_ELEMENTS(in_cloud  ) EQ 0) THEN in_cloud   = 1
IF (N_ELEMENTS(threshold ) EQ 0) THEN threshold  = 1000.0
IF (N_ELEMENTS(xrange	 ) EQ 0) THEN xrange     = [  0, 800]
IF (N_ELEMENTS(xtitle	 ) EQ 0) THEN xtitle     = 'Ozone (ppbv)'
IF (N_ELEMENTS(yrange	 ) EQ 0) THEN yrange     = [-15,   5]
IF (N_ELEMENTS(ytitle	 ) EQ 0) THEN ytitle     = 'Relative Altitude'
IF (N_ELEMENTS(nxbin 	 ) EQ 0) THEN nxbin      = 50
IF (N_ELEMENTS(nybin 	 ) EQ 0) THEN nybin      = 50


IF (KEYWORD_SET(BMP)) THEN BEGIN
	scheme_arr = ['morrison_ysu','nssl_ysu','milbyau_ysu']
	label	   = ['morrison', 'nssl', 'milbyau']
	key 	   = 'bmp'
ENDIF

IF (KEYWORD_SET(PBL)) THEN BEGIN
	scheme_arr = ['nssl_ysu','nssl_qnse','nssl_acm2']
	label	   = ['ysu', 'qnse', 'acm2']
	key 	   = 'pbl'
ENDIF

IF (KEYWORD_SET(CHEM)) THEN BEGIN
	scheme_arr = ['nssl_ysu','mozcart_new','nssl_cbmz_new']
	label	   = ['racm-esrl', 'mozart', 'cbmz']
	key 	   = 'chem'
ENDIF


IF (N_ELEMENTS(region) EQ 0) THEN BEGIN
	IF (event EQ '20120519') THEN region = [50, 50, 250, 190]
;  	IF (event EQ '20120529') THEN region = [50, 80, 230, 250]
; 	IF (scheme EQ '15-3km' ) THEN region = [50, 90, 210, 190]
ENDIF


IF in_cloud EQ 1 THEN BEGIN 
	cld_title = 'in_cloud'
	ENDIF ELSE BEGIN
	cld_title = 'out_cloud'
ENDELSE



outdir  = !WRF_DIRECTORY + event + '/paper/plots/chemistry_profiles/'
epsfile = outdir + key + '_' + end_date + '_' + cld_title + '.eps'						;EPS filename
pdffile = outdir + key + '_' + end_date + '_' + cld_title + '.pdf'						;PDF filename
pngfile = outdir + key + '_' + end_date + '_' + cld_title + '.png'						;PNG filename

FILE_MKDIR, outdir

date_arr = MK_DATE_ARR(event, scheme_arr[0], start_date, end_date, /DATE)	


;IF KEYWORD_SET(eps) THEN BEGIN	
;	PS_ON, FILENAME = epsfile, PAGE_SIZE = [10.0, 6.0], MARGIN = 0.0, /INCHES					;Switch to Postscript device
;	DEVICE, /ENCAPSULATED
;	!P.FONT     = 0																				;Hardware fonts
;	!P.CHARSIZE = 1.5
;	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
;		LOAD_BASIC_COLORS																		;Load basic color definitions
;ENDIF ELSE BEGIN
;	SET_PLOT, 'X'
;	WINDOW, XSIZE = 1200, YSIZE = 800															;Open graphics window
;	!P.COLOR      = COLOR_24('black')															;Foreground color
;	!P.BACKGROUND = COLOR_24('white')															;Background color
;	!P.CHARSIZE   = 3.1		
;	!P.FONT       = -1																			;Use Hershey fonts
;ENDELSE
;!P.MULTI = [0, 2, 2]


in_cloud = 0
c = 0
FOR c = 0, 1 DO BEGIN
	PRINT, c
	in_cloud = c



PRINT, c


var_arr = ['O3' , 'H2O', 'CO']

IF (KEYWORD_SET(var2)) THEN var_arr = ['HNO3', 'SO2', 'HCHO']

FOREACH var, var_arr DO BEGIN

IF c EQ 0 THEN BEGIN
	IF var EQ 'O3'   THEN position = [3,2,1]
	IF var EQ 'H2O'  THEN position = [3,2,2]
	IF var EQ 'CO'   THEN position = [3,2,3]
ENDIF 
IF c EQ 1 THEN BEGIN
	IF var EQ 'O3'   THEN position = [3,2,4]
	IF var EQ 'H2O'  THEN position = [3,2,5]
	IF var EQ 'CO'   THEN position = [3,2,6]
ENDIF

IF (KEYWORD_SET(var2)) THEN BEGIN
	IF c EQ 0 THEN BEGIN
		IF var EQ 'HNO3'  THEN position = [3,2,1]
		IF var EQ 'SO2'   THEN position = [3,2,2]
		IF var EQ 'HCHO'  THEN position = [3,2,3]
	ENDIF 
	IF c EQ 1 THEN BEGIN
		IF var EQ 'HNO3'  THEN position = [3,2,4]
		IF var EQ 'SO2'   THEN position = [3,2,5]
		IF var EQ 'HCHO'  THEN position = [3,2,6]
	ENDIF
ENDIF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;											;Aircraft
gv_cloud  = (DC3_READ_VAR('CONC1DC100_LWIO', event)).values
dc8_cloud = (DC3_READ_VAR('cloud', event, /DC8)).values
			
gv_z  = (DC3_READ_VAR('GGALT', event)).values														;Read aircraft altitude
dc8_z = (DC3_READ_VAR('G_ALT', event, /DC8)).values
	
gv_z_trop  = (DC3_READ_VAR('GFS_TROP_HGT', event)).values
dc8_z_trop = (DC3_READ_VAR('GFS_TROP_HGT', event, /DC8)).values


IF KEYWORD_SET(in_cloud) THEN BEGIN																;Sort values in-cloud vs out of cloud
	dc8_values = WHERE((dc8_cloud EQ 2.0) OR (dc8_cloud EQ 3.0), dc8_count)					;Find DC8 in-cloud values
	IF (dc8_count GT 0) THEN BEGIN
		dc8_cloud  = dc8_cloud  [dc8_values]
		dc8_z      = dc8_z      [dc8_values]
		dc8_z_trop = dc8_z_trop [dc8_values]
	ENDIF
	gv_values = WHERE(gv_cloud GT 0.0, gv_count)										;Find GV in-cloud values
	IF (gv_count GT 0) THEN BEGIN
		gv_cloud  = gv_cloud  [gv_values]
		gv_z      = gv_z      [gv_values]
		gv_z_trop = gv_z_trop [gv_values]
	ENDIF
ENDIF ELSE BEGIN
	dc8_values  = WHERE(dc8_cloud EQ 0.0, dc8_count)										;Find DC8 non-cloud values
	IF (dc8_count GT 0) THEN BEGIN
		dc8_cloud  = dc8_cloud  [dc8_values]
		dc8_z      = dc8_z      [dc8_values]
		dc8_z_trop = dc8_z_trop [dc8_values]
	ENDIF
	gv_values = WHERE(gv_cloud EQ 0.0, gv_count)											;Find GV non-cloud values
	IF (gv_count GT 0) THEN BEGIN
		gv_cloud  = gv_cloud  [gv_values]
		gv_z      = gv_z      [gv_values]
		gv_z_trop = gv_z_trop [gv_values]
	ENDIF
ENDELSE

gv_z_trop  = CALC_TROP_MODE_OBS(gv_z_trop , threshold)
dc8_z_trop = CALC_TROP_MODE_OBS(dc8_z_trop, threshold)

z_obs      = [gv_z, dc8_z]
z_trop_obs = [gv_z_trop, dc8_z_trop]
ralt_obs   = (z_obs - z_trop_obs) * 1.0E-3

CASE var OF  
	'O3' : BEGIN
		tracer_gv  = (DC3_READ_VAR('FO3_ACD', event)).values												;Read ozone data
		tracer_dc8 = (DC3_READ_VAR('O3_CL', event, /DC8)).values	 
		XRANGE 	   = [0, 400]
		XTITLE     = 'Ozone (ppbv)'

	END
	
	'H2O' : BEGIN
		tracer_gv  = (DC3_READ_VAR('X_H2O', event)).values																		;Read water vapor data
		tracer_dc8 = (DC3_READ_VAR('H2O_vapor_ppmv_DLH', event, /DC8)).values
		XRANGE 	   = [0, 500]
		XTITLE     = 'Water Vapor (ppmv)'

	END
	
	'CO' : BEGIN
		tracer_gv  = (DC3_READ_VAR('CO', event)).values																		;Read carbon monoxide data
		tracer_dc8 = (DC3_READ_VAR('CO_ppbv_DACOM', event, /DC8)).values
		XRANGE     = [0, 200]
		XTITLE     = 'Carbon Monoxide (ppbv)'

	END
	
	'HNO3' : BEGIN
		tracer_gv  = (DC3_READ_VAR('HNO3', event)).values																		;Read nitric acid data
		tracer_dc8 = (DC3_READ_VAR('HNO3_SAGA', event, /DC8)).values	
		XRANGE     = [0, 3000]
		XTITLE     = 'Nitric Acid (pptv)'
	END
		
	'NOx' : BEGIN
		tracer_gv  = (DC3_READ_VAR('NO+NO2', event)).values																		;Read nitric acid data
		tracer_dc8 = (DC3_READ_VAR('NO2_LIF', event, /DC8)).values	
		XRANGE     = [0, 2000]
		XTITLE     = 'Nitrogen Oxides (pptv)'
	END

	'SO2' : BEGIN
		tracer_gv  = (DC3_READ_VAR('SO2', event)).values																		;Read nitric acid data
		tracer_dc8 = (DC3_READ_VAR('SO2_GTCIMS', event, /DC8)).values	
		XRANGE     = [0, 250]
		XTITLE     = 'Sulfur Dioxide (pptv)'
	END
	
	'HCHO' : BEGIN
		tracer_gv  = (DC3_READ_VAR('CH2O_CAMS_pptv', event)).values																		;Read nitric acid data
		tracer_dc8 = (DC3_READ_VAR('CH2O_DFGAS_pptv', event, /DC8)).values	
		XRANGE     = [0, 3000]
		XTITLE     = 'Formaldehyde (pptv)'
	END
		
ENDCASE


tracer_gv  = tracer_gv [gv_values]
tracer_dc8 = tracer_dc8[dc8_values]
tracer_obs = [tracer_gv, tracer_dc8]

good = WHERE(((tracer_obs GE xrange[0]) AND $																			;Find data to use
				  (tracer_obs LE xrange[1]) AND $
				  (ralt_obs   GE yrange[0]) AND $
				  (ralt_obs   LE yrange[1])), good_count)

IF (good_count GT 0) THEN BEGIN
	tracer_obs = tracer_obs[good]
	ralt_obs   =   ralt_obs[good]
ENDIF 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;							;End Aircraft


color 	   = [COLOR_24('red'), COLOR_24('darkgreen'), COLOR_24('blue'), COLOR_24('gray50')]

i = 0

figure = PLOT(tracer_obs, ralt_obs, /NODATA, $
		XRANGE = xrange, $
		YRANGE = [-11, 5], $
;		YTITLE = 'Tropopause Relative (km)', $
		LAYOUT = position, $
		/CURRENT)

figure = PLOT(tracer_obs, ralt_obs, /NODATA, $
		XRANGE = xrange, $
		YRANGE = [-11, 5], $
		LAYOUT = position, $
		/OVERPLOT)


figure2 = PLOT(tracer_obs, ralt_obs,SYMBOL=3,LINESTYLE=6, COLOR = color[3], LAYOUT = position, /OVERPLOT)

 
FOREACH scheme, scheme_arr DO BEGIN
	plt_var1 = [ ] 
	FOREACH date, date_arr DO BEGIN

		z		 = (WRF_READ_VAR('Z'     , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read height values
		dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
		z_trop   = (WRF_READ_VAR('Z_trop', date, event, scheme, DOMAIN = domain, INDICES = region)).values
		z_trop   = CALC_TROP_MODE(z_trop, scheme, threshold) 												;Filter tropopause values
		xyz_trop = REBIN(z_trop, dim[0], dim[1], dim[2], /SAMPLE)
		filt     = WHERE(xyz_trop EQ 999999 , filt_count, COMPLEMENT = good, NCOMPLEMENT = good_count)

		z[filt] 	    = !Values.F_NaN
		xyz_trop[filt]  = !Values.F_NaN

		cloud   		= (WRF_READ_VAR('CLOUD_MIX_TOTAL' , date, event, scheme, DOMAIN = domain, INDICES = region)).values * 1.0E3			;Read in cloud mixing ratio values
		cloud[filt]     = !Values.F_NaN

		plt_var      	= (WRF_READ_VAR(var      		  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values
		plt_var[filt]   = !Values.F_NaN
		plt_var1		= [[[plt_var1]], [[plt_var]]]													


		IF var EQ 'O3'   THEN factor = 1.0E3
		IF var EQ 'H2O'  THEN factor = 1.0E6
		IF var EQ 'CO'   THEN factor = 1.0E3
		IF var EQ 'HNO3' THEN factor = 1.0E6
		IF var EQ 'NOX'  THEN factor = 1.0E6
		IF var EQ 'SO2'  THEN factor = 1.0E6
		IF var EQ 'HCHO' THEN factor = 1.0E6
		
		
		tracer = plt_var * factor
		ralt   = (z - xyz_trop) * 1.0E-3

		cloud_values = WHERE(cloud GE 0.1, cld_count, COMPLEMENT = non_cloud, $							;Find values in cloud 
							NCOMPLEMENT = ncld_count)

		IF KEYWORD_SET(in_cloud) THEN BEGIN																;Sort values in cloud vs out of cloud
			IF (cld_count GT 0) THEN BEGIN
				tracer[non_cloud] = !Values.F_NaN
				ralt  [non_cloud] = !Values.F_NaN
			ENDIF
		ENDIF ELSE BEGIN
			IF (ncld_count GT 0) THEN BEGIN
				tracer [cloud_values] = !Values.F_NaN
				ralt   [cloud_values] = !Values.F_NaN
			ENDIF
		ENDELSE


		;IF KEYWORD_SET(dbl_trop) THEN BEGIN																;If secondary tropopause is present, filter out those values
		;	prim_trop = WHERE (z_trop LT upper_trop, trop_count, COMPLEMENT = bad, NCOMPLEMENT = nbad)
		;	
		;	IF (nbad GT 0) THEN BEGIN
		;		tracer [bad] = !Values.F_NaN
		;		ralt   [bad] = !Values.F_NaN
		;	ENDIF
		;ENDIF	
		
		good = WHERE(((tracer GE xrange[0]) 	AND $												;Use specified data range
						(tracer LE xrange[1]) 	AND $												;Only used if filtering not desired
						(ralt   GE yrange[0])   AND $
						(ralt   LE yrange[1])), good_count, COMPLEMENT=out, NCOMPLEMENT = nout)


				  
		IF (nout GT 0) THEN BEGIN																	;Save good data points
			tracer [out] = !Values.F_NaN
			ralt   [out] = !Values.F_NaN 
		ENDIF 

		tracer = REFORM(tracer,dim[0],dim[1],dim[2])
		ralt   = REFORM(ralt  ,dim[0],dim[1],dim[2])

		tracer = REFORM(tracer,dim[0]*dim[1],dim[2])
		ralt   = REFORM(ralt  ,dim[0]*dim[1],dim[2])

	ENDFOREACH
		tr_med1    = [ ]
		tr_min1    = [ ]
		tr_max1    = [ ] 
		ralt_mean1 = [ ]

		FOR k = 0, dim[2]-1 DO BEGIN
			lev = SORT(tracer[*,k])
			tr1 = tracer[lev,k]

			tr_med = MEDIAN(tr1)
			tr_min = MIN(tr1,/NAN)
			tr_max = MAX(tr1,/NAN)
		;	tr_min = tr1[PERCENTILE(tr1,1,/NOSORT,/NAN)]
		;	tr_max = tr1[PERCENTILE(tr1,99,/NOSORT,/NAN)]

			ralt_mean  = MEAN(ralt[*,k],/NAN) 
			ralt_mean1 = [ralt_mean1, ralt_mean]
	
			tr_med1 = [tr_med1,tr_med]
			tr_min1 = [tr_min1,tr_min]
			tr_max1 = [tr_max1,tr_max]
		ENDFOR

		numlev = [0:dim[2]-1]

		;figure = PLOT( tr_med1, ralt_mean1, /NODATA, $
		;		XRANGE = [0, 400], $
		;		YRANGE = [-11, 5], $
		;		/OVERPLOT) 
		figure3 = PLOT(tr_med1, ralt_mean1, LINESTYLE =0, COLOR = color[i], THICK = 2, /OVERPLOT, LAYOUT = position)		
		figure4 = PLOT(tr_max1, ralt_mean1, LINESTYLE =1, COLOR = color[i], THICK = 2, /OVERPLOT, LAYOUT = position)
		figure5 = PLOT(tr_min1, ralt_mean1, LINESTYLE =2, COLOR = color[i], THICK = 2, /OVERPLOT, LAYOUT = position)

		PRINT, scheme
	i = i + 1
ENDFOREACH

figure5 = PLOT(xrange,[0.0,0.0], /OVERPLOT, LAYOUT = position, XTITLE = xtitle)

ENDFOREACH

;c = c + 1
ENDFOR

t1 = TEXT(530,455,  'Obs', FONT_SIZE=10, /DEVICE, FONT_COLOR = !COLOR.gray)

IF (KEYWORD_SET(BMP)) THEN BEGIN
	t2 = TEXT(565,455,  'MOR', FONT_SIZE=10, /DEVICE, FONT_COLOR = !COLOR.blue)
	t3 = TEXT(565,440,  'NSSL',FONT_SIZE=10, /DEVICE, FONT_COLOR = !COLOR.dark_green)
	t4 = TEXT(565,425,  'MY',  FONT_SIZE=10, /DEVICE, FONT_COLOR = !COLOR.red)
ENDIF

IF (KEYWORD_SET(PBL)) THEN BEGIN
	t2 = TEXT(565,455,  'YSU',  FONT_SIZE=10, /DEVICE, FONT_COLOR = !COLOR.blue)
	t3 = TEXT(565,440,  'QNSE', FONT_SIZE=10, /DEVICE, FONT_COLOR = !COLOR.dark_green)
	t4 = TEXT(565,425,  'ACM2', FONT_SIZE=10, /DEVICE, FONT_COLOR = !COLOR.red)
ENDIF

IF (KEYWORD_SET(CHEM)) THEN BEGIN
	t2 = TEXT(565,455,  'RACM', FONT_SIZE=10, /DEVICE, FONT_COLOR = !COLOR.blue)
	t3 = TEXT(565,440,  'MOZ' , FONT_SIZE=10, /DEVICE, FONT_COLOR = !COLOR.dark_green)
	t4 = TEXT(565,425,  'CBMZ', FONT_SIZE=10, /DEVICE, FONT_COLOR = !COLOR.red)
ENDIF



!P.MULTI = 0
IF KEYWORD_SET(eps) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																;Reset color table to linear ramp
	PS_OFF																						;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)															;Write PNG file

END