PRO WRF_HYM_PROFILE, event, start_date, end_date, $
	DOMAIN   = domain, $
	IN_CLOUD = in_cloud, $
	MASS 	 = mass, $
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
;		WRF_HYM_PROFILE
; Purpose:
;		Currently set up to combine multiple time steps and produce line plots of 
;		frozen hydrometeors.
;		(e.g., Similar to 'WRF_OBS_CHEM_PROFILE', but for hydrometeor number conc.)
; Calling sequence:
;		WRF_HYM_PROFILE, run, scheme, start_date, end_date
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		scheme 	   : Model initial state. Typically 'morrison' or 'nssl'.
;		start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;		end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
; Output:
;		Line plots of the mean hydrometeor # conc. profile from multiple timesteps.
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel B. Phoenix	    2017-01-12. 
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 2											;Set default domain
IF (N_ELEMENTS(in_cloud  ) EQ 0) THEN in_cloud   = 1
IF (N_ELEMENTS(threshold ) EQ 0) THEN threshold  = 1000.0
IF (N_ELEMENTS(xrange	 ) EQ 0) THEN xrange     = [  0, 1.0E8]
IF (N_ELEMENTS(xtitle	 ) EQ 0) THEN xtitle     = 'Ozone (ppbv)'
IF (N_ELEMENTS(yrange	 ) EQ 0) THEN yrange     = [-15,   5]
IF (N_ELEMENTS(ytitle	 ) EQ 0) THEN ytitle     = 'Relative Altitude'
IF (N_ELEMENTS(nxbin 	 ) EQ 0) THEN nxbin      = 50
IF (N_ELEMENTS(nybin 	 ) EQ 0) THEN nybin      = 50

xrange = [0,10]

CASE event OF
	'20120519' : BEGIN
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
						scheme_arr = ['nssl_ysu','mozcart_new','cbmz_noaerosols']
						label	   = ['racm-esrl', 'mozart', 'cbmz']
						key 	   = 'chem'
					ENDIF
				END
		
	'20120529' : BEGIN
					IF (KEYWORD_SET(BMP)) THEN BEGIN
						scheme_arr = ['morrison','nssl','milbyau']
						label	   = ['morrison', 'nssl', 'milbyau']
						key 	   = 'bmp'
					ENDIF

					IF (KEYWORD_SET(PBL)) THEN BEGIN
						scheme_arr = ['nssl','qnse','acm2']
						label	   = ['ysu', 'qnse', 'acm2']
						key 	   = 'pbl'
					ENDIF

					IF (KEYWORD_SET(CHEM)) THEN BEGIN
						scheme_arr = ['nssl','radmaq','cbmz_noaerosols']
						label	   = ['racm-esrl', 'radmaq', 'cbmz']
						key 	   = 'chem'
					ENDIF
				END

	'20110518' : BEGIN
					scheme_arr = ['seasonal_final/corrected']
					label	   = ['seasonal']
					key		   = ['seasonal']
				END
ENDCASE


IF (N_ELEMENTS(region) EQ 0) THEN BEGIN
	IF (event EQ '20120519') THEN region = [50, 50, 250, 190]
  	IF (event EQ '20120529') THEN region = [50, 80, 230, 250]
; 	IF (scheme EQ '15-3km' ) THEN region = [50, 90, 210, 190]
ENDIF


outdir  = !WRF_DIRECTORY + event + '/paper/plots/hydrometeor_profiles/'
epsfile = outdir + key + '_' + end_date + '.eps'						;EPS filename
pdffile = outdir + key + '_' + end_date + '.pdf'						;PDF filename
pngfile = outdir + key + '_' + end_date + '.png'						;PNG filename

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

var_arr = ['SNOW_NUM' , 'GRAUPEL_NUM', 'ICE_NUM']
IF KEYWORD_SET(mass) THEN var_arr = ['SNOW_MIX', 'GRAUPEL_MIX', 'ICE_MIX']

PRINT, var_arr
v=0
FOREACH var, var_arr DO BEGIN

;IF KEYWORD_SET(mass) THEN xrange = [MIN(var), MAX(var)]

IF var EQ 'SNOW_NUM'    THEN position = [3,1,1]
IF var EQ 'GRAUPEL_NUM' THEN position = [3,1,2]
IF var EQ 'ICE_NUM'   	THEN position = [3,1,3]

IF KEYWORD_SET(mass) THEN BEGIN
	IF var EQ 'SNOW_MIX'    THEN position = [3,1,1]
	IF var EQ 'GRAUPEL_MIX' THEN position = [3,1,2]
	IF var EQ 'ICE_MIX'   	THEN position = [3,1,3]
ENDIF

;color 	   = [COLOR_24_DP('LIGHTBLUE'), COLOR_24_DP('BLUE'), COLOR_24_DP('CYAN'), COLOR_24_DP('gray50')]		;Salmon, Red, Yellow
;color 	   = [COLOR_24_DP('RED'), COLOR_24_DP('BLUE'), COLOR_24_DP('YELLOW'), COLOR_24_DP('gray50')]
colors 	= HCL_COLOR_TABLE(3, SAT_RANGE = [0.2,1.0],HUE_RANGE = [240.0, 360.0])
rgb 	= COMPONENT_24(colors)

rgb[*,0]=[204,204,255]
rgb[*,1]=[191,76,191] 
rgb[*,2]=[127,0,0]

i = 0
event = '20120519'

gv_cloud1    = (DC3_READ_VAR('CONC1DC100_LWIO', event)).values
gv_cloud2    = (DC3_READ_VAR('CONC1DC150_LWIO', event)).values
gv_cloud3    = (DC3_READ_VAR('CONC1DC_LWIO'   , event)).values
gv_cloud = gv_cloud1 + gv_cloud2 + gv_cloud3

gv_z 	    = (DC3_READ_VAR('GGALT'		     , event)).values														;Read aircraft altitude
gv_z_trop   = (DC3_READ_VAR('GFS_TROP_HGT'	 , event)).values

gv_values = WHERE(gv_cloud GT 0.0, gv_count)										;Find GV in-cloud values
IF (gv_count GT 0) THEN BEGIN
	gv_cloud  = gv_cloud  [gv_values]
	gv_z      = gv_z      [gv_values]
	gv_z_trop = gv_z_trop [gv_values]
ENDIF

gv_z_trop  = CALC_TROP_MODE_OBS(gv_z_trop , threshold)
ralt_obs   = (gv_z - gv_z_trop) * 1.0E-3

event = '20110518'
tracer_obs = (WRF_READ_VAR(var, date_arr[0], event, scheme_arr[0], DOMAIN = domain, INDICES = region)).values					;Read ozone values
ralt_obs   = (WRF_READ_VAR('Z', date_arr[0], event, scheme_arr[0], DOMAIN = domain, INDICES = region)).values					;Read height values

;tracer_obs = gv_cloud
IF KEYWORD_SET(MASS) THEN xrange = [0,0.085]
IF KEYWORD_SET(MASS) THEN xrange = [0,0.35]

yrange = [-5, 3]

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

;figure2 = PLOT(tracer_obs, ralt_obs,SYMBOL=3,LINESTYLE=6, COLOR = color[3], LAYOUT = position, /OVERPLOT)


FOREACH scheme, scheme_arr DO BEGIN
	plt_var1 = [ ] 
	FOREACH date, date_arr DO BEGIN

 		z		 = (WRF_READ_VAR('Z'     , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read height values
		dim 	 = SIZE(z, /DIMENSIONS)																						;Get dimensions (to reform arrays later)
		z_trop   = (WRF_READ_VAR('Z_trop', date, event, scheme, DOMAIN = domain, INDICES = region)).values
		z_trop   = CALC_TROP_MODE(z_trop, scheme, threshold) 																;Filter tropopause values
		xyz_trop = REBIN(z_trop, dim[0], dim[1], dim[2], /SAMPLE)
		filt     = WHERE(xyz_trop EQ 999999 , filt_count, COMPLEMENT = good, NCOMPLEMENT = good_count)

		z[filt] 	    = !Values.F_NaN
		xyz_trop[filt]  = !Values.F_NaN

		plt_var      	= (WRF_READ_VAR(var      		  , date, event, scheme, DOMAIN = domain, INDICES = region)).values					;Read ozone values
		plt_var[filt]   = !Values.F_NaN
		plt_var1		= [[[plt_var1]], [[plt_var]]]													

		factor = 1.0E3																		;Convert kg/kg to g/kg				
		tracer = plt_var * factor
		ralt   = (z - xyz_trop) * 1.0E-3

;		PRINT, 'pre-filtered'
;		PRINT, MIN(tracer), MAX(tracer)		
;		good = WHERE(((tracer GE xrange[0]) 	AND $												;Use specified data range
;						(tracer LE xrange[1]) 	AND $												;Only used if filtering not desired
;						(ralt   GE yrange[0])   AND $
;						(ralt   LE yrange[1])), good_count, COMPLEMENT=out, NCOMPLEMENT = nout)
;				  
;		IF (nout GT 0) THEN BEGIN																	;Save good data points
;			tracer [out] = !Values.F_NaN
;			ralt   [out] = !Values.F_NaN 
;		ENDIF 

;		PRINT, 'post-filtered'
;		PRINT, MIN(tracer), MAX(tracer)		
		
		tracer = REFORM(tracer,dim[0],dim[1],dim[2])
		ralt   = REFORM(ralt  ,dim[0],dim[1],dim[2])

		tracer = REFORM(tracer,dim[0]*dim[1],dim[2])
		ralt   = REFORM(ralt  ,dim[0]*dim[1],dim[2])

	ENDFOREACH ;date
		tr_max1    = [ ] 
		tr_mean1   = [ ] 
		ralt_mean1 = [ ]
		tr_mean2   = [ ]

		FOR k = 0, dim[2]-1 DO BEGIN
			lev = SORT(tracer[*,k])
			tr1 = tracer[lev,k]


; Only average where there are grid points with conc. > 0			
;			non0= WHERE(tr1 GT 0.0, count)
;			IF (count GT 0) THEN BEGIN
;				tr2 = tr1[non0]
;			ENDIF 
;			IF (count LE 0) THEN BEGIN
;				tr2 = tr1
;			ENDIF
;
;			tr2_mean = MEAN(tr2,/NAN) 
; end
		
			tr_max  = MAX(tr1,/NAN)
			tr_mean = MEAN(tr1,/NAN)

			ralt_mean  = MEAN(ralt[*,k],/NAN) 
			ralt_mean1 = [ralt_mean1, ralt_mean]
		
			tr_max1  = [tr_max1 , tr_max ]
			tr_mean1 = [tr_mean1, tr_mean]
;			tr_mean2 = [tr_mean2, tr2_mean]

		ENDFOR

		numlev = [0:dim[2]-1]

;		xrange = [10, 1.0E7]

		figure = PLOT(tr_mean1, ralt_mean1, /NODATA, $
;				XRANGE = xrange, $
;				XLOG   = 1, $
				YRANGE = yrange, $	
				LAYOUT = position, $			
				/OVERPLOT) 
;		figure4 = PLOT(tr_max1 , ralt_mean1, LINESTYLE =2, COLOR = color[i], THICK = 2, /OVERPLOT, LAYOUT = position)
		figure6 = PLOT(tr_mean1, ralt_mean1, LINESTYLE =2, COLOR = rgb[*,i], THICK = 2, /OVERPLOT, LAYOUT = position)

	i = i + 1

PRINT, MAX(tr_mean1)
ENDFOREACH ;scheme

figure5 = PLOT(xrange,[0.0,0.0], /OVERPLOT, LAYOUT = position, XTITLE = var_arr[v])
v = v + 1

ENDFOREACH ;var

t1 = TEXT(510,445,  'Obs', FONT_SIZE=10, /DEVICE, FONT_COLOR = !COLOR.gray)

IF (KEYWORD_SET(BMP)) THEN BEGIN
	t2 = TEXT(555,445,  'MOR', FONT_SIZE=10, /DEVICE, FONT_COLOR = rgb[*,0]);!COLOR.blue)
	t3 = TEXT(555,430,  'NSSL',FONT_SIZE=10, /DEVICE, FONT_COLOR = rgb[*,1]);!COLOR.red)
	t4 = TEXT(555,415,  'MY',  FONT_SIZE=10, /DEVICE, FONT_COLOR = rgb[*,2]);!COLOR.cyan)
ENDIF

IF (KEYWORD_SET(PBL)) THEN BEGIN
	t2 = TEXT(565,455,  'YSU',  FONT_SIZE=10, /DEVICE, FONT_COLOR = rgb[*,0]);!COLOR.blue)
	t3 = TEXT(565,440,  'QNSE', FONT_SIZE=10, /DEVICE, FONT_COLOR = rgb[*,1]);!COLOR.red)
	t4 = TEXT(565,425,  'ACM2', FONT_SIZE=10, /DEVICE, FONT_COLOR = rgb[*,2]);!COLOR.cyan)
ENDIF

IF (KEYWORD_SET(CHEM)) THEN BEGIN
	t2 = TEXT(565,455,  'RACM', FONT_SIZE=10, /DEVICE, FONT_COLOR = rgb[*,0]);!COLOR.blue)
	t3 = TEXT(565,440,  'MOZ' , FONT_SIZE=10, /DEVICE, FONT_COLOR = rgb[*,1]);!COLOR.red)
	t4 = TEXT(565,425,  'CBMZ', FONT_SIZE=10, /DEVICE, FONT_COLOR = rgb[*,2]);!COLOR.cyan)
ENDIF


!P.MULTI = 0
IF KEYWORD_SET(eps) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																;Reset color table to linear ramp
	PS_OFF																						;Turn PS off
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)															;Write PNG file

END