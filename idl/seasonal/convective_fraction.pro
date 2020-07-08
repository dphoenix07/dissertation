PRO CONVECTIVE_FRACTION, event, scheme, start_date, end_date, $
	DOMAIN   = domain, $
	REGION   = region, $
	TROP_REL = trop_rel, $
	REFL	 = refl, $
	PNG	     = png, $
	EPS   	 = eps


;+
; Name:
;		CONVECTIVE_FRACTION
; Purpose:
;		Computes fraction of domain where updraft_tracer = 1
; Calling sequence:
;		CONVECTIVE_FRACTION, run, scheme, start_date, end_date
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		scheme 	   : Model initial state. Typically 'morrison' or 'nssl'.
;		start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;		end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
; Output:
;		Timeseries of fraction of convection in domain and chemical signature 
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel B. Phoenix	    2017-07-18. 
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain


outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/plots/chemistry_scatter/'
epsfile = outdir + scheme + '_' + end_date  + '.eps'						;EPS filename
pdffile = outdir + scheme + '_' + end_date  + '.pdf'						;PDF filename
pngfile = outdir + scheme + '_' + end_date  + '.png'						;PNG filename

FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(event, scheme, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(event, scheme, start_date, end_date, /DATE)	

conv_fraction = [ ]
max_h2o		  = [ ]
ave_h2o_nc	  = [ ]
ave_h2o_c	  = [ ]
ave_o3_nc	  = [ ]
ave_o3_c	  = [ ]

i = 0
FOREACH date, date_arr DO BEGIN
	PRINT, 'Processing: ', date
	updraft  = (WRF_READ_VAR('Updraft_tracer' , date, event, scheme, DOMAIN=domain, INDICES=region)).values
	h2o		 = (WRF_READ_VAR('H2O' 			  , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E6
	cld_conv = (WRF_READ_VAR('Cloud_tracer'   , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    o3       = (WRF_READ_VAR('O3'             , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3					
    co       = (WRF_READ_VAR('CO'             , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3					
;    tr      = (WRF_READ_VAR('UTLS_tracer'    , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3
    utls_o3 = (WRF_READ_VAR('UTLS_tracer'    , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3
    y       = (WRF_READ_VAR('Latitude'       , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    x       = (WRF_READ_VAR('Longitude'      , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    z       = (WRF_READ_VAR('Z'		         , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    ztrop   = (WRF_READ_VAR('Z_trop'	     , date, event, scheme, DOMAIN=domain, INDICES=region)).values	
	cloud   =  WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, DOMAIN=domain, INDICES=region)
 	theta	   	 = WRF_READ_VAR('T', date, event, scheme, DOMAIN = domain)							;Read temperature variable from WRF output
	theta.values = ((1000.0/(WRF_READ_VAR('P', date, event, scheme, DOMAIN = domain)).values)^(!Rair/!Cp))*(theta.values)

	dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)

 ;	zc_arr  = [ ]
;	cld_arr = [ ]
 ;	FOR k = 0, dim[2]-2 DO BEGIN
 ;		zc 		= (z[*,*,k] + z[*,*,k+1]) / 2
 ;		cld_c   = (cloud.values[*,*,k] + cloud.values[*,*,k+1]) / 2
 ;		zc_arr  = [[[zc_arr ]], [[zc   ]]]
 ;		cld_arr = [[[cld_arr]], [[cld_c]]]
 ;	ENDFOR

	convection = FLOAT(WHERE(updraft GT 0.1, count, COMPLEMENT=nconv))

	h2o_strat = WHERE(((theta.values GT 350.0) AND (theta.values LT 400.0)), sh2o_count, COMPLEMENT=h2o_trop)
	overshoot = WHERE(cloud.values[h2o_strat] GT 1.0E-5, count, COMPLEMENT = nover)
	
	conv_vol = FINDGEN(dim[0],dim[1],dim[2])*0.0
		
	conv_vol[overshoot ] = 1.0
	conv_vol[nover	   ] = 0.0
	
	volume1 = TOTAL(conv_vol * 3.0 * 3.0 * z)
	
	vol_size   = SIZE(conv_vol,/DIMENSIONS)
		
	total_size = TOTAL(3.0 * 3.0 * z)
	
;	conv_fraction = [TEMPORARY(conv_fraction), FLOAT((count/total_size))]
	conv_fraction = [TEMPORARY(conv_fraction), FLOAT((volume1/total_size))] 

	o3_convection = WHERE(cld_conv GT 0.1, count, COMPLEMENT=o3_nconv)

	dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
    ztrop    = CALC_TROP_MODE(ztrop, scheme, threshold) 												;Filter tropopause values
    xyz_trop = REBIN(ztrop, dim[0], dim[1], dim[2], /SAMPLE)
    filt     = WHERE(xyz_trop EQ 999999 , filt_count, COMPLEMENT = good, NCOMPLEMENT = good_count)
    
    xyz_trop[good] = xyz_trop[good]
    xyz_trop[filt] = !Values.F_NaN
 
 	h2o_c  = FINDGEN(dim[0],dim[1],dim[2])*0.0
 	h2o_nc = FINDGEN(dim[0],dim[1],dim[2])*0.0
 	o3_c   = FINDGEN(dim[0],dim[1],dim[2])*0.0
 	o3_nc  = FINDGEN(dim[0],dim[1],dim[2])*0.0
 	 	
 	h2o_c  [convection   ] = h2o[convection   ]
 	o3_c   [o3_convection] = o3 [o3_convection]
 	h2o_c  [nconv   	 ] = !Values.F_NaN
 	o3_c   [o3_nconv	 ] = !Values.F_NaN

 	h2o_nc [nconv   	 ] = h2o[nconv   ]
 	o3_nc  [o3_nconv	 ] = o3 [o3_nconv]
 	h2o_nc [convection   ] = !Values.F_NaN
 	o3_nc  [o3_convection] = !Values.F_NaN
 		
;	strat = WHERE((z-xyz_trop GT 990.0) AND (z-xyz_trop LT 1010.0), strat_count, COMPLEMENT=trop)
;	strat = WHERE((theta.values GT 370.0) AND (theta.values LT 400.0), strat_count, COMPLEMENT=trop)
	h2o_strat = WHERE(((theta.values GT 350.0) AND (theta.values LT 400.0)), sh2o_count, COMPLEMENT=h2o_trop)
	o3_trop   = WHERE((((xyz_trop - z) GT 0.0) AND ((xyz_trop - z) LT 5000.0)), so3_count, COMPLEMENT=o3_strat)
;	o3_trop   = WHERE((theta.values LT 310.0), so3_count, COMPLEMENT=o3_strat)

;    h2o_c [h2o_trop ] = !Values.F_NaN
; 	h2o_c [h2o_strat] = h2o_c[h2o_strat]
;    h2o_nc[h2o_trop ] = !Values.F_NaN
;    h2o_nc[h2o_strat] = h2o_nc[h2o_strat]
; 
;    o3_c  [o3_strat] = !Values.F_NaN
;    o3_c  [o3_trop ] = o3_c[o3_trop] 
;    o3_nc [o3_strat] = !Values.F_NaN
;    o3_nc [o3_trop ] = o3_nc[o3_trop]

    co  [o3_strat] = !Values.F_NaN
	
	max_h2o    = [TEMPORARY(max_h2o)   , MAX (h2o			   ,/NAN)]
	ave_h2o_c  = [TEMPORARY(ave_h2o_c ), MEAN(h2o_c [h2o_strat],/NAN)]
	ave_h2o_nc = [TEMPORARY(ave_h2o_nc), MEAN(h2o_nc[h2o_strat],/NAN)]
	ave_h2o_bg = [TEMPORARY(ave_h2o_bg), MEAN(h2o   [h2o_strat],/NAN)]
		
;	o3_co     = [TEMPORARY(o3_co    ), MEAN(o3/co	      ,/NAN)]
	ave_o3_c  = [TEMPORARY(ave_o3_c ), MEAN(o3_c[o3_trop] ,/NAN)]
	ave_o3_nc = [TEMPORARY(ave_o3_nc), MEAN(o3_nc[o3_trop],/NAN)]
	
ENDFOREACH	;date

delta_over = [ ]
delta_h2o  = [ ]

FOR i = 0, N_ELEMENTS(ave_o3_nc)-2 DO BEGIN
	delta_over = [TEMPORARY(delta_over), (conv_fraction[i+1]-conv_fraction[i])]
	delta_h2o  = [TEMPORARY(delta_h2o ), (ave_h2o_c[i+1]-ave_h2o_c[i])]
ENDFOR

color0 = COLOR_24('black')
color1 = COLOR_24('blue' )
color2 = COLOR_24('red'	 )
color3 = COLOR_24('darkgreen')

time = FINDGEN(N_ELEMENTS(date_arr))

PLOT, time, time, /NODATA, $																							;Set up plot
	THICK    = 2, $
	XRANGE   = [0, N_ELEMENTS(date_arr)-1], $
	XSTYLE   = 1, $
	YRANGE   = [0, 1], $
	YTICKS   = 1, $
	YTICKN   = [' ', ' '], $
	YSTYLE   = 1, $
	TITLE    = 'Convective Fraction vs H2O concentration'

AXIS, YAXIS = 0, $																								;Draw altitude axis
	SAVE   = 1, $
	YRANGE = [0.0, 0.1], $
	YTITLE = 'Convective Fraction (%)', $
	YTICKS = 1, $
	YSTYLE = 1
	COLOR  = color0

OPLOT, time, conv_fraction*100.0, COLOR = color0, THICK = 2													;Plot temperature measurments

;AXIS, YAXIS = 1, $																								;Draw temperature axis
;	SAVE   = 1, $
;	YRANGE = [50, 300], $
;	YTITLE = 'H2O concentration (ppmv)', $
;	YTICKS = 1, $
;	YSTYLE = 1, $
;	COLOR  = color1
;
;OPLOT, time, max_h2o, COLOR = color1, THICK = 2													;Plot temperature measurments

AXIS, YAXIS = 1, $																								;Draw temperature axis
	SAVE   = 1, $
	YRANGE = [0, 10], $
	YTITLE = 'H2O concentration (ppmv)', $
	YTICKS = 1, $
	YSTYLE = 1, $
	COLOR  = color1

OPLOT, time, ave_h2o_c , COLOR = color1, THICK = 2, LINESTYLE = 0													;Plot temperature measurments
OPLOT, time, ave_h2o_nc, COLOR = color1, THICK = 2, LINESTYLE = 1													;Plot temperature measurments
OPLOT, time, ave_h2o_bg, COLOR = color2, THICK = 2, LINESTYLE = 0 

AXIS, YAXIS = 0, $																								;Draw temperature axis
	SAVE   = 1, $
	YRANGE = [-0.5, 0.5], $
	YTITLE = 'delta H2O', $
	YTICKS = 1, $
	YSTYLE = 1, $
	COLOR  = color3

OPLOT, time, delta_h2o/(MEAN(ave_h2o_bg)), COLOR = color3, THICK = 3, LINESTYLE = 0

AXIS, YAXIS = 2, $																								;Draw temperature axis
	SAVE   = 1, $
	YRANGE = [-2.0, 2.0], $
	YTITLE = 'delta overshoot', $
	YTICKS = 1, $
	YSTYLE = 1, $
	COLOR  = color3
OPLOT, time, (delta_over*100.0)/MEAN(conv_fraction*100.0), COLOR = color3, THICK = 3, LINESTYLE = 1

; AXIS, YAXIS = 2, $																								;Draw temperature axis
; 	SAVE   = 1, $
; 	YRANGE = [10, 200], $
; 	YTITLE = 'O3 concentration (ppbv)', $
; 	YTICKS = 1, $
; 	YSTYLE = 1, $
; 	COLOR  = color2
; 
; OPLOT, time, ave_o3_c , COLOR = color2, THICK = 2,  LINESTYLE = 0													;Plot temperature measurments
; OPLOT, time, ave_o3_nc, COLOR = color2, THICK = 2,  LINESTYLE = 1													;Plot temperature measurments


IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file

STOP	

END