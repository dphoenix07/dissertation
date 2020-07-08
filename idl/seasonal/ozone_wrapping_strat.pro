PRO OZONE_WRAPPING_STRAT, event, scheme, start_date, end_date, $
	DOMAIN   = domain, $
	REGION   = region, $
	TROP_REL = trop_rel, $
	REFL	 = refl, $
	PNG	     = png, $
	EPS   	 = eps


;+
; Name:
;		OZONE_WRAPPING
; Purpose:
;		Find occurrences of ozone wrapping and estimate scale. Calculate min potential
;		temperature surface corresponding to a non-zero stratosphere tracer at time when
;		convective tracer is zero. Then calculate min potential temperature surface during 
;		active convection.  
; Calling sequence:
;		OZONE_WRAPPING, run, scheme, start_date, end_date
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		scheme 	   : Model initial state. Typically 'morrison' or 'nssl'.
;		start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;		end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
; Output:
;		Number of grid points where ozone > 200 ppb is transported from stratosphere to
;		troposphere.
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel B. Phoenix	    2017-12-18. 
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


FOREACH date, date_arr DO BEGIN
	PRINT, date
	
	updraft = (WRF_READ_VAR('Updraft_tracer' , date, event, scheme, DOMAIN=domain, INDICES=region)).values
	conv    = (WRF_READ_VAR('Cloud_tracer'   , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    o3      = (WRF_READ_VAR('O3'             , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3					
    co      = (WRF_READ_VAR('CO'             , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3					
	st		= (WRF_READ_VAR('ST_tracer'      , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3
    y       = (WRF_READ_VAR('Latitude'       , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    x       = (WRF_READ_VAR('Longitude'      , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    z       = (WRF_READ_VAR('Z'		         , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    ztrop   = (WRF_READ_VAR('Z_trop'	     , date, event, scheme, DOMAIN=domain, INDICES=region)).values	
 	R 	    =  WRF_READ_VAR('REFL'           , date, event, scheme, DOMAIN=domain, INDICES=region)
	cloud   =  WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, DOMAIN=domain, INDICES=region)
    w       = (WRF_READ_VAR('w'	             , date, event, scheme, DOMAIN=domain, INDICES=region)).values
	theta   = (WRF_READ_VAR('T'			     , date, event, scheme, DOMAIN=domain, INDICES=region)).values					
	theta   = ((1000.0/(WRF_READ_VAR('P', date, event, scheme, DOMAIN = domain, INDICES=region)).values)^(!Rair/!Cp))*(theta)

    dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
    ztrop    = CALC_TROP_MODE(ztrop, scheme, threshold) 												;Filter tropopause values
    xyz_trop = REBIN(ztrop, dim[0], dim[1], dim[2], /SAMPLE)
    yy       = REBIN(y,     dim[0], dim[1], dim[2], /SAMPLE)
    xx       = REBIN(x,     dim[0], dim[1], dim[2], /SAMPLE)
    filt     = WHERE(xyz_trop EQ 999999 , filt_count, COMPLEMENT = good, NCOMPLEMENT = good_count)
    
    xyz_trop[good] = xyz_trop[good]
    xyz_trop[filt] = !Values.F_NaN
 
; 	strat    = WHERE((z - xyz_trop) GT 250.0, count, COMPLEMENT = non_strat)
; 	st_o3    = WHERE(st GT 0.125, count, COMPLEMENT = non_o3)
; 	trp_conv = WHERE((conv EQ 1), tp_good, COMPLEMENT=tp_bad)

;	goodc  = WHERE(((z - xyz_trop) GT 250.0) AND (updraft GT 0.125) AND (conv GT 0.99), countc , COMPLEMENT = badc )
;	goodnc = WHERE(((z - xyz_trop) GT 250.0) AND (updraft GT 0.125) AND (conv LT 0.01), countnc, COMPLEMENT = badnc)

	goodc  = WHERE(((z - xyz_trop) GT 250.0) AND (conv GT 0.99), countc , COMPLEMENT = badc )
	goodnc = WHERE(((z - xyz_trop) GT 250.0) AND (conv LT 0.01), countnc, COMPLEMENT = badnc)

	z_conv	    = FLTARR(dim[0],dim[1],dim[2])
	st_conv	    = FLTARR(dim[0],dim[1],dim[2])
	
	z_nconv	    = FLTARR(dim[0],dim[1],dim[2])
	st_nconv	= FLTARR(dim[0],dim[1],dim[2])

	z_conv [goodc] = z[goodc]
	st_conv[goodc] = 1.0 
	z_conv [badc ] = !Values.F_NaN
	st_conv[badc ] = !Values.F_NaN

	z_nconv [goodnc] = z[goodnc]
	st_nconv[goodnc] = 1.0 
	z_nconv [badnc ] = !Values.F_NaN
	st_nconv[badnc ] = !Values.F_NaN
	 	
	 	 
;	FOR ii = 0, dim[0]-1 DO BEGIN
;		FOR jj = 0, dim[1]-1 DO BEGIN
;			PRINT, 'Min altitude convection= ', MIN(st_conv [ii,jj,*] * z_conv [ii,jj,*], /NAN)
;			PRINT, 'Min altitude non-convec= ', MIN(st_nconv[ii,jj,*] * z_nconv[ii,jj,*], /NAN)
;			PRINT, ' ' 
;		ENDFOR
;	ENDFOR


	PRINT, 'Min altitude convection= ', MEAN(MIN(st_conv  * z_conv , /NAN), /NAN)
	PRINT, 'Min altitude non-convec= ', MEAN(MIN(st_nconv * z_nconv, /NAN), /NAN)

	PRINT, 'Min theta level convection= ', MEAN(MIN(st_conv  * theta, /NAN), /NAN)
	PRINT, 'Min theta level non-convec= ', MEAN(MIN(st_nconv * theta, /NAN), /NAN)
	
	PRINT, 'convective points (%) = ', FLOAT(countc)/FLOAT(N_ELEMENTS(z)) * 100.0
	PRINT, 'non-convective points (%) = ', FLOAT(countnc)/FLOAT(N_ELEMENTS(z)) * 100.0



;;	0.001*(MAX((cloud.values GE 1.0E-5)*z, DIM = 3, /NAN))	
;	cld_thres = WHERE(cld_arr GE 1.0E-5, count, COMPLEMENT=no_cld) 
;	IF (count GT 0) THEN BEGIN
;		cld_arr[cld_thres] = 1.0
;		cld_arr[no_cld   ] = !Values.F_NaN
;	ENDIF
;	
;	utls = WHERE((zc_arr GT 5000.0) AND ((xyz_trop- zc_arr GT 750.0)), COMPLEMENT = out)
;	zc_arr[utls] = zc_arr[utls]
;	zc_arr[out ] = !Values.F_NaN
;	zc_arr  	 = REFORM(zc_arr , dim[0], dim[1], dim[2]-1)
;	cld_arr 	 = REFORM(cld_arr, dim[0], dim[1], dim[2]-1)
;
;	o3	   [utls] = o3	   [utls]
;	updraft[utls] = updraft[utls]
;	
;	o3 	   [out] = 0.0
;	updraft[out] = 0.0
;
;	cloud_depth = FINDGEN(dim[0],dim[1]) * 0.0
;
;;; Find cloud depth
;	FOR i = 0, dim[0]-1 DO BEGIN
;		FOR j = 0, dim[1]-1 DO BEGIN
;			FOR k = 0, dim[2]-1 DO BEGIN
;				IF (cloud.values[i,j,k] GT 1.0E-5) THEN BEGIN
;					ks = k
;					WHILE (cloud.values[i,j,k] GT 1.0E-5) DO BEGIN
;					k = k + 1
;					ENDWHILE
;					ke = k
;					cloud_depth [i,j] = (z[i,j,ke] - z[i,j,ks])
;	 				IF (cloud_depth [i,j] GT (0.5*ztrop[i,j])) THEN updraft [i,j,*] = 0.0
;				ENDIF 
;			ENDFOR			
;		ENDFOR
;	ENDFOR
;	
;
;	w_thres  = WHERE(w GT 2.0, w_count)
;
;	updraft[w_thres] = 0.0
;	o3     [w_thres] = 0.0
;	
;	o3_thres = WHERE((o3 GT 200.0) AND (o3 LT 1000.0), count)	
;	up_thres = WHERE((updraft GT 0.1) AND (updraft LT 0.99), count1)
;	
;	FOR i = 0, count-1 DO BEGIN
;		icoung = WHERE(((o3_thres[i] - up_thres) LT 7), count)
;		IF (count GT 0) THEN PRINT, i, o3[o3_thres[i]], ' wrapping!'
;		IF (count EQ 0) THEN PRINT, i, ' no wrapping!'
;	ENDFOR
;	
;	wrap_arr = [ ]
;	dum 	 = FLTARR(dim[0],dim[1],dim[2])
;
;time0 = SYSTIME(/SECONDS)
;
;
;
;
; 
;    trop = WHERE((xyz_trop - z) GT 250.0, tr_count, COMPLEMENT = strat)	
;    IF (tr_count GT 0) THEN BEGIN
;    	tr[trop ] = tr[trop]
;    	tr[strat] = !Values.F_NaN
;    ENDIF
;    
;;    wrap = WHERE((tr GT 200.0) AND (co LT 90.0), wr_count)
; 	wrap = WHERE((tr GT 200.0), wr_count)
;    
;    PRINT, date
;    PRINT, 'qualified counts=', wr_count
;	PRINT, 'maximum o3 concentration (ppbv)=', MAX(tr,/NAN)
;	
;	max_tr  = MAX(tr,/NAN)
;	loc     = WHERE(tr EQ max_tr)
;	
;	PRINT, 'longitude=',xx[loc]
;	PRINT, 'latitude=', yy[loc]
;	PRINT, 'tropopause relative (m)=', xyz_trop[loc]-z[loc]
;
;
;bad = WHERE (R.values EQ 0.0, bad_count, COMPLEMENT = good, NCOMPLEMENT = good_count)
;R.values [bad ] = -35.0000
;R.values [good] = R.values [good]
;
;dim = SIZE(x, /DIMENSIONS)																			;Get dimension sizes
;
;offset = 0
;y0 = y[          offset ,          offset ]														;Set domain boundary points
;y1 = y[          offset ,dim[1]-(1+offset)]
;y2 = y[dim[0]-(1+offset),dim[1]-(1+offset)]
;y3 = y[dim[0]-(1+offset),          offset ]
;x0 = x[          offset ,          offset ]
;x1 = x[          offset ,dim[1]-(1+offset)]
;x2 = x[dim[0]-(1+offset),dim[1]-(1+offset)]
;x3 = x[dim[0]-(1+offset),          offset ]
;
;xc = INTERPOLATE(x, 0.5*(dim[0]-1), 0.5*(dim[1]-1))											;Get central grid point
;yc = INTERPOLATE(y, 0.5*(dim[0]-1), 0.5*(dim[1]-1))
;
;table   = [COLOR_24(200, 200, 200),VISUALIZE_88D_COLOR()]												;Set reflectivity color table
;rlevels = [-100.0, 5.0*FINDGEN(N_ELEMENTS(table))]													;Set reflectivity contour levels
;;rlevels = [-100.0, 5.0 + 5.0*FINDGEN(N_ELEMENTS(table))]											;***for comparison with t-matrix
;wfactor = 400.0/(dim[0]) + 400.0/(dim[1])																	;Set map factor
;
;dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
;date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string
;
;;map_pos = [0.0, 0.0, 1.0, 1.0]																				;map position for paper
;map_pos = [0.05, 0.15, 0.95, 0.95]																			;Set map position
;bar_pos = [0.25, 0.10, 0.75, 0.12]																			;Set color bar position
;
;IF KEYWORD_SET(z_buff) THEN BEGIN
;	SET_PLOT, 'Z'																									;Output to Z buffer
;	DEVICE, SET_PIXEL_DEPTH = 24, SET_RESOLUTION = [wfactor*(dim[0]), wfactor*(dim[1])], $	;Set device resolution and bit depth
;		SET_CHARACTER_SIZE = [12, 20]
;	!P.COLOR      = COLOR_24('black')																		;Foreground color
;	!P.BACKGROUND = COLOR_24('white')																		;Background color
;	!P.CHARSIZE   = 1.5																							;Set character size
;	!P.FONT       = -1
;ENDIF ELSE BEGIN
;	IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN	
;		PS_ON, FILENAME = epsfile, PAGE_SIZE = [4.0,4.0], MARGIN = 0.0, /INCHES;PAGE_SIZE = 0.001*dim*wfactor			;Switch to Postscript device
;		DEVICE, /ENCAPSULATED
;		!P.FONT     = 0																								;Hardware fonts
;		!P.CHARSIZE = 0.75	
;		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
;			LOAD_BASIC_COLORS																							;Load basic color definitions
;	ENDIF ELSE BEGIN
;		SET_PLOT, 'X'
;		WINDOW, XSIZE = wfactor*(dim[0]), YSIZE = wfactor*(dim[1])										;Open graphics window
;		!P.COLOR      = COLOR_24('black')																		;Foreground color
;		!P.BACKGROUND = COLOR_24('white')																		;Background color
;		!P.CHARSIZE   = 2.0		
;		!P.FONT       = -1		
;		thick_scale   = 1
;	ENDELSE
;ENDELSE
;
;MAP_SET, yc, xc, 0, CONIC = 1, /NOBORDER, $																;Draw map
;	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
;	ISOTROPIC = 1, $
;	TITLE     = 'WRF-' + scheme + ' ' + dom_string + ' valid ' + date_string, $
;	LATLAB	  = 1, $
;	LONLAB 	  = 1, $
;	LABEL	  = 1, $
;	POSITION  = map_pos
;
;
;IF KEYWORD_SET(image) THEN BEGIN
;	ij0 = CONVERT_COORD([(!X.WINDOW)[0],(!Y.WINDOW)[0]], /NORMAL, /TO_DEVICE)
;	ij1 = CONVERT_COORD([(!X.WINDOW)[1],(!Y.WINDOW)[1]], /NORMAL, /TO_DEVICE)
;
;	xsize = LONG(ij1[0] - ij0[0])
;	ysize = LONG(ij1[1] - ij0[1])
;
;	image0 = (MAX(R.values, DIM=3))[offset:(dim[0]-(1+offset)), offset:(dim[1]-(1+offset))]
;	dim    = dim - (2*offset)
;	
;	IF KEYWORD_SET(eps) THEN $
;		image0 = INTERPOLATE(image0, MAKEN(0, dim[0]-1, xsize/10), MAKEN(0, dim[1]-1, ysize/10), /GRID) ELSE $
;		image0 = INTERPOLATE(image0, MAKEN(0, dim[0]-1, xsize   ), MAKEN(0, dim[1]-1, ysize   ), /GRID)
;
;	image0 = IMAGE_24(COLOR_LOOKUP_24((image0 < 75.0), table[1:*], MIN = 0.0, MAX = 75.0, $
;				MISSING = COLOR_24(200, 200, 200), /NAN))
;	TV, image0, ij0[0], ij0[1], TRUE = 3, XSIZE = xsize, YSIZE = ysize, /DEVICE
;ENDIF ELSE $	
;
;	 map_plot      = 0.001*(MAX((cloud.values GE 1.0E-5)*z, DIM = 3, /NAN))						;Set map variable to cloud top altitude
;	 map_bar_title = 'Cloud Top Altitude (km)'														;Set color bar title
;	 map_bar_min   = 5.0																						;Set echo top minimum
;	 map_bar_max   = 20.0																					;Set echo top maximum
;	 map_bar_ticks = 3																						;Set number of color bar ticks
;	 map_table     = [COLOR_24(230, 230, 230),VISUALIZE_88D_COLOR(1)]							;Set color table
;	 map_levels    = [0.0, 5.0 + FINDGEN(N_ELEMENTS(map_table))]								;Set contour levels
;
;	IF (KEYWORD_SET(TROP_REL)) THEN BEGIN
;		map_plot      = 0.001*((MAX((cloud.values GE 1.0E-5)*z, DIM = 3, /NAN)) - xyz_trop)						;Set map variable to cloud top altitude
;		
;		bad = WHERE(FINITE(map_plot,/NAN), bad_count, COMPLEMENT=good, NCOMPLEMENT=good_count)
;		IF (bad_count GT 0) THEN BEGIN
;			map_plot[bad ] = 0.0
;			map_plot[good] = map_plot[good]
;		ENDIF
;		
;	 	map_bar_title = 'Cloud Top Relative-Altitude (km)'														;Set color bar title
;	 	map_bar_min   = -2.5																						;Set echo top minimum
;	 	map_bar_max   = 2.5																				;Set echo top maximum
;	 	map_bar_ticks = 3																						;Set number of color bar ticks
;	 	map_table     = BLUE_GRAY_RED_24(10)							          					    ;Set color table
;	 	map_levels    = [-2.5 + 0.5*FINDGEN(11)]  									              ;Set contour levels
;	ENDIF
;
;	IF KEYWORD_SET(refl) THEN BEGIN
;		table   = [COLOR_24(200, 200, 200),VISUALIZE_88D_COLOR()]												;Set reflectivity color table
;		rlevels = [-100.0, 5.0*FINDGEN(N_ELEMENTS(table))]													;Set reflectivity contour levels
;
;		CONTOUR, MAX(R.values, DIM=3), x, y, $															;Contour reflectivity values
;			OVERPLOT  = 1, $
;			FILL      = 1, $
;			LEVELS    = rlevels, $
;	        TITLE     = date_string, $
;			C_COLOR   = table, $
;      	    POSITION  = map_pos
;
;	ENDIF ELSE BEGIN
;	
;       	CONTOUR, map_plot, x, y, $																	;Contour values
;               OVERPLOT  = 1, $
;               FILL      = 1, $
;               LEVELS    = map_levels, $
;               C_COLOR   = map_table, $
;               TITLE     = date_string, $
;               POSITION  = map_pos
;	ENDELSE
;
;USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation
;
;IF (wr_count GT 0) THEN BEGIN
;;	FOR i=0, wr_count-1 DO BEGIN
;;		PLOTS, (xx)[wrap[i]], (yy)[wrap[i]], $																		;Overplot plane symbol
;;			PSYM    = 8, $
;;			SYMSIZE = 4 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
;;			NOCLIP  = 0, $
;;			COLOR   = COLOR_24('black')
;;	ENDFOR
;
;;	FOR i = 0, N_ELEMENTS(wrap_arr)-1 DO BEGIN
;;		PLOTS, (xx)[wrap_arr], (yy)[wrap_arr], $
;;			PSYM    = 8, $
;;			SYMSIZE = 4 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
;;			NOCLIP  = 0, $
;;			COLOR   = COLOR_24('darkgreen')
;;	ENDFOR
;
;	FOR i = 0, N_ELEMENTS(o3_thres)-1 DO BEGIN
;		PLOTS, (xx)[o3_thres[i]], (yy)[o3_thres[i]], $
;			PSYM    = 8, $
;			SYMSIZE = 4 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
;			NOCLIP  = 0, $
;			COLOR   = COLOR_24('orange')
;	ENDFOR	
;
;ENDIF
;
;;; plots max ozone wrapping location
;;PLOTS, (xx)[loc], (yy)[loc], $																		;Overplot plane symbol
;;	PSYM    = 8, $
;;	SYMSIZE = 4 - 4*(KEYWORD_SET(eps) OR KEYWORD_SET(pdf)), $
;;	NOCLIP  = 0, $
;;	COLOR   = COLOR_24('black')
;;;
;
;MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines
;
;IF KEYWORD_SET(section) THEN BEGIN
;	IF (run EQ '20110408') THEN ij = [100, 119, 300, 157]
;	IF (run EQ '20110521') THEN ij = [150, 116, 350, 130]
;	IF (run EQ '20110618') THEN ij = [040, 060, 240, 080]
;
;	xysect = MAP_2POINTS((x)[ij[0],ij[1]],(y)[ij[0],ij[1]],$
;								(x)[ij[2],ij[3]],(y)[ij[2],ij[3]], NPATH = 10)
;	
;	OPLOT, xysect[0,*], xysect[1,*], THICK = 4
;	XYOUTS, xysect[0,0], xysect[1,0], 'A', ALIGN = 1
;	XYOUTS, xysect[0,-1], xysect[1,-1], 'B', ALIGN = 0
;ENDIF
;
;MAP_SET, yc, xc, 0, CONIC = 1, $																				;Redraw map boundaries
;	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
;	ISOTROPIC = 1, $
;	NOERASE   = 1, $
;	LATLAB	  = 1, $
;	LONLAB 	  = 1, $
;	LABEL	  = 1, $
;	POSITION  = map_pos
;
;IF KEYWORD_SET(refl) THEN BEGIN
;	COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
;		RANGE = [0, 75], $
;		TICKS = 5, $
;		TITLE = 'Reflectivity (dBZ)', $
;		POSIT = bar_pos
;ENDIF ELSE BEGIN
;
;	COLOR_BAR_24_KPB, map_table[1:*], OVER = map_table[-1], UNDER = map_table[0], $						;Draw map color bar
;		TICKS = map_bar_ticks, $
;		RANGE = [map_bar_min, map_bar_max], $
;		TITLE = map_bar_title, $
;		POSIT = bar_pos
;ENDELSE
;
;IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
;	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
;		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
;	PS_OFF																											;Turn PS off
;	
;	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
;ENDIF ELSE IF KEYWORD_SET(png) THEN $
;	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file


ENDFOREACH	

END