PRO MAP_ISEN_TROP, event, scheme, start_date, end_date, $
	DOMAIN   = domain, $
	REGION   = region, $
	TRACER   = tracer, $
	THRES	 = thres, $
	TROP_REL = trop_rel,$
	ANOMALY  = anomaly, $
	ALT	     = alt, $
	PNG	     = png, $
	EPS   	 = eps


;+
; Name:
;		MAP_ISEN_TROP
; Purpose:
;		Calculated ozone gradients and map theta in areas of high ozone gradients. 
; Calling sequence:
;		MAP_OZONE_GRAD_THETA, run, scheme, start_date, end_date
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		scheme 	   : Model initial state. Typically 'morrison' or 'nssl'.
;		start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;		end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
;		isen	   : Isentrope at which to map ozone tracer values 
; Output:
;		2D contour map of theta.
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		TRACER  : Ozone tracer to use (UTLS_tracer is default).
; Author and history:
;		Daniel B. Phoenix	    2018-01-19. Currently works best with keyword ALT set.
;								
;								
;-

COMPILE_OPT IDL2																				;Set compile options

time0 = SYSTIME(/SECONDS)
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain
IF (N_ELEMENTS(tracer 	 ) EQ 0) THEN tracer 	 = 'UTLS_tracer'
IF (N_ELEMENTS(thres	 ) EQ 0) THEN thres	     = 5.0

outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/plots/ozone_grad_theta/'
FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(event, scheme, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(event, scheme, start_date, end_date, /DATE)	


FOREACH date, date_arr DO BEGIN
	PRINT, date
	
 	R 	    =  WRF_READ_VAR('REFL'           , date, event, scheme, DOMAIN=domain, INDICES=region)
	cloud   =  WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, DOMAIN=domain, INDICES=region)
    o3      = (WRF_READ_VAR('O3'             , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3					
 	o3_tr	= (WRF_READ_VAR(tracer           , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E3
    y       = (WRF_READ_VAR('Latitude'       , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    x       = (WRF_READ_VAR('Longitude'      , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    z       = (WRF_READ_VAR('Z'		         , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    ztrop1  = (WRF_READ_VAR('Z_trop'	     , date, event, scheme, DOMAIN=domain, INDICES=region)).values	
	theta   = (WRF_READ_VAR('T'			     , date, event, scheme, DOMAIN=domain, INDICES=region)).values					
	theta   = ((1000.0/(WRF_READ_VAR('P', date, event, scheme, DOMAIN = domain, INDICES=region)).values)^(!Rair/!Cp))*(theta)

	ztrop1   = MEDIAN(ztrop1, 100)
    dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
    xyz_trop = REBIN(ztrop1, dim[0], dim[1], dim[2], /SAMPLE)
 
	isen_trop = FLTARR(dim[0],dim[1])   
    FOR ii = 0, dim[0]-1 DO BEGIN
    	FOR jj = 0, dim[1]-1 DO BEGIN
    		kk = INDEX_OF_NEAREST_CRH(z[ii,jj,*],xyz_trop[ii,jj,*])
    		isen_trop [ii,jj] = theta[ii,jj,kk]
    	ENDFOR
    ENDFOR
 		
dim = SIZE(x, /DIMENSIONS)																			;Get dimension sizes

offset = 0
y0 = y[          offset ,          offset ]														;Set domain boundary points
y1 = y[          offset ,dim[1]-(1+offset)]
y2 = y[dim[0]-(1+offset),dim[1]-(1+offset)]
y3 = y[dim[0]-(1+offset),          offset ]
x0 = x[          offset ,          offset ]
x1 = x[          offset ,dim[1]-(1+offset)]
x2 = x[dim[0]-(1+offset),dim[1]-(1+offset)]
x3 = x[dim[0]-(1+offset),          offset ]

xc = INTERPOLATE(x, 0.5*(dim[0]-1), 0.5*(dim[1]-1))											;Get central grid point
yc = INTERPOLATE(y, 0.5*(dim[0]-1), 0.5*(dim[1]-1))

table   = [COLOR_24(200, 200, 200),VISUALIZE_88D_COLOR()]												;Set reflectivity color table
rlevels = [-100.0, 5.0*FINDGEN(N_ELEMENTS(table))]													;Set reflectivity contour levels
;rlevels = [-100.0, 5.0 + 5.0*FINDGEN(N_ELEMENTS(table))]											;***for comparison with t-matrix
wfactor = 400.0/(dim[0]) + 400.0/(dim[1])																	;Set map factor

dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string

;map_pos = [0.0, 0.0, 1.0, 1.0]																				;map position for paper
map_pos = [0.05, 0.15, 0.95, 0.95]																			;Set map position
bar_pos = [0.25, 0.10, 0.75, 0.12]																			;Set color bar position

IF KEYWORD_SET(z_buff) THEN BEGIN
	SET_PLOT, 'Z'																									;Output to Z buffer
	DEVICE, SET_PIXEL_DEPTH = 24, SET_RESOLUTION = [wfactor*(dim[0]), wfactor*(dim[1])], $	;Set device resolution and bit depth
		SET_CHARACTER_SIZE = [12, 20]
	!P.COLOR      = COLOR_24('black')																		;Foreground color
	!P.BACKGROUND = COLOR_24('white')																		;Background color
	!P.CHARSIZE   = 1.5																							;Set character size
	!P.FONT       = -1
ENDIF ELSE BEGIN
	IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN	
		PS_ON, FILENAME = epsfile, PAGE_SIZE = [4.0,4.0], MARGIN = 0.0, /INCHES;PAGE_SIZE = 0.001*dim*wfactor			;Switch to Postscript device
		DEVICE, /ENCAPSULATED
		!P.FONT     = 0																								;Hardware fonts
		!P.CHARSIZE = 0.75	
		IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
			LOAD_BASIC_COLORS																							;Load basic color definitions
	ENDIF ELSE BEGIN
		SET_PLOT, 'X'
		WINDOW, XSIZE = wfactor*(dim[0]), YSIZE = wfactor*(dim[1])										;Open graphics window
		!P.COLOR      = COLOR_24('black')																		;Foreground color
		!P.BACKGROUND = COLOR_24('white')																		;Background color
		!P.CHARSIZE   = 2.0		
		!P.FONT       = -1		
		thick_scale   = 1
	ENDELSE
ENDELSE

MAP_SET, yc, xc, 0, CONIC = 1, /NOBORDER, $																;Draw map
	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
	ISOTROPIC = 1, $
	TITLE     = 'WRF-' + scheme + ' ' + dom_string + ' valid ' + date_string, $
	LATLAB	  = 1, $
	LONLAB 	  = 1, $
	LABEL	  = 1, $
	POSITION  = map_pos


IF KEYWORD_SET(image) THEN BEGIN
	ij0 = CONVERT_COORD([(!X.WINDOW)[0],(!Y.WINDOW)[0]], /NORMAL, /TO_DEVICE)
	ij1 = CONVERT_COORD([(!X.WINDOW)[1],(!Y.WINDOW)[1]], /NORMAL, /TO_DEVICE)

	xsize = LONG(ij1[0] - ij0[0])
	ysize = LONG(ij1[1] - ij0[1])

	image0 = (MAX(R.values, DIM=3))[offset:(dim[0]-(1+offset)), offset:(dim[1]-(1+offset))]
	dim    = dim - (2*offset)
	
	IF KEYWORD_SET(eps) THEN $
		image0 = INTERPOLATE(image0, MAKEN(0, dim[0]-1, xsize/10), MAKEN(0, dim[1]-1, ysize/10), /GRID) ELSE $
		image0 = INTERPOLATE(image0, MAKEN(0, dim[0]-1, xsize   ), MAKEN(0, dim[1]-1, ysize   ), /GRID)

	image0 = IMAGE_24(COLOR_LOOKUP_24((image0 < 75.0), table[1:*], MIN = 0.0, MAX = 75.0, $
				MISSING = COLOR_24(200, 200, 200), /NAN))
	TV, image0, ij0[0], ij0[1], TRUE = 3, XSIZE = xsize, YSIZE = ysize, /DEVICE
ENDIF ELSE $	

	bad = WHERE(FINITE(map_o3,/NAN), bad_count, COMPLEMENT=good, NCOMPLEMENT=good_count)
	IF (bad_count GT 0) THEN BEGIN
		map_o3[bad ] = -100.0
		map_o3[good] = map_o3[good]
	ENDIF

;	 map_plot      = map_o3																			;Set map variable to cloud top altitude
	 map_plot 	   = isen_trop
;	 map_bar_title = 'Tracer = ' + STRING(tracer) + ', Threshold (K) = '	+ STRING(thres)												;Set color bar title
	 map_bar_title = 'Potential Temperature at tropopause'
	 map_bar_min   = 300.0																						;Set echo top minimum
	 map_bar_max   = 420.0																					;Set echo top maximum
	 map_bar_ticks = 4																						;Set number of color bar ticks
     map_table     = [COLOR_24(200, 200, 200),HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])]							;Set color table
     map_levels    = [-100.0,((map_bar_max-map_bar_min)/20.0)*FINDGEN(20)+320.0]								;Set contour levels

	 map_plot1      = 0.001*((MAX((cloud.values GE 1.0E-5)*z, DIM = 3, /NAN)) - xyz_trop)																;Set map variable to cloud top altitude
     map_table1     = COLOR_24('gray50')																;Set contour cross-section specs						;Set color table
     map_levels1    = 0.0	
	 cont_sect1_thick  = 2

	ic = WHERE(map_plot1 GE 0.0, ic_count, COMPLEMENT = oc, NCOMPLEMENT = oc_count)
	IF (ic_count GT 0) THEN BEGIN
		trop_arr[oc] = trop_arr[oc]
		trop_arr[ic] = 0.0
	ENDIF

	IF (KEYWORD_SET(TROP_REL)) THEN BEGIN
		map_plot      = 0.001*(ztrop1 - trop_arr)						;Set map variable to cloud top altitude

;;+ Not doing this filtering method anymore		
;		bad = WHERE(FINITE(map_plot,/NAN), bad_count, COMPLEMENT=good, NCOMPLEMENT=good_count)
;		IF (bad_count GT 0) THEN BEGIN
;			map_plot[bad ] = 0.0
;			map_plot[good] = map_plot[good]
;		ENDIF
;;-
		
	 	map_bar_title = 'dO3/dz gradient altitude relative to tropopause'														;Set color bar title
	 	map_bar_min   = -2.5																						;Set echo top minimum
	 	map_bar_max   = 2.5																				;Set echo top maximum
	 	map_bar_ticks = 3																						;Set number of color bar ticks
	 	map_table     = BLUE_GRAY_RED_24(10)							          					    ;Set color table
	 	map_levels    = [-2.5 + 0.5*FINDGEN(11)]  									              ;Set contour levels
	ENDIF

	IF KEYWORD_SET(ALT) THEN BEGIN
	 	map_bar_title = 'dO3/dz gradient altitude (km)'														;Set color bar title
	 	map_bar_min   = 5.0																					;Set echo top minimum
	 	map_bar_max   = 15.0																				;Set echo top maximum
	 	map_bar_ticks = 4																						;Set number of color bar ticks
		map_table     = [COLOR_24(200, 200, 200),HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])]												;Set reflectivity color table
		map_levels    = [-100.0, 0.5*FINDGEN(N_ELEMENTS(map_table))+5.0]													;Set reflectivity contour levels

		map_plot = 0.001*trop_arr
		
		IF (KEYWORD_SET(ANOMALY)) THEN BEGIN
			map_bar_title = 'dO3/dz gradient altitude anomaly (km)'														;Set color bar title
			map_bar_min   = -5.0
    		map_bar_max   = 5.0
    		map_table     = [HCL_COLOR_TABLE(21, HUE_RANGE = [100.0, 300.0])]												;Set reflectivity color table
    		map_levels    = [0.5*FINDGEN(N_ELEMENTS(map_table))-5.0]													;Set reflectivity contour levels

			trop_med = MEDIAN(trop_arr,33)
			map_plot = 0.001*(trop_arr - trop_med)
		ENDIF
	ENDIF 
	
	CONTOUR, map_plot, x, y, $																	;Contour values
             OVERPLOT  = 1, $
             FILL      = 1, $
             LEVELS    = map_levels, $
             C_COLOR   = map_table, $
             TITLE     = date_string, $
             POSITION  = map_pos

	CONTOUR, map_plot1, x, y, $														;Overplot contour cross-section
	         OVERPLOT  = 1, $
	         FOLLOW    = 1, $
             LEVELS    = map_levels1, $
	         C_COLOR   = map_table1, $
	         C_THICK   = cont_sect1_thick*thick_scale
	
USERSYM_CIRCLE, /FILL																					;Load plane symbol at flight path orientation

MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines

IF KEYWORD_SET(section) THEN BEGIN
	IF (run EQ '20110408') THEN ij = [100, 119, 300, 157]
	IF (run EQ '20110521') THEN ij = [150, 116, 350, 130]
	IF (run EQ '20110618') THEN ij = [040, 060, 240, 080]

	xysect = MAP_2POINTS((x)[ij[0],ij[1]],(y)[ij[0],ij[1]],$
								(x)[ij[2],ij[3]],(y)[ij[2],ij[3]], NPATH = 10)
	
	OPLOT, xysect[0,*], xysect[1,*], THICK = 4
	XYOUTS, xysect[0,0], xysect[1,0], 'A', ALIGN = 1
	XYOUTS, xysect[0,-1], xysect[1,-1], 'B', ALIGN = 0
ENDIF

MAP_SET, yc, xc, 0, CONIC = 1, $																				;Redraw map boundaries
	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
	ISOTROPIC = 1, $
	NOERASE   = 1, $
	LATLAB	  = 1, $
	LONLAB 	  = 1, $
	LABEL	  = 1, $
	POSITION  = map_pos

COLOR_BAR_24_KPB, map_table[1:*], OVER = map_table[-1], UNDER = map_table[0], $						;Draw map color bar
	TICKS = map_bar_ticks, $
	RANGE = [map_bar_min, map_bar_max], $
	TITLE = map_bar_title, $
	POSIT = bar_pos

date1   = MAKE_ISO_DATE_STRING(date,/COMPACT,/UTC)

epsfile = outdir +  date1  + '.eps'						;EPS filename
pdffile = outdir +  date1  + '.pdf'						;PDF filename
pngfile = outdir +  date1  + '.png'						;PNG filename

IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file

PRINT, ' '
PRINT, SYSTIME(/SECONDS)-time0

ENDFOREACH	
END