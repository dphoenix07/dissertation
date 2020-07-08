PRO H2O_COLUMN_MASS_TOTAL, event, scheme, start_date, end_date, $
	DOMAIN   = domain, $
	REGION   = region, $
	TROP_REL = trop_rel, $
	ECHO_TOP = echo_top, $
	PNG	     = png, $
	EPS   	 = eps


;+
; Name:
;		H2O_COLUMN_MASS_TOTAL
; Purpose:
;		Plot total mass of H2O in stratosphere column
; Calling sequence:
;		H2O_COLUMN_MASS_TOTAL, run, scheme, start_date, end_date
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		scheme 	   : Model initial state. Typically 'morrison' or 'nssl'.
;		start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;		end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
; Output:
;		Contour plot of total mass of h2o in column above tropopause
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel B. Phoenix	    2019-01-29. 
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20120519T2300Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20120520T0200Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain

IF (event EQ '20110518') THEN midlats = 1
IF (event EQ '20130805') THEN tropics = 1

outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/plots/h2o_strat_col_mass_filter/'
FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(event, scheme, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(event, scheme, start_date, end_date, /DATE)	

z = (WRF_READ_VAR('Z', date_arr[0], event, scheme, DOMAIN=domain, INDICES=region)).values
dim = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)

total_mass = FLTARR(dim[0],dim[1])
FOREACH date, date_arr DO BEGIN
	PRINT, date
	conv  = (WRF_READ_VAR('Updraft_tracer' , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    h2o    = (WRF_READ_VAR('H2O'           , date, event, scheme, DOMAIN=domain, INDICES=region)).values*1.0E6					
    y     = (WRF_READ_VAR('Latitude'       , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    x     = (WRF_READ_VAR('Longitude'      , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    z     = (WRF_READ_VAR('Z'		       , date, event, scheme, DOMAIN=domain, INDICES=region)).values
    ztrop = (WRF_READ_VAR('Z_trop'	       , date, event, scheme, DOMAIN=domain, INDICES=region)).values	
	cloud = (WRF_READ_VAR('CLOUD_MIX_TOTAL', date, event, scheme, DOMAIN=domain, INDICES=region)).values
    press = (WRF_READ_VAR('P'	           , date, event, scheme, DOMAIN=domain, INDICES=region)).values							
    temp  = (WRF_READ_VAR('T'	           , date, event, scheme, DOMAIN=domain, INDICES=region)).values						
  
    dim 	 = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later)
    ztrop    = MEDIAN(ztrop,30)																	;Filter tropopause values
    trop_3d  = REBIN(ztrop, dim[0], dim[1], dim[2], /SAMPLE)
    yy       = REBIN(y,     dim[0], dim[1], dim[2], /SAMPLE)
    xx       = REBIN(x,     dim[0], dim[1], dim[2], /SAMPLE)

	IF (KEYWORD_SET(tropics)) THEN igood = WHERE(trop_3d GE 14000.0, count, COMPLEMENT = ibad)
	IF (KEYWORD_SET(midlats)) THEN igood = WHERE(trop_3d LT 14000.0, count, COMPLEMENT = ibad)

	IF (count GT 0) THEN h2o [ibad] = !Values.F_NaN

	molec_weight = 18.0
    r_star = 8.314
	dx = 3000.0
    h2o_ugm3   = (h2o * press*1.0E2 * molec_weight) / (r_star * temp)

	tracer = 0.001*((MAX((conv GE 0.5)*z, DIM = 3, /NAN)) - trop_3d)									

 	;Filter out air that has not been influenced by convection
 	trp_conv = WHERE(conv GT 0.5, tp_good, COMPLEMENT=tp_bad)
 	
 	;Only do this if there IS convection
 	IF (tp_good GT 0) THEN BEGIN
        h2o_ugm3[trp_conv] = h2o[trp_conv]  
        h2o_ugm3[tp_bad]   = !Values.F_NaN

		;Filter out areas less than 750 m above the LRT
        trop = WHERE((z-trop_3d) GT 750.0, tr_count, COMPLEMENT = strat)	
        IF (tr_count GT 0) THEN BEGIN
        	h2o_ugm3[trop ] = h2o[trop]
        	h2o_ugm3[strat] = !Values.F_NaN
        ENDIF
 		
 		;Filter out air that is within cloud
 	    ncloud = WHERE(cloud LT 1.0E-5, ncloud_count, COMPLEMENT=cloud_h2o)
        IF (ncloud_count GT 0) THEN BEGIN
        	h2o_ugm3[ncloud   ] = h2o[ncloud]
        	h2o_ugm3[cloud_h2o] = !Values.F_NaN
        ENDIF
 
 		h2o_kg = FLTARR(dim[0],dim[1],dim[2])
   		FOR ii=0,dim[0]-1 DO BEGIN
   			FOR jj=0,dim[1]-1 DO BEGIN
   			index1= MAX(VALUE_LOCATE(REFORM(z[ii,jj,*],dim[2],1,1),REFORM(trop_3d[ii,jj,*],dim[2],1,1)))
				FOR kk=index1,dim[2]-5 DO BEGIN
					h2o_kg[ii,jj,kk]=h2o_ugm3[ii,jj,kk]*dx*dx*(z[ii,jj,kk+1]-z[ii,jj,kk])*1.0E-9 				;convert h2o in ug/m3 to kg using dz of layer 
   				ENDFOR
   			ENDFOR
   		ENDFOR
   		total_mass = total_mass + TOTAL(h2o_kg,3,/NAN)
   	ENDIF
        
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
    wfactor = 400.0/(dim[0]) + 400.0/(dim[1])																	;Set map factor
    
    dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
    date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string
    
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

     map_plot 	   = TOTAL(h2o_kg,3,/NAN)
	 map_bar_title = 'H2O Total Mass (kg)'														;Set color bar title
	 map_bar_min   = 0.0																						;Set echo top minimum
	 map_bar_max   = 500.0																					;Set echo top maximum
	 map_bar_ticks = 2																						;Set number of color bar ticks
	 map_table 	   = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])
	 map_levels    = ((map_bar_max-map_bar_min)/20.0)*FINDGEN(20)										;Set contour levels

	CONTOUR, map_plot, x, y, $																	;Contour values
        OVERPLOT  = 1, $
        FILL      = 1, $
        LEVELS    = map_levels, $
        C_COLOR   = map_table, $
        TITLE     = date_string, $
        POSITION  = map_pos

	CONTOUR, tracer, x, y, $																	;Contour values
        OVERPLOT  = 1, $
        FILL      = 0, $
        LEVELS    = 0.0, $
        THICK 	  = 1.5,$
        C_COLOR   = COLOR_24('gray60'), $
        TITLE     = date_string, $
        POSITION  = map_pos


MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines

MAP_SET, yc, xc, 0, CONIC = 1, $																				;Redraw map boundaries
	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
	ISOTROPIC = 1, $
	NOERASE   = 1, $
	POSITION  = map_pos
	
COLOR_BAR_24_KPB, map_table[1:*], OVER = map_table[-1], UNDER = map_table[0], $						;Draw map color bar
	TICKS = map_bar_ticks, $
	RANGE = [map_bar_min, map_bar_max], $
	TITLE = map_bar_title, $
	POSIT = bar_pos

date1   = MAKE_ISO_DATE_STRING(date,/COMPACT,/UTC)

epsfile = outdir +  'h2o_strat_mass_750m_' + date1+ '.eps'						;EPS filename
pdffile = outdir +  'h2o_strat_mass_750m_' + date1+ '.pdf'						;PDF filename
pngfile = outdir +  'h2o_strat_mass_750m_' + date1+ '.png'						;PNG filename

IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file
ENDFOREACH	

STOP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Now plot total mass over entire period

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
    wfactor = 400.0/(dim[0]) + 400.0/(dim[1])																	;Set map factor
    
    dom_string  = 'd' + STRING(domain, FORMAT="(I2.2)")													;Set domain string
    date_string = MAKE_ISO_DATE_STRING(date, PREC='MINUTE', /COMPACT, /UTC)							;Create date string
    
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

     map_plot 	   = total_mass
	 map_bar_title = 'H2O Total Mass (kg)'														;Set color bar title
	 map_bar_min   = 0.0																						;Set echo top minimum
	 map_bar_max   = 3000.0																					;Set echo top maximum
	 map_bar_ticks = 2																						;Set number of color bar ticks
	 map_table 	   = HCL_COLOR_TABLE(20, HUE_RANGE = [100.0, 300.0])
	 map_levels    = ((map_bar_max-map_bar_min)/20.0)*FINDGEN(20)										;Set contour levels

	CONTOUR, map_plot, x, y, $																	;Contour values
        OVERPLOT  = 1, $
        FILL      = 1, $
        LEVELS    = map_levels, $
        C_COLOR   = map_table, $
        TITLE     = date_string, $
        POSITION  = map_pos

MAP_CONTINENTS, /CONT, /USA																					;Draw continental outlines

MAP_SET, yc, xc, 0, CONIC = 1, $																				;Redraw map boundaries
	LIMIT     = [y0,x0,y1,x1,y2,x2,y3,x3], $
	ISOTROPIC = 1, $
	NOERASE   = 1, $
	POSITION  = map_pos
	
COLOR_BAR_24_KPB, map_table[1:*], OVER = map_table[-1], UNDER = map_table[0], $						;Draw map color bar
	TICKS = map_bar_ticks, $
	RANGE = [map_bar_min, map_bar_max], $
	TITLE = map_bar_title, $
	POSIT = bar_pos

date1   = MAKE_ISO_DATE_STRING(date,/COMPACT,/UTC)

epsfile = outdir +  'h2o_strat_total_mass_1.25km.eps'						;EPS filename
pdffile = outdir +  'h2o_strat_total_mass_1.25km.pdf'						;PDF filename
pngfile = outdir +  'h2o_strat_total_mass_1.25km.png'						;PNG filename


IF KEYWORD_SET(eps) OR KEYWORD_SET(pdf) THEN BEGIN
	IF (LONG((STRSPLIT(!VERSION.RELEASE, '.', /EXTRACT))[0]) LE 7) THEN $
		LOAD_BASIC_COLORS, /RESET																				;Reset color table to linear ramp
	PS_OFF																											;Turn PS off
	
	IF KEYWORD_SET(pdf) THEN PSTOPDF, epsfile, PDFFILE = pdffile, /DELETEPS						;Convert to PDF
ENDIF ELSE IF KEYWORD_SET(png) THEN $
	WRITE_PNG, pngfile, TVRD(TRUE = 1)																		;Write PNG file


STOP
END