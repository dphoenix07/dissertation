PRO PLOT_OZONE_ANOMALY, event, scheme, start_date, end_date, $
	DOMAIN     = domain, $
	REGION     = region, $
	TROP_REL   = trop_rel, $
	REFL	   = refl, $
	PNG	       = png, $
	EPS   	   = eps


;+
; Name:
;		PLOT_OZONE_ANOMALY
; Purpose:
;		Reads files produced by OZONE_ANOMALY_BOX to plot fields of wrapping depth.
; Calling sequence:
;		PLOT_OZONE_ANOMALY, run, scheme, start_date, end_date
; Input:
;		event      : Model simulation name. (e.g., '20120519')
;		scheme 	   : Model initial state. Typically 'morrison' or 'nssl'.
;		start_date : String variable of the start date requested (e.g., '20120519T2300Z')
;		end_date   : String variable of the end date requested (e.g., '20120520T0300Z')
; Output:
;		Map of ozone anomaly.
; Keywords:
;		DOMAIN  : Optional keyword to specify output domain name. Default is 'd01'.
;		VERBOSE : If set, message writing process.
; Author and history:
;		Daniel B. Phoenix	    2018-06-13. 
;-

COMPILE_OPT IDL2																				;Set compile options
	
IF (N_ELEMENTS(start_date) EQ 0) THEN start_date = '20110522T2000Z'								;Set default start date
IF (N_ELEMENTS(end_date  ) EQ 0) THEN end_date   = '20110524T0400Z'								;Set default end date
IF (N_ELEMENTS(domain    ) EQ 0) THEN domain     = 1											;Set default domain


outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/plots/utls_tracer_box3/'

FILE_MKDIR, outdir

IF domain EQ 1 THEN $
date_arr = MK_DATE_ARR_D01(event, scheme, start_date, end_date, /DATE)	

IF domain EQ 2 THEN $
date_arr = MK_DATE_ARR_D02(event, scheme, start_date, end_date, /DATE)	


FOREACH date, date_arr DO BEGIN
	map_plot2 = (WRF_READ_VAR('Wrapping_Depth'      , date, event, scheme, DOMAIN=domain, INDICES=region, WRAPPING = 1)).values
    y         = (WRF_READ_VAR('Latitude'            , date, event, scheme, DOMAIN=domain, INDICES=region, WRAPPING = 1)).values
    x         = (WRF_READ_VAR('Longitude'           , date, event, scheme, DOMAIN=domain, INDICES=region, WRAPPING = 1)).values
    z         = (WRF_READ_VAR('Z'                   , date, event, scheme, DOMAIN=domain, INDICES=region, WRAPPING = 1)).values
   ; xyz_trop  = (WRF_READ_VAR('Tropopause_Height'   , date, event, scheme, DOMAIN=domain, INDICES=region, WRAPPING = 1)).values

   ; cloud     = (WRF_READ_VAR('Cloud_Mixing_Ratio'  , date, event, scheme, DOMAIN=domain, INDICES=region, WRAPPING = 1))
	;R 		  = (WRF_READ_VAR('Reflectivity'        , date, event, scheme, DOMAIN=domain, INDICES=region, WRAPPING = 1))

      z_trop  = (WRF_READ_VAR('Z_trop'   , date, event, scheme, DOMAIN=domain, INDICES=region)).values

   cloud      = (WRF_READ_VAR('CLOUD_MIX_TOTAL'  , date, event, scheme, DOMAIN=domain, INDICES=region))
   R 		  = (WRF_READ_VAR('REFL'        , date, event, scheme, DOMAIN=domain, INDICES=region))
 
    dim 	  = SIZE(z, /DIMENSIONS)																		;Get dimensions (to reform arrays later) 
	
	z_trop = MEDIAN(z_trop,30)
	xyz_trop = REBIN(z_trop, dim[0],dim[1],dim[2],/SAMPLE)
     
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
    
    	 map_plot      = map_plot2														;Set map variable to cloud top altitude
    	 map_bar_title = 'Anvil Wrapping Depth (km)'														;Set color bar title
    	 map_bar_min   = 0.0																						;Set echo top minimum
    	 map_bar_max   = 4.0																					;Set echo top maximum
    	 map_bar_ticks = 3																						;Set number of color bar ticks
    	 map_table     = [COLOR_24(230, 230, 230),VISUALIZE_88D_COLOR(3)]							;Set color table
    	 map_levels    = [MAKEN(0,4.0,N_ELEMENTS(map_table))]								;Set contour levels
    
    	 map_plot1      = 0.001*((MAX((cloud.values GE 1.0E-5)*z, DIM = 3, /NAN)) - xyz_trop)															;Set map variable to cloud top altitude
         map_table1     = COLOR_24('gray50')																;Set contour cross-section specs						;Set color table
         map_levels1    = 0.0	
    	 cont_sect1_thick  = 2
    
    	IF (KEYWORD_SET(TROP_REL)) THEN BEGIN
    		map_plot      = 0.001*((MAX((cloud.values GE 1.0E-5)*z, DIM = 3, /NAN)) - xyz_trop)						;Set map variable to cloud top altitude
    		
    		bad = WHERE(FINITE(map_plot,/NAN), bad_count, COMPLEMENT=good, NCOMPLEMENT=good_count)
    		IF (bad_count GT 0) THEN BEGIN
    			map_plot[bad ] = 0.0
    			map_plot[good] = map_plot[good]
    		ENDIF
    		
    	 	map_bar_title = 'Cloud Top Relative-Altitude (km)'														;Set color bar title
    	 	map_bar_min   = -2.5																						;Set echo top minimum
    	 	map_bar_max   = 2.5																				;Set echo top maximum
    	 	map_bar_ticks = 3																						;Set number of color bar ticks
    	 	map_table     = BLUE_GRAY_RED_24(10)							          					    ;Set color table
    	 	map_levels    = [-2.5 + 0.5*FINDGEN(11)]  									              ;Set contour levels
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
    
     	table   = COLOR_24('red')												;Set reflectivity color table
    	rlevels = 30.0																					;Set reflectivity contour levels
 		table   = [COLOR_24(200, 200, 200),VISUALIZE_88D_COLOR()]												;Set reflectivity color table
		rlevels = [1.0+5.0*FINDGEN(N_ELEMENTS(table))]													;Set reflectivity contour levels
   
    	CONTOUR, MAX(R.values, DIM=3), x, y, $															;Contour reflectivity values
    		OVERPLOT  = 1, $
    		FILL      = 1, $
    		LEVELS    = rlevels, $
    		C_COLOR   = table, $
            POSITION  = map_pos
    
    
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
    
    IF KEYWORD_SET(refl) THEN BEGIN
    	COLOR_BAR_24_KPB, table[1:*], OVER = table[-1], $
    		RANGE = [0, 75], $
    		TICKS = 5, $
    		TITLE = 'Reflectivity (dBZ)', $
    		POSIT = bar_pos
    ENDIF ELSE BEGIN
    
    	COLOR_BAR_24_KPB, map_table[1:*], OVER = map_table[-1], UNDER = map_table[0], $						;Draw map color bar
    		TICKS = map_bar_ticks, $
    		RANGE = [map_bar_min, map_bar_max], $
    		TITLE = map_bar_title, $
    		POSIT = bar_pos
    ENDELSE

    date1   = MAKE_ISO_DATE_STRING(date,/COMPACT,/UTC)
	outdir  = !WRF_DIRECTORY + event + '/' + scheme + '/plots/utls_tracer_box3/'
     
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

	
ENDFOREACH	;date

END